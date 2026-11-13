import Models.*
import SkillsJarService.{NotFound, ServiceError}
import com.jamesward.zio_mavencentral.MavenCentral
import zio.http.*
import zio.http.codec.*
import zio.http.endpoint.Endpoint
import zio.schema.*
import zio.schema.codec.JsonCodec

/** HTTP API descriptions. The representation (HTML, markdown, or JSON) is chosen from the request's
  * `Accept` header by zio-http's content negotiation. HTML is listed first so it is the default for
  * browsers and for wildcard, missing, or unsupported `Accept` headers.
  */
object Endpoints:

  case class SearchResults(query: Option[String], skillsJars: Seq[SkillsJar], buildTool: BuildTool)

  case class LatestVersion(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version) derives Schema

  given Schema[MavenCentral.GroupId] = Schema[String].transform(MavenCentral.GroupId(_), _.toString)
  given Schema[MavenCentral.ArtifactId] = Schema[String].transform(MavenCentral.ArtifactId(_), _.toString)
  given Schema[MavenCentral.Version] = Schema[String].transform(MavenCentral.Version(_), _.toString)

  // JSON wire formats, kept separate from the domain types so the API contract is explicit.
  private case class SkillsJarJson(
    groupId: MavenCentral.GroupId,
    artifactId: MavenCentral.ArtifactId,
    name: String,
    description: String,
    versions: List[MavenCentral.Version],
    securityScanned: Boolean,
    allowedTools: Map[String, String],
  ) derives Schema

  private case class SearchResultsJson(query: Option[String], skillsJars: List[SkillsJarJson]) derives Schema

  private case class ErrorJson(error: String) derives Schema

  private def toJson(sj: SkillsJar): SkillsJarJson =
    SkillsJarJson(sj.groupId, sj.artifactId, sj.name, sj.description, sj.versions.toList, sj.securityScanned, sj.allowedTools)

  private def fromJson(json: SkillsJarJson): SkillsJar =
    SkillsJar(json.groupId, json.artifactId, json.versions, json.name, json.description, json.securityScanned, json.allowedTools)

  private val searchResultsJsonSchema: Schema[SearchResults] =
    Schema[SearchResultsJson].transform(
      // The build tool only selects HTML dependency snippets, so it is not part of the JSON.
      json => SearchResults(json.query, json.skillsJars.map(fromJson), BuildTool.Maven),
      results => SearchResultsJson(results.query, results.skillsJars.map(toJson).toList),
    )

  def serviceErrorMessage(error: ServiceError): String =
    error match
      case ServiceError.FetchFailed(_) => "Failed to fetch SkillsJars from Maven Central"

  private val serviceErrorJsonSchema: Schema[ServiceError] =
    Schema[ErrorJson].transform(json => ServiceError.FetchFailed(json.error), e => ErrorJson(serviceErrorMessage(e)))

  private val notFoundJsonSchema: Schema[NotFound] =
    Schema[ErrorJson].transformOrFail(
      _ => Left("A NotFound can not be decoded from JSON"),
      nf => Right(ErrorJson(s"No SkillsJar found for ${nf.groupId}:${nf.artifactId}")),
    )

  // zio-http's default codec error body includes internal class names, so requests it can't
  // decode get the same {"error": ...} shape as the endpoints' own errors.
  def requestErrorMessage(error: HttpCodecError): String =
    error match
      case HttpCodecError.MalformedQueryParam(_, cause) => cause.message
      case HttpCodecError.MissingQueryParam(name) => s"Missing query parameter $name"
      case _ => "Invalid request"

  private val requestErrorJsonSchema: Schema[HttpCodecError] =
    Schema[ErrorJson].transform(json => HttpCodecError.CustomError("RequestError", json.error), e => ErrorJson(requestErrorMessage(e)))

  private def requestErrorCodec(content: HttpContentCodec[HttpCodecError]): HttpCodec[HttpCodecType.ResponseType, HttpCodecError] =
    HttpCodec.content(using content) ++ HttpCodec.status(Status.BadRequest)

  private val markdownMediaTypes = List(Markdown.mediaType, MediaType("text", "x-markdown"))

  private def json[A](schema: Schema[A]): (MediaType, BinaryCodecWithSchema[A]) =
    MediaType.application.json -> BinaryCodecWithSchema(config => JsonCodec.schemaBasedBinaryCodec(config.schemaConfig)(using schema), schema)

  // HTML and markdown are rendered views of a value, so they only encode.
  private def renderOnly[A](mediaType: MediaType)(render: A => String): (MediaType, BinaryCodecWithSchema[A]) =
    val schema = Schema[String].transformOrFail[A](_ => Left(s"${mediaType.fullType} can not be decoded"), a => Right(render(a)))
    mediaType -> BinaryCodecWithSchema(TextBinaryCodec.fromSchema(using schema), schema)

  private def html[A](render: A => zio.http.template2.Dom): (MediaType, BinaryCodecWithSchema[A]) =
    renderOnly[A](MediaType.text.html)(a => "<!DOCTYPE html>" + render(a).render)

  private def markdown[A](render: A => String): List[(MediaType, BinaryCodecWithSchema[A])] =
    markdownMediaTypes.map(renderOnly[A](_)(render))

  private def searchResultsCodec(tailwind: URL): HttpContentCodec[SearchResults] =
    HttpContentCodec.from(
      html(r => UI.index(r.skillsJars, r.query, r.buildTool, tailwind)),
      json(searchResultsJsonSchema) +: markdown[SearchResults](r => Markdown.skillsList(r.skillsJars, r.query))*,
    )

  private def searchErrorCodec(tailwind: URL): HttpContentCodec[ServiceError] =
    HttpContentCodec.from(
      html(e => UI.index(Seq.empty, None, BuildTool.Maven, tailwind, Some(e))),
      json(serviceErrorJsonSchema) +: markdown[ServiceError](e => s"# Error\n\n${serviceErrorMessage(e)}\n")*,
    )

  private def searchRequestErrorCodec(tailwind: URL): HttpContentCodec[HttpCodecError] =
    HttpContentCodec.from(
      html(e => UI.requestError(requestErrorMessage(e), tailwind)),
      json(requestErrorJsonSchema) +: markdown[HttpCodecError](e => s"# Invalid request\n\n${requestErrorMessage(e)}\n")*,
    )

  def search(tailwind: URL) =
    given HttpContentCodec[SearchResults] = searchResultsCodec(tailwind)
    given HttpContentCodec[ServiceError] = searchErrorCodec(tailwind)
    Endpoint(Method.GET / Root)
      .query(HttpCodec.query[String]("q").optional)
      .query(HttpCodec.query[BuildTool]("bt").optional)
      .out[SearchResults]
      .outError[ServiceError](Status.BadGateway)
      .outCodecError(requestErrorCodec(searchRequestErrorCodec(tailwind)))

  private val groupIdCodec = PathCodec.string("groupId").transform(MavenCentral.GroupId(_))(_.toString)
  private val artifactIdCodec = PathCodec.string("artifactId").transform(MavenCentral.ArtifactId(_))(_.toString)

  val latest =
    given HttpContentCodec[LatestVersion] = HttpContentCodec.json.only[LatestVersion]
    Endpoint(Method.GET / groupIdCodec / artifactIdCodec / "latest")
      .out[LatestVersion]
      .outErrors[ServiceError | NotFound](
        HttpCodec.content[ServiceError](using HttpContentCodec.from(json(serviceErrorJsonSchema))) ++ HttpCodec.status(Status.BadGateway),
        HttpCodec.content[NotFound](using HttpContentCodec.from(json(notFoundJsonSchema))) ++ HttpCodec.status(Status.NotFound),
      )
      .outCodecError(requestErrorCodec(HttpContentCodec.from(json(requestErrorJsonSchema))))
