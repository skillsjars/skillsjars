import zio.*
import zio.direct.*
import zio.http.*
import zio.http.Header.Authorization
import zio.schema.*
import zio.schema.annotation.*

trait HerokuInference:
  def req(prompt: String): ZIO[Client & Scope, Throwable, HerokuInference.MessageResponse]

object HerokuInference:
  case class Message(role: String = "user", content: String) derives Schema

  case class InferenceRequest(model: String, messages: List[Message]) derives Schema

  case class MessageChoice(
                            index: Int,
                            message: Message,
                            @fieldName("finish_reason") finishReason: String
                          ) derives Schema

  case class MessageUsage(
                           @fieldName("prompt_tokens") promptTokens: Int,
                           @fieldName("completion_tokens") completionTokens: Int,
                           @fieldName("total_tokens") totalTokens: Int
                         ) derives Schema

  case class MessageResponse(
                              id: String,
                              `object`: String,
                              created: Int,
                              model: String,
                              choices: List[MessageChoice],
                              usage: MessageUsage
                            ) derives Schema

  case class Live(inferenceUrl: String, inferenceKey: String, inferenceModelId: String) extends HerokuInference:
    private val url = URL.decode(inferenceUrl).getOrElse(throw new RuntimeException("invalid inference url"))

    def req(prompt: String): ZIO[Client & Scope, Throwable, MessageResponse] =
      defer:
          val client = ZIO.serviceWith[Client](_.addHeader(Authorization.Bearer(token = inferenceKey)).url(url)).run
          val inferenceRequest = InferenceRequest(inferenceModelId, List(Message(content = prompt)))
          val resp = client.post("/v1/chat/completions")(Body.json(inferenceRequest)).run
          if !resp.status.isSuccess then
            val errorBody = resp.body.asString.run
            ZIO.logError(s"Inference request failed.\n${prompt}\nStatus ${resp.status}\n$errorBody").run
            ZIO.fail(RuntimeException(s"Inference request failed with status ${resp.status}: $errorBody")).run
          else
            resp.body.asJson[MessageResponse].run

  val live: ZLayer[Any, Throwable, HerokuInference] =
    ZLayer.fromZIO:
      defer:
        val system = ZIO.system.run
        val inferenceUrl = system.env("INFERENCE_URL").map(_.filter(_.nonEmpty)).someOrFail(new RuntimeException("INFERENCE_URL env var not set")).run
        val inferenceKey = system.env("INFERENCE_KEY").map(_.filter(_.nonEmpty)).someOrFail(new RuntimeException("INFERENCE_KEY env var not set")).run
        val inferenceModelId = system.env("INFERENCE_MODEL_ID").map(_.filter(_.nonEmpty)).someOrFail(new RuntimeException("INFERENCE_MODEL_ID env var not set")).run
        HerokuInference.Live(inferenceUrl, inferenceKey, inferenceModelId)
