import org.webjars.WebJarVersionLocator
import zio.http.{Path, Routes, Scheme, URL}

class WebJarsDev extends WebJars:
  private val locator = WebJarVersionLocator()
  private val baseUrl = URL(Path("/webjars"))

  override def url(webjar: String, path: String): URL =
    baseUrl.addPath(locator.path(webjar, path))

  override def routes[Any, Nothing]: Routes[Any, Nothing] = Routes.serveResources(Path.empty / "webjars", WebJarVersionLocator.WEBJARS_PATH_PREFIX)
