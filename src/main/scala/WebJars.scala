import zio.http.*
import org.webjars.WebJarVersionLocator

trait WebJars {
  def url(webjar: String, path: String): URL
  def routes[Env, Err]: Routes[Env, Err]
}

class WebJarsLive extends WebJars:
  private val locator = WebJarVersionLocator()
  private val baseUrl = URL(Path("/webjars"), URL.Location.Absolute(Scheme.HTTPS, "cdn.jsdelivr.net", None))

  override def url(webjar: String, path: String): URL =
    baseUrl.addPath(locator.groupId(webjar)).addPath(locator.path(webjar, path))

  override def routes[Env, Err]: Routes[Any, Nothing] = Routes.empty
