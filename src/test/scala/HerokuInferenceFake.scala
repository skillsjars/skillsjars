import zio.*
import zio.http.*

class HerokuInferenceFake(responseFn: String => String = _ => """[]""") extends HerokuInference:
    def req(prompt: String): ZIO[Client & Scope, Throwable, HerokuInference.MessageResponse] =
      ZIO.attempt:
        val messageBody = responseFn(prompt)

        HerokuInference.MessageResponse(
          "mock",
          "chat.completion",
          1745619466,
          "mock",
          List(
            HerokuInference.MessageChoice(
              0,
              HerokuInference.Message("assistant", messageBody),
              "stop"
            )
          ),
          HerokuInference.MessageUsage(0, 0, 0)
        )

object HerokuInferenceFake:
  val layer: ZLayer[Any, Nothing, HerokuInference] = ZLayer.succeed(HerokuInferenceFake())
  def layerWith(fn: String => String): ZLayer[Any, Nothing, HerokuInference] = ZLayer.succeed(HerokuInferenceFake(fn))
