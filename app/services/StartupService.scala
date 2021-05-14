package services

import akka.actor.{ActorRef, ActorSystem, Props}
import javax.inject._
import play.api.Logger
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import network.Client

@Singleton
class StartupService @Inject()(appLifecycle: ApplicationLifecycle, system: ActorSystem, node: Client)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()


  appLifecycle.addStopHook { () =>
    logger.info("App stopped!")
    Future.successful(())
  }
}
