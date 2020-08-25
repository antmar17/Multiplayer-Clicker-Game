package tests

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import clicker._
import clicker.game.GameActor
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration._
import scala.io.Source


class TestClick extends TestKit(ActorSystem("TestGame"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Clicker actor" must {
    "react to clicks and equipment purchases" in {

      val configuration: String = Source.fromFile("goldConfig.json").mkString
      val gameActor = system.actorOf(Props(classOf[GameActor], "test", configuration))

      gameActor ! Click

      expectNoMessage(200.millis)


      gameActor ! Update
      var state: GameState = expectMsgType[GameState](1000.millis)

      var jsonState = state.gameState
      var gameState: JsValue = Json.parse(jsonState)
      println(gameState)
      var gold = (gameState \ "currency").as[Double]
      val expectedGold = 1.0
      assert(equalDoubles(gold, expectedGold))

      //test Buy Equipment get 10 gold to buy shovel
      //-----------------------------------------------------------------
      for(i<- 0 until 9){
        gameActor ! Click

        expectNoMessage(200.millis)

      }
      gameActor ! Update


      state= expectMsgType[GameState](1000.millis)
      jsonState = state.gameState
      gameState = Json.parse(jsonState)
      gold = (gameState \ "currency").as[Double]
      assert(equalDoubles(gold, 10))


      //use 10 gold to get shovel
      gameActor ! BuyEquipment("shovel")

      expectNoMessage(200.millis)

      gameActor ! Click
      expectNoMessage(200.millis)


      gameActor!Update
      state= expectMsgType[GameState](1000.millis)
      jsonState = state.gameState
      gameState = Json.parse(jsonState)
      gold = (gameState \ "currency").as[Double]
      println(gold)
      assert(equalDoubles(gold, 2))

//-----------------------------------------------------------------






 //Test buying Excator as well as gold per second
//-----------------------------------------------------------------
      println("long loop begin")

      for(i<- 0 until 200){
        gameActor ! Click

        expectNoMessage(200.millis)
        gameActor !Update
        state= expectMsgType[GameState](1000.millis)


      }
      println("long loop OVA")



      gameActor ! BuyEquipment("excavator")

      expectNoMessage(200.millis)
      gameActor!Update
      state= expectMsgType[GameState](1000.millis)
      jsonState = state.gameState
      gameState = Json.parse(jsonState)
      gold = (gameState \ "currency").as[Double]
      println(gold)









    }
  }
}
