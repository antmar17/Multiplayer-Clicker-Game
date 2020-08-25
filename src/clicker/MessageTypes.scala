package clicker

import clicker.game.GameActor

// Received by  GameActors
case object Update
case object Click
case class BuyEquipment(equipmentId: String)
case object Save
case class Load(stats:String)

// Received by ClickerServer
case object UpdateGames
case object SaveGames
case class GameState(gameState: String)

case class sendClick(gameActor: GameActor)
case class sendBuy(equipmentId: String)
case class GameSave(username:String,gameState:String,config:String)

