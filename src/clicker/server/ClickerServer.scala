package clicker.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestKit
import clicker.{BuyEquipment, Click, GameSave, GameState, Load, Save, SaveGames, Update, UpdateGames, sendClick}
import clicker.game.GameActor
import com.corundumstudio.socketio.listener.{ConnectListener, DataListener, DisconnectListener}
import com.corundumstudio.socketio.{AckRequest, Configuration, SocketIOClient, SocketIOServer}
import play.api.libs.json.{JsValue, Json}
import java.sql.{Connection, DriverManager, ResultSet}

import scala.io.Source


class ClickerServer(val configuration: String) extends Actor {



  val system = ActorSystem("FirstSystem")

  var socketToUsername: Map[SocketIOClient, String] = Map()
  var usernameToSocket: Map[ String,SocketIOClient] = Map()
  var usernameToActor: Map[ String,ActorRef] = Map()
  var socketToActor:Map[SocketIOClient,ActorRef]=Map()
  var actorToSocket:Map[ActorRef,SocketIOClient]=Map()
  var actorToUsername:Map[ActorRef,String]=Map()

  //DATABASE STUFF
  //---------------------------------------------------------------------------------------
  var ExistingUsernames:List[String]=ClickerDatabase.getUsernames()




  //---------------------------------------------------------------------------------------


  val config: Configuration = new Configuration {
    setHostname("localhost")
    setPort(8080)
  }

  val server: SocketIOServer = new SocketIOServer(config)
  server.start()



  server.addConnectListener(new ConnectionListener(this))
  server.addDisconnectListener(new DisconnectionListener(this))


  server.addEventListener("startGame",classOf[String],new StartGameListener(this))
  server.addEventListener("click",classOf[Nothing],new ClickListener(this))
  server.addEventListener("buy",classOf[String],new BuyListener(this))









  override def receive: Receive = {
    case UpdateGames=>for(i<-usernameToActor){i._2 ! Update}


    case gameState: GameState=>
      val socket=actorToSocket(sender())
      socket.sendEvent("gameState",gameState.gameState)



    case SaveGames=>for(i<-usernameToActor){i._2 ! Save}


    case gs:GameSave=>

      if(!ExistingUsernames.contains(gs.username)) {
        ClickerDatabase.pushNewPlayertoDB(gs.username, gs.gameState, gs.config)
        ExistingUsernames=ExistingUsernames:+gs.username
        println("new user! : "+gs.username)

      }
      else{

        ClickerDatabase.updatePlayer(gs.username,gs.gameState,gs.config)

      }
  }


  override def postStop(): Unit = {
    println("stopping server")
    server.stop()
  }
}



class ConnectionListener(server: ClickerServer) extends ConnectListener {
  override def onConnect(socket: SocketIOClient): Unit = {
    println("user connected: " + socket.getSessionId)
  }
}


class DisconnectionListener(server: ClickerServer) extends DisconnectListener {
  override def onDisconnect(socket: SocketIOClient): Unit = {
    println(server.usernameToActor)
    println("user disconnected: " + socket.getSessionId)

  }
}


class StartGameListener(server: ClickerServer) extends DataListener[String] {
  override def onData(socket: SocketIOClient, username: String, ackRequest: AckRequest): Unit = {


    //new username get pushed to List in recieve method in server so dont worry about it here
  //if new player
    if(!server.ExistingUsernames.contains(username)) {
      val actor = server.system.actorOf(Props(classOf[GameActor], username, server.configuration))


      server.usernameToSocket += (username -> socket)
      server.usernameToActor += (username -> actor)
      server.socketToUsername += (socket -> username)
      server.socketToActor += (socket -> actor)
      server.actorToSocket += (actor -> socket)
      server.actorToUsername+=(actor->username)

      socket.sendEvent("initialize",server.configuration)

    }
      //if existing player
    else{
      val config=ClickerDatabase.getConfig(username)
      val stats=ClickerDatabase.getStats(username)

      val actor1 = server.system.actorOf(Props(classOf[GameActor], username, config))


      server.usernameToSocket += (username -> socket)
      server.usernameToActor += (username -> actor1)
      server.socketToUsername += (socket -> username)
      server.socketToActor += (socket -> actor1)
      server.actorToSocket += (actor1 -> socket)
      server.actorToUsername+=(actor1->username)
      actor1 ! Load(stats)

      socket.sendEvent("initialize",config)
    }



  }
}

class ClickListener(server: ClickerServer) extends DataListener[Nothing] {
  override def onData(socket: SocketIOClient, data: Nothing, ackRequest: AckRequest): Unit = {
    if(server.socketToActor.contains(socket)){

      var a=server.socketToActor(socket)
      a ! Click
    }

  }

}


class BuyListener(server: ClickerServer) extends DataListener[String] {
  override def onData(socket: SocketIOClient, equipmentID: String, ackRequest: AckRequest): Unit = {
    if(server.socketToActor.contains(socket)){

      var b=server.socketToActor(socket)
      b ! BuyEquipment(equipmentID)
    }

  }

}


object ClickerServer {

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher

    import scala.concurrent.duration._

    val configuration: String = Source.fromFile("goldConfig.json").mkString

    val server = actorSystem.actorOf(Props(classOf[ClickerServer], configuration))

    actorSystem.scheduler.schedule(0.milliseconds, 100.milliseconds, server, UpdateGames)
    actorSystem.scheduler.schedule(0.milliseconds, 500.milliseconds, server, SaveGames)

  }
}

object ClickerDatabase {

  // connect to the database named "mysql" on the localhost
  val driver = "com.mysql.jdbc.Driver"

  val url = "jdbc:mysql://localhost:3306/sys?serverTimezone=UTC"
  val username = "root"
  val password = "Password@123"

  // there's probably a better way to do this
  var connection: Connection = DriverManager.getConnection(url, username, password)
  setupTable()


  def setupTable(): Unit = {
    val statement = connection.createStatement()


    // Don't do this in a real app. If the table exists all your data will be deleted.
    // We're doing this in testing to avoid duplicate entries
    statement.execute("CREATE TABLE IF NOT EXISTS players(username TEXT,stats JSON, config JSON)")

    // Use in real app to avoid deleting your table
    //statement.execute("CREATE TABLE IF NOT EXISTS players (username TEXT, points INT, locationX DOUBLE, locationY Double)")
  }

  def pushNewPlayertoDB(username:String,stats:String,config:String): Unit ={
      val statement = connection.prepareStatement("INSERT INTO players VALUE(?,?,?)  ")
    statement.setString(1,username)
    statement.setString(2,stats)
    statement.setString(3,config)


    statement.execute()



  }
  def getUsernames():List[String]= {
    val statement = connection.createStatement()
    val result: ResultSet = statement.executeQuery("SELECT * FROM players")
    var ExistingUsers: List[String] = List()

    while (result.next()) {
      val username = result.getString("username")
      ExistingUsers = ExistingUsers :+ username


    }
    return ExistingUsers
  }


  def getConfig(username:String): String ={
    val statement = connection.prepareStatement("SELECT * FROM players WHERE username=?")
    statement.setString(1, username)
    val result: ResultSet = statement.executeQuery()
    result.next()
    return result.getString("config")

  }

  def getStats(username:String): String ={
    val statement = connection.prepareStatement("SELECT * FROM players WHERE username=?")
    statement.setString(1, username)
    val result: ResultSet = statement.executeQuery()
    result.next()
    return result.getString("stats")

  }


  def updatePlayer(username: String,stats:String,config:String ): Unit = {
    val statement = connection.prepareStatement("UPDATE players SET  username= ?, stats = ?, config = ? WHERE username = ?")

    statement.setString(1, username)
    statement.setString(2, stats)
    statement.setString(3, config)
    statement.setString(4, username)

    statement.execute()
  }







}


