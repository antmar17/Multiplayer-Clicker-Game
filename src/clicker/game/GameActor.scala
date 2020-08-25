package clicker.game

import akka.actor.Actor
import clicker.{BuyEquipment, Click}
import play.api.libs.json.{JsValue, Json}
import clicker._

class Equipment(val id:String,val name:String,val incomePerClick:Double,val incomePerSecond:Double,val initialCost:Double,val priceExponent:Double){
  var currentCost=initialCost
  var amountOwned:Int=0
  def buy():Unit={
    amountOwned+=1
    currentCost=currentCost+(currentCost*priceExponent)
  }

}

class GameActor(username: String, configuration: String) extends Actor {
//TODO: -initialization code to set up game as descibed by configuration


  //STATS
  //-------------------------------------------------
  var equipmentMap:Map[String,Equipment]=Map()
  var goldOwned:Double=0.0
  var goldPerClick:Double=1.0
  var goldPerSecond:Double=0.0
  //-------------------------------------------------

  //INITIALIZATION
  //-------------------------------------------------------------------
  var parsed:JsValue=Json.parse(configuration)
  println(parsed)

  val currency:String=(parsed\"currency").as[String]
  var equipmentParse=(parsed\"equipment").as[List[JsValue]]






//  fill up equipment map
  for(index<-0 until equipmentParse.length){

    val id:String=((parsed \ "equipment")(index) \"id").as[String]
    val name:String=((parsed \ "equipment")(index) \"name").as[String]
    val IPC:Double=((parsed \ "equipment")(index) \"incomePerClick").as[Double]
    val IPS:Double=((parsed \ "equipment")(index) \"incomePerSecond").as[Double]
    val initialCost:Double=((parsed \ "equipment")(index) \"initialCost").as[Double]
    val priceExponent:Double=((parsed \ "equipment")(index) \"priceExponent").as[Double]

    val ob=(id->new Equipment(id,name,IPC,IPS,initialCost,priceExponent))

    equipmentMap+=ob

  }


  //-------------------------------------------------------------------
  var timeOfLastUpdate:Long=System.nanoTime()




  def UpdateGame():String={
    val last:Long=timeOfLastUpdate
    val now:Long=System.nanoTime()-last
    this.timeOfLastUpdate=System.nanoTime()



    //keep track by miliseconds
    this.goldOwned+=((goldPerSecond/1000)*(now*.000001))


    val jsonUser=Json.toJson(this.username)
    val jsonCurrency=Json.toJson(this.goldOwned)
    val jsonTimeStamp=Json.toJson(this.timeOfLastUpdate)
    var EquipmentList:List[Map[String,JsValue]]=List()
    //fill equipment list up
    for(equipment<-this.equipmentMap){

      val jsonId:JsValue=Json.toJson(equipment._1)
      val jsonNumberOwned:JsValue=Json.toJson((equipment._2.amountOwned))
      val jsonCost:JsValue=Json.toJson(equipment._2.currentCost)

      val e:Map[String,JsValue]=Map("id"->jsonId,"numberOwned"->jsonNumberOwned,"cost"->jsonCost)
      EquipmentList=EquipmentList:+e
    }
    val jsonEquipmentList:JsValue=Json.toJson(EquipmentList)

    val jsonMap:Map[String,JsValue]=Map(
      "username"->jsonUser,
      "currency"->jsonCurrency,
      "time_stamp"->jsonTimeStamp,
      "equipment"->jsonEquipmentList
    )
    Json.stringify(Json.toJson(jsonMap))

  }


  def LoadGame(previousGameState:String):Unit={
    var parsed:JsValue=Json.parse(previousGameState)
    val times_stamp=(parsed\"time_stamp").as[Long]
    val currency=(parsed\"currency").as[Double]
    this.timeOfLastUpdate=times_stamp
    this.goldOwned=currency
    var GPS:Double=0
    var GPC:Double=0
    var equipParse=(parsed\"equipment").as[List[JsValue]]
    for(index<-0 until equipParse.length){

      val id:String=((parsed\"equipment")(index)\"id").as[String]
      val numberOwned:Int=((parsed\"equipment")(index)\"numberOwned").as[Int]
      val cost:Double=((parsed\"equipment")(index)\"cost").as[Double]
      this.equipmentMap(id).currentCost=cost
      this.equipmentMap(id).amountOwned=numberOwned
      GPS +=( this.equipmentMap(id).amountOwned* this.equipmentMap(id).incomePerSecond)
      GPC +=( this.equipmentMap(id).amountOwned* this.equipmentMap(id).incomePerClick)


    }
    this.goldPerSecond=GPS
    this.goldPerClick=GPC

  }

  //TODO:behavior for message type: Update
  override def receive: Receive = {
    case Click => this.goldOwned += this.goldPerClick

    case buyEquipment:BuyEquipment=>
      var e=equipmentMap(buyEquipment.equipmentId)

      if(goldOwned>=e.currentCost) {
        goldOwned -= e.currentCost
        e.buy()
        goldPerClick+=e.incomePerClick
        goldPerSecond+=e.incomePerSecond
      }

    case Update=>
       sender() ! GameState(UpdateGame())


    case Save=>
      sender()! GameSave(username,UpdateGame(),this.configuration)




    case l:Load=>
      this.LoadGame(l.stats)




  }



}






