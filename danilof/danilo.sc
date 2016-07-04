import scala.annotation.tailrec
sealed trait Suit
object Diamonds extends Suit
object Hearts extends Suit
object Clubs extends Suit
object Spades extends Suit
sealed trait Value
object Ace extends Value
object Jack extends Value
object Queen extends Value
object King extends Value
object Invalid extends Value
case class Number(value: Int) extends Value
object PokerApp {

  def classifyHand(hand: List[String]): String = hand  match {
      case (l) if(l.size < 5)=> "Invalid hand: Too few cards"
      case (l) if(l.size > 5)=> "Invalid hand: Too many cards"
      case (l) if(l.toSet.size != l.size) =>
        lazy val duplicates = getDuplicated(l)
        "Invalid hand: "+ duplicates._1 + " appears "+ duplicates._2 +" times"
      case (_)  => "High card: "+getHighestCard(l)
  }

  def getDuplicated(s: List[String]): (String, Int)  ={
    s.groupBy(identity).mapValues(_.size).maxBy(_._2)
  }

  def getHighestCard(l: List[String]) = {
    @tailrec
    def recHigh(card:String,max:Int,l:List[String]):String = l match{
      case Nil => card
      case(_) =>{
        def valueCard= getValue(getValueFromStr(l.head.dropRight(1)))
        if(valueCard < max) recHigh(card,max,l.tail)
        else recHigh(l.head,valueCard,l.tail)
      }
    }
    recHigh(l.head,0,l)
  }

  def getValue(x: Value): Int = x match {
    case Jack => 11
    case Queen => 12
    case King => 13
    case Ace => 14
    case Number(n) => n
  }

  def getValueFromStr(str: String): Value = str match {
    case "A" => Ace
    case "J" => Jack
    case "Q" => Queen
    case "K" => King
    case _ => Number(str.toInt )
  }
}
def l = List("2C", "4D", "QC", "KC", "AC")
//println(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")))
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: 3H appears 2 times")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: AC")
