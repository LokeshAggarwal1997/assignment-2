import scala.annotation.tailrec

object assign {



  @tailrec
  def listadd(list1:List[Int],list2:List[Int],list3:List[Int]):List[Int]={
    if(list1==Nil || list2==Nil)
      list3
    print(list1.head)
    listadd (list1.tail,list2.tail,(list1.head+list2.head)::list3)
  }

   @tailrec
  def fib(a: Int, b: Int, n: Int,list:List[Int]): List[Int] = {
    n match {
      case 0 => list
      case _ =>fib (b, b + a, n - 1, a :: list)
    }
  }


  val rectangle=(l:Int,b:Int)=> {l*b}
  val rhombus = (l:Int,b:Int)=>(l*b)/2
  val parallel = (l:Int,b:Int)=>l*b

  def area(name:String,l:Int,b:Int,f:(Int,Int)=>Int):String={
    name match{
      case "rectangle" => s"Area of ${name} is ${rectangle(l,b)} "
      case "rhombus" => s"Area of ${name} is ${rhombus(l,b)}"
      case "parallel" => s"Area of ${name} is ${parallel(l,b)}"
      case _ => s"Not defined yet for ${name}"
    }
  }


  def doubleusingmap(list:List[Int]):List[Int]={
    list.map(x=>x*2)
  }
  @tailrec
  def kth(list: List[Int], n: Int, counter: Int): Int = {
    n match{
      case 0 => list(counter)
      case _ => kth(list,n-1,counter+1)
    }
  }

  @tailrec
  def lengthoflist(list: List[Int],n:Int): Int = {
    list match {
      case Nil =>  n
      case x :: xs =>  lengthoflist(xs,n+1)
    }
  }

  @tailrec
  def reverselist1(list: List[Int], list1: List[Int]): List[Int] = {
    list match {
      case Nil => list1
      case x :: xs => reverselist1(xs, x :: list1)
    }
  }

  
}
