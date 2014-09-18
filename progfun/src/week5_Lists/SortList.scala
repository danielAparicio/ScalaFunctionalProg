package week5_Lists

class SortList {
  
  //List descending sort 
  //with both functions we go till the end of the list and start building it back including
  //the incoming elements in the right position
  def sortList(xs: List[Int]):List[Int] = xs match {
    //empty list ....we return an empty list, also you can return a Nil
    case List() => List()
    case y::ys => insert(y,ys)    
  }
  
  def insert(x: Int,xs: List[Int]):List[Int] = xs match {
    //IMPORTANT if the rest of the list is empty we should give back the list of the element (last element)
    case List() => List(x)
    /*if the element is minor that the first element of the tail with which we have called insert
     * we return the list with that element in the first place
     * else we put the first element of the tail in the first place (as its minor than x) and we call recursively insert
     * with the element and with the tail of the list without y
     */   
    case y::ys => if (x<=y) x::xs else y::(insert(x,ys))             
  }
}  