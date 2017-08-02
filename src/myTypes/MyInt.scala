/**
 * This object has the filter method for the type Int
 */
object MyInt {
  /**
   * Method that receives a predicate and a Map, and return a list with the ids
   * of the elements in the map which fulfilled the predicate
   */
  def filterAMap(p:((Int)=>Boolean),map: Map[Int,Int]):List[Int] = {
    var result: List[Int] = List()
    for(i: Int <- map.keySet){
      if(p(map.get(i).get)&& map.get(i).get!=Int.MinValue){
        result.::=(i)
      }
    }
    return result.sorted
  }
}