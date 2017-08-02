/**
 * This object has the filter method for the type Double
 */
object MyDouble{
  /**
   * Method that receives a predicate and a Map, and return a list with the ids
   * of the elements in the map which fulfilled the predicate
   */
  def filterAMap(p:((Double)=>Boolean),map: Map[Int,Double]):List[Int] = {
    var result: List[Int] = List()
    for(i: Int <- map.keySet){
      if(p(map.get(i).get) && map.get(i).get!=Double.MinValue){
        result.::=(i)
      }
    }
    return result.sorted
  }
}