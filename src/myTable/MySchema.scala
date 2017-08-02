/**
 * Class that represents a schema in a table
 */
class MySchema(nNames: Array[String],nTypes: Array[MyType.Value]) {
  /**
   * Array of names of the columns
   */
  def names: Array[String] = nNames
  
  /**
   * Array of types of the table
   */
  private def types: Array[MyType.Value] = nTypes
  
  /**
   * Map where the key is the name of the column and the value the id of the column
   */
  private def namesIdsMap: Map[String,Int] = generateIds()
  
  /**
   * Auxiliary method which initializes the map namesIdsMap
   */
  private def generateIds():Map[String,Int] = {
    var returnMap: Map[String,Int] = Map()
    for(i <- 0 to (nNames.length - 1)){
      returnMap.+=(nNames(i) -> i)
    }
    return returnMap   
  }
  
  /**
   * Gets the id of the column given the name
   */
  def getIdFromName(name: String): Option[Int] = {
    namesIdsMap.get(name)
  }
  
  /**
   * Gets the name of the column given the id
   */
  def getNameFromId(id: Int): Option[String] = {
    try{
      val name: String = names(id)
      return Some(name)
    }catch{
      case _: Throwable => None
    }
  }
  
  /**
   * Gets the type of the column given the id
   */
  def getTypeFromId(id: Int): Option[MyType.Value] = {
    try{
      val name: MyType.Value = types(id)
      return Some(name)
    }catch{
      case _: Throwable => None
    }
  }
}