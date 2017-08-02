/**
 * Class that represents a column in the table
 */
class MyColumn[T](i: Int, typ: MyType.Value) {
  /**
   * the type of the column
   */
  def myType = typ
  
  /**
   * the id of the column
   */
  def id: Int = i
  
  /**
   * a column, that is a Map where the keys are the ids of the rows and the values are
   * the corresponding values for that column and that row
   */
  private var column: Map[Int,T] = Map()
  
  /**
   * filters the column according the given operator and the given value
   */
  def myFilter(operatorName: OperatorNames.Value,value:Any):List[Int] = {
    var result: List[Int]=  List()
    var i = (myType match {
      case MyType.DOUBLE  => {
        result =(operatorName match{
          case OperatorNames.EQUAL => MyDouble.filterAMap((x:Double)=>{x==value.asInstanceOf[Double]},column.asInstanceOf[Map[Int,Double]])
          case OperatorNames.LOWER => MyDouble.filterAMap((x:Double)=>{x<value.asInstanceOf[Double]},column.asInstanceOf[Map[Int,Double]])
          case OperatorNames.LOWER_EQUAL => MyDouble.filterAMap((x:Double)=>{x<=value.asInstanceOf[Double]},column.asInstanceOf[Map[Int,Double]])
          case OperatorNames.GREATHER => MyDouble.filterAMap((x:Double)=>{x>value.asInstanceOf[Double]},column.asInstanceOf[Map[Int,Double]])
          case OperatorNames.GREATHER_EQUAL => MyDouble.filterAMap((x:Double)=>{x>=value.asInstanceOf[Double]},column.asInstanceOf[Map[Int,Double]])
        })
      } 
      case MyType.INT   => {
        result =(operatorName match{
          case OperatorNames.EQUAL => MyInt.filterAMap((x:Int)=>{x==value.asInstanceOf[Int]},column.asInstanceOf[Map[Int,Int]])
          case OperatorNames.LOWER => MyInt.filterAMap((x:Int)=>{x<value.asInstanceOf[Int]},column.asInstanceOf[Map[Int,Int]])
          case OperatorNames.LOWER_EQUAL => MyInt.filterAMap((x:Int)=>{x<=value.asInstanceOf[Int]},column.asInstanceOf[Map[Int,Int]])
          case OperatorNames.GREATHER => MyInt.filterAMap((x:Int)=>{x>value.asInstanceOf[Int]},column.asInstanceOf[Map[Int,Int]])
          case OperatorNames.GREATHER_EQUAL => MyInt.filterAMap((x:Int)=>{x>=value.asInstanceOf[Int]},column.asInstanceOf[Map[Int,Int]])         
        })
      }
      case MyType.STRING  => {
        result=(operatorName match{
          case OperatorNames.EQUAL => MyString.filterAMap((x:String)=>{x==value.asInstanceOf[String]},column.asInstanceOf[Map[Int,String]])
          case OperatorNames.LOWER => MyString.filterAMap((x:String)=>{x<value.asInstanceOf[String]},column.asInstanceOf[Map[Int,String]])
          case OperatorNames.LOWER_EQUAL => MyString.filterAMap((x:String)=>{x<=value.asInstanceOf[String]},column.asInstanceOf[Map[Int,String]])
          case OperatorNames.GREATHER => MyString.filterAMap((x:String)=>{x>value.asInstanceOf[String]},column.asInstanceOf[Map[Int,String]])
          case OperatorNames.GREATHER_EQUAL => MyString.filterAMap((x:String)=>{x>=value.asInstanceOf[String]},column.asInstanceOf[Map[Int,String]])         
        })
      }
      case _ => "NULL"
    })
    return result
  }
  
  /**
   * adds a new value in the column, given the id of the row and the value
   */
  def addValue(key: Int, value: T): Option[Int] = {
    addValueMap(key, value)
  }
  
  /**
   * removes a value from the column, given the id of the row
   */
  def removeValue(key: Int): Option[Int] = {
    removeValueMap(key)
  }
  
  /**
   * get a value from the column, given the id of the row
   */
  def getValue(key: Int): Option[T] = {
    column.get(key)
  }
  
  /**
   * get all the ids of the column
   */
  def getAllIds(): Set[Int] = {
    column.keySet
  }
  
  /**
   * Auxiliary method to adda value in a column
   */
  private def addValueMap(key: Int, value: T): Option[Int] = {
    try{
      column.+=(key -> value)
      return Some(key)
    }catch{
      case _: Throwable => None
    }
  }
  
  /**
   * Auxiliary method to remove a value from the column
   */
  private def removeValueMap(key: Int): Option[Int] = {
    try{
      column.-=(key)
      return Some(key)
    }catch{
      case _: Throwable => None
    }
  }
}