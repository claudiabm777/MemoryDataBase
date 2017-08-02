/**
 * This class represents a Table in the database
 */
class MyTable(nNumberCols: Int,nNames: Array[String],nTypes: Array[MyType.Value],nameTable:String) {
  /**
   * Name of the table
   */
  val myName:String= nameTable
  
  /**
   * Number of rows in the table
   */
  var numberRows: Int = 0
  
  /**
   * Current last number assigned as ID to a row in the table
   */
  var numberRowId: Int = 0
  
  /**
   * Cumber of columns in the table
   */
  def numberCols: Int = nNumberCols 
  
  /**
   * The table. It is an array of columns
   */
  var table: Array[MyColumn[Any]] = createTable()
  
  /**
   * The schema of the table. It contains information about the columns
   * like the type, names, etc.
   */
  def schema: MySchema = new MySchema(nNames,nTypes)
  
  /**
   * This method creates an empty table
   */
  def createTable(): Array[MyColumn[Any]] = {
    var resultTable:Array[MyColumn[Any]] = new Array[MyColumn[Any]](numberCols)
    val cols = nNumberCols - 1
    for(n <- 0 to cols){
      var col: MyColumn[Any] = new MyColumn[Any](n,nTypes(n))
      resultTable(n)=col
    }
    return resultTable
  }
  
  /**
   * This method receives the id of the row, and returns an array with all the
   * elements contained in the row
   */
  def getRow(idRow: Int):Array[Any] = {
     var row:Array[Any]=new Array[Any](numberCols)
      for(i:Int <- 0 to (numberCols-1)){
        row(i)=table(i).getValue(idRow).get
      }
     return row
  }
  
  /**
   * This method updates a row. It receives the id of the row and an array with the
   * new row.
   */
  def updateRow(idRow: Int,row: Array[Any]):Int = {
    removeRow(idRow)
    addNewRow(row)
    return numberRowId
  }
  
  /**
   * This method checks if there is a row with the id given as parameter.
   * Returns true if there is such id, or false if not.
   */
  def checkIdRow(id:Int):Boolean = {
    try{
    val set:Set[Int]=table(0).getAllIds()
    val result:Boolean=set.contains(id)
    return result
    }catch{
      case ex:Throwable => false
    }
  }
  
  /**
   * Method that adds a new row in the table
   * values: the array with the new values
   */
  def addNewRow(values: Array[Any]): Int = {
    numberRowId+=1
    val cols = nNumberCols - 1
     for(n <- 0 to cols){
       table(n).addValue(numberRowId, values(n))
     }
    numberRows+=1
    return numberRowId
  }
  
  /**
   * Method that removes a row from the table
   */
  def removeRow(id: Int): Int = {
    val cols = nNumberCols - 1
    for(n <- 0 to cols){
       table(n).removeValue(id)
     }
    numberRows-=1
    return id
  }
  
  /**
   * Method that returns the id of a column in the table
   * name: the name of the column
   */
  def getColId(name: String):Int = {
    schema.getIdFromName(name).get
  }
  
  /**
   * Method that query the table according to the received operator
   * nameColumn: name of the column of the query
   * operatorName: operator of the query
   * value: value to be compared with the elements in the column
   */
  def myFilter(nameColumn: String,operatorName: OperatorNames.Value, value:Any):MyTable = {
    val idColumnOptional: Option[Int]=schema.getIdFromName(nameColumn)
    val result: MyTable=(idColumnOptional match {
      case Some(i) => myFilterColumn(i,operatorName,value)
      case None => println(nameColumn);throw new Exception("Invalid column name")
    })
    return result
  }
  
  /**
   * Checks if the given column name exists in the table
   * nameColumn: name of the column
   */
  def checkColumnNameExists(nameColumn:String):Boolean = {
    val id:Option[Int]=schema.getIdFromName(nameColumn)
    val result:Boolean = (id match{
      case Some(i) => true
      case None => false
    })
    return result
  }
  
  /**
   * Returns the type of the column
   * nameColumn: name of the column
   */
  def checkTypeColumn(nameColumn:String):MyType.Value = {
    val id:Int=schema.getIdFromName(nameColumn).get
    val typ:MyType.Value = schema.getTypeFromId(id).get
    return typ
  }
  
  /**
   * Auxiliary method to filter
   */
  private def myFilterColumn(idColumn: Int, operatorName: OperatorNames.Value, value:Any): MyTable = {
    val column:MyColumn[Any] = table(idColumn)
    val listIdsRows:List[Int]= column.myFilter(operatorName, value)
    var result = new MyTable(numberCols, nNames,nTypes,"query")
    for( j:Int <- listIdsRows){
      var rowToBeAdded:Array[Any]=new Array[Any](numberCols)
      var counter:Int =0
      for(i:Int <- 0 to (numberCols-1)){
        rowToBeAdded(i)=table(i).getValue(j).get
        counter+=1
      }
      
      result.addNewRow(rowToBeAdded)
    }
    println("##################### Result of query #####################")
    println(printQuery(listIdsRows,result))
    return result
  }
  
  /**
   * method that print the result of a query
   */
  def printQuery(keysIdsRows2:List[Int],table1:MyTable):String={
    val table2=table1.table
    var result: String ="ID\t\t"
    for(n<-table1.schema.names){
      result+=n+"\t\t"
    }
    result+="\n"
    val keysIdsColumns:Set[Int] = table2(0).getAllIds()
    val keysIdsColumns2:List[Int]= keysIdsColumns.toList.sorted
    var counter:Int=0
    for(n: Int <- keysIdsRows2 ){
      result+=n
      
      for(j: Int <- 0 to (table2.length-1)){
        val cell:Option[Any]=table2(j).getValue(keysIdsColumns2(counter))
        val i = (cell match {
          case Some(x:Int) => {
            if(x!=Int.MinValue){
              x
            }else{
              "NULL"
            }
            } // this extracts the value in a as an Int
          case Some(y:Double) => {
            if(y!=Double.MinValue){
              y 
            }else{
              "NULL"
            }
          }
          case Some(x:String) => x
          case _ => "NULL"
        })
        result+="\t\t| "+i
      }
      counter+=1
      result+="\n"
    }
    return result
  }
  
  /**
   * implementation of toString of MyTable
   */
  override def toString():String = {
    var result: String ="ID\t\t"
    for(n<-schema.names){
      result+=n+"\t\t"
    }
    result+="\n"
    val keysIdsColumns:Set[Int] = table(0).getAllIds()
    val keysIdsColumns2:List[Int]= keysIdsColumns.toList.sorted
    for(n: Int <- keysIdsColumns2){
      result+=n
      for(j: Int <- 0 to (table.length-1)){
        val cell:Option[Any]=table(j).getValue(n)
        val i = (cell match {
          case Some(x:Integer) => {
            if(x!=Int.MinValue){
              x
            }else{
              "NULL"
            }
            } // this extracts the value in a as an Int
          case Some(y:Double) => {
            if(y!=Double.MinValue){
              y 
            }else{
              "NULL"
            }
          }
          case Some(x:String) => x
          case _ => "NULL"
        })
        result+="\t\t| "+i
      }
      result+="\n"
    }
    return result
  }
   
}