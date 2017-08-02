import scala.util.matching.Regex
/**
 * this object represents a database
 */
object MyDataBase {
  /*
   * THE FOLLOWING VALUES ARE REGULAR EXPRESSIONS TO EVALUATE THE QUERIES IN THE GENERIC 
   * DATABASE
   */
  /**
   * CREATE TABLE QUERY REGEX
   */
  val createTableRegex:Regex="^CREATE_TABLE:([a-zA-Z0-9])+:(([a-zA-Z0-9])+(,))*([a-zA-Z0-9])+:((((INT|STRING|DOUBLE)(,))*)(INT|STRING|DOUBLE));$".r
  
  /**
   * DELETE TABLE QUERY REGEX
   */
  val deleteTableRegex:Regex="^DELETE_TABLE:([a-zA-Z0-9])+;$".r
  /**
   * INSERT ROW IN TABLE QUERY REGEX
   */
  val insertRowInTableRegex:Regex="^INSERT_ROW:([a-zA-Z0-9])+:((((([a-zA-Z0-9])+)|([-|+]?[0-9]+\\.[0-9]+)|([-|+]?[0-9]+))(,))|(([a-zA-Z0-9])+(([a-zA-Z0-9])|\\s)*(,)))*(([a-zA-Z0-9])|([-|+]?[0-9]+)|([-|+]?[0-9]+\\.[0-9]+))+;$".r
  /**
   * DELETE ROW IN TABLE QUERY REGEX
   */
  val deleteRowInTableRegex:Regex="^DELETE_ROW:([a-zA-Z0-9])+:([0-9]+);$".r
  /**
   * QUERY TABLE QUERY REGEX
   */
  val queryTableRegex:Regex="^QUERY_TABLE:([a-zA-Z0-9])+:([a-zA-Z0-9])+:(==|>=|>|<=|<):((([-|+]?[0-9]+\\.[0-9]+))|([-|+]?[0-9]+)|(([a-zA-Z0-9])+));".r
  /**
   * SELECT A TABLE QUERY REGEX
   */
  val selectATable:Regex="^SELECT_TABLE:([a-zA-Z0-9])+;$".r
  /**
   * UPDATE A ROW IN TABLE QUERY REGEX
   */
  val updateARowInTable:Regex="^UPDATE_ROW:([a-zA-Z0-9])+:([0-9]+):(([a-zA-Z0-9])+(,))*([a-zA-Z0-9])+:((((([a-zA-Z0-9])+)|([-|+]?[0-9]+\\.[0-9]+)|([-|+]?[0-9]+))(,))|(([a-zA-Z0-9])+(([a-zA-Z0-9])|\\s)*(,)))*(([a-zA-Z0-9])|([-|+]?[0-9]+)|([-|+]?[0-9]+\\.[0-9]+))+;$".r
  
  /**
   * The data base is a map where the keys are the names of the tables, and the 
   * values are the tables
   */
  var dataBase:Map[String,MyTable]=Map()
  
  /**
   * creates a table
   */
  def createTable(nameTable:String,nNumberCols: Int,nNames: Array[String],nTypes: Array[MyType.Value]):Unit ={
    if(dataBase.contains(nameTable)){
      println("A database with this name is already created!! Try with a new name")
    }else{
      dataBase.+=(nameTable -> (new MyTable(nNumberCols,nNames,nTypes,nameTable)))
      println("The database was created successfully with name: "+nameTable)
    }
  }
  /**
   * deletes a table
   */
  def deleteTable(nameTable:String): Unit ={
    if(dataBase.contains(nameTable)){
      dataBase.-=(nameTable)
      println("The table with name: "+nameTable+" was deleted.")
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  /**
   * insert a row in an specified table
   */
  def insertRowInATable(nameTable:String,values: Array[Any]):Unit = {
    if(dataBase.contains(nameTable)){
      try{
        if(dataBase.get(nameTable).get.numberCols==values.length){
        dataBase.get(nameTable).get.addNewRow(values)
        println("The row was added successfully to the table")
        }else{
          println("The number of rows were not correct: needed: "+nameTable+"")
        }
      }catch{
        case ex: Throwable =>{
            println("Sorry, your query was not well formed. Try again")
         }
      }
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  
  /**
   * deletes row in a table
   */
  def deleteRowInATable(nameTable:String, id:Int):Unit = {
    if(dataBase.contains(nameTable)){
      try{
        dataBase.get(nameTable).get.removeRow(id)
        println("The row was deleted successfully from the table: "+nameTable+"")
      }catch{
        case ex: Throwable =>{
            println("Sorry, your query was not well formed. Try again")
         }
      }
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  
  /**
   * queries a table
   */
  def queryATable(nameTable:String, nameColumn:String,operatorName:OperatorNames.Value,value:Any):Unit = {
    
    if(dataBase.contains(nameTable)){
      if(dataBase.get(nameTable).get.checkColumnNameExists(nameColumn)){
        try{
          val filteredTable:MyTable=dataBase.get(nameTable).get.myFilter(nameColumn, operatorName, value)
          println("------------------------------------------------")
        }catch{
          case ex: Throwable =>{
              println("Sorry, your query was not well formed. Try again")
           }
        }
      }else{
        println("Sorry, the column name you provide does not exist")
      }
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  
  /**
   * selects a table and shows it
   */
  def selectATable(nameTable:String):Unit = {
    if(dataBase.contains(nameTable)){
      try{
        println("##################### Table "+nameTable+" #####################")
        println(dataBase.get(nameTable).get.toString())
      }catch{
        case ex: Throwable =>{
            println("Sorry, your query was not well formed. Try again")
         }
      }
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  
  /**
   * updates a row in the table
   */
  def updateRowInATable(nameTable:String, newValues:Map[String,Any],idRow:Int):Unit = {
    if(dataBase.keySet.contains(nameTable)){
      try{
        var continue:Boolean = true
        for(n <- newValues.keySet){
          if(!dataBase.get(nameTable).get.checkColumnNameExists(n)){
            println("Sorry, the column "+n+" does not exist in the table "+nameTable+". Try again")
            continue=false
          }
        }
        if(continue && dataBase.get(nameTable).get.checkIdRow(idRow)){
          val row: Array[Any]=dataBase.get(nameTable).get.getRow(idRow)
          val rowToAdd: Array[Any]=row
          for(j <- newValues.keySet){
            val idCol=dataBase.get(nameTable).get.getColId(j)
            rowToAdd(idCol)=newValues.get(j).get
          }
          dataBase.get(nameTable).get.updateRow(idRow, rowToAdd)
          println("The row was updated successfully")
        }else{
          if(continue){
            println("Sorry, the row "+idRow+" does not exist in the table "+nameTable+". Try again")
          }
        }
      }catch{
        case ex: Throwable =>{
            println("Sorry, your query was not well formed. Try again")
         }
      }
    }else{
      println("Sorry, a database with name: "+nameTable+" does not exist.")
    }
  }
  
  /**
   * this method process the string query of CREATE TABLE given by the user in the console
   */
  def procCreateTable(query:String): Unit = {
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    val names:Array[String]=parts(2).split(",")
    val types:Array[MyType.Value]=procValues(0,parts(3).split(","),new Array[MyType.Value](names.length))
    if(names.length==types.length){
      createTable(tableName, names.length, names, types)
    }else{
      println("The size of the types and the names must be the same. Found lengths where (names:"+names.length+", types:"+types.length+")")
    }
  }
 
  /**
   * this method process the string query of UPDATE ROW given by the user in the console
   */
  def procUpdateRow(query:String): Unit = {
    try{
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    val idRow:Int=parts(2).toInt
    val names:Array[String]=parts(3).split(",")
    val types:Array[Any]=procUpdate(dataBase.get(tableName).get, names, parts(4).split(","))
    var map:Map[String, Any]=buildMap(names,types)
    if(names.length==types.length){
      updateRowInATable(tableName, map, idRow)
    }else{
      println("The size of the types and the names must be the same. Found lengths where (names:"+names.length+", types:"+types.length+")")
    }
    }catch{
      case e: Throwable => println("The name of the table or the columns names or values does not match in the table")
    }
  }
  
  /**
   * This method creates a Map, where names are going to be the keys and
   * values the values
   */
  def buildMap(names:Array[String],values:Array[Any]):Map[String,Any]={
    var map:Map[String ,Any]=Map()
    for(i<- 0 to names.length-1){
      map.+=(names(i)->values(i))
    }
    return map
  }
  
  /**
   * this method process the string query of DELETE TABLE given by the user in the console
   */
  def procDeleteTable(query:String): Unit = {
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    deleteTable(tableName)
  }
  
  /**
   * this method process the string query of QUERY TABLE given by the user in the console
   */
  def procQueryTable(query:String): Unit = {
    try{
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    val columnName:String= parts(2)
    val table:MyTable=dataBase.get(tableName).get
    val queryType:OperatorNames.Value=procQueryType(parts(3))
    val value:Any = procValAny(table,parts(4),columnName)
    queryATable(tableName, columnName, queryType, value)
    }catch{
      case e: Throwable => println("The name of the table or the column name or value does not match in the table")
    }
  }
  
  /**
   * This method return an right Any value, according to the type of the column
   */
  private def procValAny(table:MyTable,value:String,colName:String):Any={
    val typ=table.schema.getTypeFromId(table.schema.getIdFromName(colName).get).get
     if(typ.equals(MyType.INT)){
        if(!value.equals("NULL")){
          value.toInt
        }else{
          Int.MinValue
        }
      }else if(typ.equals(MyType.DOUBLE)){
        if(!value.equals("NULL")){
          value.toDouble
        }else{
          Double.MinValue
        }
      }else{
        value
      }
     
  }
  
  /**
   * this method returns the right operator according the string given 
   */
  private def procQueryType(query:String):OperatorNames.Value={
    if(query.equals("==")){
      OperatorNames.EQUAL
    }else if(query.equals(">=")){
      OperatorNames.GREATHER_EQUAL
    }else if(query.equals(">")){
     OperatorNames.GREATHER
    }else if(query.equals("<=")){
      OperatorNames.LOWER_EQUAL
    }else{
      OperatorNames.LOWER
    }
  }
  
  /**
   * this method process the string query of DELETE ROW given by the user in the console
   */
  def procDeleteRow(query:String): Unit = {
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    val id:Int = parts(2).toInt
    deleteRowInATable(tableName, id)
  }
  
  /**
   * this method process the string query of INSERT ROW given by the user in the console
   */
  def procInsertRowInTable(query:String): Unit = {
    try{
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    if(dataBase.get(tableName).get.numberCols==parts(2).split(",").length){
    val option= dataBase.get(tableName)
    (option match{
      case Some(i) => insertRowInATable(tableName, procAnys(i,0,parts(2).split(","),new Array[Any](i.numberCols)))
      case None => println("The name of the table does not exist")
    })
    }else{
      println("Incorrect number of columns. Check the number of elements you are adding into a row.")
    }
    }catch{
      case e: Throwable =>println("Sorry, your query was incorrect. Check the number of elements you are inserting and the types")
    }
  }
  
  /**
   * this method process the string query of SELECT TABLE given by the user in the console
   */
  def procSelectTable(query:String): Unit = {
    val parts:Array[String]=query.replace(";","").split(":")
    val tableName:String = parts(1)
    selectATable(tableName)
  }
  
  /**
   * Auxiliary method for the query UPDATE
   */
  private def procUpdate(table:MyTable,names:Array[String],values:Array[String]):Array[Any]={
    var result:Array[Any]=new Array[Any](names.length) 
    var counter:Int=0
    for(i<-names){
       if(table.schema.getTypeFromId(table.schema.getIdFromName(i).get).get.equals(MyType.INT)){
         result(counter)=values(counter).toInt
       }else if(table.schema.getTypeFromId(table.schema.getIdFromName(i).get).get.equals(MyType.DOUBLE)){
         result(counter)=values(counter).toDouble
       }  else{
         result(counter)=values(counter)
       }
       counter+=1
     }
    return result
  }
  
  /**
   * Auxiliary method to know the types of the toProc values
   * according their respective column type
   */
  private def procValues(cont:Int,toProc:Array[String],result:Array[MyType.Value]):Array[MyType.Value]={
    if(cont>=toProc.length){
      return result
    }else{
      if(toProc(cont).equals("INT")){
        result(cont)=MyType.INT
      }else if(toProc(cont).equals("DOUBLE")){
        result(cont)=MyType.DOUBLE
      }else{
        result(cont)=MyType.STRING
      }
      val c=cont+1
      return procValues(c,toProc,result)
    }
  }
  
  /**
   * Auxiliary method to know the values according to the types of the toProc values
   * according their respective column type
   */
  private def procAnys(table:MyTable,cont:Int,toProc:Array[String],result:Array[Any]):Array[Any]={
    if(cont>=toProc.length){
      result
    }else{
      if(table.schema.getTypeFromId(cont).get.equals(MyType.INT)){
        if(!toProc(cont).equals("NULL")){
          result(cont)=toProc(cont).toInt
        }else{
          result(cont)=Int.MinValue
        }
      }else if(table.schema.getTypeFromId(cont).get.equals(MyType.DOUBLE)){
        if(!toProc(cont).equals("NULL")){
          result(cont)=toProc(cont).toDouble
          
        }else{
          result(cont)=Double.MinValue
        }
      }else{
        result(cont)=toProc(cont)
      }
      val c=cont+1
      return procAnys(table, c, toProc, result)
    }
  }
  
  /**
   * It is called when the user selects option 2 LIBRARY
   * calls and loads the related information
   */
  def mainLibraryProccess(query:String){
    val parts=query.replace(";", "").split(":")
    if(query.startsWith("INSERT:")){
      try{
      Library.insert(parts(1), parts(2))
      }catch{
        case ex: Throwable => println("Your input was not in the correct format")
      }
    }else if(query.startsWith("DELETE:")){
      try{
      Library.delete(parts(1), parts(2).toInt)
      }catch{
        case ex: Throwable => println("Your input was not in the correct format")
      }
    }else if(query.startsWith("BORROW:")){
      try{
        Library.borrow(parts(1), parts(2).toInt, parts(3))
      }catch{
        case ex: Throwable => println("Your input was not in the correct format")
      }
    }else if(query.startsWith("ACCESS_INTERNAL:")){
      try{
        Library.internal(parts(1), parts(2).toInt, parts(3))
      }catch{
        case ex: Throwable => println("Your input was not in the correct format")
      }
    }else if(query.startsWith("ACCESS_EXTERNAL:")){
      Library.external(parts(1), parts(2).toInt, parts(3))
    }else if(query.startsWith("SEE_OBJECT:")){
      Library.showTable(parts(1))
    }else{
      println("The query does not exist! Try again")
    }
  }
  
  /**
   * It is called when the user selects option 1 GENERIC DATABASE
   * calls and loads the related information
   */
  def mainQueryProcess(query:String){
    val matches=createTableRegex.findAllIn(query) 
      if(matches.size==1){
        procCreateTable(query)
      }else{
        val matches=deleteTableRegex.findAllIn(query) 
        if(matches.size==1){
          procDeleteTable(query)
        }else{
          val matches=insertRowInTableRegex.findAllIn(query) 
          if(matches.size==1){
            procInsertRowInTable(query)
          }else{
            val matches=deleteRowInTableRegex.findAllIn(query) 
            if(matches.size==1){
              procDeleteRow(query)
            }else{
              val matches=queryTableRegex.findAllIn(query) 
              if(matches.size==1){
                procQueryTable(query)
              }else{
                val matches=selectATable.findAllIn(query) 
                if(matches.size==1){
                  procSelectTable(query)
                }else{
                  val matches=updateARowInTable.findAllIn(query) 
                  if(matches.size==1){
                    procUpdateRow(query)
                  }else{
                    println("BAD QUERY!!! "+query)
                  }
                }
              }
            }
          }
        }
      }  
    
  }
  
  /**
   * MAIN
   */
  def main(args: Array[String]): Unit = {
    println("Do you want to use the GENERIC DATABASE (1) or the LIBRARY DAYABASE (2)? (type 1 or 2)")
    val num:String=scala.io.StdIn.readLine()
    if(num.equals("1")){
      while(true){
        println("Type the query: ")
        val query:String=scala.io.StdIn.readLine()
        mainQueryProcess(query)
      }
    }else if(num.equals("2")){
      Library.initLibrary()
      while(true){
        println("What do you want to do in the library?: ")
        val query:String=scala.io.StdIn.readLine()
        
        mainLibraryProccess(query)
      }
    }else{
      println("You must type 1 or 2")
    }
    
  }
}