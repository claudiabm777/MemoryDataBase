/**
 * object that represents the library
 */
object Library {
  /**
   * list of electronic books
   */
  var eBooks:List[ElectronicalBook] = List()
  
  /**
   * list of electronic magazines
   */
  var eMag:List[ElectronicalMagazine] = List()
  
  /**
   * list of physical books
   */
  var pBooks:List[PhysicalBook] = List()
  
  /**
   * list of physical magazines
   */
  var pMag:List[PhysicalMagazine] = List()
  
  /**
   * this method initializes the library
   * creates the 4 tables
   */
  def initLibrary(){
    MyDataBase.createTable("electronicalBook", 5, Array("author","title","numberPages","externalAccess","internalAccess"), Array(MyType.STRING,MyType.STRING,MyType.INT,MyType.INT,MyType.INT))
    MyDataBase.createTable("electronicalMagazine", 6, Array("issueNumber","author","title","numberPages","externalAccess","internalAccess"), Array(MyType.STRING,MyType.STRING,MyType.STRING,MyType.INT,MyType.INT,MyType.INT))
    MyDataBase.createTable("physicalBook", 5, Array("timesBorrowed","lastDay","author","title","numberPages"), Array(MyType.INT,MyType.STRING,MyType.STRING,MyType.STRING,MyType.INT))
    MyDataBase.createTable("physicalMagazine", 6, Array("issueNumber","timesBorrowed","lastDay","author","title","numberPages"), Array(MyType.STRING,MyType.INT,MyType.STRING,MyType.STRING,MyType.STRING,MyType.INT))
    
  }
  /**
   * Method that shows tables of the library according to the users query
   */
  def showTable(typeObject:String){
    if(typeObject.equals("electronicalBook")){
      MyDataBase.mainQueryProcess("SELECT_TABLE:electronicalBook;")
    }else if(typeObject.equals("electronicalMagazine")){
      MyDataBase.mainQueryProcess("SELECT_TABLE:electronicalMagazine;")
    }else if(typeObject.equals("physicalBook")){
      MyDataBase.mainQueryProcess("SELECT_TABLE:physicalBook;")
    }else if(typeObject.equals("physicalMagazine")){
      MyDataBase.mainQueryProcess("SELECT_TABLE:physicalMagazine;")
    }else{
      println("Sorry, the id provided does not exist, try again")
    }
  }
  
  /**
   * Inserts a row in the specified table
   */
  def insert(typeObject:String,listCols:String){
    if(typeObject.equals("electronicalBook")){
      MyDataBase.mainQueryProcess("INSERT_ROW:electronicalBook:"+listCols+";")
      try{
        var parts:Array[String]=listCols.split(",")
        var eB:ElectronicalBook=new ElectronicalBook(parts(0),parts(1),parts(2).toInt,parts(3).toInt,parts(4).toInt)
        eBooks.+:=(eB)
      }catch{
        case e: Throwable =>
      }
    }else if(typeObject.equals("electronicalMagazine")){
      MyDataBase.mainQueryProcess("INSERT_ROW:electronicalMagazine:"+listCols+";")
       try{
        var parts:Array[String]=listCols.split(",")
        var eB:ElectronicalMagazine=new ElectronicalMagazine(parts(0),parts(1),parts(2),parts(3).toInt,parts(4).toInt,parts(5).toInt)
        eMag.+:=(eB)
       }catch{
        case e: Throwable => println("Try to insert the object again")
      }
    }else if(typeObject.equals("physicalBook")){
      MyDataBase.mainQueryProcess("INSERT_ROW:physicalBook:"+listCols+";")
       try{
        var parts:Array[String]=listCols.split(",")
        var eB:PhysicalBook=new PhysicalBook(parts(0).toInt,parts(1),parts(2),parts(3),parts(4).toInt)
        pBooks.+:=(eB)
       }catch{
        case e: Throwable => println("Try to insert the object again")
      }
    }else if(typeObject.equals("physicalMagazine")){
      MyDataBase.mainQueryProcess("INSERT_ROW:physicalMagazine:"+listCols+";")
       try{
        var parts:Array[String]=listCols.split(",")
        var eB:PhysicalMagazine=new PhysicalMagazine(parts(0).toInt,parts(1),parts(2),parts(3),parts(4),parts(5).toInt)
        pMag.+:=(eB)
       }catch{
        case e: Throwable => println("Try to insert the object again")
      }
    }else{
      println("That type of object does not exist in the library")
    }
  }
  
  /**
   * borrow a physical object
   */
  def borrow(typeObject:String,id:Int,name:String){
    var times:Int = -1
    if(typeObject.equals("physicalBook")){
      for(book<-pBooks){
        if(book.title.equals(name)){
          times=book.timesBorrowed
        }
      }
      if(times!= -1){
      MyDataBase.mainQueryProcess("UPDATE_ROW:physicalBook:"+id+":timesBorrowed:+"+(times + 1)+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else if(typeObject.equals("physicalMagazine")){
      for(book<-pMag){
        if(book.title.equals(name)){
          times=book.timesBorrowed
        }
      }
      if(times!= -1){
        MyDataBase.mainQueryProcess("UPDATE_ROW:physicalMagazine:"+id+":timesBorrowed:+"+(times + 1)+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else{
      println("That type of object does not exist or can not be borrowed")
    }
  }
  
  /**
   * consult an electronic object internally
   */
  def internal(typeObject:String,id:Int,name:String){
    var times:Int = -1
    if(typeObject.equals("electronicalBook")){
      for(book<-eBooks){
        if(book.title.equals(name)){
          times=book.internalAccess
        }
      }
      if(times!= -1){
        times += 1
      MyDataBase.mainQueryProcess("UPDATE_ROW:electronicalBook:"+id+":internalAccess:+"+times+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else if(typeObject.equals("electronicalMagazine")){
      for(book<-eMag){
        if(book.title.equals(name)){
          times=book.internalAccess
        }
      }
      if(times!= -1){
        MyDataBase.mainQueryProcess("UPDATE_ROW:electronicalMagazine:"+id+":internalAccess:+"+(times + 1)+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else{
      println("That type of object does not exist or can not be accessed")
    }
  }
  
  /**
   * consult an electronic object externally
   */
  def external(typeObject:String,id:Int,name:String){
    var times:Int = -1
    if(typeObject.equals("electronicalBook")){
      for(book<-eBooks){
        if(book.title.equals(name)){
          times=book.internalAccess
        }
      }
      if(times!= -1){
      MyDataBase.mainQueryProcess("UPDATE_ROW:electronicalBook:"+id+":externalAccess:+"+(times + 1)+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else if(typeObject.equals("electronicalMagazine")){
      for(book<-eMag){
        if(book.title.equals(name)){
          times=book.internalAccess
        }
      }
      if(times!= -1){
        MyDataBase.mainQueryProcess("UPDATE_ROW:electronicalMagazine:"+id+":externalAccess:+"+(times + 1)+";")
      }else{
        println("Sorry, the id provided does not exist, try again")
      }
    }else{
      println("That type of object does not exist or can not be accessed")
    }
  }
  
  /**
   * delete a row from the specified table
   */
  def delete(typeObject:String,id:Int){
    if(typeObject.equals("electronicalBook")){
      MyDataBase.mainQueryProcess("DELETE_ROW:electronicalBook:"+id+";")
    }else if(typeObject.equals("electronicalMagazine")){
      MyDataBase.mainQueryProcess("DELETE_ROW:electronicalMagazine:"+id+";")
    }else if(typeObject.equals("physicalBook")){
      MyDataBase.mainQueryProcess("DELETE_ROW:physicalBook:"+id+";")
    }else if(typeObject.equals("physicalMagazine")){
      MyDataBase.mainQueryProcess("DELETE_ROW:physicalMagazine:"+id+";")
    }else{
      println("Sorry, the id provided does not exist, try again")
    }
  }
  
  
}