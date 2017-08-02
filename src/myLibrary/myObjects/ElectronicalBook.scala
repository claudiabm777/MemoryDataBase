/**
 * electronic book class
 */
class ElectronicalBook(nAuthor:String,nTitle:String,nNumberPages:Int,nExternalAccess:Int,nInternalAccess:Int) extends PublishObject with Electronic {
  /**
   * author of the book
   */
  override var author:String =nAuthor
  /**
   * title of the book
   */
  override var title:String = nTitle
  /**
   * number of pages of the book
   */
  override var numberPages:Int = nNumberPages
  /**
   * number of external access 
   */
  override var externalAccess:Int = nExternalAccess
  /**
   * number of external access
   */
  override var internalAccess:Int = nInternalAccess

}