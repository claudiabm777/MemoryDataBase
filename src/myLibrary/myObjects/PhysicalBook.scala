/**
 * physical book class
 */
class PhysicalBook(timesB:Int,lastD:String,nAuthor:String,nTitle:String,nNumberPages:Int) extends PublishObject with Physic{
  /**
   * times that has been borrowed
   */
  override var timesBorrowed:Int = timesB
  /**
   * last day actualization
   */
  override var lastDay:String = lastD
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
}