/**
 * class that represents a physical magazine
 */
class PhysicalMagazine(timesB:Int,lastD:String,nIssueNumber:String,nAuthor:String,nTitle:String,nNumberPages:Int) extends PublishObject with Physic with Magazine{
  /**
   * issue number of the magazine
   */
  override var issueNumber:String=nIssueNumber
  /**
   * times that has been borrowed
   */
  override var timesBorrowed:Int = timesB
  /**
   * last day actualization
   */
  override var lastDay:String = lastD
  /**
   * author of the magazine
   */
  override var author:String =nAuthor
  /**
   * title of the magazine
   */
  override var title:String = nTitle
  /**
   * number of pages of the magazine
   */
  override var numberPages:Int = nNumberPages
}