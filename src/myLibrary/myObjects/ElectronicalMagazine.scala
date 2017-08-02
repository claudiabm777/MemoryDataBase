/**
 * electronic magazine class
 */
class ElectronicalMagazine(nIssueNumber:String,nAuthor:String,nTitle:String,nNumberPages:Int,nExternalAccess:Int,nInternalAccess:Int) extends PublishObject with Electronic with Magazine{
  /**
   * issue number of the magazine
   */
  override var issueNumber: String = nIssueNumber
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
  /**
   * number of external access 
   */
  override var externalAccess:Int = nExternalAccess
  /**
   * number of internal access
   */
  override var internalAccess:Int = nInternalAccess
}