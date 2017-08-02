/**
 * this trait represents the electronic behavior
 */
trait Electronic extends PublishObject {
  /**
   * number of external access
   */
  var externalAccess:Int
  
  /**
   * number of internal access
   */
  var internalAccess:Int
}