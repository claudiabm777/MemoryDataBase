/**
 * this trait represents the electronic behavior
 */
trait Physic extends PublishObject {
  /**
   * times that the object has been borrowed
   */
  var timesBorrowed:Int
  
  /**
   * last day that the object was consulted
   */
  var lastDay:String
}