
/**
 * enum that contains the possible operators to apply in a query in the database
 */
object OperatorNames extends Enumeration {
   type IntOperatorNames = Value
    val GREATHER, GREATHER_EQUAL,LOWER,LOWER_EQUAL, EQUAL = Value
}