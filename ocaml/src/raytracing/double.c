#include "double.h"

/*******************************************************************************
 * @return true if d1 is less than d2
*******************************************************************************/
bool Double::IsLessThan(double d1, double d2, double epsilon) {
  return d1-d2 < -epsilon ;
}

/*******************************************************************************
 * @return true if d1 is equal to d2
*******************************************************************************/
bool Double::AreEqual(double d1, double d2, double epsilon) {
  double diff = d1 - d2 ;
  return (diff <= epsilon && diff >= -epsilon) ;
}

/*******************************************************************************
 * @return true if d1 is less than d2 or d1 is equal to d2
*******************************************************************************/
bool Double::IsLessEq(double d1, double d2, double epsilon) {
  return d1-d2 <= epsilon ;  
}
