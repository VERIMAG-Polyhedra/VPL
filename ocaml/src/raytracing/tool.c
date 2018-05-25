#include <limits>
#include <cmath>
#include "tool.h"

double Tool::GetDotProductThreshold(const Vector& vec1, const Vector& vec2) {
  int size = vec1.size() ;
  double norm1 = vec1.norm() ;
  double norm2 = vec2.norm() ;
  return GetDotProductThreshold(size, norm1, norm2) ;
}

double Tool::GetDotProductThreshold(int size, double norm1, double norm2) {
  double epsilon = std::numeric_limits<double>::epsilon() ;
  int s = ceil( log2(size) ) + 1 ; 
  return (s * epsilon) / (1 - s * epsilon) * norm1  * norm2 ;
}
