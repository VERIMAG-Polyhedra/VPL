#ifndef _RAYTRACING_TOOL
#define _RAYTRACING_TOOL

#include <eigen3/Eigen/Dense>
#include <flint/fmpq_matxx.h>
typedef Eigen::MatrixXd Matrix ;
typedef Eigen::RowVectorXd Vector ;
typedef Eigen::MatrixXi MatrixZ ;
typedef Eigen::RowVectorXi VectorZ ;
typedef flint::fmpqxx RNumber ;
typedef flint::fmpq_matxx RMatrix ;
typedef flint::fmpzxx ZNumber ;

class Tool {
public: 
  long int static GetGcd(const long int val1, const long int val2) {
    long int res = val2 == 0 ? val1 : GetGcd(val2, val1 % val2) ;
    return std::abs(res) ;
  } 

  double static GetGcd(const double val1, const double val2) {
    double res = val2 == 0 ? val1 : GetGcd( val2, fmod(val1, val2) ) ;
    return std::abs(res) ;
  }

  double static GetGcd(const Vector& cons, double constant) {
    double gcd = Tool::GetGcd( constant, cons(0) ) ;
    if (gcd != 1) {
      for (int j = 1; j < cons.cols(); ++ j) {
        gcd = Tool::GetGcd( gcd, cons(j) ) ;
        if (gcd == 1) break ;
      }
    }
    return gcd ;
  }

  double static GetDotProductThreshold(const Vector& vec1, const Vector& vec2) ;
  double static GetDotProductThreshold(int size, double norm1, double norm2) ;

  RMatrix static GetBlock(const RMatrix& matrix, 
      int startRow, int startCol, int rowNum, int colNum) ;
} ;

#endif
