/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
 * A polyhedron contains many constraints in the form:
 * a_1 x_1 + a_2 x_2 + ... + a_n x_n <op> b, where <op> is <, <= or =, and 
 * b is a constant. <op> is stored in _operators, and 0 means "<", 1 means "<="
 * and 2 means "=".
 * The matrix _coefficients stores the coefficients of the variables, and the
 * vector _constants stores the constant in each row 
 * _internal_point is a point inside of the polyhedron. It maybe a central point,
 * in which case the sum of distance from this point to each facet of the 
 * polyhedron is minimized.
 * _active_table is used to mark redundancy now, i.e. its ith value is true if
 * the ith constraint is irredundant. 
 * A witness point for constraint Ci is a point which violates Ci and satisfies 
 * the other constraints. We sotre the direction of the ray which reaches the
 * witness point from the central point while minimizing the polyhedra. If the
 * witness points are need, we can compute them as we know the ray. (We do not
 * compute the witness points during minimization to make the minimization 
 * process be efficient. Besides not all the cases of minimization require the
 * witness points). 
*******************************************************************************/

#ifndef _RAYTRACING_POLYHEDRON
#define _RAYTRACING_POLYHEDRON

#include <vector>
#include <map>
#include "point.h"
#include "double.h"
#include "tool.h"

/* TODO TODO
The chunk size should be adjusted so that sub-minimizations are not too costly.
*/
#if defined(_TBB)
//#define VERIMAG_POLYHEDRA_MINIMIZE_TBB
#elif defined (_OPENMP)
#define VERIMAG_POLYHEDRA_MINIMIZE_OPENMP
#elif defined( __cilk)
#define VERIMAG_POLYHEDRA_MINIMIZE_CILK
#endif

#ifdef DEBUGINFO_PLP
#include <mutex>
extern std::mutex log_mtx_raytracing;
#endif


enum class ConstraintState {redundant, irredundant} ;
enum class ConstraintOperator {less, lesseq, equal} ;
enum class CmpOperator {less, equal, greater} ;
enum PointPos {inside=-1, outside=-2} ;

class Polyhedron {
public:
  Polyhedron() ;
  void SetSize(int consNum, int variNum) ;
  Polyhedron(int consNum, int variNum) ; 
  /*
  Polyhedron(const Polyhedron& from) = delete ;
  Polyhedron& operator=(const Polyhedron& from) = delete ;
  Polyhedron(Polyhedron&& from) = default ;
  Polyhedron& operator=(Polyhedron&& from) = default ;
  */
  void Init() ;
  bool Minimize(bool getWitness = false) ;
  void MinimizeSimple() ;
  Polyhedron GetMinimizedPoly(bool getWitness = false) ;
  
//#if VERIMAG_POLYHEDRA_WITH_APRON
  //bool EqualApronMinimize() ;
//#endif
  
  Polyhedron GetSubPoly(const std::vector<int>& indexVec) ;
  bool IsEqualTo(const Polyhedron& poly) ;
  bool Include(Polyhedron& poly) ;
  bool IncludeStandard(Polyhedron& poly) ;
  Polyhedron Combine(const Polyhedron& poly1) ;
  bool Satisfy(const Point& point, int index, bool strict=false) const ;
  bool Satisfy( const Point& point, bool strict=false) const ;
  int PointOnBoundary(const Point& point) const ;
  std::vector<int> GetDuplicateIdx() ;
  std::vector<int> GetActiveIdx() const ;
  std::vector<int> GetInactiveIdx() const ;
  bool IsActive(int consIdx) const ;
  std::string GetOperatorStr(int consIdx) const ;
  bool IsEmpty() const ; 
  bool IsOpen() const ;
  bool IsMinimized() const ;
  void PrintActiveIdx() const ;
  void Print() const ;
  void PrintConstraint(int idx) const ;
  //void SetWitnessRay(int idx, const Vector& rayDirect) ;
  //Vector GetWitnessRay(int idx) const ;
  std::vector<Point> GetWitness() ;
  bool GetExactSolution(int objdir) ;
  bool GetExactSolution(const Vector& obj, int objdir) ;
  // inline functions 
  int get_variable_num() const {
    return _variable_num ;
  }
  void set_variable_num(int num) {
    _variable_num = num ;
  }
  
  int get_constraint_num() const {
    return _constraint_num ;
  }
  void set_constraint_num(int num) {
    _constraint_num = num ;  
  }

  int get_redundant_num() const {
    return _redundant_num ;
  }
  void set_redundant_num(int num) {
    _redundant_num = num ;
  }

  int get_zero_num() const {
    return _zero_num ;
  }
  void set_zero_num(int num) {
    _zero_num = num ;
  }

  int get_generator_num() const {
    return _generator_num ;
  }
  void set_generator_num(int num) {
    _generator_num = num ;
  }

  int get_id() const {
    return _id ;
  }
  void set_id(int id) {
    _id = id ;
  }

  void set_internal_point(const Point& point) {
    _internal_point = point ;
  }
  Point get_internal_point() const {
    return _internal_point ;
  }

  Vector GetConstraint(int consIdx) const {
    return _coefficients.row(consIdx) ;
  }
  void SetConstraint(int consIdx, const Vector& cons) {
    _coefficients.row(consIdx) = cons ;
  }

  double GetConstant(int consIdx) const {
    return _constants(consIdx) ;
  } 
  void SetConstant(int consIdx, double val) {
    _constants(consIdx) = val ;
  }

  ConstraintOperator GetOperator(int consIdx) const {
    return _operators[consIdx] ;
  }
  void SetOperator(int consIdx, ConstraintOperator op) {
    _operators[consIdx] = op ;
  }

  double GetCoef(int consIdx, int variIdx) const {
    return _coefficients(consIdx, variIdx) ;
  }
  void SetCoef(int consIdx, int variIdx, double val) {
    _coefficients(consIdx, variIdx) = val ;
  }
  
  const Matrix& get_coefficients() const {
    return _coefficients;
  }

  void set_coefficients(const Matrix& matrix) {
    _coefficients = matrix ;
  }

  const Vector& get_constants() const {
    return _constants;
  }

  void set_constants(const Vector& vec) {
    _constants = vec ;
  }

  ConstraintState GetConstraintState(int idx) const {
    return _active_table[idx] ;
  } 
  void Activate(int consIdx) {
#ifdef VERIMAG_POLYHEDRA_MINIMIZE_OPENMP
#pragma omp atomic write
#endif
    _active_table[consIdx] = ConstraintState::irredundant ;
  }
  void Deactivate (int consIdx) {
#ifdef VERIMAG_POLYHEDRA_MINIMIZE_OPENMP
#pragma omp atomic write
#endif
    _active_table[consIdx] = ConstraintState::redundant ;
  }
  void set_dis_lower_bound(double val) {
    _dis_lower_bound = val ;
  }
  double get_dis_lower_bound() {
    return _dis_lower_bound ;
  }
  void AddWitnessPoint(int idx, const Vector& witness) {
    _witness_point[idx] = Point(witness) ;
  }
private:
  Matrix _coefficients ;
  Vector _constants ;
  std::vector<ConstraintOperator> _operators ;
  int _constraint_num ;
  int _variable_num ;
  int _redundant_num ;
  int _zero_num ;
  int _generator_num ;
  int _id ; 
  Point _internal_point ;
  //std::vector<char> _active_table ;
  std::vector<ConstraintState> _active_table ;
  bool _is_minimized ;
  //Matrix _witness_ray ;
  std::vector<Point> _witness_point ;
  // the lower bound of the distance from a point to any constraint
  double _dis_lower_bound ;
  CmpOperator CmpConstraint(const Vector& cons1, const Vector& cons2) ;
  void SortMatrix(Matrix& m) ;

  //TODO just have a try
  RNumber _exact_solution ; 
} ;

#endif
