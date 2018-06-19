/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
*******************************************************************************/

#include <iostream>
#include <algorithm>
#include "raytracing.h"
#include "double.h"

#include <chrono>

#ifdef VERIMAG_POLYHEDRA_MINIMIZE_TBB
#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#endif

#ifdef DEBUGINFO_RAYTRACING
std::mutex log_mtx_raytracing;
#endif

bool SortConstraints(std::pair<int, double> c1, std::pair<int, double> c2) { 
  return c1.second > c2.second ;
}

Raytracing::Raytracing(Polyhedron& poly, const Point& point, bool getWitness)
    : _polyptr(&poly), _start_point(&point), _hasInclusion(true),
    _check_inclusion(false), _start_idx(0), _get_witness(getWitness) { 
  if ( poly.IsEmpty() ) {
    std::cerr << "Cannot create raytracing. The polyhedron is empty."
      << std::endl ;
    std::terminate() ;
  }
  if ( point.IsEmpty() ) {
    std::cerr << "Cannot create raytracing. The start point is empty."
      << std::endl ;
    std::terminate() ;
  }
  _evaluate = poly.get_constants() - 
      point.get_coordinates() * poly.get_coefficients().transpose() ;
}  

/*******************************************************************************
 * Set the parameters for inclusion
 * @para startIdx if we test if P1 is included in P2, then startIdx is the index
 * of the first constraint of P2
*******************************************************************************/
void Raytracing::SetInclusion(int startIdx) {
  _check_inclusion = true ;
  _start_idx = startIdx ;
}

/*******************************************************************************
 * Determine() is the second step of examining the redundancy of constraints.
 * It checks the undetermined constraints until all constraints are determined 
 * as irredundant or redundant. 
 * If any constraint cannot be determied the program aborts. 
 * @para checkInclusion is true if Determin() is called for check inclusion
 * @para startIdx the index of the first constraint of P2, if we are checking
 * if P1 is included in P2
*******************************************************************************/
void Raytracing::Determine_step(int i) {
  if (_check_inclusion && !_hasInclusion) return;
  int currIdx = _undetermined.at(i) ;
  if (! (_check_inclusion && currIdx < _start_idx)) {
    std::vector<int> headIdx ;
    headIdx.push_back(currIdx) ;
    int currHeadIdx ;
    for (int i = 0; i < (int)_intersectHead[currIdx].size(); ++ i) {
      currHeadIdx = _intersectHead[currIdx].at(i) ;
      if (currHeadIdx != currIdx) {
	headIdx.push_back( _intersectHead[currIdx].at(i) ) ;
      }
    }
    bool isRedundant = CheckRedundant(currIdx, headIdx) ;
    if( ! isRedundant) {
      if (_check_inclusion) {
	_hasInclusion = false ;
#ifdef VERIMAG_POLYHEDRA_MINIMIZE_TBB
	// cancel all tasks in the parallel_for
	tbb::task::self().cancel_group_execution();
#endif
	return;
      }
      _polyptr->Activate(currIdx) ;
#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Second step: constraint " << currIdx
      << " is irredundant" << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif
    }
  }
}

#ifdef VERIMAG_POLYHEDRA_MINIMIZE_TBB
class determine_body {
  Raytracing *raytracing;
public:
  determine_body(Raytracing *r) : raytracing(r) { }
  
  void operator()(tbb::blocked_range<unsigned> &range) const {
    //std::cout << "chunk: " << range.size() << std::endl;
    for(unsigned i=range.begin(); i<range.end(); i++) {
      raytracing->Determine_step(i);
    }
  }
};
#endif

void Raytracing::Determine() {  

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "Second step of raytracing starts." << std::endl ;
#endif

  if (_check_inclusion && !_hasInclusion) {
    return ;
  }

#ifdef VERIMAG_POLYHEDRA_MINIMIZE_TBB
  tbb::parallel_for(tbb::blocked_range<unsigned>(0, _undetermined.size()),
		    determine_body(this));
#else
#ifdef VERIMAG_POLYHEDRA_MINIMIZE_OPENMP
#pragma omp parallel for schedule(dynamic)
  for
#elif defined(VERIMAG_POLYHEDRA_MINIMIZE_CILK)
  _Cilk_for
#else
  for
#endif
    (int i = 0; i < (int)_undetermined.size(); ++ i) {
      Determine_step(i);
    }
#endif
}

/*******************************************************************************
 * GetIntersections() computes the distance between the start point and each 
 * intersection of a ray and the constraints. It stores the index of constraints
 * which are nearest to the start point.
 * @para cuurIdx the index of the constraint which is orthogonal to the ray. We
 * assume it is nearest one to the start point at first.
 * @para ray the ray which provides the direction.
 * @return a list of index of constraints, which are nearest to the start point. 
*******************************************************************************/
std::vector<int> Raytracing::GetIntersections(const Ray& ray, int currIdx) {
  std::vector<int> head ;
  int consNum ;
  if (_check_inclusion) {
    consNum = _start_idx ;
  }
  else {
    consNum = _polyptr->get_constraint_num() ;
  }
  int variNum = _polyptr->get_variable_num() ;
  Vector products = ray.get_direction() * _polyptr -> get_coefficients().transpose()  ;
  double currIdxDis = GetDistanceInverse( currIdx, products(currIdx) ) ;

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "GetIntersections: distance to the constraint to be determined: "
      << currIdxDis  << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

  double threshold, currNorm, currDistance = -1, sndDistance = -1 ;
  for (int i = 0; i < consNum; ++ i) {
    //if (_check_inclusion && i >= _start_idx) continue ;
    if (currIdx == i) continue ;
    double consDirect = products(i);
    currNorm = _polyptr->get_coefficients().row(i).norm() ;
    threshold = Tool::GetDotProductThreshold(variNum, currNorm, 1) ;
    if ( Double::IsLessEq(consDirect, 0.0, threshold) ) continue ;
    currDistance = GetDistanceInverse(i, consDirect) ;

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "GetIntersections: currDistance inverse: "
      << currDistance << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    // the threshold 1e-10 will not effect the result
    if ( Double::IsLessEq(currIdxDis, currDistance, 1e-10) ) {
      head.push_back(i) ;
    }
    else if (currDistance > sndDistance) {
      sndDistance = currDistance ;
    }
  }
  if (head.size() == 0) {
    std::vector<int> singleVec ;
    singleVec.push_back(currIdx) ;
    if (_get_witness) {
      double newDis ;
      if (sndDistance == -1) {
        newDis =  (1/currIdxDis + 1) ;
      }
      else {
        newDis = (1/currIdxDis + 1/sndDistance) / 2 ;
      }
      Vector witness = _start_point->get_coordinates() +
          ray.get_direction() *  newDis ;
      _polyptr->AddWitnessPoint(currIdx, witness) ;

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Second step: Get witness for " << currIdx
      << " : " << witness << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    }

    return singleVec ;
  } 
  else {
    return head ;
  }
}


/*******************************************************************************
 * GetDistanceInverse() computes the distance between a point to a constraint in the 
 * direction of a ray. We compute the multiplicative inverse of the distance
 * to avoid dividing by 0.
 * @para currIdx the index of the constraint to compute
 * @para ray the ray which provides the direction
 * @return the multiplicative inverse of the distance 
*******************************************************************************/
double Raytracing::GetDistanceInverse(const int currIdx, const double consDirect) {
  double norm = _polyptr->get_coefficients().row(currIdx).norm() ;
  int variNum = _polyptr->get_variable_num() ;
  double threshold = Tool::GetDotProductThreshold(variNum, norm, 1) ;
  if ( Double::AreEqual(consDirect, 0, threshold) ) {
    return 0 ;
  }
  double temp1 = _evaluate(currIdx) ; 
  double res = consDirect / temp1 ;
  return res ;
}

/*******************************************************************************
 * eva = constant - cons * point.get_coordinates().transpose() ;
 * Be attention: if necessary, check the point is not on the boundaries.
*******************************************************************************/
double Raytracing::GetDistanceInverse(const Vector& cons,
    const Ray& ray, double eva) {
  double den = ray.get_direction() * cons.transpose() ;
  double threshold = Tool::GetDotProductThreshold(cons.size(), cons.norm(), 1) ;
  if ( Double::AreEqual(den, 0, threshold) ) {
    return 0 ;
  }
  return den/eva ;
}
/*
double Raytracing::GetDistanceInverse(const Point& point, const Vector& cons, 
      double constant, const Ray& ray) {
  double den = ray.get_direction() * cons.transpose() ;
  double threshold = Tool::GetDotProductThreshold(cons.size(), cons.norm(), 1) ;
  if ( Double::AreEqual(den, 0, threshold) ) {
    return 0 ;
  }
  double num = constant - cons * point.get_coordinates().transpose() ;
  return den/num ;
}
*/

/*******************************************************************************
 * Use it when we know the den is not 0
*******************************************************************************/
double Raytracing::GetDistance(const Point& point, const Vector& cons, 
      double constant, const Ray& ray) {
  double den = ray.get_direction() * cons.transpose() ;
  double num = constant - cons * point.get_coordinates().transpose() ;
  return num/den ;
}

/*******************************************************************************
 * CheckRedundant() checks each undetermined constraint, and determine if the 
 * constraint is redundant.
 * @para currIdx the constraint to determine
 * @para headIdx the list of index of constraints which are nearest to the 
 * start point in the direction of the ray orthogonal to the constraint currIdx. 
 * @return ture if the constraint is redundant.
*******************************************************************************/
bool Raytracing::CheckRedundant(const int currIdx, std::vector<int>& headIdx) {
  GlpkInterface glpkinter ;

  // the loop has an upper bound which is the number of the constraints
  int consNum = _polyptr->get_constraint_num() ;
  std::vector<int> allHead = headIdx ;
  for (int i = 0; i < consNum; ++ i) {

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Check redundancy of constraint " << currIdx << std::endl ;
  std::cout << "Head index: " ;
  for (int i = 0; i < (int)headIdx.size(); ++ i) {
    std::cout << headIdx[i] << " " ;
  }
  std::cout << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    Point newPoint = glpkinter.GetIrddWitness(headIdx, *_polyptr) ;

    if ( newPoint.IsEmpty() ) {

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Cannot find a irredundant witness point."
      << "The current constraint is redundant" << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

      return true ;
    }

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "found point: " << newPoint << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    Ray currRay(newPoint, *_start_point) ;
    std::vector<int> currInter = GetIntersections(currRay, currIdx) ;
    if (currInter.size() == 1 && currInter[0] == currIdx) {
      return false ;
    }
    else {
      if (_check_inclusion) {
        headIdx = _sndHead[currIdx] ; 
      }
      else {
        headIdx.clear() ;
        for (int i = 0; i < (int)currInter.size(); ++ i) {
          if (currInter[i] == currIdx) continue ;
          auto exist = std::find( allHead.begin(), allHead.end(), currInter[i] ) ;
          if ( exist != allHead.end() ) {

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "head " << currInter[i] << " exist, use Farkas" << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

            return Farkas(currIdx, currRay) ; 
          } 
          headIdx.push_back( currInter[i] ) ;
        }
        for (int i = 0; i < (int)currInter.size(); ++ i) {
          allHead.push_back( currInter[i] ) ;
        }
        // The ray hit a constraint of P2, then P1 is not included in P2,
        // i.e. one of the constraints of P2 is irredundant.
        //if (_check_inclusion && headIdx.size() == 0) return false ;
      }
    }
  }
  // It should NOT reach here. Just in case.
  std::cout << "raytracing cannot determine the current constraint" 
      << std::endl ;
  std::terminate() ;
  //return Farkas(currIdx) ;
}

/*******************************************************************************
 * the first step of testing the redundancy of constraints.
 * It creates an orthogonal ray for each constraint. In the direction of each 
 * ray, it compute the distance between the start point to each constraint. 
 * If a constraint is the single nearest one, then it is irredundant.
 * This function computes the intersections with matrix, and it tests with
 * two direction rays.
 * @para checkInclusion is true if Determin() is called for check inclusion
 * @para startIdx the index of the first constraint of P2, if we are checking
 * if P1 is included in P2
********************************************************************************/
void Raytracing::RayHitting() {
#ifdef DEBUGINFO_RAYTRACING
  std::cout << "First step of raytracing starts." << std::endl ;
#endif

  const int consNum = _polyptr->get_constraint_num() ;
  int variNum = _polyptr->get_variable_num() ;
  Matrix rayMatrix = _polyptr->get_coefficients().transpose() ;
  for (int i = 0 ; i < consNum; ++ i) {
    rayMatrix.col(i).normalize() ; 
  }
  Matrix normMatrix = _polyptr->get_coefficients() * rayMatrix ;

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "The matrix of a*r: " << std::endl << normMatrix << std::endl ;
#endif

  double currConsNorm ;
  for (int i = 0; i < normMatrix.rows(); ++ i) {
    currConsNorm = _polyptr->get_coefficients().row(i).norm() ;   
    for (int j = 0; j < normMatrix.cols(); ++ j) {
      if ( std::abs( normMatrix(i, j) ) < Tool::GetDotProductThreshold(variNum, currConsNorm, 1) ) {
        normMatrix(i, j) = 0 ;
#ifdef DEBUGINFO_RAYTRACING
  std::cout << "Set the matrix of a*r at (" << i << "," << j << ") as 0"  << std::endl ;
#endif
      } 
    }
  }
  Matrix evaMatrix = _evaluate.transpose().rowwise().replicate(consNum) ; 
  Matrix distanceMatrix = normMatrix.cwiseQuotient(evaMatrix) ; 
  // the result in the form of 
  //      r1     r2   ....   rn
  // c1  res11  res12       res1n
  // ...
  // cn  resn1              resnn                 
  // we need to find the minimum positive value of each column 
  // To simplify the computation, we store 1/distance, which means
  // we will find the maximum value in each column
  GetIrredundantCons(distanceMatrix) ;
  
  if(_check_inclusion == true && _hasInclusion == false) {
    return ;
  }

  // push from the last ones for checking inclusion
  for (int i = consNum-1; i >= 0; -- i) {
    if ( _polyptr->GetConstraintState(i) == ConstraintState::redundant) {
      _undetermined.push_back(i) ;
    }
  }
  for (int i = 0; i < (int)_undetermined.size(); ++ i) {
    int currIdx = _undetermined[i] ;
    if (_check_inclusion) {
      _intersectHead[currIdx] =
          GetAllMetCons(distanceMatrix.col(currIdx).transpose(), currIdx) ;
    }
    else {
      _intersectHead[currIdx] =
          GetSortedCons(distanceMatrix.col(currIdx).transpose(), currIdx) ;
    }
  }
}

std::vector<int> Raytracing::GetAllMetCons(Vector distanceVec, int currIdx) {
  std::vector<int> newVec ;
  double curr ;
  for (int i = 0; i < distanceVec.size(); ++ i) {
    if (_check_inclusion && i >= _start_idx) continue ;
    curr = distanceVec(i) ;
    if ( curr > 0 ) {
      newVec.push_back(i) ;
    }
    else {
      _sndHead[currIdx].push_back(i) ;
    }
  }
  return newVec ;
}

std::vector<int> Raytracing::GetSortedCons(Vector distanceVec, int currIdx) {
  std::vector< std::pair<int, double> > pairVec ;
  std::vector<int> newVec ;
  double curr ;
  double currConsDis = distanceVec(currIdx) ; 
  for (int i = 0; i < distanceVec.size(); ++ i) {
    if (_check_inclusion && i >= _start_idx) continue ;
    curr = distanceVec(i) ;
    // Note that the values in distanceVec are the inverse numbers of distance
    if (currConsDis < curr) {
      pairVec.push_back( std::pair<int, double>(i, curr) ) ;
    }
  }
  std::sort(pairVec.begin(), pairVec.end(), SortConstraints) ; 
  for (int i = 0; i < _polyptr->get_variable_num()
      && i < (int)pairVec.size(); ++ i) {
    newVec.push_back(pairVec[i].first) ;
  }
  return newVec ;
} 

/*******************************************************************************
 * Calculate the distance for obtaining the witness point in the first phase
*******************************************************************************/
Distance Raytracing::GetIrrdDistance(const Vector& disVec) {
  Distance distance ;
  double maxVal = 0.0, minVal = 0.0 ;
  int maxIdx = -1, minIdx = -1 ;
  // find the first met constraints in two directions
  for (int i = 0; i < disVec.size(); ++ i) {
    double currVal = disVec(i) ;
    if (currVal < 0.0) {
      // if by chance they are really equal...
      // the value of the threshold is not important,
      // as it will not effect the result
      if ( Double::AreEqual(currVal, minVal, 1e-6) ) {
        minIdx = -1 ;
      }
      else if (currVal < minVal) {
        minVal = currVal ;
        minIdx = i ;
      }
    }
    else {
      // if by chance they are really equal...
      // the value of the threshold is not important,
      // as it will not effect the result
      if ( Double::AreEqual(currVal, maxVal, 1e-6) ) {
        maxIdx = -1 ;
      }
      else if (maxVal < currVal) {
        maxVal = currVal ;
        maxIdx = i ;
      } 
    }
  }
  if (maxIdx != -1) {
    distance.fst = 1/maxVal ;
    distance.idx = maxIdx ;
  }
  if (minIdx != -1) {
    distance.oppoFst = -1/minVal ;
    distance.oppoIdx = minIdx ;
  }

  if (_get_witness) {
    double sndMaxVal = 0.0, sndMinVal = 0.0 ;
    int sndMaxIdx = -2, sndMinIdx = -2 ;
    // find the second met constraints in two directions
    for (int i = 0; i < disVec.size(); ++ i) {
      double currVal = disVec[i] ;
      if ( i == maxIdx || i == minIdx) continue ;
      if (currVal < 0.0) {
        if (currVal < sndMinVal) {
          sndMinVal = currVal ;
          sndMinIdx = i ;
        }
      }
      else {
        if (sndMaxVal < currVal) {
          sndMaxVal = currVal ;
          sndMaxIdx = i ;
        }
      }
    }
    if (sndMaxIdx != -2) {
      distance.snd = 1/sndMaxVal ;
    }
    if (sndMinIdx != -2) {
      distance.oppoSnd = -1/sndMinVal ;
    }
  }

  return  distance ;
}

/*******************************************************************************
 * Calculate the witness point in the second phase
*******************************************************************************/
Vector Raytracing::CalWitnessPoint(const Ray& ray, int currIdx) {
  int consNum = _polyptr->get_constraint_num() ;
  int variNum = _polyptr->get_variable_num() ;
  double product = _polyptr->get_coefficients().row(currIdx)
      * ray.get_direction().transpose() ;
  double currIdxDis = GetDistanceInverse(currIdx, product) ;
  double threshold, currNorm, consDirect, currDistance, sndDistance = -1 ;
  for (int i = 0; i < consNum; ++ i) {
    if (currIdx == i) continue ;
    currNorm = _polyptr->get_coefficients().row(i).norm() ;
    threshold = Tool::GetDotProductThreshold(variNum, currNorm, 1) ;
    consDirect = _polyptr->get_coefficients().row(i)
        * ray.get_direction().transpose() ;
    if ( Double::IsLessEq(consDirect, 0.0, threshold) ) continue ;
    currDistance = GetDistanceInverse(i, consDirect) ;
    if (currDistance > sndDistance) {
      sndDistance = currDistance ;
    }
  }
  double newDis ;
  if (sndDistance != -1) {
    newDis = (1/currIdxDis + 1/sndDistance) / 2 ; 
  }
  else {
    newDis = 1/currIdxDis + 1 ;
  }
  return _start_point->get_coordinates() + ray.get_direction() * newDis ;
}


/*******************************************************************************
 * Gets the maximum value of each column, and return the coressponding index of
 * the constraints which to be activated. Gets the minimum value of each column, and 
 * activate the coressponding constraints.
 * @para matrix the distance matrix getten from RayHittingMatrix() 
 * if P1 is included in P2
 * @return the index of the nearest constraint if it is the single nearest one
********************************************************************************/
void Raytracing::GetIrredundantCons(const Matrix& matrix) {
#ifdef DEBUGINFO_RAYTRACING
  std::cout << "Distance matrix (multiplicative inverse of distance):"
      << std::endl << matrix << std::endl ;
#endif

  for (int j = 0; j < matrix.cols(); ++ j) {
    // if the current constraint has been hit
    if ( _polyptr->IsActive(j) ) continue ;
    Distance distance = GetIrrdDistance( matrix.col(j).transpose() ) ;
    if (distance.idx != -1) {
      _polyptr->Activate(distance.idx) ;

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "First step: constraint " << distance.idx
      << " is irredundant." << std::endl ;
#endif

    // compute witness
    if (_get_witness) {
      double witnessDis ;
      Ray currRay( _polyptr->get_coefficients().row(j) ) ;
      if (distance.snd > 0.0) {
        witnessDis = (distance.fst + distance.snd) / 2 ; 
      }
      else {
        witnessDis = distance.fst + 1.0 ; 
      }
      Vector witness = _start_point->get_coordinates() 
            + currRay.get_direction() * witnessDis ;
      _polyptr->AddWitnessPoint(distance.idx, witness) ;

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "First step: Get witness for " << distance.idx
      << " : " << witness << std::endl ;
#endif

      }
    }

    // the same for the opposite direction
    if (distance.oppoIdx != -1) {
      _polyptr->Activate(distance.oppoIdx) ;

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "First step opposite direction: constraint " << distance.oppoIdx
      << " is irredundant." << std::endl ;
#endif

    // compute witness
    if (_get_witness) {
      double witnessDis ;
      Ray currRay( - _polyptr->get_coefficients().row(j) ) ;
      if (distance.oppoSnd > 0.0) {
        witnessDis = (distance.oppoFst + distance.oppoSnd) / 2 ; 
      }
      else {
        witnessDis = distance.oppoFst + 1.0 ; 
      }
      Vector witness = _start_point->get_coordinates() 
            + currRay.get_direction() * witnessDis ;
      _polyptr->AddWitnessPoint(distance.oppoIdx, witness) ;

#ifdef DEBUGINFO_RAYTRACING
  std::cout << "First step opposite direction: Get witness for "
      << distance.oppoIdx << " : " << witness << std::endl ;
#endif

      }
    }
  }
}

bool Raytracing::HasInclusion() const {
  return _hasInclusion ;
}

/*******************************************************************************
 * Test if the current constraint is redundant using Farkas' lemma. 
 * @para currIdx the index of the constraint to be tested
 * @return true if the current constraint is redundant 
********************************************************************************/
bool Raytracing::Farkas(int currIdx, const Ray& ray) {
  int consNum = _polyptr->get_constraint_num() ;
  int variNum = _polyptr->get_variable_num() ;
  int lpConsNum = variNum + 1 ;
  int lpVariNum = consNum ; 
  Polyhedron lp(lpConsNum, lpVariNum) ;
  double curr ;
  // coefficients
  for (int j = 0; j < variNum; ++ j) {
    for (int i = 0, k = 0; i < consNum; ++ i) {
      if (i == currIdx) continue ;
      curr = _polyptr->GetCoef(i, j) ; 
      lp.SetCoef(j, k, curr) ; 
      ++ k ;
    }
    lp.SetCoef(j, lpVariNum-1, 0.0) ; 
    curr = _polyptr->GetCoef(currIdx, j) ;
    lp.SetConstant(j, curr) ;
    lp.SetOperator(j, ConstraintOperator::equal) ;
  }
  // constants
  for (int i = 0, k = 0; i < consNum; ++ i) {
    if (i == currIdx) continue ;
    curr = _polyptr->GetConstant(i) ; 
    lp.SetCoef(lpConsNum-1, k, curr) ; 
    ++ k ;
  }
  lp.SetCoef(lpConsNum-1, lpVariNum-1, 1.0) ; 
  curr = _polyptr->GetConstant(currIdx) ;
  lp.SetConstant(lpConsNum-1, curr) ;
  lp.SetOperator(lpConsNum-1, ConstraintOperator::equal) ;

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Farkas: test constraint " << currIdx << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

  GlpkInterface glpk ;
  bool res =  glpk.Simplex(lp, Vector(), true, true, false) ;
#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "Farkas result: " << res << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

  // add witness point
  if (_get_witness && res == false) {
    Vector witness = CalWitnessPoint(ray, currIdx) ; 
    _polyptr->AddWitnessPoint(currIdx, witness) ;
  }

  return res ;
}
