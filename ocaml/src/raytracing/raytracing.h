/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
 * _polyptr is a pointer to the polyhedron which calls the raytracing method. 
 * _start_point is the point where the ray starts.
 * _evaluate stores the evaluation of dot product of _start_point and each 
 * constraints of the polyhedra which _polyptr points to. 
 * The direction of the ray is contains in Ray objects (see Ray class).
 * Raytracing class contains methods of eliminating redundant constraints and
 * check inclusions.
*******************************************************************************/

#ifndef _EIGEN
#define _EIGEN
#include <eigen3/Eigen/Dense>
#endif
#include <vector>
#include <map>
#ifndef _POLYH
#define _POLYH
#include "polyhedron.h"
#endif
#include "ray.h"

class Raytracing {
public:
  Raytracing (Polyhedron& poly, const Point& point) ;  
  void RayHitting () ;
  void Determine (const bool checkInclusion = false, const int startIdx = 0) ;
  std::vector<int> GetIntersections (const Ray& ray) ;
  bool CheckRedundant (const int currIdx, const std::vector<int>& headIdx) ;
  double GetDistance (const int currIdx, const Ray& ray) ; 
  double GetDistance (const int currIdx, const double consDirect) ;
  // method 2: raytracing with two-direction ray
  void RayHittingTwoDirect() ;
  std::vector<int> GetInterTwoDirect (const Ray& ray) ;
  // method 3: raytracing with matrix
  void RayHittingMatrix () ;
  std::vector< std::vector<int> > GetMaxCoefIdx (const Matrix& matrix) ;
  // method 4: raytracing with matrix
  void RayHittingMatrixTwoDir (const bool checkInclusion = false, const int startIdx = 0) ;
  std::vector< std::vector<int> > GetMaxMinCoefIdx (const Matrix& matrix, const bool checkInclusion, const int startIdx) ;
  bool HasInclusion () const ;
private:
  Polyhedron* _polyptr ;
  const Point* _start_point ;
  Vector _evaluate ;
  // the index of the undetermined constraints
  std::vector<int> _undetermined ; 
  // the index of intersection for each constraint
  std::map< int, std::vector<int> > _intersectHead ;
  bool _hasInclusion ;
} ;
