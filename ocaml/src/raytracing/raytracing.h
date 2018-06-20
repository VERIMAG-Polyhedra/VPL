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

#ifndef _RAYTRACING_RAYTRACING
#define _RAYTRACING_RAYTRACING

#include <eigen3/Eigen/Dense>
#include <vector>
#include <map>
#include "polyhedron.h"
#include "ray.h"
#include "glpkInterface.h"
#include "tool.h"

#ifdef DEBUGINFO_RAYTRACING
#include <mutex>
#endif

struct Distance {
  Distance () : fst(-1.0), snd(-1.0), idx(-1.0),
      oppoFst(-1.0), oppoSnd(-1), oppoIdx(-1) {}
  double fst ;
  double snd ;
  int idx ;
  double oppoFst ;
  double oppoSnd ;
  int oppoIdx ;
} ;

class Raytracing {
public:
  Raytracing(Polyhedron& poly, const Point& point, bool getWitness) ;  
  void SetInclusion(int startIdx) ;

  void Determine() ;
  void Determine_step(int i);
  
  std::vector<int> GetIntersections(const Ray& ray, int currIdx) ;
  bool CheckRedundant(const int currIdx, std::vector<int>& headIdx) ;
  double GetDistanceInverse(const int currIdx, const double consDirect) ;
  static double GetDistanceInverse(const Vector& cons, const Ray& ray, double eva) ;
  static double GetDistance(const Point& point, const Vector& cons, 
      double constant, const Ray& ray) ;
  void RayHitting() ;
  void GetIrredundantCons(const Matrix& matrix) ;
  bool HasInclusion() const ;
private:
  std::vector<int> GetAllMetCons(Vector distanceVec, int currIdx) ;
  std::vector<int> GetSortedCons(Vector distanceVec, int currIdx) ;
  Distance GetIrrdDistance(const Vector& disVec) ; 
  Vector CalWitnessPoint(const Ray& ray, int currIdx) ;
  bool Farkas(int currIdx, const Ray& ray) ;
  Polyhedron* _polyptr ;
  const Point* _start_point ;
  Vector _evaluate ;
  // the index of the undetermined constraints
  std::vector<int> _undetermined ; 
  // the index of intersection for each constraint
  std::map< int, std::vector<int> > _intersectHead ;
  std::map< int, std::vector<int> > _sndHead ;

  bool _hasInclusion ;
  bool _check_inclusion ;
  int _start_idx ;
  bool _get_witness ;
} ;

#endif
