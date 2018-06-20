/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
 * Point class contains the coordinate of a point, and the boolean value marks
 * if the point is empty.
*******************************************************************************/

#ifndef _RAYTRACING_POINT
#define _RAYTRACING_POINT

#include <eigen3/Eigen/Dense>
#include <ostream>
#include "tool.h"

class Point {
public:
  Point() ;
  Point(const Vector& coordinates) ;
  void set_coordinates(const Vector& coordinates) ;
  Vector get_coordinates() const ;
  bool IsEmpty() const ;
  void Clear() ;
  friend std::ostream& operator<<(std::ostream& out, const Point& point) ;
  Point& operator=(const Point& point) ;
  void SetCoordinate(int idx, double val) ;
  void set_is_empty(bool empty) ;
  double GetPointDistance(const Point& p) const ;
private:
  Vector _coordinates ;
  bool _is_empty ;
} ;

#endif
