/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
*******************************************************************************/

#include "point.h"

Point::Point() : _is_empty(true) {}

Point::Point(const Vector& coordinates) {
  _coordinates = coordinates ;
  if (coordinates.size() != 0) {
    _is_empty =false ;
  }    
  else {
    _is_empty =true ;
  }
}

void Point::set_coordinates(const Vector& coordinates) {
  _coordinates = coordinates ;
  if (coordinates.size() != 0) {
    _is_empty = false ;
  }
}

Vector Point::get_coordinates(void) const {
  return _coordinates ;
}

bool Point::IsEmpty() const {
  return _is_empty ;
}

void Point::Clear() {
  _is_empty = true ; 
}

std::ostream& operator<<(std::ostream& out, const Point& point)
{
  out << point._coordinates ; 
  return out ;
}

Point& Point::operator=(const Point& point) {
  _coordinates = point._coordinates ;
  _is_empty = point._is_empty ;
  return *this ;
}

/*******************************************************************************
 * Set the coordinates at idx as val.
 * Be attention: this function will NOT change the state of the point, 
 * i.e. after calling this function the point is still empty,
 * Please call set_is_empty() to change the state of the point.
*******************************************************************************/
void Point::SetCoordinate(int idx, double val) {
  _coordinates[idx] = val ;
}

void Point::set_is_empty(bool empty) {
  _is_empty = empty ; 
}

/*******************************************************************************
 * The distance from the point to another point provided by the argument.
*******************************************************************************/
double Point::GetPointDistance(const Point& p) const {
  Vector res = _coordinates - p._coordinates ;
  return res.norm() ; 
}
