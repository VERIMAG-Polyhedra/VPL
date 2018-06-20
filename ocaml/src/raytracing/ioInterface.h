/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
 * This class provides some static functions for reading or writing data.
*******************************************************************************/

#ifndef _RAYTRACING_IOINTER
#define _RAYTRACING_IOINTER

#include <vector>
#include <string>
#include "polyhedron.h"

class IoInterface {
public:
  IoInterface() ;
  std::vector<Polyhedron> LoadPolyhedra (const char* filepath) ; 
  static void WritePoly(const std::string& filepath, const Polyhedron& poly) ;
  int get_cons_num() ;
  int get_vari_num() ;
  int get_zero_num() ;
  int get_redundancy() ;
private:
  std::string _filename ;
  int _cons_num ;
  int _vari_num ;
  int _zero_num ;
  int _redundancy ;
} ;

#endif
