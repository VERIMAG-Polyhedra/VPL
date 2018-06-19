/*******************************************************************************
 * Copyright (c) 2016 Dec. Verimag. All rights reserved.
 * @author Hang YU
 * TODO add condition to glp_set_row_bnds (when operator is "=") if needed
*******************************************************************************/

#include "glpkInterface.h"
#include <unistd.h>

double GlpkInterface::_epsilon = 1 ;
double GlpkInterface::_threshold = 1e-6 ;

GlpkInterface::GlpkInterface() {
  _glp = glp_create_prob() ;
}

GlpkInterface::~GlpkInterface() { 
  glp_delete_prob(_glp) ;
}

/*******************************************************************************
 * Call simplex method in GLPK and compute the central point of the polyhedron
 * @para poly the given polyhedron
 * @return the central point  
*******************************************************************************/
Point GlpkInterface::GetCentralPoint(Polyhedron& poly) {
  GlpkInterface glpkinter ;
  glp_set_obj_dir(glpkinter._glp, GLP_MAX) ;
  int consNum = poly.get_constraint_num() ;
  int variNum = poly.get_variable_num() ;
  glp_add_rows(glpkinter._glp, consNum) ; 
  // the constraints: ax <= b
  // ||ax - b|| / ||a|| >= r1  =>  ax + ||a||r1 <= b 
  // 0 <= r1
  for(int i = 0; i < consNum; ++ i) {
    double constant = poly.GetConstant(i) ;
    glp_set_row_bnds(glpkinter._glp, i+1, GLP_UP, 0.0, constant) ;
  } 
  // there is a new column for r1
  glp_add_cols(glpkinter._glp, variNum+1) ;
  for(int j = 1; j < variNum+1; ++ j) {
    glp_set_col_bnds(glpkinter._glp, j, GLP_FR, 0.0, 0.0) ;
  }
  glp_set_col_bnds(glpkinter._glp, variNum+1, GLP_LO, 0.0, 0.0) ;
  // obj = max r1
  glp_set_obj_coef(glpkinter._glp, variNum+1, 1.0) ;
  int valNum = consNum * (variNum + 1) ;
  int* idxi = new int[valNum+1] ;
  int* idxj = new int[valNum+1] ;
  double* vals = new double[valNum+1] ;
  int idx = 1 ;
  for(int i = 0; i < consNum; ++ i) {
    for(int j = 0; j < variNum; ++ j) {
      idxi[idx] = i + 1 ;
      idxj[idx] = j + 1 ;
      vals[idx] = poly.GetCoef(i, j) ;
      ++ idx ;
    }
  } 
  for(int i = 0; i < consNum; ++ i) {
    double currNorm = poly.get_coefficients().row(i).norm() ;
    idxi[idx] = i + 1 ;
    idxj[idx] = variNum + 1 ;
    vals[idx] = currNorm ;
    ++ idx ;
  }
  glp_load_matrix(glpkinter._glp, valNum, idxi, idxj, vals) ;
  glp_smcp para ;
  glp_init_smcp(&para) ;
  para.msg_lev = GLP_MSG_OFF ;
  para.meth = GLP_DUAL; // DM
  glp_simplex(glpkinter._glp, &para) ;
  Point point ;
  if (glp_get_status(glpkinter._glp) == GLP_OPT) {
    double r1 = glp_get_col_prim(glpkinter._glp, variNum+1) ;
    if (r1 == 0) {
      std::cerr << "GetCentralPoint: the polyhedron is too small "
          << "Cannot find a point inside." << std::endl ;
      std::terminate() ;
    }

    Vector coord(variNum) ;
    for(int j = 0; j < variNum; ++ j) {
      coord(j) = glp_get_col_prim(glpkinter._glp, j+1) ;
    } 
    point.set_coordinates(coord) ;
    poly.set_dis_lower_bound( glp_get_col_prim(glpkinter._glp, variNum+1) ) ; 
  }

  delete[] idxi ;
  delete[] idxj ;
  delete[] vals ;

  return point ;
}

Point GlpkInterface::GetSatPoint(Polyhedron& poly, double epsilon) {
  GlpkInterface glpkInter ;
  glp_set_obj_dir(glpkInter._glp, GLP_MIN) ;
  int consNum = poly.get_constraint_num() ;
  int variNum = poly.get_variable_num() ;
  glp_add_rows(glpkInter._glp, consNum) ; 
  double constant ;
  for(int i = 0; i < consNum; ++ i) {
    constant = poly.GetConstant(i) ;
    // Ax <= b - epsilon * ||A||, i.e. shift the constraint by ||A||, 
    // i.e. distance from point to constraints larger than epsilon 
    double shift = poly.get_coefficients().row(i).norm() * epsilon ;
    glp_set_row_bnds(glpkInter._glp, i+1, GLP_UP, 0.0, constant-shift) ;
  }
  glp_add_cols(glpkInter._glp, variNum) ;
  for(int j = 1; j < variNum+1; ++ j) {
    glp_set_col_bnds(glpkInter._glp, j, GLP_FR, 0.0, 0.0) ;
  }
  // set the object as 0 to avoid the optimization phase
  glp_set_obj_coef(glpkInter._glp, 1, 0.0) ; 
  int valNum = consNum * variNum ;
  int* idxi = new int[valNum+1] ;
  int* idxj = new int[valNum+1] ;
  double* vals = new double[valNum+1] ;
  int idx = 1 ;
  for(int i = 0; i < consNum; ++ i) {
    for(int j = 0; j < variNum; ++ j) {
      idxi[idx] = i + 1 ;
      idxj[idx] = j + 1 ;
      vals[idx] = poly.GetCoef(i, j) ;
      ++ idx ;
    }
  } 
  glp_load_matrix(glpkInter._glp, valNum, idxi, idxj, vals) ;
  glp_smcp para ;
  glp_init_smcp(&para) ;
  para.msg_lev = GLP_MSG_OFF ;
  para.meth = GLP_DUAL; // DM
  glp_simplex(glpkInter._glp, &para) ;
  Point point ;
  if (glp_get_prim_stat(glpkInter._glp) == GLP_FEAS) {
    Vector coord(variNum) ;
    for(int j = 0; j < variNum; ++ j) {
      coord(j) = glp_get_col_prim(glpkInter._glp, j+1) ;
    } 
    point.set_coordinates(coord) ;

    poly.set_dis_lower_bound(epsilon) ; 
  }
  delete[] idxi ;
  delete[] idxj ;
  delete[] vals ;
  return point ;
}


/*******************************************************************************
 * Calls simplex method in GLPK and compute a satisfied point, i.e. this point
 * satisfy all the constraints of the polyhedron
 * @para headIdx the index of constraints which are met by the ray first
 * @para poly the given polyhedron
 * @return the satisfied point  
*******************************************************************************/
Point GlpkInterface::GetIrddWitness(const std::vector<int>& headIdx, 
    const Polyhedron& poly, double epsilon) {
  int consNum = headIdx.size() ;
  int variNum = poly.get_variable_num() ;
  int oriConsNum = glp_get_num_rows(_glp) ;
  glp_add_rows(_glp, consNum) ; 
  double constant ;
  for(int i = 0; i < consNum; ++ i) {
    constant = poly.GetConstant( headIdx[i] ) ;
    // minus _epsilon for getting a point inside the polyhedron
    if (oriConsNum + i == 0) {
      // the first constraint is Ax >= b + epsilon, others are Ax <= b - epsilon
      double shift = poly.get_coefficients().row( headIdx[0] ).norm() * epsilon ;
      glp_set_row_bnds(_glp, 1, GLP_LO, constant+shift, 0.0) ;
    _first_constraint.resize(variNum+1) ;
    _first_constraint.head(variNum) = poly.get_coefficients().row( headIdx[0] ) ;
    _first_constraint(variNum) = poly.GetConstant( headIdx[0] ) ;
    _currIdx = headIdx[0] ;
    }
    else {
      // the other constraints are not strict inequalities
      glp_set_row_bnds(_glp, oriConsNum+i+1, GLP_UP, 0.0, constant) ;
    }
  }
  int* idxj = new int[variNum+1] ;
  double* vals = new double[variNum+1] ;
  if (oriConsNum == 0) {
    glp_set_obj_dir(_glp, GLP_MIN) ;
    glp_add_cols(_glp, variNum) ;
    for(int j = 1; j < variNum+1; ++ j) {
      glp_set_col_bnds(_glp, j, GLP_FR, 0.0, 0.0) ;
    }
    // set the object as 0 to avoid the optimization phase
    glp_set_obj_coef(_glp, 1, 0.0) ; 
  } 
  for(int j = 0; j < variNum; ++ j) {
    idxj[j+1] = j + 1 ;
  }
  for(int i = 0; i < consNum; ++ i) {
    int consIdx = headIdx[i] ;
    for(int j = 0; j < variNum; ++ j) {
      vals[j+1] = poly.GetCoef(consIdx, j) ;
    }
    glp_set_mat_row(_glp, oriConsNum+i+1, variNum, idxj, vals) ;
  } 
  glp_smcp para ;
  glp_init_smcp(&para) ;
  para.msg_lev = GLP_MSG_OFF ;
  para.meth = GLP_DUAL; // DM
  glp_simplex(_glp, &para) ;
  Point point ;
  if (glp_get_prim_stat(_glp) == GLP_FEAS) {
    Vector coord(variNum) ;
    for(int j = 0; j < variNum; ++ j) {
      coord(j) = glp_get_col_prim(_glp, j+1) ;
    } 
    point.set_coordinates(coord) ;
  }
  // try to solve a optimization problem
  // check if the constraint has been shifted too much
  else if (glp_get_prim_stat(_glp) == GLP_NOFEAS) {

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "GetIrddWitness(): not feasible, try optimization problem"
      << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    for (int j = 0; j < variNum; ++ j) {
      glp_set_obj_coef( _glp, j+1, _first_constraint(j) ) ;
    }
    constant = _first_constraint(variNum) ;
    glp_set_row_bnds(_glp, 1, GLP_LO, constant, 0.0) ;


    glp_simplex(_glp, &para) ;
    if (glp_get_status(_glp) == GLP_OPT) {
      Vector coord(variNum) ;
      for(int j = 0; j < variNum; ++ j) {
        coord(j) = glp_get_col_prim(_glp, j+1) ;
      } 
      point.set_coordinates(coord) ;

#ifdef DEBUGINFO_RAYTRACING
  log_mtx_raytracing.lock() ; 
  std::cout << "GetIrddWitness(): found point by optimization problem"
      << std::endl ;
  log_mtx_raytracing.unlock() ; 
#endif

    }

#ifdef DEBUGINFO_RAYTRACING
  else {
    log_mtx_raytracing.lock() ; 
    std::cout << "Optimization problem cannot find point."
        << std::endl ;
    log_mtx_raytracing.unlock() ;
  } 
#endif

  }
 
  delete[] idxj ;
  delete[] vals ;
  return point ;
}


/*******************************************************************************
 * Tests if the {idx}th constraint is redundant.
*******************************************************************************/
bool GlpkInterface::Sat(const Polyhedron& poly, int idx, double threshold) {
  GlpkInterface glpkinter ;
  int consNum = poly.get_constraint_num() ;
  int variNum = poly.get_variable_num() ;
  glp_add_rows(glpkinter._glp, consNum) ; 
  double constant ;
  for(int i = 0; i < consNum; ++ i) {
    constant = poly.GetConstant(i) ;
    glp_set_row_bnds(glpkinter._glp, i+1, GLP_UP, 0.0, constant) ;
  }
  constant = poly.GetConstant(idx) ;
  glp_set_row_bnds(glpkinter._glp, idx+1, GLP_LO, constant+threshold, 0.0) ;
  int* idxj = new int[variNum+1] ;
  double* vals = new double[variNum+1] ;
  glp_set_obj_dir(glpkinter._glp, GLP_MIN) ;
  glp_add_cols(glpkinter._glp, variNum) ;
  for(int j = 1; j < variNum+1; ++ j) {
    glp_set_col_bnds(glpkinter._glp, j, GLP_FR, 0.0, 0.0) ;
  }
  // set the object as 0 to avoid the optimization phase
  glp_set_obj_coef(glpkinter._glp, 1, 0.0) ;  
  for(int i = 0; i < consNum; ++ i) {
    for(int j = 0; j < variNum; ++ j) {
      idxj[j+1] = j + 1 ;
      vals[j+1] = poly.GetCoef(i, j) ;
    }
    glp_set_mat_row(glpkinter._glp, i+1, variNum, idxj, vals) ;
  } 
  glp_smcp para ;
  glp_init_smcp(&para) ;
  para.msg_lev = GLP_MSG_OFF ;
  para.meth = GLP_DUAL; // DM
  glp_simplex(glpkinter._glp, &para) ;
  bool res ;
  if (glp_get_prim_stat(glpkinter._glp) == GLP_FEAS) {
    res = true ;
  }
  else {
    res = false ;
  }
 
  delete[] idxj ;
  delete[] vals ;
  return res ;
}

/*******************************************************************************
 * Solve LP with glpk simplex 
 * Minimize the objective. If miximization is needed, use -obj.
 * @para poly the polyhedron to be solved
 * @para obj the objective function
 * @para variNonNeg true if all the decision variables are non-negative
 * @para askFeasible true if just a feasible solution is needed
 * @return true if the feasible/optimal solution exists 
*******************************************************************************/
bool GlpkInterface::Simplex(const Polyhedron& poly, const Vector& obj, 
    bool variNonNeg, bool askFeasible, bool getBasis) {
    glp_set_obj_dir(_glp, GLP_MIN) ;
  int consNum = poly.get_constraint_num() ;
  int variNum = poly.get_variable_num() ;
  glp_add_rows(_glp, consNum) ; 
  for(int i = 0; i < consNum; ++ i) {
    double constant = poly.GetConstant(i) ;
    if (poly.GetOperator(i) == ConstraintOperator::lesseq) {
      glp_set_row_bnds(_glp, i+1, GLP_UP, 0.0, constant) ;
    } 
    else if (poly.GetOperator(i) == ConstraintOperator::equal) {
      glp_set_row_bnds(_glp, i+1, GLP_FX, constant, constant) ;
    }
    /*
    else if (poly.GetOperator(i) == OP_LESS) {
      // do something
    }
    */
  } 
  glp_add_cols(_glp, variNum) ;
  for(int j = 0; j < variNum; ++ j) {
    if (variNonNeg == true) {
      glp_set_col_bnds(_glp, j+1, GLP_LO, 0.0, 0.0) ;
    }
    else {
      glp_set_col_bnds(_glp, j+1, GLP_FR, 0.0, 0.0) ;
    }
  }
  if (askFeasible) {
      glp_set_obj_coef(_glp, 1, 0.0) ;
  }
  else {
    for(int j = 0; j < variNum; ++ j) {
      glp_set_obj_coef( _glp, j+1, obj(j) ) ;
    }
  }
  int valNum = consNum * variNum ;
  int* idxi = new int[valNum+1] ;
  int* idxj = new int[valNum+1] ;
  double* vals = new double[valNum+1] ;
  int idx = 1 ;
  for(int i = 0; i < consNum; ++ i) {
    for(int j = 0; j < variNum; ++ j) {
      idxi[idx] = i + 1 ;
      idxj[idx] = j + 1 ;
      vals[idx] = poly.GetCoef(i, j) ;
      ++ idx ;
    }
  } 
  glp_load_matrix(_glp, valNum, idxi, idxj, vals) ;
  glp_smcp para ;
  glp_init_smcp(&para) ;
  para.msg_lev = GLP_MSG_OFF ;
  //para.meth = GLP_DUAL; // DM
  glp_simplex(_glp, &para) ; 
  Vector point(variNum) ;
  bool result = false ;
  if (askFeasible == false) {
    // dual feasible = optimal solution
    if (glp_get_status(_glp) == GLP_OPT) {
      _simplex_optimal = glp_get_obj_val(_glp) ;
      result = true ;
    }
  }
  else {
    // primal feasible = feasible solution
    if (glp_get_prim_stat(_glp) == GLP_FEAS) {
      // need to get result?
      result = true ;
    }
  }

#ifdef DEBUGINFO_PLP
std::cout << "glpk result: value of variables: " ;
for (int i = 0; i < variNum; ++ i) {
  std::cout << glp_get_col_prim(_glp, i+1) << " " ; 
}
std::cout << std::endl ;
#endif

  if(getBasis) {
    GetBasis() ; 
  }

  delete[] idxi ;
  delete[] idxj ;
  delete[] vals ;
  return result ;
}

double GlpkInterface::get_simplex_optimal() {
  return _simplex_optimal ;
}
  
/*******************************************************************************
 * Gets the basic variable index of rows and non-basic variable index of columns
*******************************************************************************/
void GlpkInterface::GetBasis() {
  int rowNum = glp_get_num_rows(_glp) ; 
  int colNum = glp_get_num_cols(_glp) ; 
  for (int j = 0; j < colNum; ++ j) {
    if (glp_get_col_stat(_glp, j+1) == GLP_BS) {
      _basic_idx.push_back(j) ;
    }
    else if (glp_get_col_stat(_glp, j+1) == GLP_NL 
        || glp_get_col_stat(_glp, j+1) == GLP_NU) {
      _non_basic_idx.push_back(j) ;
    }
  }
  for (int i = 0; i < rowNum; ++ i) {
    if (glp_get_row_stat(_glp, i+1) == GLP_BS) {
      _basic_idx.push_back(colNum+i) ;
    }
    else if (glp_get_row_stat(_glp, i+1) == GLP_NL
        || glp_get_row_stat(_glp, i+1) == GLP_NU) {
      _non_basic_idx.push_back(colNum+i) ;
    }
  } 
}

/*******************************************************************************
 * Gets the index of all basic variables
 * x1 x2 ... xn slack1 slack2 ... slackn
*******************************************************************************/
const std::vector<int>& GlpkInterface::get_basic_idx() {
  return _basic_idx ;
}

/*******************************************************************************
 * Gets the index of all nonbasic variables
 * x1 x2 ... xn slack1 slack2 ... slackn
*******************************************************************************/
const std::vector<int>& GlpkInterface::get_non_basic_idx() {
  return _non_basic_idx ;
}

/*******************************************************************************
 * Gets the state of variables
 * @para idx the index of variable x1 x2 ... xn slack1 slack2 ... slackn
*******************************************************************************/
int GlpkInterface::GetVariState(int idx) {
  int colNum = glp_get_num_cols(_glp) ;
  int state = -1 ;
  if (idx < colNum) {
    state = glp_get_col_stat(_glp, idx+1) ;
  }
  else {
    state = glp_get_row_stat(_glp, idx-colNum+1) ;
  }
  return state ;
}

/*******************************************************************************
 * Gets the current value of variables
 * @para idx the index of variable x1 x2 ... xn slack1 slack2 ... slackn
 * @return the value of variables
*******************************************************************************/
double GlpkInterface::GetVariVal(int idx) {
  int colNum = glp_get_num_cols(_glp) ;
  double val ;
  if (idx < colNum) {
    val = glp_get_col_prim(_glp, idx+1) ;
  }
  else {
    val = glp_get_row_prim(_glp, idx-colNum+1) ;
  }
  return val ;
}

/*******************************************************************************
 * @return the number of rows in LP
*******************************************************************************/
int GlpkInterface::GetRowNum() {
  return glp_get_num_rows(_glp) ;
}

/*******************************************************************************
 * @return the number of columns in LP
*******************************************************************************/
int GlpkInterface::GetColNum() {
  return glp_get_num_cols(_glp) ;
}
