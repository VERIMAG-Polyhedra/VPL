#include <iostream>
#include <vector>
#include "ioInterface.h"
#include "glpkInterface.h"

extern "C"{
#include <stdio.h>
#include <memory.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
}

extern "C" Polyhedron* new_poly(value n_cons_, value n_var_){
	int n_cons = Int_val(n_cons_);
	int n_var = Int_val(n_var_);
	return new Polyhedron(n_cons, n_var);
}

extern "C" void delete_poly(Polyhedron* poly){
	delete(poly);
}

extern "C" void set_coeff(Polyhedron* poly, value i_cons_, value i_var_, value coeff_){
	int i_cons = Int_val(i_cons_);
	int i_var = Int_val(i_var_);
	double coeff = Double_val(coeff_);
	poly->SetCoef(i_cons, i_var, coeff);
}

extern "C" void minimize(Polyhedron* poly){
	poly->Minimize();
}

extern "C" value get_true_constraints(Polyhedron* poly){
	CAMLparam0();
	CAMLlocal1(r);
/*	
	std::vector<int> ids = poly->GetActiveIdx();
	int* ids_a = &ids[0];
	r = caml_alloc_array(caml_copy_int32, ids_a);
	CAMLreturn (r);*/
}


extern "C" value is_empty(Polyhedron* poly){
	return Val_bool(poly->IsEmpty());
}

extern "C" value is_true(Polyhedron* poly, value id_){
	int id = Int_val(id_);
	std::vector<int> ids = poly->GetActiveIdx();
	if(std::find(ids.begin(), ids.end(), id) != ids.end())
		return Val_bool(true);
	else
		return Val_bool(false);
}

extern "C" value get_witness_coeff(Polyhedron* poly, value id_, value var_){
	int id = Int_val(id_);
	int var = Int_val(var_);
	Point p = poly->GetWitness()[id];
	double coeff = p.get_coordinates()[var];
	
	return caml_copy_double(coeff);
}
