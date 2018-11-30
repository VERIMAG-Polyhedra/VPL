#include "lex_pivot.h"

extern "C"{
#include <stdio.h>
#include <memory.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
}

extern "C" value create(value n_rows_){
  CAMLparam1(n_rows_);
  int n_rows = Int_val(n_rows_);
  CAMLreturn((value) (new LexPivot(n_rows)));
}

extern "C" value set_A(value pivot, value n_row_, value n_col_, value num_, value den_){
    CAMLparam5(pivot, n_row_, n_col_, num_, den_);
    int n_row = Int_val(n_row_);
    int n_col = Int_val(n_col_);
    int num = Int_val(num_);
    int den = Int_val(den_);
    ((LexPivot*) pivot)->set_A(n_row, n_col, num, den);
    CAMLreturn(Val_unit);
}

extern "C" value set_b(value pivot, value n_row_, value num_, value den_){
    CAMLparam4(pivot, n_row_, num_, den_);
    int n_row = Int_val(n_row_);
    int num = Int_val(num_);
    int den = Int_val(den_);
    ((LexPivot*) pivot)->set_b(n_row, num, den);
    CAMLreturn(Val_unit);
}

extern "C" value set_column(value pivot, value n_row_, value num_, value den_){
    CAMLparam4(pivot, n_row_, num_, den_);
    int n_row = Int_val(n_row_);
    int num = Int_val(num_);
    int den = Int_val(den_);
    ((LexPivot*) pivot)->set_column(n_row, num, den);
    CAMLreturn(Val_unit);
}

extern "C" value solve(value pivot){
    CAMLparam1(pivot);
    int res = ((LexPivot*) pivot)->solve();
    delete((LexPivot*) pivot);
    CAMLreturn (Val_int(res));
}

extern "C" value print(value pivot){
    CAMLparam1(pivot);
    ((LexPivot*) pivot)->print();
    CAMLreturn(Val_unit);
}
