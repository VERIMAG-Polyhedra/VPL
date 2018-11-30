#ifndef _LEX_PIVOT_H
#define _LEX_PIVOT_H

#include <flint/fmpq_matxx.h>
typedef flint::fmpq_matxx RMatrix ;

class LexPivot {

public:
    LexPivot(int n_rows);

    ~LexPivot();

    void set_A(int n_row, int n_col, int num, int den);

    void set_b(int n_row, int num, int den);

    void set_column(int n_row, int num, int den);

    void print();

    int solve();

private:
    RMatrix A;
    RMatrix b;
    RMatrix column;
    int size;
};

#endif
