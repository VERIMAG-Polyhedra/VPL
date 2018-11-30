#include <flint/fmpqxx.h>
#include <flint/fmpzxx.h>
#include "lex_pivot.h"

using namespace flint;

LexPivot::LexPivot(int n_rows):
    A(n_rows, n_rows),
    b(n_rows,1),
    column(n_rows,1),
    size(n_rows)
{}

LexPivot::~LexPivot()
{
    /*
    A.clear();
    b.clear();
    column.clear();
    */
}

void LexPivot::set_A(int n_row, int n_col, int num_, int den_){
    fmpzxx num(num_);
    fmpzxx den(den_);
    fmpqxx coeff(num, den);
    A.at(n_row, n_col) = coeff;
}

void LexPivot::set_b(int n_row, int num_, int den_)
{
    fmpzxx num(num_);
    fmpzxx den(den_);
    fmpqxx coeff(num, den);
    b.at(n_row, 0) = coeff;
}

void LexPivot::set_column(int n_row, int num_, int den_)
{
    fmpzxx num(num_);
    fmpzxx den(den_);
    fmpqxx coeff(num, den);
    column.at(n_row, 0) = coeff;
}

void LexPivot::print()
{
    A.print();
    b.print();
    column.print();
}

bool diff_is_lexpositive(fmpq_matxx& M, int row1, int row2, int n_cols)
{
    for (int col = 0 ; col < n_cols ; ++col) {
        if (M.at(row1, col) - M.at(row2, col) < fmpqxx::zero()) {
            return false;
        } else if (M.at(row1, col) - M.at(row2, col) > fmpqxx::zero()) {
            return true;
        }
    }
    return false;
}

int LexPivot::solve()
{
    A = A.inv();
    RMatrix M(size, size+1);
    for (int row = 0 ; row < size ; ++row) {
        for (int col = 0 ; col <= size ; ++col) {
            if (col == 0) {
                M.at(row, col) = b.at(row, 0) / column.at(row,0);
            } else {
                M.at(row, col) = A.at(row, col-1) / column.at(row,0);
            }
        }
    }

    M.print();

    bool is_min;
    for (int row = 0 ; row < size ; ++row) {
        if (column.at(row, 0) > fmpqxx::zero()) {
            is_min = true;
            for (int row2 = 0 ; row2 < size ; ++row2) {
                if (row != row2 && column.at(row2, 0) > fmpqxx::zero() && !diff_is_lexpositive(M, row2, row, size + 1)) {
                    is_min = false;
                    break;
                }
            }
            if (is_min) {
                return row;
            }
        }
    }
    return -1;
}
