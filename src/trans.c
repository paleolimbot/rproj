#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "libproj.h"

#include "rlibproj-proj.h"


SEXP proj_c_trans_matrix(SEXP pj_xptr, SEXP x_sexp, SEXP direction_sexp) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  int direction = INTEGER(direction_sexp)[0];

  int nrow = Rf_nrows(x_sexp);
  int ncol = Rf_ncols(x_sexp);
  double* x = REAL(x_sexp);

  SEXP x_out_sexp = PROTECT(Rf_allocMatrix(REALSXP, nrow, 5));
  double* x_out = REAL(x_out_sexp);

  PJ_COORD coord;
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < 4; j++) {
      coord.v[j] = x[(j * nrow) + i];
    }

    coord = proj_trans(pj, direction, coord);
    x_out[(4 * nrow) + i] = proj_errno(pj);
    for (int j = 0; j < 4; j++) {
      x_out[(j * nrow) + i] = coord.v[j];
    }
  }

  UNPROTECT(1);
  return x_out_sexp;
}

