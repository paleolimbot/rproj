#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "libproj.h"

#include "rlibproj-context.h"
#include "rlibproj-proj.h"


SEXP proj_c_trans_matrix(SEXP pj_xptr, SEXP x_sexp, SEXP direction_sexp,
                         SEXP verbose_sexp) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_pj_xptr(pj_xptr);

  int direction = INTEGER(direction_sexp)[0];
  int verbose = LOGICAL(verbose_sexp)[0];

  int nrow = Rf_nrows(x_sexp);
  int ncol = Rf_ncols(x_sexp);
  double* x = REAL(x_sexp);

  SEXP x_out_sexp = PROTECT(Rf_allocMatrix(REALSXP, nrow, 5));
  double* x_out = REAL(x_out_sexp);

  int previous_log_level = PJ_LOG_ERROR;
  if (!verbose) {
    previous_log_level = proj_log_level(ctx, PJ_LOG_NONE);
  }

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

  if (!verbose) {
    proj_log_level(ctx, previous_log_level);
  } else {
    // if there was a logged error, the first one would have been
    // swallowed by the error logger
    SEXP ctx_xptr = R_ExternalPtrTag(pj_xptr);
    const char* logger_err = rlibproj_logger_error(ctx_xptr);
    if (logger_err != NULL) {
      REprintf("[first error was] %s\n", logger_err);
    }
  }

  UNPROTECT(1);
  return x_out_sexp;
}

