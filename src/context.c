#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

SEXP proj_c_pj_default_ctx() {
  SEXP context_xptr = PROTECT(R_MakeExternalPtr(PJ_DEFAULT_CTX, R_NilValue, R_NilValue));
  Rf_setAttrib(context_xptr, R_ClassSymbol, Rf_mkString("rlibproj_context"));
  UNPROTECT(1);
  return context_xptr;
}
