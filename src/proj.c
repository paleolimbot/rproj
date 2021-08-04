#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

#include "context.h"

void proj_xptr_destroy(SEXP proj_xptr) {
  PJ* obj = (PJ*) R_ExternalPtrAddr(proj_xptr);
  if (obj != NULL) {
    proj_destroy(obj);
  }
}

SEXP proj_c_create(SEXP ctx_xptr, SEXP definition_sexp) {
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);
  const char* definition = Rf_translateCharUTF8(STRING_ELT(definition_sexp, 0));

  PJ* obj = proj_create(ctx, definition);
  if (obj == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP pj_xptr = PROTECT(R_MakeExternalPtr(obj, ctx_xptr, R_NilValue));
  R_RegisterCFinalizer(pj_xptr, &proj_xptr_destroy);
  Rf_setAttrib(pj_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  UNPROTECT(1);
  return pj_xptr;
}
