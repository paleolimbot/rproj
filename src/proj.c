#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

#include "context.h"


PJ* rlibproj_pj_from_xptr(SEXP pj_xptr) {
  if (TYPEOF(pj_xptr) != EXTPTRSXP) {
    Rf_error("`obj` must be an external pointer");
  }

  if (!Rf_inherits(pj_xptr, "rlibproj_proj")) {
    Rf_error("`obj` must inherit from 'rlibproj_context");
  }

  PJ* pj = (PJ*) R_ExternalPtrAddr(pj_xptr);
  if (pj == NULL) {
    Rf_error("`obj` is external pointer to NULL");
  }

  return pj;
}


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

SEXP proj_c_proj_info(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_PROJ_INFO info = proj_pj_info(pj);

  const char* names[] = {"id", "description", "definition", "has_inverse", "accuracy", ""};
  SEXP out = PROTECT(Rf_mkNamed(VECSXP, names));
  if (info.id == NULL) {
    SEXP na = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(na, 0, NA_STRING);
    SET_VECTOR_ELT(out, 0, na);
    UNPROTECT(1);
  } else {
    SET_VECTOR_ELT(out, 0, Rf_mkString(info.id));
  }

  SET_VECTOR_ELT(out, 1, Rf_mkString(info.description));
  SET_VECTOR_ELT(out, 2, Rf_mkString(info.definition));
  SET_VECTOR_ELT(out, 3, Rf_ScalarLogical(info.has_inverse));

  if (info.accuracy == -1) {
    SET_VECTOR_ELT(out, 4, Rf_ScalarReal(NA_REAL));
  } else {
    SET_VECTOR_ELT(out, 4, Rf_ScalarReal(info.accuracy));
  }

  UNPROTECT(1);
  return out;
}

SEXP proj_c_get_type(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  return Rf_ScalarInteger(proj_get_type(pj));
}
