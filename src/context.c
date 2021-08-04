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

void proj_context_xptr_destroy(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  if (context != NULL) {
    proj_context_destroy(context);
  }
}

SEXP proj_c_context_clone(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  PJ_CONTEXT* ctx_clone = proj_context_clone(context);
  SEXP ctx_clone_xptr = PROTECT(R_MakeExternalPtr(ctx_clone, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ctx_clone_xptr, &proj_context_xptr_destroy);
  Rf_setAttrib(ctx_clone_xptr, R_ClassSymbol, Rf_mkString("rlibproj_context"));
  UNPROTECT(1);
  return ctx_clone_xptr;
}

SEXP proj_c_context_is_network_enabled(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  int value = proj_context_is_network_enabled(context);
  return Rf_ScalarLogical(value);
}

SEXP proj_c_context_get_url_endpoint(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  const char* value = proj_context_get_url_endpoint(context);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_context_get_user_writable_directory(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  const char* value = proj_context_get_user_writable_directory(context, 0);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_context_get_use_proj4_init_rules(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  int value = proj_context_get_use_proj4_init_rules(context, 0);
  return Rf_ScalarLogical(value);
}

SEXP proj_c_context_get_database_path(SEXP context_xptr) {
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);
  const char* value = proj_context_get_database_path(context);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}
