#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"
#include "rlibproj-context.h"

// using a custom log function to keep error text with the error
// the logger is a list(NULL or character(1)) that can be reset
void rlibproj_logger_fun(void* data, int level, const char* msg) {
  if (level == PJ_LOG_ERROR) {
    SEXP logger = (SEXP) data;

    // don't swallow more than one error message in case
    // one of the other ones is important (could also implement
    // vector of length > 1 here but I think this is rare)
    if (VECTOR_ELT(logger, 0) == R_NilValue) {
      SEXP err = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(err, 0, Rf_mkCharCE(msg, CE_UTF8));
      SET_VECTOR_ELT(logger, 0, err);
      UNPROTECT(1);
      return;
    }
  }

  REprintf("[%d] %s\n", level, msg);
}

SEXP rlibproj_logger_create() {
  return Rf_allocVector(VECSXP, 1);
}

void rlibproj_logger_reset(SEXP ctx_xptr) {
  SEXP logger = R_ExternalPtrTag(ctx_xptr);
  if (logger == R_NilValue) {
    return;
  }

  SET_VECTOR_ELT(logger, 0, R_NilValue);
}

const char* rlibproj_logger_error(SEXP ctx_xptr) {
  SEXP logger = R_ExternalPtrTag(ctx_xptr);
  if (logger == R_NilValue) {
    return NULL;
  }

  if (VECTOR_ELT(logger, 0) == R_NilValue) {
    return NULL;
  } else {
    return Rf_translateChar0(STRING_ELT(VECTOR_ELT(logger, 0), 0));
  }
}

// use these functions in other files to handle errors
PJ_CONTEXT* rlibproj_ctx_from_xptr(SEXP ctx_xptr) {
  if (TYPEOF(ctx_xptr) != EXTPTRSXP) {
    Rf_error("`ctx` must be an external pointer");
  }

  if (!Rf_inherits(ctx_xptr, "rlibproj_context")) {
    Rf_error("`ctx` must inherit from 'rlibproj_context");
  }

  // technically NULL is OK here in the sense that it's identical
  // to the default context; however, if this happens it's because
  // a session was restarted (we never use the default context
  // except when creating a new one)
  PJ_CONTEXT* ctx = (PJ_CONTEXT*) R_ExternalPtrAddr(ctx_xptr);
  if (ctx == NULL) {
    Rf_error("`ctx` is an external pointer to NULL");
  }

  rlibproj_logger_reset(ctx_xptr);
  return (PJ_CONTEXT*) R_ExternalPtrAddr(ctx_xptr);
}

void rlibproj_ctx_stop_for_error(SEXP ctx_xptr) {
  // don't reset the error here!
  PJ_CONTEXT* ctx = (PJ_CONTEXT*) R_ExternalPtrAddr(ctx_xptr);
  int errno = proj_context_errno(ctx);
  const char* errstring = proj_context_errno_string(ctx, errno);
  const char* log_errstring = rlibproj_logger_error(ctx_xptr);
  if (log_errstring == NULL && errstring == NULL) {
    Rf_error("Unknown error");
  } else if (log_errstring == NULL) {
    Rf_error(errstring);
  } else if (errstring == NULL) {
    Rf_error(log_errstring);
  } else {
    Rf_error("%s\n%s", errstring, log_errstring);
  }
}

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
  // NULL is OK here so we just use the raw address without checking
  PJ_CONTEXT* context = (PJ_CONTEXT*) R_ExternalPtrAddr(context_xptr);

  PJ_CONTEXT* ctx_clone = proj_context_clone(context);
  if (ctx_clone == NULL) {
    Rf_error("Unknown error on proj_context_clone()");
  }

  SEXP ctx_clone_xptr = PROTECT(R_MakeExternalPtr(ctx_clone, R_NilValue, R_NilValue));

  SEXP logger = PROTECT(rlibproj_logger_create());
  R_SetExternalPtrTag(ctx_clone_xptr, logger);
  UNPROTECT(1);

  proj_log_func(ctx_clone, logger, &rlibproj_logger_fun);

  R_RegisterCFinalizer(ctx_clone_xptr, &proj_context_xptr_destroy);
  Rf_setAttrib(ctx_clone_xptr, R_ClassSymbol, Rf_mkString("rlibproj_context"));
  UNPROTECT(1);
  return ctx_clone_xptr;
}

SEXP proj_c_context_is_network_enabled(SEXP context_xptr) {
  PJ_CONTEXT* context = rlibproj_ctx_from_xptr(context_xptr);
  int value = proj_context_is_network_enabled(context);
  return Rf_ScalarLogical(value);
}

SEXP proj_c_context_get_url_endpoint(SEXP context_xptr) {
  PJ_CONTEXT* context = rlibproj_ctx_from_xptr(context_xptr);
  const char* value = proj_context_get_url_endpoint(context);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_context_get_user_writable_directory(SEXP context_xptr) {
  PJ_CONTEXT* context = rlibproj_ctx_from_xptr(context_xptr);
  const char* value = proj_context_get_user_writable_directory(context, 0);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_context_get_use_proj4_init_rules(SEXP context_xptr) {
  PJ_CONTEXT* context = rlibproj_ctx_from_xptr(context_xptr);
  int value = proj_context_get_use_proj4_init_rules(context, 0);
  return Rf_ScalarLogical(value);
}

SEXP proj_c_context_get_database_path(SEXP context_xptr) {
  PJ_CONTEXT* context = rlibproj_ctx_from_xptr(context_xptr);
  const char* value = proj_context_get_database_path(context);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}
