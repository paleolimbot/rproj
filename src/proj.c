#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "libproj.h"

#include "rlibproj-context.h"
#include "rlibproj-proj.h"


PJ_CONTEXT* rlibproj_ctx_from_pj_xptr(SEXP pj_xptr) {
  SEXP ctx_xptr = R_ExternalPtrTag(pj_xptr);
  return (PJ_CONTEXT*) R_ExternalPtrAddr(ctx_xptr);
}

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

  // reset the error
  proj_errno_reset(pj);
  return pj;
}

void rlibproj_pj_stop_for_error(SEXP pj_xptr) {
  // don't reset the errors here!
  PJ* pj = (PJ*) R_ExternalPtrAddr(pj_xptr);
  SEXP ctx_xptr = R_ExternalPtrTag(pj_xptr);
  PJ_CONTEXT* ctx = (PJ_CONTEXT*) R_ExternalPtrAddr(ctx_xptr);

  int errno = proj_errno(pj);
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

SEXP proj_c_create_crs_to_crs(SEXP ctx_xptr,
                              SEXP source_crs_xptr, SEXP target_crs_xptr,
                              SEXP area_sexp, SEXP options_sexp) {
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);
  PJ* source_crs = rlibproj_pj_from_xptr(source_crs_xptr);
  PJ* target_crs = rlibproj_pj_from_xptr(target_crs_xptr);

  // options is a NULL-terminated char[]
  const char** options = malloc((Rf_length(options_sexp) + 1) * sizeof(char*));
  for (int i = 0; i < Rf_length(options_sexp); i++) {
    options[i] = Rf_translateCharUTF8(STRING_ELT(options_sexp, i));
  }
  options[Rf_length(options_sexp)] = NULL;

  // nothing in here should longjmp (before we can free options)
  PJ* obj;
  if (area_sexp == R_NilValue) {
    obj = proj_create_crs_to_crs_from_pj(ctx, source_crs, target_crs, NULL, options);
  } else {
    double* area = REAL(area_sexp);
    PJ_AREA* pj_area = proj_area_create();
    proj_area_set_bbox(pj_area, area[0], area[1], area[2], area[3]);
    obj = proj_create_crs_to_crs_from_pj(ctx, source_crs, target_crs, pj_area, options);
    proj_area_destroy(pj_area);
  }

  free(options);

  if (obj == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP pj_xptr = PROTECT(R_MakeExternalPtr(obj, ctx_xptr, R_NilValue));
  R_RegisterCFinalizer(pj_xptr, &proj_xptr_destroy);
  Rf_setAttrib(pj_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  UNPROTECT(1);
  return pj_xptr;
}

SEXP proj_c_create_from_wkt(SEXP ctx_xptr, SEXP wkt_sexp, SEXP options_sexp) {
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);
  const char* wkt = Rf_translateCharUTF8(STRING_ELT(wkt_sexp, 0));

  // options is a NULL-terminated char[]
  const char** options = malloc((Rf_length(options_sexp) + 1) * sizeof(char*));
  for (int i = 0; i < Rf_length(options_sexp); i++) {
    options[i] = Rf_translateCharUTF8(STRING_ELT(options_sexp, i));
  }
  options[Rf_length(options_sexp)] = NULL;

  PROJ_STRING_LIST warnings = NULL;
  PROJ_STRING_LIST grammar_errors = NULL;

  PJ* obj = proj_create_from_wkt(ctx, wkt, options, &warnings, &grammar_errors);

  free(options);

  // because we report grammar errors, some failed constructions are ok
  // technically these allocs could longjmp and leak warnings/grammar_errors
  // but the allocs are small and this is unlikely
  SEXP pj_xptr;
  if ((obj == NULL) && ((warnings != NULL) || (grammar_errors != NULL))) {
    pj_xptr = PROTECT(Rf_allocVector(VECSXP, 0));
  } else if (obj != NULL) {
    // wrap the PJ* pointer first so it will be destroyed if any of the below fails
    pj_xptr = PROTECT(R_MakeExternalPtr(obj, ctx_xptr, R_NilValue));
    R_RegisterCFinalizer(pj_xptr, &proj_xptr_destroy);
    Rf_setAttrib(pj_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  } else {
    // it's worth stopping here because the logger probably picked up an error
    // (e.g., bad options)
    if (warnings != NULL) proj_string_list_destroy(warnings);
    if (grammar_errors != NULL) proj_string_list_destroy(grammar_errors);
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  int n_warnings = 0;
  while (warnings && warnings[n_warnings]) n_warnings++;
  int n_grammar_errors = 0;
  while (grammar_errors && grammar_errors[n_grammar_errors]) n_grammar_errors++;

  SEXP warnings_sexp = PROTECT(Rf_allocVector(STRSXP, n_warnings));
  for (int i = 0; i < n_warnings; i++) {
    SET_STRING_ELT(warnings_sexp, i, Rf_mkCharCE(warnings[i], CE_UTF8));
  }

  SEXP grammar_errors_sexp = PROTECT(Rf_allocVector(STRSXP, n_grammar_errors));
  for (int i = 0; i < n_grammar_errors; i++) {
    SET_STRING_ELT(grammar_errors_sexp, i, Rf_mkCharCE(grammar_errors[i], CE_UTF8));
  }

  if (warnings != NULL) proj_string_list_destroy(warnings);
  if (grammar_errors != NULL) proj_string_list_destroy(grammar_errors);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(out, 0, pj_xptr);
  SET_VECTOR_ELT(out, 1, warnings_sexp);
  SET_VECTOR_ELT(out, 2, grammar_errors_sexp);

  UNPROTECT(4);
  return out;
}

SEXP proj_c_get_source_crs(SEXP pj_xptr, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);

  PJ* source_crs = proj_get_source_crs(ctx, pj);
  if (source_crs == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  // the context associated with this PJ* is that of the original
  // (by a reading of the source code)
  SEXP ctx_pj = R_ExternalPtrTag(pj_xptr);
  SEXP source_crs_xptr = PROTECT(R_MakeExternalPtr(source_crs, ctx_pj, R_NilValue));
  R_RegisterCFinalizer(source_crs_xptr, &proj_xptr_destroy);
  Rf_setAttrib(source_crs_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  UNPROTECT(1);
  return source_crs_xptr;
}

SEXP proj_c_normalize_for_visualization(SEXP pj_xptr, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);

  PJ* pj_out = proj_normalize_for_visualization(ctx, pj);
  if (pj_out == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP pj_out_xptr = PROTECT(R_MakeExternalPtr(pj_out, ctx_xptr, R_NilValue));
  R_RegisterCFinalizer(pj_out_xptr, &proj_xptr_destroy);
  Rf_setAttrib(pj_out_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  UNPROTECT(1);
  return pj_out_xptr;
}

SEXP proj_c_get_target_crs(SEXP pj_xptr, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);
  PJ* target_crs = proj_get_target_crs(ctx, pj);
  if (target_crs == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP ctx_pj = R_ExternalPtrTag(pj_xptr);
  SEXP target_crs_xptr = PROTECT(R_MakeExternalPtr(target_crs, ctx_pj, R_NilValue));
  R_RegisterCFinalizer(target_crs_xptr, &proj_xptr_destroy);
  Rf_setAttrib(target_crs_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
  UNPROTECT(1);
  return target_crs_xptr;
}

SEXP proj_c_get_non_deprecated(SEXP pj_xptr, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);

  PJ_OBJ_LIST* pj_lst = proj_get_non_deprecated(ctx, pj);
  if (pj_lst == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  // pj_list could leak if any of the below fails
  SEXP out = PROTECT(Rf_allocVector(VECSXP, proj_list_get_count(pj_lst)));
  for (int i = 0; i < proj_list_get_count(pj_lst); i++) {
    PJ* new_pj = proj_list_get(ctx, pj_lst, i);
    SEXP new_pj_xptr = PROTECT(R_MakeExternalPtr(new_pj, ctx_xptr, R_NilValue));
    R_RegisterCFinalizer(new_pj_xptr, &proj_xptr_destroy);
    Rf_setAttrib(new_pj_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
    SET_VECTOR_ELT(out, i, new_pj_xptr);
    UNPROTECT(1);
  }

  proj_list_destroy(pj_lst);

  UNPROTECT(1);
  return out;
}

SEXP proj_c_identify(SEXP pj_xptr, SEXP auth_name_sexp, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);

  const char* auth_name = Rf_translateCharUTF8(STRING_ELT(auth_name_sexp, 0));

  int* out_confidence;
  PJ_OBJ_LIST* pj_lst = proj_identify(ctx, pj, auth_name, NULL, &out_confidence);
  if (pj_lst == NULL) {
    if (out_confidence != NULL) proj_int_list_destroy(out_confidence);
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  // pj_lst and out_confidence could leak if any of these allocs fail
  SEXP out = PROTECT(Rf_allocVector(VECSXP, proj_list_get_count(pj_lst)));
  SEXP conf_out = PROTECT(Rf_allocVector(INTSXP, proj_list_get_count(pj_lst)));
  for (int i = 0; i < proj_list_get_count(pj_lst); i++) {
    PJ* new_pj = proj_list_get(ctx, pj_lst, i);
    SEXP new_pj_xptr = PROTECT(R_MakeExternalPtr(new_pj, ctx_xptr, R_NilValue));
    R_RegisterCFinalizer(new_pj_xptr, &proj_xptr_destroy);
    Rf_setAttrib(new_pj_xptr, R_ClassSymbol, Rf_mkString("rlibproj_proj"));
    SET_VECTOR_ELT(out, i, new_pj_xptr);
    SET_INTEGER_ELT(conf_out, i, out_confidence[i]);
    UNPROTECT(1);
  }

  proj_list_destroy(pj_lst);
  proj_int_list_destroy(out_confidence);

  SEXP container = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(container, 0, out);
  SET_VECTOR_ELT(container, 1, conf_out);

  UNPROTECT(3);
  return container;
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

SEXP proj_c_is_deprecated(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  return Rf_ScalarLogical(proj_is_deprecated(pj));
}

SEXP proj_c_is_crs(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  return Rf_ScalarLogical(proj_is_crs(pj));
}

SEXP proj_c_is_equivalent_to(SEXP pj_xptr, SEXP other_xptr,
                             SEXP criterion_sexp, SEXP ctx_xptr) {

  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ* other = rlibproj_pj_from_xptr(other_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_xptr(ctx_xptr);

  int criterion = INTEGER(criterion_sexp)[0];
  int result = proj_is_equivalent_to_with_ctx(ctx, pj, other, criterion);
  return Rf_ScalarLogical(result);
}

SEXP proj_c_get_remarks(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  const char* remarks = proj_get_remarks(pj);
  if (remarks == NULL) {
    rlibproj_pj_stop_for_error(pj_xptr);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(remarks, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_get_scope(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  const char* scope = proj_get_scope(pj);
  if (scope == NULL) {
    rlibproj_pj_stop_for_error(pj_xptr);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(scope, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_get_area_of_use(SEXP pj_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_pj_xptr(pj_xptr);

  SEXP area_sexp = PROTECT(Rf_allocVector(REALSXP, 4));
  double* area = REAL(area_sexp);
  SEXP name_sexp = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(name_sexp, 0, NA_STRING);

  const char* name_ptr = NULL;

  int code = proj_get_area_of_use(
    ctx,
    pj,
    area + 0, area + 1, area + 2, area + 3,
    &name_ptr
  );

  if (code != 1) {
    rlibproj_pj_stop_for_error(pj_xptr);
  }

  if (name_ptr != NULL) {
    SET_STRING_ELT(name_sexp, 0, Rf_mkCharCE(name_ptr, CE_UTF8));
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, area_sexp);
  SET_VECTOR_ELT(out, 1, name_sexp);
  UNPROTECT(3);
  return out;
}

SEXP proj_c_as_wkt(SEXP pj_xptr, SEXP wkt_type_sexp, SEXP options_sexp, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_pj_xptr(pj_xptr);
  int wkt_type = INTEGER(wkt_type_sexp)[0];

  const char** options = malloc((Rf_length(options_sexp) + 1) * sizeof(char*));
  for (int i = 0; i < Rf_length(options_sexp); i++) {
    options[i] = Rf_translateCharUTF8(STRING_ELT(options_sexp, i));
  }
  options[Rf_length(options_sexp)] = NULL;

  const char* value = proj_as_wkt(ctx, pj, wkt_type, options);
  free(options);
  if (value == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_as_proj_string(SEXP pj_xptr, SEXP proj_string_type_sexp, SEXP options_sexp, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_pj_xptr(pj_xptr);
  int proj_string_type = INTEGER(proj_string_type_sexp)[0];

  const char** options = malloc((Rf_length(options_sexp) + 1) * sizeof(char*));
  for (int i = 0; i < Rf_length(options_sexp); i++) {
    options[i] = Rf_translateCharUTF8(STRING_ELT(options_sexp, i));
  }
  options[Rf_length(options_sexp)] = NULL;

  const char* value = proj_as_proj_string(ctx, pj, proj_string_type, options);
  free(options);
  if (value == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}

SEXP proj_c_as_projjson(SEXP pj_xptr,  SEXP options_sexp, SEXP ctx_xptr) {
  PJ* pj = rlibproj_pj_from_xptr(pj_xptr);
  PJ_CONTEXT* ctx = rlibproj_ctx_from_pj_xptr(pj_xptr);

  const char** options = malloc((Rf_length(options_sexp) + 1) * sizeof(char*));
  for (int i = 0; i < Rf_length(options_sexp); i++) {
    options[i] = Rf_translateCharUTF8(STRING_ELT(options_sexp, i));
  }
  options[Rf_length(options_sexp)] = NULL;

  const char* value = proj_as_projjson(ctx, pj, options);
  free(options);
  if (value == NULL) {
    rlibproj_ctx_stop_for_error(ctx_xptr);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharCE(value, CE_UTF8));
  UNPROTECT(1);
  return out;
}
