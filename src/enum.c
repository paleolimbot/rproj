#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

const char* rlibproj_type(int type) {
  switch(type) {
  case PJ_TYPE_UNKNOWN: return "UNKNOWN";
  case PJ_TYPE_ELLIPSOID: return "ELLIPSOID";
  case PJ_TYPE_PRIME_MERIDIAN: return "PRIME_MERIDIAN";
  case PJ_TYPE_GEODETIC_REFERENCE_FRAME: return "GEODETIC_REFERENCE_FRAME";
  case PJ_TYPE_DYNAMIC_GEODETIC_REFERENCE_FRAME: return "DYNAMIC_GEODETIC_REFERENCE_FRAME";
  case PJ_TYPE_VERTICAL_REFERENCE_FRAME: return "VERTICAL_REFERENCE_FRAME";
  case PJ_TYPE_DYNAMIC_VERTICAL_REFERENCE_FRAME: return "DYNAMIC_VERTICAL_REFERENCE_FRAME";
  case PJ_TYPE_DATUM_ENSEMBLE: return "DATUM_ENSEMBLE";

  /** Abstract type: return ""; not returned by proj_get_type() */
  case PJ_TYPE_CRS: return "CRS";

  case PJ_TYPE_GEODETIC_CRS: return "GEODETIC_CRS";
  case PJ_TYPE_GEOCENTRIC_CRS: return "GEOCENTRIC_CRS";

  /** proj_get_type() will never return that type: return ""; but
   * case PJ_TYPE_GEOGRAPHIC_2D_CRS or case PJ_TYPE_GEOGRAPHIC_3D_CRS. */
  case PJ_TYPE_GEOGRAPHIC_CRS: return "GEOGRAPHIC_CRS";
  case PJ_TYPE_GEOGRAPHIC_2D_CRS: return "GEOGRAPHIC_2D_CRS";
  case PJ_TYPE_GEOGRAPHIC_3D_CRS: return "GEOGRAPHIC_3D_CRS";
  case PJ_TYPE_VERTICAL_CRS: return "VERTICAL_CRS";
  case PJ_TYPE_PROJECTED_CRS: return "PROJECTED_CRS";
  case PJ_TYPE_COMPOUND_CRS: return "COMPOUND_CRS";
  case PJ_TYPE_TEMPORAL_CRS: return "TEMPORAL_CRS";
  case PJ_TYPE_ENGINEERING_CRS: return "ENGINEERING_CRS";
  case PJ_TYPE_BOUND_CRS: return "BOUND_CRS";
  case PJ_TYPE_OTHER_CRS: return "OTHER_CRS";
  case PJ_TYPE_CONVERSION: return "CONVERSION";
  case PJ_TYPE_TRANSFORMATION: return "TRANSFORMATION";
  case PJ_TYPE_CONCATENATED_OPERATION: return "CONCATENATED_OPERATION";
  case PJ_TYPE_OTHER_COORDINATE_OPERATION: return "OTHER_COORDINATE_OPERATION";
  case PJ_TYPE_TEMPORAL_DATUM: return "TEMPORAL_DATUM";
  case PJ_TYPE_ENGINEERING_DATUM: return "ENGINEERING_DATUM";
  case PJ_TYPE_PARAMETRIC_DATUM: return "PARAMETRIC_DATUM";
  default: return "";
  }
}

SEXP proj_c_type_name(SEXP type_sexp) {
  R_xlen_t n = Rf_xlength(type_sexp);
  int* type = INTEGER(type_sexp);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, Rf_xlength(type_sexp)));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_STRING_ELT(out, i, Rf_mkChar(rlibproj_type(type[i])));
  }
  UNPROTECT(1);
  return out;
}
