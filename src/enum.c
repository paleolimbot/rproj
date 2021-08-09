#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

SEXP rlibproj_enum_name(SEXP type_sexp, const char* (*func)(int code)) {
  R_xlen_t n = Rf_xlength(type_sexp);
  int* type = INTEGER(type_sexp);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, Rf_xlength(type_sexp)));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_STRING_ELT(out, i, Rf_mkChar(func(type[i])));
  }
  UNPROTECT(1);
  return out;
}


const char* rlibproj_type(int type) {
  switch (type) {
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
  return rlibproj_enum_name(type_sexp, &rlibproj_type);
}

const char* rlibproj_comp(int comparison_criterion) {
  switch (comparison_criterion) {
  case PJ_COMP_STRICT: return "STRICT";
  case PJ_COMP_EQUIVALENT: return "EQUIVALENT";
  case PJ_COMP_EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS:
    return "EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS";
  default: return "";
  }
}

SEXP proj_c_comp_name(SEXP comp_sexp) {
  return rlibproj_enum_name(comp_sexp, &rlibproj_comp);
}

const char* rlibproj_wkt_type(int wkt_type) {
  switch (wkt_type) {
  case PJ_WKT2_2015: return "WKT2_2015";
  case PJ_WKT2_2015_SIMPLIFIED: return "WKT2_2015_SIMPLIFIED";
  case PJ_WKT2_2019: return "WKT2_2019";
  case PJ_WKT2_2019_SIMPLIFIED: return "WKT2_2019_SIMPLIFIED";
  case PJ_WKT1_GDAL: return "WKT1_GDAL";
  case PJ_WKT1_ESRI: return "WKT1_ESRI";
  default: return "";
  }
}

SEXP proj_c_wkt_type(SEXP wkt_type_sexp) {
  return rlibproj_enum_name(wkt_type_sexp, &rlibproj_wkt_type);
}

const char* rlibproj_proj_string_type(int code) {
  switch (code) {
  case PJ_PROJ_5: return "PROJ_5";
  case PJ_PROJ_4: return "PROJ_4";
  default: return "";
  }
}

SEXP proj_c_proj_string_type(SEXP code_sexp) {
  return rlibproj_enum_name(code_sexp, &rlibproj_proj_string_type);
}
