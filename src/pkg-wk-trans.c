#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <memory.h>
#include <wk-v1.h>

#include "libproj.h"

#include "rproj-proj.h"

typedef struct {
  PJ* pj;
  PJ_COORD coord;
  PJ_DIRECTION direction;
  int use_z_src;
  int use_z_dst;
  int use_m_src;
  int use_m_dst;
} rproj_trans_proj_t;

int rproj_trans_proj_trans(R_xlen_t feature_id, const double* xyzm_in, double* xyzm_out, void* trans_data) {
  rproj_trans_proj_t* data = (rproj_trans_proj_t*) trans_data;

  memcpy(data->coord.v, xyzm_in, 4 * sizeof(double));
  data->coord = proj_trans(data->pj, data->direction, data->coord);
  if (proj_errno(data->pj)) {
    xyzm_out[0] = NA_REAL;
    xyzm_out[1] = NA_REAL;
    xyzm_out[2] = NA_REAL;
    xyzm_out[3] = NA_REAL;
  } else {
    memcpy(xyzm_out, data->coord.v, 4 * sizeof(double));
  }

  return WK_CONTINUE;
}

void rproj_trans_proj_finalize(void* trans_data) {
  free(trans_data);
}

SEXP proj_c_trans(SEXP pj_xptr, SEXP use_z_sexp, SEXP use_m_sexp, SEXP direction_sexp) {
  // prepare data for C struct / validate args
  // we need the source and dest z/m info to invert properly
  PJ* pj = rproj_pj_from_xptr(pj_xptr);
  int* use_z = LOGICAL(use_z_sexp);
  int* use_m = LOGICAL(use_m_sexp);
  int direction = INTEGER(direction_sexp)[0];

  // create the trans object
  wk_trans_t* trans = wk_trans_create();
  trans->trans = &rproj_trans_proj_trans;
  trans->finalizer = &rproj_trans_proj_finalize;

  rproj_trans_proj_t* data = (rproj_trans_proj_t*) malloc(sizeof(rproj_trans_proj_t));
  if (data == NULL) {
    free(trans); // # nocov
    Rf_error("Failed to alloc rproj_trans_proj_t"); // # nocov
  }

  data->pj = pj;
  data->direction = direction;
  data->use_z_src = use_z[0];
  data->use_z_dst = use_z[1];
  data->use_m_src = use_m[0];
  data->use_m_dst = use_m[1];

  trans->use_z = data->use_z_dst;
  trans->use_m = data->use_m_dst;
  trans->trans_data = data;

  // keep the pj_xptr as a tag because we need the PJ* to stay valid
  return wk_trans_create_xptr(trans, pj_xptr, R_NilValue);
}

SEXP proj_c_trans_inverse(SEXP trans_xptr) {
  // very important that this is a trans pointing to this type of trans
  if (!Rf_inherits(trans_xptr, "rproj_trans_proj")) {
    Rf_error("`trans` must inherit from 'rlibrpoj_trans_proj'");
  }
  wk_trans_t* trans_template = (wk_trans_t*) R_ExternalPtrAddr(trans_xptr);

  // create the trans object
  wk_trans_t* trans = wk_trans_create();
  trans->trans = &rproj_trans_proj_trans;
  trans->finalizer = &rproj_trans_proj_finalize;

  // copy the data
  rproj_trans_proj_t* data = (rproj_trans_proj_t*) malloc(sizeof(rproj_trans_proj_t));
  if (data == NULL) {
    free(trans); // # nocov
    Rf_error("Failed to alloc rproj_trans_proj_t"); // # nocov
  }
  memcpy(data, trans_template->trans_data, sizeof(rproj_trans_proj_t));

  // reverse the direction
  if (data->direction == PJ_FWD) {
    data->direction = PJ_INV;
  } else if (data->direction == PJ_INV) {
    data->direction = PJ_FWD;
  } else {
    data->direction = PJ_IDENT; // # nocov
  }

  // shuffle source and dst dimension params
  trans->use_z = data->use_z_src;
  trans->use_m = data->use_m_src;
  data->use_z_src = data->use_z_dst;
  data->use_m_src = data->use_z_dst;
  data->use_z_dst = trans->use_z;
  data->use_m_dst = trans->use_m;

  trans->trans_data = data;

  // keep the pj_xptr as a tag because we need the PJ* to stay valid
  return wk_trans_create_xptr(trans, R_ExternalPtrTag(trans_xptr), R_NilValue);
}

SEXP proj_c_trans_get_pj(SEXP trans_xptr) {
  if (!Rf_inherits(trans_xptr, "rproj_trans_proj")) {
    Rf_error("`trans` must inherit from 'rlibrpoj_trans_proj'");
  }

  return R_ExternalPtrTag(trans_xptr);
}

SEXP proj_c_trans_get_direction(SEXP trans_xptr) {
  if (!Rf_inherits(trans_xptr, "rproj_trans_proj")) {
    Rf_error("`trans` must inherit from 'rlibrpoj_trans_proj'");
  }

  wk_trans_t* trans = (wk_trans_t*) R_ExternalPtrAddr(trans_xptr);
  rproj_trans_proj_t* data = (rproj_trans_proj_t*) trans->trans_data;
  return Rf_ScalarInteger(data->direction);
}
