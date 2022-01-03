
#ifndef rproj_PROJ_H_INCLUDED
#define rproj_PROJ_H_INCLUDED

#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

PJ_CONTEXT* rproj_ctx_from_pj_xptr(SEXP pj_xptr);
void rproj_pj_stop_for_error(SEXP pj_xptr);
PJ* rproj_pj_from_xptr(SEXP pj_xptr);

#endif
