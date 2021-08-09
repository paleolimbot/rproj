
#ifndef RLIBPROJ_PROJ_H_INCLUDED
#define RLIBPROJ_PROJ_H_INCLUDED

#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

PJ_CONTEXT* rlibproj_ctx_from_pj_xptr(SEXP pj_xptr);
void rlibproj_pj_stop_for_error(SEXP pj_xptr);
PJ* rlibproj_pj_from_xptr(SEXP pj_xptr);

#endif
