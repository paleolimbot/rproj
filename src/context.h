
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

PJ_CONTEXT* rlibproj_ctx_from_xptr(SEXP ctx_xptr);
void rlibproj_ctx_stop_for_error(SEXP ctx_ptr);
