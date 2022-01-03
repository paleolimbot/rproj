
#ifndef rproj_CONTEXT_H_INCLUDED
#define rproj_CONTEXT_H_INCLUDED

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "libproj.h"

PJ_CONTEXT* rproj_ctx_from_xptr(SEXP ctx_xptr);
void rproj_ctx_stop_for_error(SEXP ctx_ptr);
const char* rproj_logger_error(SEXP ctx_xptr);

#endif
