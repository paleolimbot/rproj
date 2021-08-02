
proj_xptr_addr <- function(x) {
  .Call(proj_c_xptr_addr, x)
}
