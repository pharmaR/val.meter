#' Check whether we're currently evaluating RStudio's completions
#' 
#' @param call Which call to check. Defaults to the first call in the call
#'   stack, which will always be the originating call of RStudio's completions
#'   calls.
#' 
#' @keywords internal
#'
is_rs_rpc_get_completions_call <- function(call = sys.call(1L)) {
  identical(call[[1L]], quote(.rs.rpc.get_completions))
}