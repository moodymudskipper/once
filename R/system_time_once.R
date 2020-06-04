#' Print the system.time of a Function's Next Call
#'
#' This doesn't affect the function's output, which would not be the case
#' if we ran `surround_once(f, system.time, gcFirst)`
#'
#' @param f function
#' @inheritParams base::system.time
#'
#' @return invisible()
#' @export
#' @examples
#' system_time_once(sample)
#' x <- sample(1e6)
system_time_once <- function(f, gcFirst = TRUE){
  eval.parent(substitute(surround_once(
    f,
    function(expr){
      print(system.time(x <- expr, gcFirst))
      x}
    )))
}


