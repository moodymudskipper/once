#' Trace a function once
#'
#' Works like trace but the function will return to normal after a single run.
#' It has a couple more differences : `print` is `FALSE` by default, and it
#' gains a `...` argument which is an often more convenient alternative to
#' the `tracer` argument.
#'
#' @inheritParams base::trace
#' @param ... unquoted expressions, if used rather than the `tracer` argument,
#' will be quoted and turned into the `tracer` argument fed to `trace()`
#'
#' @export
#'
#' @examples
#' add <- function(x, y) x + y
#' trace_once(add, print("entering 'add'"))
#' add(1,3)
#'
#' # after a calls the behavior returns to normal
#' add(1,3)
trace_once <- function(what, ..., tracer, exit, at, print = FALSE, signature,
                      where = topenv(parent.frame()), edit = FALSE){
  mc <- match.call(expand.dots = FALSE)
  # replace trace_once call by trace call
  mc[[1]] <- quote(trace)

  if(...length()){
    if (!missing(tracer))
      stop("you can't use both `...` and `tracer` argument\n",
           "use `...` to feed unquoted expressions and tracer` to feed a quoted expression")
    mc[["tracer"]] <- substitute(quote({...}))
    mc[["..."]] <- NULL
  }
  if(missing(print)) mc[["print"]] <- FALSE

  # define or append exit argument to untrace
  mc[["exit"]] <-
    if (missing(exit)) substitute(quote(eval.parent(quote(untrace(what)))))
  else c(as.expression(exit), substitute(eval.parent(quote(untrace(what)))))
  eval.parent(mc)
  invisible()
}
