#' Apply a function operator to a function for one call only
#'
#' @param f a function
#' @param decorator a function operator
#' @param ... additional argument passed to decorator
#'
#' @export
#' @examples
#' decorate_once(is.logical, Negate)
#' is.logical(TRUE)
#' is.logical(TRUE)
decorate_once <- function(f, decorator, ...) {

  f_sym <- substitute(f)
  f_nm <- deparse(f_sym)
  f2 <- args(f)
  env <-  parent.frame()
  body(f2) <- bquote({
    on.exit({
      if(identical(environment(f), env)){
        assign(.(f_nm), f, envir = env)
      }
    })
    rm(.(f_sym), envir = env)
    # edit call to replace f() by decorator(f)() and run
    call <- sys.call()
    call[[1]] <- quote(.(as.call(c(substitute(decorator),f, eval(substitute(alist(...)))))))
    eval.parent(call)
  })
  environment(f2) <- environment()
  assign(f_nm, value = f2, envir = env)
  invisible()
}
