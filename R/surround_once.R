#' Surround a function call once
#'
#' Some functions, such as `suppressWarnings()`,  take an expression as their main
#' input. `surround_once()`
#' proposes a way to modify a function for one call so it behaves as if it were
#' surrounded by this surrounding function. `with()` also takes an expression, but
#' in second position, which makes it a bit awkward to use with `surround_once()`,
#' `with_once()` solves this issue and can be used with all functions sharing
#' the syntax (e.g. the `with_*` functions of the withr package).
#'
#' @param f a function
#' @param surround_fun a surrounding function used by surround_once
#' @param with_fun a surrounding function used by with_once
#' @param ... additional parameters passed to surround_fun
#'
#' @export
#'
#' @examples
#' add <- function(x, y) {
#'  warning("we will perform an addition!")
#'  x + y
#' }
#'
#' surround_once(add, suppressWarnings)
#' add(1,2)
#' add(1,2)
#'
#' Sepal.Length <- 1:3
#' with_once(head, iris)
#' head(Sepal.Length, 2)
#' head(Sepal.Length, 2)
#'
#' # we can get `with_once()`'s behavior with `surround_once()` but
#' # we must name the data argument as it comes first in `with()`
#' surround_once(head, with, data = iris)
#' head(Sepal.Length, 2)
#' head(Sepal.Length, 2)
#'
#' \dontrun{
#' #' with_once(print,list(digits = 3),  with_fun = withr::with_options)
#' print(pi)
#' print(pi)
#' }
surround_once <- function(f, surround_fun, ...){
  f_sym <- substitute(f)
  f_nm <- deparse(f_sym)
  f2 <- args(f)
  env <- parent.frame()
  body(f2) <- bquote({
    on.exit({
      if(identical(environment(f), env)){
        assign(.(f_nm), f, envir = env)
      }
    })
    rm(.(f_sym), envir = env)
    call <- sys.call()
    call[[1]] <- f
    eval.parent(substitute(
      .(as.call(c(substitute(surround_fun), quote(CALL), eval(substitute(alist(...)))))),
      list(CALL = call)
      ))
  })
  environment(f2) <- environment()
  assign(f_nm, value = f2, envir = env)
  invisible()
}

#' @export
#' @rdname surround_once
with_once <- function(f, ..., with_fun = with){
  f_sym <- substitute(f)
  f_nm <- deparse(f_sym)
  f2 <- args(f)
  env <- parent.frame()
  dots <- eval(substitute(alist(...)))
  body(f2) <- bquote({
    on.exit({
      if(identical(environment(f), env)){
        assign(.(f_nm), f, envir = env)
      }
    })
    rm(.(f_sym), envir = env)
    call <- sys.call()
    call[[1]] <- f
    eval.parent(substitute(
      .(as.call(c(substitute(with_fun), dots[1], quote(CALL), dots[-1]))),
      list(CALL = call)
    ))
  })
  environment(f2) <- environment()
  assign(f_nm, value = f2, envir = env)
  invisible()
}
