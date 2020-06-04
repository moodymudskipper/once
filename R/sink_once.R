#' Sink the Output of a Function Once
#'
#' Works a lot like `base::sink()` except that it's applied on the output of a specific
#' function (only once). There is no need to call `sink()` a second time to end
#' the output diversion. The other difference is that one can feed
#' `type = c("output", "message")` rather than choose one of them or call `sink()`
#' for each one.
#'
#' @inheritParams base::sink
#' @param f a function
#' @param type either "output", "message", or a vector containing both, they can
#' be abbreviated
#'
#' @return `invisible()`
#'
#' @examples
#' greet <- function(x,y){
#'   message(x)
#'   print(y)
#' }
#'
#' file <- tempfile()
#' sink_once(greet, file, type = c("o","m"))
#'
#' # output is diverted
#' greet("Hi", "What's up?")
#'
#' # let's read it back
#' readLines(file)
#'
#' # further calls work normally
#' greet("Hi", "What's up?")
#' @export
sink_once <- function(
  f,
  file,
  append = FALSE,
  type = "output",
  split = FALSE){

  type <- match.arg(type, choices = c("output", "message"), several.ok = TRUE)
  if(missing(file)) stop("`file` cannot be missing")

  sink_calls_enter <- lapply(type, function(x) bquote(sink(
    file = `*con*`, append = .(append), type = .(x),  split = .(split))))

  sink_calls_enter <- c(bquote(`*con*` <- file(.(file))), sink_calls_enter)

  sink_calls_exit <- lapply(type, function(x) bquote(sink(type = .(x))))

  trace_call <- bquote(
    trace_once(
      .(substitute(f)),
      tracer = quote(.(as.call(c(quote(`{`), sink_calls_enter)))),
      exit = quote(.(as.call(c(quote(`{`), sink_calls_exit))))))

  eval.parent(trace_call)
  invisible()
}
