#' Use a progress bar with regular for loops
#'
#' These functions wrap the progress bar utilities of the *progress* package
#' to be able to use progress bar with regular `for`, `while` and `repeat` loops conveniently.
#' They forward all their
#' parameters to `progress::progress_bar$new()`. `pb_while()` and `pb_repeat()`
#' require the `total` argument.
#'
#' @param total for `pb_while()` and `pb_repeat()`, an estimation of the
#' number of iteration.
#' @param format The format of the progress bar.
#' @param width Width of the progress bar.
#' @param complete Completion character.
#' @param incomplete Incomplete character.
#' @param current Current character.
#' @param callback Callback function to call when the progress bar finishes.
#'   The progress bar object itself is passed to it as the single parameter.
#' @param clear Whether to clear the progress bar on completion.
#' @param show_after Amount of time in seconds, after which the progress bar is
#'   shown on the screen. For very short processes, it is probably not worth
#'   showing it at all.
#' @param force Whether to force showing the progress bar, even if the given (or default) stream does not seem to support it.
#' @param tokens A list of unevaluated expressions, using `alist`, to be passed
#'   passed to the `tick` method of the progress bar
#' @param message A message to display on top of the bar
#'
#' @export
#'
#' @examples
#' pb_for()
#' for (i in 1:10) {
#'   # DO SOMETHING
#'   Sys.sleep(0.5)
#' }
#'
#' pb_for(format = "Working hard: [:bar] :percent :elapsed",
#'        callback = function(x) message("Were'd done!"))
#' for (i in 1:10) {
#'   # DO SOMETHING
#'   Sys.sleep(0.5)
#' }
pb_for <-
  function(
    # all args of progress::progress_bar$new() except `total` which needs to be
    # infered from the 2nd argument of the `for` call, and `stream` which is
    # deprecated
    format = "[:bar] :percent",
    width = options("width")[[1]] - 2,
    complete = "=",
    incomplete = "-",
    current =">",
    callback = invisible, # doc doesn't give default but this seems to work ok
    clear = TRUE,
    show_after = .2,
    force = FALSE,
    message = NULL,
    tokens = alist()){

    # create the function that will replace `for`
    f <- function(it, seq, expr){
      # to avoid notes at CMD check
      PB <- IT <- SEQ <- EXPR <- TOKENS <- NULL

      # forward all arguments to progress::progress_bar$new() and add
      # a `total` argument compted from `seq` argument
      pb <- progress::progress_bar$new(
        format = format, width = width, complete = complete,
        incomplete = incomplete, current = current,
        callback = callback,
        clear = clear, show_after = show_after, force = force,
        total = length(seq))
      if(!is.null(message)) pb$message(message)

      # using on.exit allows us to self destruct `for` if relevant even if
      # the call fails.
      # It also allows us to send to the local environment the changed/created
      # variables in their last state, even if the call fails (like standard for)
      on.exit({
        list2env(mget(ls(env),envir = env), envir = parent.frame())
        rm(`for`,envir = parent.frame())
      })

      # we build a regular `for` loop call with an updated loop code including
      # progress bar.
      # it is executed in a dedicated environment
      env <- new.env(parent = parent.frame())
      eval(substitute(
        env = list(IT = substitute(it), SEQ = substitute(seq),
                   EXPR = do.call(substitute, list(substitute(expr),list(message = pb$message))),
                   TOKENS = tokens, PB = pb
        ),
        base::`for`(IT, SEQ,{
          EXPR
          PB$tick()
        })), envir = env)
    }
    # override `for` in the parent frame
    assign("for", value = f,envir = parent.frame())
    invisible()
  }
