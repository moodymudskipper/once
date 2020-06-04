#' @export
#' @rdname pb_for
pb_while <-
  function(
    # all args of progress::progress_bar$new() except `stream` which is
    # deprecated
    total,
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
    tokens = alist()) {
    if(missing(total))
      stop("you must provide the argument `total` to `pb_while()` to give a length to the bar")

    # create the function that will replace `while`
    f <- function(cond, expr){
      # to avoid notes at CMD check
      PB <- COND <- EXPR <- TOKENS <- NULL

      # forward all arguments to progress::progress_bar$new() and add
      # a `total` argument compted from `seq` argument
      pb <- progress::progress_bar$new(
        format = format, width = width, complete = complete,
        incomplete = incomplete, current = current,
        callback = callback,
        clear = clear, show_after = show_after, force = force,
        total = total)
      if(!is.null(message)) pb$message(message)

      # using on.exit allows us to self destruct `while` if relevant even if
      # the call fails.
      # It also allows us to send to the local environment the changed/created
      # variables in their last state, even if the call fails (like standard while)
      on.exit({
        list2env(mget(ls(env),envir = env), envir = parent.frame())
        rm(`while`,envir = parent.frame())
      })

      # we build a regular `while` loop call with an updated loop code including
      # progress bar.
      # it is executed in a dedicated environment
      env <- new.env(parent = parent.frame())
      eval(substitute(
        env = list(COND = substitute(cond),
                   EXPR = do.call(substitute, list(substitute(expr),list(message = pb$message))),
                   TOKENS = tokens, PB = pb
        ),
        base::`while`(COND,{
          EXPR
          if(PB$finished)  {
            #copyEnv(progress_bar$new(), PB)
            # browser()
            (function(pb){
               pb$.__enclos_env__$private$current <- 0
               pb$.__enclos_env__$private$complete <- FALSE
               pb$finished <- FALSE}) (PB)
            PB$message("Progress bar finished, starting a new one")
          }
          PB$tick()
        })), envir = env)
    }
    # override `while` in the parent frame
    assign("while", value = f,envir = parent.frame())
    invisible()
  }
