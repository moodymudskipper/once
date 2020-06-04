#' verbose control flow
#'
#' These functions make `for`, `while` or `if` more talkative for their next call.
#' `verbose_for()` will print the iterator, or the latter transformed by the `fun`
#' argument. `verbose_if()` and `verbose_while()` will break down the boolean
#' condition and display it as a one row `data.frame`.
#'
#' @param fun a function to apply on `for`'s iterator
#' @importFrom stats setNames
#' @export
#' @examples
#' roses_are_red <- TRUE
#' violets_are_blue <- TRUE
#' love <- 1i
#' verbose_if()
#' if (is.logical(love) || roses_are_red && violets_are_blue)
#'   print("Sugar is sweet my love, but not as sweet as you.")
#'
#' x <- 1
#' y <- 2
#' verbose_while()
#' while(x < 5 && y > 0) x <- x + 1
#'
#' verbose_for(dim)
#' l <- list(iris, cars)
#' for (x in l) {
#' print(head(x,1))
#' }
verbose_for <- function(fun = identity) {
  # create the function that will replace `for`

  f <- function(it, seq, expr){
    # remove modified for already
    rm(`for`,envir = parent.frame())

    # break down the condition and print
    print(fun(it))

    # reevaluate the call
    eval.parent(substitute(
      for (it in seq){
        print(fun(it))
        expr
      }))
  }
  body(f) <- do.call(substitute, list(body(f), list(fun = substitute(fun))))
  # override `if` in the parent frame
  assign("for", value = f,envir = parent.frame())
  invisible()
}


#' @export
#' @rdname verbose_for
verbose_if <- function() {
  # create the function that will replace `if`
  f <- function(cond, yes, no){
    # remove modified if already
    rm(`if`,envir = parent.frame())

    # break down the condition and print
    print(break_down_cond(substitute(cond)))

    # reevaluate the call
    eval.parent(sys.call())
  }
  # override `if` in the parent frame
  assign("if", value = f,envir = parent.frame())
}

#' @export
#' @rdname verbose_for
verbose_while <- function() {
  # create the function that will replace `while`
  f <- function(cond, expr){
    # remove modified if already
    rm(`while`,envir = parent.frame())

    # reevaluate the call, printing the breakdown before going through expr
    eval.parent(substitute({
      print(break_down_cond(quote(cond)))
      while(cond){
        expr
        print(break_down_cond(quote(cond), names = FALSE))
      }
    })
    )
  }
  # override `if` in the parent frame
  assign("while", value = f,envir = parent.frame())
  invisible()
}


# breakdown condition into a one row data frame
breakdown <- function(expr, l = list(), env = parent.frame()){

  if(!is.call(expr))
    return(append(l, setNames(list(eval(expr, envir = env)), deparse(expr))))

  # if "!", breakdown negated expression
  if(identical(expr[[1]], quote(`!`)))
    return(breakdown(expr[[2]], l, env))

  # if is.* function, apply (for POC) heuristic
  if(grepl("^is[._]", deparse(expr[[1]]))) {
    val <- eval(expr, envir = env)
    if(val)
      l <- append(l, setNames(list(TRUE), deparse(expr)))
    else
      l <- append(l, setNames(
        paste("FALSE:", vctrs::vec_ptype_abbr(eval(expr[[2]], envir = env))), deparse(expr)))
    return(l)
  }
  l <- append(l, setNames(list(eval(expr, envir = env)), deparse(expr)))

  # if `&&`, breakdown both sides
  if(identical(expr[[1]], quote(`&&`))){
    l <- breakdown(expr[[2]], l, env)
    l <- breakdown(expr[[3]], l, env)
    return(l)
  }

  # if `||`, breakdown lhs and sometimes rhs if lhs is FALSE
  if(identical(expr[[1]], quote(`||`))){
    l <- breakdown(expr[[2]], l, env)

    if( l[[length(l)]] == "TRUE") # is.logical(l[[length(l)]]) &&
      # if lhs is TRUE we don't go further
      l <- append(l, setNames(list("*ignored*"), deparse(expr[[3]])))
    else
      # else we breakdown the rhs too
      l <- breakdown(expr[[3]], l, env)
    return(l)
  }

  # if comparison, breakdown both sides unless they're not language
  if(list(expr[[1]]) %in% c(quote(`<`), quote(`<=`), quote(`>`), quote(`>=`))){
    if(is.language(expr[[2]])) l <- breakdown(expr[[2]], l, env)
    if(is.language(expr[[3]])) l <- breakdown(expr[[3]], l, env)
    return(l)
  }
  l
}

#' Break down a condition
#'
#' Used by `verbose_if()` and `verbose_while()` and exported for convenience
#' @param cond expression
#' @param env environment
#' @param names boolean
#' @export
break_down_cond <- function(cond,  env = parent.frame(), names = TRUE){
  bd <- breakdown(cond, env = env)
  nms <- paste0("`",names(bd),"`")
  if(!names) nms <- gsub("."," ", nms)
  setNames(as.data.frame(bd), nms)
}


