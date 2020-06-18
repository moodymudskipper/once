
<!-- README.md is generated from README.Rmd. Please edit that file -->

# once <img src='man/figures/logo.png' align="right" height="139" />

`once` provides a collection of single use function operators. These are
functions that modify a function for one call only. `debugonce()` is a
base R function that matches this description already, and its approach
is generalized.

The modified functions include control flow constructs as you can see
below.

  - `pb_for()`, `pb_while()` and `pb_repeat()` add a progress bar to
    `for`, `while` or `repeat` loops. It wraps the package *progress*
    and leverages its flexibility.
  - `verbose_if()` and `verbose_while()` print a breakdown of the
    boolean condition
  - `verbose_for()` prints the iterator, or the latter modified by a
    give transformation function
  - `trace_once()` is essentially like trace but for single use
  - `system_time_once()` prints the `system.time()` of the next relevant
    call
  - `sink_once()` uses `sink()` to divert the ouput of the next relevant
    call
  - `surround_once()` surrounds the next relevant call with a
    surrounding function, such as `suppressWarnings`
  - `with_once()` uses `with()` a single time on the next relevant call,
    and support similar functions such as `withr::with_*` functions
  - `decorate_once()` applies a decorator (a.k.a function operator or
    adverb) to a function for a single call

Using these functions is often more compact that alternatives, but they
are other advantages :

  - arguably more readable in many case
  - Avoid parenthesis overload
  - Avoid having to insert closing parenthesis at an awkward place, and
    associated potential mistakes
  - Can be used to decorate a step of a magrittr pipe chain without
    making a mess
  - trivial to comment in and out when debugging

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/once")
```

## Examples

``` r
library(once)
```

### progress bars

You’ll have to run those yourself to see the progress bar.

``` r
# just call pb_for() to get a progress bar
pb_for()
for (i in 1:10) {
  Sys.sleep(1)
}

# leverage features of {progress} package
pb_for(format = "Working hard: [:bar] :percent :elapsed",
       callback = function(x) message("Were'd done!"))
for (i in 1:10) {
  Sys.sleep(1)
}

# also for while and repeat!
# but we must provide expected iteration number

# if we estimate too high, bar is not completed
pb_while(20)
x <- 1
while (x < 10) {
  Sys.sleep(1)
  x <- x + 1
}

# if we estimate too low, another bar starts
pb_repeat(5)
x <- 1
repeat  {
  Sys.sleep(1)
  x <- x + 1
  if(x == 10) break
}
```

### verbose control flows

``` r
roses_are_red <- TRUE
violets_are_blue <- TRUE
love <- 1i
verbose_if()
if (is.logical(love) || roses_are_red && violets_are_blue)
  print("Sugar is sweet my love, but not as sweet as you.")
#>   `is.logical(love) || roses_are_red && violets_are_blue` `is.logical(love)`
#> 1                                                    TRUE         FALSE: cpl
#>   `roses_are_red && violets_are_blue` `roses_are_red` `violets_are_blue`
#> 1                                TRUE            TRUE               TRUE
#> [1] "Sugar is sweet my love, but not as sweet as you."

x <- 1
y <- 2
verbose_while()
while(x < 5 && y > 0) x <- x + 1
#>   `x < 5 && y > 0` `x < 5` `x` `y > 0` `y`
#> 1             TRUE    TRUE   1    TRUE   2
#>                                           
#> 1             TRUE    TRUE   2    TRUE   2
#>                                           
#> 1             TRUE    TRUE   3    TRUE   2
#>                                           
#> 1             TRUE    TRUE   4    TRUE   2
#>                                           
#> 1            FALSE   FALSE   5    TRUE   2

verbose_for(dim)
l <- list(iris, cars)
for (x in l) {
print(head(x,1))
}
#> NULL
#> [1] 150   5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> [1] 50  2
#>   speed dist
#> 1     4    2
```

### … and all the rest\!

`trace_once()` is essentially like trace but for single use, it gained a
`...` argument to pass unquoted expression more easily.

``` r
add <- function(x, y) x + y
trace_once(add, print(typeof(x)))
add(3, 4)
#> [1] "double"
#> [1] 7
```

`system_time_once()` prints the `system.time()` of the next relevant
call.

``` r
system_time_once(sample)
x <- sample(1e6)
#>    user  system elapsed 
#>    0.09    0.00    0.09
x <- sample(1e6)
```

`sink_once()` uses `sink()` to divert the ouput of the next relevant
call.

``` r
greet <- function(x,y){
  message(x)
  print(y)
}

file <- tempfile()
sink_once(greet, file, type = c("o","m")) # c("o","m") for output AND message

# output is diverted
greet("Hi", "What's up?")

# let's read it back
readLines(file)
#> [1] "Hi"                 "[1] \"What's up?\""

# further calls work normally
greet("Hi", "What's up?")
#> Hi
#> [1] "What's up?"
```

`surround_once()` surrounds the next relevant call with a surrounding
function, such as `suppressWarnings`.

`with_once()` uses `with()` a single time on the next relevant call, and
support similar functions such as `withr::with_*` functions.

``` r
add <- function(x, y) {
 warning("we will perform an addition!")
 x + y
}

surround_once(add, suppressWarnings)
add(1,2)
#> [1] 3
add(1,2)
#> Warning in add(1, 2): we will perform an addition!
#> [1] 3

Sepal.Length <- 1:3
with_once(head, iris)
head(Sepal.Length, 2)
#> [1] 5.1 4.9
head(Sepal.Length, 2)
#> [1] 1 2

# we can get `with_once()`'s behavior with `surround_once()` but
# we must name the data argument as it comes first in `with()`
surround_once(head, with, data = iris)
head(Sepal.Length, 2)
#> [1] 5.1 4.9
head(Sepal.Length, 2)
#> [1] 1 2

with_once(print,list(digits = 3),  with_fun = withr::with_options)
print(pi)
#> [1] 3.14
print(pi)
#> [1] 3.141593
```

`decorate_once()` applies a decorator (a.k.a function operator or
adverb) to a function for a single call.

``` r
decorate_once(is.logical, Negate)
is.logical(TRUE)
#> [1] FALSE
is.logical(TRUE)
#> [1] TRUE
```
