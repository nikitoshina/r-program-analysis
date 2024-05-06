foo <- function(a, b) {
    return(a + b * bop())
}

bar <- function(a, b) {
    sum <- foo(a, b)
    return(sum * constant)
}

baz <- function(a, b) {
    bar(a, b) * bar(a, b)
}

var <- baz(a = 2, b = b)

constant <- 5

bop <- function() {
    return(5)
}

baz(1, var)
