#!  /usr/bin/env Rscript

## Andy: n >= 4
## Bill: n < 4
## Charlie: n >= 1
## At least one is right: A | B | C
## Exactly one is right: (A ==> (!B & !C)) & (B ==> (!A & !B)) & (C ==> (!B & !A))
## Numerical relations: (!A | (!B & C)) & (A | B) & (!B | !A) & (B | (A & C)) & (C | (!A & B))

`%AND%` <- function(t, u) { }
`%OR%` <- function(t, u) { }
not <- function(t) { }
`%==>%` <- function(t) { }

cnf <- function(fml, p)
{
    exp  <- parse(text=fml)[[1]]
    ## atom: proposition
    if (class(exp) == "name") {
        res  <- p[[as.character(exp)]]
        if (is.null(res))
            res  <- 0
        return (list(res))
    }
    if (class(exp) == "(")
        return (cnf(as.character(exp[2]), p))
    op  <- as.character(exp[1])
    x  <- as.character(exp[2])
    if (op == "not") {
        x.cnf  <- cnf(x, p)
        not.x  <- lapply(x.cnf, function(r) -1*r)
        lens  <- lapply(not.x, function(r) 1:length(r))
        grid  <- expand.grid(lens)
        grid  <- split(grid, unlist(seq(nrow(grid))))
        res  <- lapply(grid, function (g) sapply(seq_along(not.x), function (k) not.x[[k]][g[,k]]))
        names(res)  <- NULL
        return(res)
    }
    y  <- as.character(exp[3])
    if (op == "%AND%") {
        x.cnf  <- cnf(x, p)
        y.cnf  <- cnf(y, p)
        return (c(x.cnf, y.cnf))
    }
    if (op == "%OR%") {
        x.cnf  <- cnf(x, p)
        y.cnf  <- cnf(y, p)
        grid  <- expand.grid(1:length(x.cnf), 1:length(y.cnf))
        grid  <- split(grid, seq(nrow(grid)))
        res  <- lapply(grid, function(g) c(unlist(x.cnf[[g[,1]]]), unlist(y.cnf[[g[,2]]])))
        names(res)  <- NULL
        return(res)
    }
}

tf <- c(TRUE, FALSE)
truth.table <- expand.grid(A=tf, B=tf, C=tf)
formula <- with(truth.table, (A | B | C) & (!A | (!B & !C)) & (!B | (!A & !C)) & (!C | (!B & !A)) & (!A | (!B & C)) & (A | B) & (!B | !A) & (B | (A & C)) & (C | (!A & B)))
truth.table$formula <- formula
