#!  /usr/bin/env Rscript

## Andy: n >= 4
## Bill: n < 4
## Charlie: n >= 1
## At least one is right: A | B | C
## Exactly one is right: (A ==> (!B & !C)) & (B ==> (!A & !B)) & (C ==> (!B & !A))
## Numerical relations: (!A | (!B & C)) & (A | B) & (!B | !A) & (B | (A & C)) & (C | (!A & B))

## Operators compose CNF formulas
`%AND%` <- function(x, y)
{
    c(x, y)
}

`%OR%` <- function(x, y)
{
    grid  <- expand.grid(1:length(x), 1:length(y))
    grid  <- split(grid, seq(nrow(grid)))
    res  <- lapply(grid, function(g) c(unlist(x[[g[,1]]]), unlist(y[[g[,2]]])))
    names(res)  <- NULL
    res
}

not <- function(x)
{
    not.x  <- lapply(x, function(r) -1*r)
    lens  <- lapply(not.x, function(r) 1:length(r))
    grid  <- expand.grid(lens)
    grid  <- split(grid, unlist(seq(nrow(grid))))
    res  <- lapply(grid,
                   function (g) sapply(seq_along(not.x),
                                       function (k) not.x[[k]][g[,k]]))
    names(res)  <- NULL
    res
}

`%==>%` <- function(x, y) { }

`%<==>%` <- function(x, y) { }

cnf <- function(fml, p=list())
{
    ex  <- parse(text=fml)[[1]]
    ## atom: proposition
    if (class(ex) == "name") {
        res  <- p[[as.character(ex)]]
        if (is.null(res))
            res  <- 0
        return (list(res))
    }
    ## remove parentheses
    if (class(ex) == "(")
        return (cnf(as.character(ex[2]), p))
    op  <- as.character(ex[1])
    x  <- as.character(ex[2])
    x.cnf  <- cnf(x, p)
    if (length(ex) < 3)
        ## unary operator: apply CNF definition
        eval(call(op, x.cnf))
    else {
        ## binary operator
        y  <- as.character(ex[3])
        if (op == "%==>%")
            ## implies is just a shorthand
            cnf(paste("not(", x, ") %OR% (", y, ")", sep=""), p)
        else {
            if (op == "%<==>%") {
                imp  <- cnf(paste("((", x, ") %==>% (", y, "))", sep=""), p)
                coimp  <- cnf(paste("((", y, ") %==>% (", x, "))", sep=""), p)
                imp %AND% coimp
            }
            else {
                ## primitive binary operator: apply CNF definition
                y.cnf  <- cnf(y, p)
                eval(call(op, x.cnf, y.cnf))
            }
        }
    }
}

tf <- c(TRUE, FALSE)
truth.table <- expand.grid(A=tf, B=tf, C=tf)
formula <- with(truth.table, (A | B | C) & (!A | (!B & !C)) & (!B | (!A & !C)) & (!C | (!B & !A)) & (!A | (!B & C)) & (A | B) & (!B | !A) & (B | (A & C)) & (C | (!A & B)))
truth.table$formula <- formula
