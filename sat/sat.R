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
        return (res)
    }
    if (class(exp) == "(")
        return (cnf(as.character(exp[2]), p))
    op  <- as.character(exp[1])
    x  <- as.character(exp[2])
    y  <- as.character(exp[3])
    if (op == "%AND%") {
        x.cnf  <- cnf(x, p)
        y.cnf  <- cnf(y, p)
        return (c(x.cnf, y.cnf))
    }
    if (op == "%OR%") {
        x.cnf  <- cnf(x, p)
        y.cnf  <- cnf(y, p)
        df  <- expand.grid(x.cnf, y.cnf, KEEP.OUT.ATTRS=FALSE)
        colnames(df)  <- NULL
        rownames(df)  <- NULL
        res  <- split(df, seq(nrow(df)))
        names(res)  <- NULL
        return(res)
    }
}

tf <- c(TRUE, FALSE)
truth.table <- expand.grid(A=tf, B=tf, C=tf)
formula <- with(combinations, (A | B | C) & (!A | (!B & !C)) & (!B | (!A & !C)) & (!C | (!B & !A)) & (!A | (!B & C)) & (A | B) & (!B | !A) & (B | (A & C)) & (C | (!A & B)))
truth.table$formula <- formula
