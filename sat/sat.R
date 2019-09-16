`%==>%` <- function(x, y) { }
`%<==>%` <- function(x, y) { }

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


## Convert formula `fml` into CNF using proposition mapping in props
## Formula `fml` must only use 
## If props is empty, generate mappings.
##
## Return list with:
##    $cnf (list): CNF representation of `fml`
##    $props (list): mapping proposition names used in `fml` to numbers used to represent them
##
## The conversion is done using distributivity and de Morgan's laws. Therefore, it
## may blow up when there is a lot of alternation between AND and OR formulas.
cnf <- function(fml, props=list())
{
    ex  <- parse(text=fml)[[1]]
    ## atom: proposition
    if (class(ex) == "name") {
        exs  <- as.character(ex)
        res  <- props[[exs]]
        if (is.null(res)) {
            res  <- length(props)+1
            props[[exs]]  <- res
        }
        return (list(cnf=list(res), props=props))
    }
    ## remove parentheses
    if (class(ex) == "(")
        return (cnf(as.character(ex[2]), props))
    op  <- as.character(ex[1])
    x  <- as.character(ex[2])
    x.rec  <- cnf(x, props)
    x.cnf  <- x.rec$cnf
    if (length(ex) < 3) {
        ## unary operator
        fml  <- eval(call(op, x.cnf))
        ##         apply CNF definition
        list(cnf=fml, props=x.rec$props)
    }  else {
        ## binary operator
        y  <- as.character(ex[3])
        ## apply CNF definition
        y.rec  <- cnf(y, x.rec$props)
        y.cnf  <- y.rec$cnf
        fml  <- eval(call(op, x.cnf, y.cnf))
        list(cnf=fml, props=y.rec$props)
    }
}



## Simplify a propositional formula by rewriting away implications and coimplications
simplify  <- function(fml, operators=identity)
{
    ex  <- parse(text=fml)[[1]]
    if (class(ex) == "call") {
        op  <- as.character(ex[1])
        x  <- simplify(as.character(ex[2]), operators)
        if (length(ex) > 2)
            y  <- simplify(as.character(ex[3]), operators)
        if (op == "%==>%") {
            return (simplify(paste("not(", x, ") %OR% (", y, ")", sep=""), operators))
        }
        if (op == "%<==>%") {
            imp  <- paste("((", x, ") %==>% (", y, "))", sep="")
            coimp  <- paste("((", y, ") %==>% (", x, "))", sep="")
            return (simplify(paste("(", imp, ") %AND% (", coimp, ")", sep=""), operators))
        }
        if (length(ex) > 2)
            return (paste(x, operators(op), y))
        else
            return (paste(operators(op), "(", x, ")", sep=""))
    }
    if (class(ex) == "(") {
        paste("(", simplify(as.character(ex[2]), operators), ")", sep="")
    } else {
        fml
    }
}

## Mapping of R's native Boolean operators
boolean.operators  <- function(op)
{
    switch(op,
           "%AND%"="&",
           "%OR%"="|",
           "not"="!",
           op)
}


## Build the truth table for formula `formula` given as a string
## If `formula` is a vector of strings, they are interpreted as a conjunction
truth.table  <- function(formula)
{
    fml  <- paste(sapply(formula, function(s) paste("(", s, ")", sep="")),
                  collapse="  %AND% ")
    sf  <- parse(text=simplify(fml, operators=boolean.operators))
    props  <- names(cnf(simplify(fml))$props)
    res  <- lapply(props, function(p) c(TRUE, FALSE))
    names(res)  <- props
    res  <- expand.grid(res)
    res$formula  <- with(res, eval(sf))
    data.frame(res)
}


## Call PicoSAT on formula `formula` given as a string
## If `formula` is a vector of strings, they are interpreted as a conjunction
check.sat  <- function(formula)
{
    library(rpicosat)
    fml  <- paste(sapply(formula, function(s) paste("(", s, ")", sep="")),
                  collapse="  %AND% ")
    sf  <- cnf(simplify(fml))
    props  <- sf$props
    sat  <- as.data.frame(picosat_sat(sf$cnf))
    sat$variable  <- sapply(sat$variable, function(n) names(props)[unlist(props)[n]])
    sat
}
