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

## Convert string `fml` into CNF using proposition mapping in props
## If props is empty, generate mappings.
## Return list with:
##    $cnf (list): CNF representation of `fml`
##    $props (list): mapping proposition names used in `fml` to numbers used to represent them
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
        ## unary operator: apply CNF definition
        fml  <- eval(call(op, x.cnf))
        list(cnf=fml, props=x.rec$props)
    }  else {
        ## binary operator
        y  <- as.character(ex[3])
        if (op == "%==>%")
            ## implies is just a shorthand
            return (cnf(paste("not(", x, ") %OR% (", y, ")", sep=""), props))
        if (op == "%<==>%") {
                imp  <- cnf(paste("((", x, ") %==>% (", y, "))", sep=""), props)
                coimp  <- cnf(paste("((", y, ") %==>% (", x, "))", sep=""), imp$props)
                fml  <- imp$cnf %AND% coimp$cnf
                return (list(cnf=fml, coimp$props))
        }
        ## primitive binary operator: apply CNF definition
        y.rec  <- cnf(y, x.rec$props)
        y.cnf  <- y.rec$cnf
        fml  <- eval(call(op, x.cnf, y.cnf))
        list(cnf=fml, props=y.rec$props)
    }
}
