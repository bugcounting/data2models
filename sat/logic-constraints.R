#!  /usr/bin/env Rscript

library(rpicosat)
source("sat.R")

simple  <- "(A %==>% B) %AND% (B %==>% C) %AND% (C %==>% A)"
tf <- c(TRUE, FALSE)
table  <- expand.grid(A=tf, B=tf, C=tf)
table$formula  <- with(table, (!A | B) & (!B | C) & (!C | A))

## Andy: n >= 4
## Beverly: n < 4
## Charlie: n >= 1
## At least one is right:
one.is.right  <- "A %OR% B %OR% C"
exactly.one  <- c("A %==>% (not(B) %AND% not(C))", # if A is right, B and C are not
                  "B %==>% (not(C) %AND% not(A))", # if B is right, C and A are not
                  "C %==>% (not(A) %AND% not(B))") # if C is right, A and B are not
numerical.relations  <- c("A %==>% (not(B) %AND% C)",
                          "B %==>% not(A)",
                          "not(A) %==>% B",
                          "not(B) %==>% (A %AND% C)",
                          "not(C) %==>% B")



## X.r.c.v denotes that cell in row r and column c has value v

## AND of all formulas `fms`
ands  <- function(fms)
{
    paste(sapply(fms, function(s) paste("(", s, ")", sep="")), collapse=" %AND% ")
}

## OR of all formulas `fms`
ors  <- function(fms)
{
    paste(sapply(fms, function(s) paste("(", s, ")", sep="")), collapse=" %OR% ")
}

## NOT applied to each formula in `fms`
nots  <- function(fms)
{
    sapply(fms, function(s) paste("not(", s, ")", sep=""))
}

## Exactly one of formulas `fms` is true
just.one  <- function(fms)
{
    ors(sapply(1:length(fms), function(n) ands(c(fms[n], nots(fms[-n])))))
}

cell  <- function(r, c, v)
{
    paste(c("X", as.character(r), as.character(c), as.character(v)), collapse=".")
}

## Cell r, c has a well-defined number between 1 and 9
## That is, not(X.r.c.v %AND% X.r.c.u) for all v != u
value.of.cell  <- function(r, c)
{
    apply(subset(expand.grid(v=1:9, u=1:9), v < u),
          1,
          function(d) paste("not(", ands(c(cell(r, c, d[1]), cell(r, c, d[2]))), ")", sep=""))
}

## Row r includes all numbers between 1 and 9
## That is, not(X.r.c1.v %AND% X.r.c2.v) for all c1 != c2 and all v
value.of.row  <- function(r)
{
    ands(
        as.vector(
            sapply(1:9,
                   function(v)
                       apply(subset(expand.grid(x=1:9, y=1:9), x < y),
                             1,
                             function(d) paste("not(", ands(c(cell(r, d[1], v),
                                                              cell(r, d[2], v))), ")", sep="")))
        )
    )
}

## Column c includes all numbers between 1 and 9
## That is, not(X.r1.c.v %AND% X.r2.c.v) for all r1 != r2 and all v
value.of.col  <- function(c)
{
    ands(
        as.vector(
            sapply(1:9,
                   function(v)
                       apply(subset(expand.grid(x=1:9, y=1:9), x < y),
                             1,
                             function(d) paste("not(", ands(c(cell(d[1], c, v),
                                                              cell(d[2], c, v))), ")", sep="")))
        )
    )
}


## Data frame with row, column coordinates of block #b
block  <- function(b)
{
    first.row  <- 1+3*((b - 1) %/% 3)
    first.col  <- 1+3*((b - 1) %% 3)
    expand.grid(x=first.row:(first.row+2), y=first.col:(first.col+2))
}

## Block b includes all numbers between 1 and 9
value.of.block  <- function(b)
{
    ands(
        as.vector(
            sapply(1:9,
                   function(v)
                       apply(subset(merge(block(b), block(b), by=c()),
                                    x.x < x.y | (x.x == x.y & y.x < y.y)),
                             1,
                             function(d) paste("not(", ands(c(cell(d[1], d[2], v),
                                                              cell(d[3], d[4], v))), ")", sep="")))
        )
    )
}


sudoku  <- c(
    ands(as.vector(apply(expand.grid(1:9, 1:9), 1, function (e) value.of.cell(e[1], e[2])))),
    ands(as.vector(sapply(1:9, function (n) value.of.row(n)))),
    ands(as.vector(sapply(1:9, function (n) value.of.col(n)))),
    ands(as.vector(sapply(1:9, function (n) value.of.block(n))))
)


## Block of cell in row x column y
block.of  <- function(x, y)
{
    x2  <- ((x-1) %/% 3)
    y2  <- ((y-1) %/% 3)
    1 + 3*x2 + y2
}


cells  <- expand.grid(row=1:9, col=1:9)
cells  <- merge(cells, list(val=1:9), by=c())
cells$var  <- 1:nrow(cells)
cells$block  <- block.of(cells$row, cells$col)


## each cell has a value between 1 and 9
has.one.value  <- list()
for (x in 1:9)  # for each row x
    for (y in 1:9) { # for each column y
        # constraint x.y.1 OR x.y.2 OR ... OR x.y.9
        has.one.value  <- c(has.one.value, list(subset(cells, row==x & col==y)$var))
    }

## the value of each cell is uniquely defined
unique.value  <- list()
for (x in 1:9) # for each row x
    for (y in 1:9) { # for each column y
        # all pairs x.y.v1, x.y.v2
        pairs  <- combn(subset(cells, row==x & col==y)$var, 2)
        # constraint not(x.y.v1 AND x.y.v2), that is not(x.y.v1) OR not(x.y.v2)
        unique.value  <- c(unique.value, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.value)  <- NULL
    }

## each value appears in each row once
unique.rows  <- list()
for (x in 1:9)  # for each row x
    for (v in 1:9) {  # for each value v
        # all pairs x.y1.v, x.y2.v
        pairs  <- combn(subset(cells, row==x & val==v)$var, 2)
        # constraint not(x.y1.v AND x.y2.v), that is not(x.y1.v) OR not(x.y2.v)
        unique.rows  <- c(unique.rows, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.rows)  <- NULL
    }

## each value appears in each column once
unique.cols  <- list()
for (y in 1:9)  # for each column y
    for (v in 1:9) {  # for each value v
        # all pairs x1.y.v, x2.y.v
        pairs  <- combn(subset(cells, col==x & val==v)$var, 2)
        # constraint not(x1.y.v AND x2.y.v), that is not(x1.y.v) OR not(x2.y.v)
        unique.cols  <- c(unique.cols, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.cols)  <- NULL
    }

## each value appears in each block once
unique.blocks  <- list()
for (b in 1:9)  # for each block y
    for (v in 1:9) {  # for each value v
        # all pairs of cells in the block
        pairs  <- combn(subset(cells, block==b & val==v)$var, 2)
        # constraint
        unique.blocks  <- c(unique.blocks, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.blocks)  <- NULL
    }

sudoku.constraints  <- c(has.one.value, unique.value, unique.rows, unique.cols, unique.blocks)
length(sudoku.constraints)

print.solution  <- function(sat.sol, cells)
{
    df  <- as.data.frame(sat.sol)
    df  <- subset(df, value) ## keep only variables that are true
    mdf  <- merge(df, cells, by.x="variable", by.y="var")
    tab  <- data.frame(matrix(nrow=0, ncol=9))
    for (x in 1:9) {
        cur.row  <- c()
        for (y in 1:9)
            cur.row  <- c(cur.row, mdf[mdf$row == x & mdf$col == y, ]$val)
        tab  <- rbind(tab, cur.row)
    }
    names(tab)  <- paste("C", 1:9, sep="")
    tab
}

## Constraint that cell at row x column y must have value v
constraint  <- function(x, y, v, cells)
{
    var  <- subset(cells, row==x & col==y & val==v)$var
    var
}

1, 1, 5
1, 2, 3
1, 5, 7
2, 1, 6
2, 4, 1
2, 5, 9
2, 6, 5
3, 2, 9
3, 3, 8
3, 8, 6
4, 1, 8
4, 5, 6
4, 9, 3
5, 1, 4
5, 4, 8
5, 6, 3
5, 9, 1
6, 1, 7
6, 5, 2
6, 9, 6
7, 2, 6
7, 7, 2
7, 8, 8
8, 4, 4
8, 5, 1
8, 6, 9
8, 9, 5
9, 5, 8
9, 8, 7
9, 9, 9
