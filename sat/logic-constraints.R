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


cells  <- expand.grid(row=1:9, col=1:9)
cells  <- merge(cells, list(val=1:9), by=c())
cells$var  <- 1:nrow(cells)

## Block of cell in row x column y
block.of  <- function(x, y)
{
    x2  <- ((x-1) %/% 3)
    y2  <- ((y-1) %/% 3)
    1 + 3*x2 + y2
}
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
        pairs  <- combn(subset(cells, col==y & val==v)$var, 2)
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
sol  <- picosat_sat(sudoku.constraints)

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

print.solution(sol, cells)


## Constraint that cell at row x column y must have value v
constraint  <- function(x, y, v, cells)
{
    var  <- subset(cells, row==x & col==y & val==v)$var
    var
}

hints  <- list(constraint(1, 1, 5, cells),
               constraint(1, 2, 3, cells),
               constraint(1, 5, 7, cells),
               constraint(2, 1, 6, cells),
               constraint(2, 4, 1, cells),
               constraint(2, 5, 9, cells),
               constraint(2, 6, 5, cells),
               constraint(3, 2, 9, cells),
               constraint(3, 3, 8, cells),
               constraint(3, 8, 6, cells),
               constraint(4, 1, 8, cells),
               constraint(4, 5, 6, cells),
               constraint(4, 9, 3, cells),
               constraint(5, 1, 4, cells),
               constraint(5, 4, 8, cells),
               constraint(5, 6, 3, cells),
               constraint(5, 9, 1, cells),
               constraint(6, 1, 7, cells),
               constraint(6, 5, 2, cells),
               constraint(6, 9, 6, cells),
               constraint(7, 2, 6, cells),
               constraint(7, 7, 2, cells),
               constraint(7, 8, 8, cells),
               constraint(8, 4, 4, cells),
               constraint(8, 5, 1, cells),
               constraint(8, 6, 9, cells),
               constraint(8, 9, 5, cells),
               constraint(9, 5, 8, cells),
               constraint(9, 8, 7, cells),
               constraint(9, 9, 9, cells))

sol2  <- picosat_sat(c(sudoku.constraints, hints))
print.solution(sol2, cells)
