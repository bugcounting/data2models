#!  /usr/bin/env Rscript

library(rpicosat)


### Simple example of SAT formula and truth table

simple  <- "(A ==> B) /\ (B ==> C) /\ (C ==> A)"
tf <- c(TRUE, FALSE)
table  <- expand.grid(A=tf, B=tf, C=tf)
table$formula  <- with(table, (!A | B) & (!B | C) & (!C | A))


### A more interesting example.
##
## Three friends, Andy, Beverly, and Charlie, disagree about the
## number `n` of paintings that a common friend owns:
##   - Andy thinks they are at least four: n >= 4
##   - Beverly thinks they are less than four: n < 4
##   - Charlie thinks they are at least one: n >= 1
## We know that exacly one of the three friends is right. How much is n?
##
## At least one is right
one.is.right  <- "A \/ B \/ C"
## Exacly one is right
exactly.one  <- c("A ==> (~B /\ ~C)", # if A is right, B and C are not
                  "B ==> (~C /\ ~A)", # if B is right, C and A are not
                  "C ==> (~A /\ ~B)") # if C is right, A and B are not
## Relations induced by arithmetic properties
numerical.relations  <- c("A ==> (~B /\ C)",
                          "B ==> ~A",
                          "~A ==> B",
                          "~B ==> (A /\ C)",
                          "~C ==> B")
table2  <- expand.grid(A=tf, B=tf, C=tf)
table2$formula  <- with (table2, (A | B | C) &
                                 (!A | (!B & !C)) & (!B | (!C & !A)) & (!C | (!A & !B)) &
                                 (!A | (!B & C)) & (!B | !A) & (A | B) & (B | (A & C)) & (C | B))

## To convert to CNF, let's use de Morgan's law: X | (Y & Z) == (X | Y) & (X | Z)
## A | B | C
## !A | !B
## !A | !C
## !B | !C
## !B | !A
## !C | !A
## !C | !B
## !A | !B
## !A | C
## !B | !A
## A | B
## B | A
## B | C
## C | B
## Mapping A -> 1, B -> 2, C -> 3, and -1 to negation:
friends.constraint  <- list(
    c(1, 2, 3),
    c(-1, -2),
    c(-1, -3),
    c(-2, -3),
    c(-2, -1),
    c(-3, -1),
    c(-3, -2),
    c(-1, -2),
    c(-1, 3),
    c(-2, -1),
    c(1, 2),
    c(2, 1),
    c(2, 3),
    c(3, 2)
)

res  <- picosat_sat(friends.constraint)
as.data.frame(res)


### SUDOKU


## Data frame with enumeration of all possible values of all cells in the grid
cells  <- expand.grid(row=1:9, col=1:9)       ## all possible row/column combinations
cells  <- merge(cells, list(val=1:9), by=c()) ## each cell may take one of 9 possible values
cells$var  <- 1:nrow(cells)  ## unique variable number to each cell/value combination

## We also need to be able to know which of nine 3x3 blocks each cell belongs to.
## Blocks are numbered from the top-left corner, going by row.
## This is consistent with numbering rows and columns from the top-left corner.

## Block of cell in row `x` column `y`
block.of  <- function(x, y)
{
    x2  <- ((x-1) %/% 3)
    y2  <- ((y-1) %/% 3)
    1 + 3*x2 + y2
}
cells$block  <- block.of(cells$row, cells$col)


## Constraint: each cell has a value between 1 and 9
has.one.value  <- list()
for (x in 1:9)  # for each row x
    for (y in 1:9) { # for each column y
        # constraint x.y.1 OR x.y.2 OR ... OR x.y.9
        has.one.value  <- c(has.one.value, list(subset(cells, row==x & col==y)$var))
    }

## Constraint: the value of each cell is uniquely defined
unique.value  <- list()
for (x in 1:9) # for each row x
    for (y in 1:9) { # for each column y
        # all pairs x.y.v1, x.y.v2
        pairs  <- combn(subset(cells, row==x & col==y)$var, 2)
        # constraint not(x.y.v1 AND x.y.v2), that is not(x.y.v1) OR not(x.y.v2)
        unique.value  <- c(unique.value, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.value)  <- NULL
    }

## Constraint: each value appears in each row once
unique.rows  <- list()
for (x in 1:9)  # for each row x
    for (v in 1:9) {  # for each value v
        # all pairs x.y1.v, x.y2.v
        pairs  <- combn(subset(cells, row==x & val==v)$var, 2)
        # constraint not(x.y1.v AND x.y2.v), that is not(x.y1.v) OR not(x.y2.v)
        unique.rows  <- c(unique.rows, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.rows)  <- NULL
    }

## Constraint: each value appears in each column once
unique.cols  <- list()
for (y in 1:9)  # for each column y
    for (v in 1:9) {  # for each value v
        # all pairs x1.y.v, x2.y.v
        pairs  <- combn(subset(cells, col==y & val==v)$var, 2)
        # constraint not(x1.y.v AND x2.y.v), that is not(x1.y.v) OR not(x2.y.v)
        unique.cols  <- c(unique.cols, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.cols)  <- NULL
    }

## Constraint: each value appears in each block once
unique.blocks  <- list()
for (b in 1:9)  # for each block y
    for (v in 1:9) {  # for each value v
        # all pairs of cells in the block
        pairs  <- combn(subset(cells, block==b & val==v)$var, 2)
        # constraint
        unique.blocks  <- c(unique.blocks, split(-1*pairs, rep(1:ncol(pairs), each=nrow(pairs))))
        names(unique.blocks)  <- NULL
    }

## All constraints together
sudoku.constraints  <- c(has.one.value, unique.value, unique.rows, unique.cols, unique.blocks)
length(sudoku.constraints)
sol  <- picosat_sat(sudoku.constraints)

## Display Sudoku grid of `sat.sol` using mapping defined in `cells`
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


## We need to add a way of add "hints", that is values that have been
## already placed on the grid.

## Constraint that cell at row `x` column `y` must have value `v`
constraint  <- function(x, y, v, cells)
{
    ## Lookup the corresponding variable, which is asserted TRUE
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

## Constraint to look for a different solution than `sat.sol`
relax  <- function(sat.sol)
{
    df  <- as.data.frame(sat.sol)
    ## variables that are true in solution
    vars  <- subset(df, value)$variable
    ## constraint for new solution: at least one of the variables is false
    list(-1*vars)
}

sol.try  <- picosat_sat(c(sudoku.constraints, hints, relax(sol2)))
print.solution(sol.try, cells)

## Very hard Sudoku
hints3  <- list(constraint(1, 3, 5, cells),
                constraint(1, 7, 1, cells),
                constraint(2, 2, 6, cells),
                constraint(2, 3, 1, cells),
                constraint(2, 7, 2, cells),
                constraint(3, 4, 3, cells),
                constraint(3, 5, 8, cells),
                constraint(4, 2, 2, cells),
                constraint(4, 9, 4, cells),
                constraint(5, 5, 3, cells),
                constraint(5, 9, 9, cells),
                constraint(6, 2, 1, cells),
                constraint(6, 3, 3, cells),
                constraint(6, 4, 5, cells),
                constraint(6, 9, 2, cells),
                constraint(7, 1, 9, cells),
                constraint(7, 6, 2, cells),
                constraint(7, 8, 4, cells),
                constraint(8, 8, 7, cells),
                constraint(9, 1, 4, cells),
                constraint(9, 5, 5, cells),
                constraint(9, 6, 9, cells),
                constraint(9, 9, 3, cells))
sol3  <- picosat_sat(c(sudoku.constraints, hints3))

## Number of different solutions of Sudoku with `hints`
count.solutions  <- function(hints)
{
    nsol  <- 0
    cur.sol  <- NULL
    new.constraints  <- list()
    while (TRUE) {
        new.c  <- if (is.null(cur.sol)) list() else relax(cur.sol)
        new.constraints  <- c(new.c, new.constraints)
        cur.sol  <- picosat_sat(c(sudoku.constraints, hints, new.constraints))
        if (picosat_solution_status(cur.sol) == "PICOSAT_SATISFIABLE")
            nsol  <- nsol + 1
        else
            break
    }
    nsol
}

count.solutions(hints3[1:23])
count.solutions(hints3[1:22])
count.solutions(hints3[1:21])
