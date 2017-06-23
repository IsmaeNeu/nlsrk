kmk <- function () 
########################################################
# Systeme SIR de Kermac-McKendrick                     #
########################################################

{
    f1 <- function(t, y, param) -param[1] * y[1] * y[2]
    f2 <- function(t, y, param) param[1] * y[1] * y[2] - param[2] * y[2]
    f3 <- function(t, y, param) param[2] * y[2]
    c(f1, f2, f3)
}
