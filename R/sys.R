sys <- function () 
{
    f1 <- function(t, y, param) -param[1] * y[1]
    f2 <- function(t, y, param) param[1] * y[1] - param[2] * 
        y[2]
    c(f1, f2)
}
