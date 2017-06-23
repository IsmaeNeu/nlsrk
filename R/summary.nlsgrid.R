summary.nlsgrid <- function (object, ...) 
{
    if (class(object) != "nlsgrid") 
        stop("Error : object x is not of class nlsgrid\n")
    n <- length(object$x)
    xstep <- diff(object$x)[1]
    ystep <- diff(object$y)[1]
    cat(" Grid dimensions :", n, " x ", n, "\n\n")
    cat(" x axis characteristics : \n")
    cat(" ------------------------ \n\n")
    cat("Min : ", min(object$x), "Max : ", max(object$x), "step :", xstep, 
        "\n\n")
    cat(" y axis characteristics : \n")
    cat(" ------------------------ \n\n")
    cat("Min : ", min(object$y), "Max : ", max(object$y), "step :", ystep, 
        "\n\n")
    cat(" Grid summary \n")
    cat(" ------------ \n\n")
    print(summary(as.vector(stack(object$grid)[1])))
}
