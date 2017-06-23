nlscontour=function (x, param1 = 1, param2 = 2, range1 = NULL, range2 = NULL, 
    npoints = 100, filled = FALSE,colored=FALSE) 
{
    pars <- x$m$getPars()
    cat("Parameters inherited from nls :\n")
    print(pars)
    A <- summary(x)
    stderrpars <- A$parameters[, 2]
    cat("Standard error on parameters :\n")
    print(stderrpars)
    if (is.null(range1)) 
        range1 <- c(pars[param1] - 4 * stderrpars[param1], pars[param1] + 
            4 * stderrpars[param1])
    if (is.null(range2)) 
        range2 <- c(pars[param2] - 4 * stderrpars[param2], pars[param2] + 
            4 * stderrpars[param2])
    cat("parameter 1 - name : ", names(stderrpars[param1]), "central value : ", 
        pars[param1], " range : ", range1, " \n")
    cat("parameter 2 - name : ", names(stderrpars[param2]), "central value : ", 
        pars[param2], " range : ", range2, " \n")
    np <- round(sqrt(npoints))
    dx <- (range1[2] - range1[1])/(np - 1)
    dy <- (range2[2] - range2[1])/(np - 1)
    ax <- seq(range1[1], range1[2], dx)
    by <- seq(range2[1], range2[2], dy)
    grid <- matrix(0, np, np)
    valpar <- pars
    for (i in 1:np) for (j in 1:np) {
        valpar[param1] <- ax[i]
        valpar[param2] <- by[j]
        nlm <- nlsModel(formula(x), data = eval(x$data), start = valpar)
        az <- sum(nlm$resid()^2)
        grid[i, j] <- az
    }
    paletta <- colors()[seq(153, 254, 10)]
    if (filled) {
	if(colored) filled.contour(ax, by, as.matrix(grid),col = rainbow(20), nlevels = 20, 
	plot.axes={axis(1);axis(2);points(pars[param1],pars[param2],pch=20);abline(pars[param2],0);abline(v=pars[param1])}) 
	else
	filled.contour(ax, by, as.matrix(grid), col = paletta, nlevels = 20,
	plot.axes={axis(1);axis(2);points(pars[param1],pars[param2],pch=20,col="white");abline(pars[param2],0);abline(v=pars[param1])})
    }
    else {
        par(pty = "m")
        contour(ax, by, grid, nlevels = 25)
        abline(v = pars[param1])
        abline(pars[param2], 0)
    }
    title(paste("Sum of squares - parameters :", as.character(names(valpar[param1])), 
        ",", as.character(names(valpar[param2]))))
    gr <- NULL
    gr$x <- ax
    gr$y <- by
    gr$grid <- as.data.frame(grid)
    class(gr) <- "nlsgrid"
    invisible(gr)
}
