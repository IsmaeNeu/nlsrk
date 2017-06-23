plot.nlsrk = function (x, data, maintitle = "Ordinary Differential Equations system", 
    ...) 
{
    if (!is.data.frame(data)) 
        stop("*** ERROR : data.frame expected ***\n")
    if (!all(names(data) == c("t", "y", "traj"))) 
        stop("*** ERROR : names t,y,traj expected ***\n")
    if (class(x) != "nls") 
        stop("*** ERROR : x of class 'nls' expected ***\n")
    plot(data$t, data$y, pch = 19 + as.numeric(data$traj))
    u <- cbind(data$t, fitted.values(x))
    p <- max(as.numeric(data$traj))
    for (i in 1:p) lines(u[data$traj == i, ], col = i)
}

