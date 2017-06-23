evrunge<-function (t, param, y0, sys, dt = 0.01, graph = FALSE,observable=rep(1,length(y0))) 
{
    n <- length(unique(t))
    if (!prod(sort(t[1:n]) == t[1:n])) 
        stop("*** ERROR *** : times not in increasing order. Please sort time vector svp\n")
    if (!(min(diff(t[1:n]) > dt))) 
        stop("*** ERROR *** : All time intervals must be greater then dt \n")
    v<-as.numeric(sort(levels(factor(observable))))
    if(!any(v==c(0,1) || v==c(0) || v==c(1) )) stop ("*** ERROR *** : observable vector must be binary (0 or 1) \n")
    if(all(v==c(0))) stop ("*** ERROR *** : At least one observable solution must be indicated \n")
    t <- unique(t)
    npas <- length(t) - 1
    Y <- y0
    nfonct <- length(Y)
    nfobs<-sum(observable)
    z <- NULL
    z <- rbind(z, as.vector(Y))
    for (i in 1:npas) {
        u <- Y
        tmin <- t[i]
        tmax <- t[i + 1]
        Y <- multirunge(y0 = u, tmin, tmax, dt, param, sys)
        z <- rbind(z, as.vector(Y))
    }
    res <- as.data.frame(cbind(t, z[,observable==1]))
    namecol <- NULL
    for (j in 1:nfobs) namecol <- c(namecol, paste("f", as.character(j), 
        sep = ""))
    namecol <- c("time", namecol)
    names(res) <- namecol
    if (graph) {
        plot(res$time, res$f1, type = "l", ylim = c(min(res[, 2:(nfobs + 
            1)]), max(res[, 2:(nfobs + 1)])))
        if(nfobs>1)for (j in 2:(nfobs+1)) lines(res$time, res[, j], lty = j)
        abline(0, 0)
        abline(v = 0)
        title("Ordinary Differential Equations system")
    }
    yhat <- NULL
    for (j in 2:(nfobs+1)) {
        yhat <- rbind(yhat, matrix(res[, j]))
    }
    invisible(yhat)
}
