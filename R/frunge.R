frunge <- function (t, param, y0, Dfdt, dt = 0.01, graph = FALSE) 
{
    n <- length(t)
    if (!prod(sort(t) == t)) 
        stop("*** Error *** : Times are not strictly increasing. Please sort the time vector\n")
    if (!(min(diff(t)) > dt)) 
        stop("*** Error *** : All time intervals must be greater then dt\n")
    y <- rep(0, n)
    y[1] <- y0
    Y <- y0
    T <- t[1]
    range <- t[n] - t[1]
    rpas <- range/dt
    npas <- floor(rpas)
    j <- 1
    for (i in 1:npas) {
        k1 <- dt * Dfdt(T, Y, param)
        k2 <- dt * Dfdt(T + dt/2, Y + k1/2, param)
        k3 <- dt * Dfdt(T + dt/2, Y + k2/2, param)
        k4 <- dt * Dfdt(T + dt, Y + k3, param)
        Y <- Y + (k1 + 2 * k2 + 2 * k3 + k4)/6
        T <- T + dt
        if (j + 1 <= n) 
            if (abs(t[j + 1] - T) < dt) {
                j <- j + 1
                y[j] <- Y
            }
            else if (abs(t[j] - T) < dt) 
                y[j] <- Y
    }
    if (graph) {
        plot(t, y, type = "l", xlab = "time", ylab = "integral")
        abline(0, 0)
        abline(v = 0)
        title("Integral curve of dy/dt=Dfdt(t,y,param)")
    }
    y
}
