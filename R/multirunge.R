multirunge <- function (y0, tmin, tmax, dt, param, sys) 
{
    y <- y0
    u <- sys()
    nfonct <- length(u)
    k1 <- k2 <- k3 <- k4 <- rep(nfonct, 0)
    t <- tmin
    while (tmax - t > dt) {
        for (index in 1:nfonct) {
            k1[index] <- dt * u[[index]](t, y, param)
            k2[index] <- dt * u[[index]]((t + dt/2), (y + k1[index]/2), 
                param)
            k3[index] <- dt * u[[index]]((t + dt/2), (y + k2[index]/2), 
                param)
            k4[index] <- dt * u[[index]]((t + dt), (y + k3[index]), 
                param)
        }
        t <- t + dt
        y <- y + (k1 + 2 * k2 + 2 * k3 + k4)/6
    }
    y
}
