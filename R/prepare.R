prepare <- function (x = data.frame, ntime = 1, cols = c(2:(length(x[1, 
    ])))) 
{
    time <- x[, ntime]
    n <- length(time)
    yobs <- NULL
    for (i in cols) {
        yobs <- rbind(yobs, cbind(time, matrix(x[, i]), rep(i - 
            1, n)))
    }
    yobs <- as.data.frame(yobs)
    names(yobs) <- c("t", "y", "traj")
    yobs$traj <- as.factor(yobs$traj)
    yobs
}
