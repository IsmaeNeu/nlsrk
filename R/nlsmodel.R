nlsModel <- function( form, data, start ) {
    thisEnv <- environment()
    env <- new.env(parent=environment(form))
    for( i in names( data ) ) {
        assign( i, data[[i]], envir = env )
    }
    ind <- as.list( start )
    parLength <- 0
    for( i in names( ind ) ) {
        temp <- start[[ i ]]
        storage.mode( temp ) <- "double"
        assign( i, temp, envir = env )
        ind[[ i ]] <- parLength + seq( along = start[[ i ]] )
        parLength <- parLength + length( start[[ i ]] )
    }
    useParams <- rep(TRUE, parLength)
    lhs <- eval( form[[2]], envir = env )
    rhs <- eval( form[[3]], envir = env )
    resid <- lhs - rhs
    dev <- sum( resid^2 )
    if( is.null( attr( rhs, "gradient" ) ) ) {
        getRHS.noVarying <- function()
          numericDeriv(form[[3]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function()
          eval( form[[3]], envir = env )
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0) {
        gradSetArgs <- vector("list", marg+1)
        for(i in 2:marg)
          gradSetArgs[[i]] <- rep(TRUE, dimGrad[i-1])
        useParams <- rep(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    npar <- length(useParams)
    gradSetArgs[[1]] <- (~attr(ans, "gradient"))[[2]]
    gradCall <-
      switch(length(gradSetArgs) - 1,
             call("[", gradSetArgs[[1]], gradSetArgs[[2]]),
             call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]]),
             call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                  gradSetArgs[[3]]),
             call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                  gradSetArgs[[3]], gradSetArgs[[4]]))
    getRHS.varying <- function()
      {
          ans <- getRHS.noVarying()
          attr(ans, "gradient") <- eval(gradCall)
          ans
      }
    QR <- qr( attr( rhs, "gradient" ) )
    qrDim <- min( dim( QR$qr ) )
    if( QR$rank < qrDim)
      stop("singular gradient matrix at initial parameter estimates")

    getPars.noVarying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )
    getPars.varying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )[useParams]
    getPars <- getPars.noVarying

    internalPars <- getPars()
    setPars.noVarying <- function(newPars)
      {
          assign("internalPars", newPars, envir = thisEnv)
          for( i in names( ind ) ) {
              assign( i, unname(newPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars.varying <- function(newPars)
      {
          internalPars[useParams] <- newPars
          for( i in names( ind ) ) {
              assign( i, unname(internalPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars <- setPars.noVarying

    on.exit(remove(i, data, parLength, start, temp, m))
    m <-
      list(resid = function() resid,
           fitted = function() rhs,
           formula = function() form,
           deviance = function() dev,
           gradient = function() attr( rhs, "gradient" ),
           conv = function()
           {
               rr <- qr.qty( QR, resid ) # rotated residual vector
               sqrt( sum( rr[1:npar]^2 ) / sum( rr[ -(1:npar) ]^2 ) )
           },
           incr = function() qr.coef( QR, resid ),
           setVarying = function(vary = rep(TRUE, length(useParams)))
           {
               assign("useParams", if(is.character(vary)) {
                   temp <- logical(length(useParams))
                   temp[unlist(ind[vary])] <- TRUE
                   temp
               } else if(is.logical(vary) && length(vary) != length(useParams))
                      stop("setVarying : vary length must match length of parameters")
               else {
                   vary
               }, envir = thisEnv)
               gradCall[[length(gradCall)]] <<- useParams
               if(all(useParams)) {
                   assign("setPars", setPars.noVarying, envir = thisEnv)
                   assign("getPars", getPars.noVarying, envir = thisEnv)
                   assign("getRHS", getRHS.noVarying, envir = thisEnv)
                   assign("npar", length(useParams), envir = thisEnv)
               } else {
                   assign("setPars", setPars.varying, envir = thisEnv)
                   assign("getPars", getPars.varying, envir = thisEnv)
                   assign("getRHS", getRHS.varying, envir = thisEnv)
                   assign("npar", length((1:length(useParams))[useParams]),
                          envir = thisEnv)
               }
           },
           setPars = function(newPars)
           {
               setPars(newPars)
               assign("resid",
                      lhs - assign("rhs", getRHS(), envir = thisEnv),
                      envir = thisEnv)
               assign("dev", sum( resid^2), envir = thisEnv)
               assign("QR", qr( attr( rhs, "gradient")), envir = thisEnv )
               return(QR$rank < min(dim(QR$qr)))  # to catch the singular gradient matrix
           },
           getPars = function() getPars(),
           getAllPars = function() getPars(),
           getEnv = function() env,
           trace = function() cat( format(dev),": ", format( getPars() ), "\n"),
           Rmat = function() qr.R( QR ),
           predict = function(newdata = list(), qr = FALSE)
           {
               eval(form[[3]], as.list(newdata), env)
           })
    class(m) <- "nlsModel"
    m
}


