## here we assuming that the user has only one x-variabe in his fitted 
## model and that this x-variable may have been transformed into a power
## x^power before fitting
## so for predictive values we need the new x values (xvalues) 
## the original name of the x variable (xname)
## and possible its power (power).
## Mikis Stasinopoulos Monday, August 2, 2004
centiles.pred <- function(obj,
                          type = c("centiles", "z-scores", "standard-centiles"),
                          xname = NULL,
                          xvalues = NULL,
                          power = NULL,
                          yval = NULL,
                          cent = c(.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                          dev = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
                          calibration = FALSE,
                          plot = FALSE,
                          legend = TRUE,
                          ylim = NULL,
                          xlim = NULL,
                          re = NULL,
                          ...)
{
  ## ------function calc.cent-----------------------------------------------------
  # Huiqi
  calc.cent <- function(xvar, cent) {
    o <- order(xvar)
    mat <- xvar[o]
    cent <- cent
    for (var in cent) {
      if (lpar == 1) {
        newcall <- call(qfun, var / 100, mu = mu[o])
      }
      else if (lpar == 2) {
        newcall <- call(qfun, var / 100, mu = mu[o], sigma = sigma[o])
      }
      else if (lpar == 3) {
        newcall <- call(qfun,
                        var / 100,
                        mu = mu[o],
                        sigma = sigma[o],
                        nu = nu[o])
      }
      else {
        newcall <- call(
          qfun,
          var / 100,
          mu = mu[o],
          sigma = sigma[o],
          nu = nu[o],
          tau = tau[o]
        )
      }
      ll <- eval(newcall)
      mat <- cbind(mat, ll)
    }
    mat <- as.data.frame(mat)
    nnn <- paste("C", as.character(cent), sep = "")
    names(mat) <- c(xname, nnn)
    return(mat)
  }
  plot.mat <- function(mat, cent, legend , ...)
  {
    lcent <- dim(mat)[2]
    ylim <- if (is.null(ylim))
      c(min(mat[, 2:lcent]), max(mat[, 2:lcent]))
    else
      ylim
    xlim <- if (is.null(xlim))
      NULL
    else
      xlim
    xleg <-  min(mat[, 1], xlim)
    yleg <- max(mat[, 2:lcent], ylim)
    plot(mat[, 1],
         mat[, 2],
         ,
         type = "n",
         ylim = ylim,
         xlim = xlim,
         ...)
    for (i in 2:lcent)
      lines(mat[, 1], mat[, i], col = i)
    if (legend)
      legend(
        list(x = xleg, y = yleg),
        legend = cent,
        col = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
        lty = 1,
        ncol = 1,
        bg = "white"
      )#
    invisible()
  }
  ##-----------------------------------------------------
  ## the main function start here
  ##  checking the object
  if (!is.gamlss(obj))
    stop(paste("This is not an gamlss object", "\n", ""))
  ## checking the xvalues
  if (is.null(xvalues))
    stop(paste("The xvalues  argument is not specified", "\n", ""))
  ## checking the xname
  if (is.null(xname))
    stop(paste("The xname argument is not specified", "\n", ""))
  if (!is.character(xname))
    stop(paste("The xname argument is not a character", "\n", ""))
  ## checking for continuous family
  #if(!obj$type=="Continuous")
  #   stop(paste("The centiles are working only with continuous distributions", "\n", ""))
  ## if power
  xvar <- if (!is.null(power)) {
    xvar <-  xvalues^power
  } else {
    xvalues
  }
  ## create a data frame
  newx <- data.frame(xvar)
  colnames(newx) <- xname
  for (r in re) {
    newx[r] = NA
  }
  lpar <- length(obj$parameters)
  ## the problem here is that if any parameters is fixed the predict will not work
  ## here is the fix MS Wednesday, June 28, 2006
  if ("mu" %in% obj$parameters)
  {
    if (is.null(obj$mu.fix))
      mu <- predict(obj,
                    what = "mu",
                    newdata = newx,
                    type = "response",
                    ...)
    else if (obj$mu.fix == TRUE)
      mu <- rep(fitted(obj, "mu")[1], length(xvar))
  }
  if ("sigma" %in% obj$parameters)
  {
    if (is.null(obj$sigma.fix))
      sigma <- predict(obj,
                       what = "sigma",
                       newdata = newx,
                       type = "response",
                       ...)
    else if (obj$sigma.fix == TRUE)
      sigma <- rep(fitted(obj, "sigma")[1], length(xvar))
  }
  if ("nu" %in% obj$parameters)
  {
    if (is.null(obj$nu.fix))
      nu <- predict(obj,
                    what = "nu",
                    newdata = newx,
                    type = "response",
                    ...)
    else if (obj$nu.fix == TRUE)
      nu <- rep(fitted(obj, "nu")[1], length(xvar))
  }
  if ("tau" %in% obj$parameters)
  {
    if (is.null(obj$tau.fix))
      tau <- predict(obj,
                     what = "tau",
                     newdata = newx,
                     type = "response",
                     ...)
    else if (obj$tau.fix == TRUE)
      tau <- rep(fitted(obj, "tau")[1], length(xvar))
  }
  ## which type
  type <- match.arg(type)
  ## only for type centiles
  if (type == "centiles")
  {
    fname <- obj$family[1]
    qfun <- paste("q", fname, sep = "")
    #  Title <- paste("Centile curves using",fname, sep=" ")
    xvar <- xvalues
    if (calibration)
    {
      z   <-  quantile(resid(obj), probs = cent / 100)
      p   <-  pNO(z, mu = 0, sigma = 1)
      cent <- round(100 * p, digits = 2)
    }
    mat <- calc.cent(xvar = xvar, cent = cent)
    #  cc <- centiles(obj, save=TRUE, plot=FALSE)
    #if (calibration)  colnames(mat) <- c("x", paste(as.character(format(cc[,2], digits=2)), names(cent), sep="|"))
    colnames(mat) <- c("x", as.character(cent))
    # paste(as.character(format(cc[,2], digits=2)), names(cent), sep="|")
    if (plot)
      plot.mat(mat, cent, legend, ...)
    return(mat)
  }
  ##   only for type z-scores
  if (type == "z-scores")
  {
    if (calibration)
      stop("calibration is not implemeted yet in z-scores")
    if (is.null(yval))
      stop("the y values should be set if type=z-scores is used")
    if (length(yval) != length(xvalues))
      stop("length of xvalues and yval is not the same")
    fname <- obj$family[1]
    qfun <- paste("p", fname, sep = "")
    if (lpar == 1)
    {
      newcall <- call(qfun, yval, mu = mu)
    }
    else if (lpar == 2)
    {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma)
    }
    else if (lpar == 3)
    {
      newcall <- call(qfun,
                      yval,
                      mu = mu,
                      sigma = sigma,
                      nu = nu)
    }
    else
    {
      newcall <- call(
        qfun,
        yval,
        mu = mu,
        sigma = sigma,
        nu = nu,
        tau = tau
      )
    }
    cdf <- eval(newcall)
    rqres <- qnorm(cdf)
    return(rqres)
  }
  ## only for type standard-centiles
  if (type == "standard-centiles")
  {
    cent <- pnorm(dev) * 100
    fname <- obj$family[1]
    qfun <- paste("q", fname, sep = "")
    #  Title <- paste("Centile curves using",fname, sep=" ")
    xvar <- xvalues
    mat <- calc.cent(xvar = xvar, cent = cent)
    nnn <- paste(as.character(dev), sep = "")
    names(mat) <- c(xname, nnn)
    if (plot)
      plot.mat(mat, dev, legend, ...)
    return(mat)
  }
  
} 
