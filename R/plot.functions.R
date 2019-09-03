
# Main plot ---------------------------------------------------------------------------------------------

##' @title Plot method for Benford Analysis
##' @description The \code{plot} method for "Benford" objects.
##' 
##' @param  x a "Benford" object
##' @param select it specifies the order and which plots are going to be plotted. If NULL, the parameter except is used.
##' @param except it specifies which plots are not going to be plotted. If NULL, the parameter select is used.
##' Currently, you can choose from 10 plots: "digits", "rootogram digits", "second order", "rootogram second order", "last two digits", "summation",
##' "mantissa", "chi square", "abs diff", "ex summation". If you want to plot all, just
##' put except = "none". The default is not to plot the "mantissa" and "abs diff". If you want to plot all, just
##' put except = "all"
##' @param multiple logical; if TRUE, all plots are grouped in the same window.
##' @param mfrow A vector of the form c(`nr`, `nc`). Subsequent figures will be drawn in an `nr`-by-`nc` array on the device by columns (`mfcol`), or rows (`mfrow`), respectively. For more details see `?par()`.
##' @param col.bar a color to be used to fill the bars. The default is lightblue.
##' @param err.bounds logical; if TRUE, the upper and lower error bounds are draw. The error bounds indicate the binomial root mean square error.
##' @param alpha it specifies level of confidence interval. The defaults to 95 percent confidence interval,i.e., the error bounds will represent 1.96 standard error from the expected count by Benford's Law.
##' @param grid logical; if TRUE, adds an rectangular grid to plot.
##' @param exp.benford logical; if TRUE, adds a line representing the expected frequencies by Berford's Law.
##' @param freq logical; if TRUE, the plot is a representation of counts; if FALSE, relative frequencies are plotted (so that the plot has a total area of one). Defaults to `TRUE`.
##' @param ... arguments to be passed to generic plot functions,
##' @return Plots the Benford object.
##' @details If both \code{select} and \code{except} arguments have been provided, but only \code{select} will be considered and \code{except} ignored.
##' @export
##' @importFrom graphics abline axis barplot legend lines par plot rect points arrows layout plot.new
##' @importFrom stats pchisq var
##' @importFrom utils head
##' @importFrom stats setNames qnorm


plot.Benford <- function(x, 
                         select = c("digits", "obs vs exp", "chi squared", "summation"), 
                         except = NULL, 
                         multiple = TRUE,  
                         col.bar = "lightblue", 
                         err.bounds = FALSE, 
                         alpha = 0.05, 
                         grid = TRUE,
                         mfrow = NULL,
                         exp.benford = TRUE,
                         freq = TRUE, ...){
  
  
  if (class(x) != "Benford") stop("Class(x) must be 'Benford'")
  
  if(!(alpha > 0 & alpha < 1)) stop(paste0(alpha, " is not a valid value for 'alpha' parameter"))
  
  available.plots <- c("digits", "rootogram digits", "second order", "rootogram second order", "summation", "mantissa", "chi squared", "ex summation", "abs diff", "obs vs exp", "last two digits")
  
  if (!is.null(select)) {
    check.plot.names(select, c(available.plots, "all"))
    select <- tolower(select)
    if (all(select == "all")) plots <- available.plots
    else plots <- select
    
    if (!is.null(except)) {
      warning("the argument 'except' was ignored. See ?plot.Benford for more datails.")
    }
    
  }else{
    
    if (!is.null(except)) {
      check.plot.names(except, c(available.plots, "none"))
      except <- tolower(except)
      if (all(except == "none")) {
        plots <- available.plots
      }else{
        ap <- available.plots
        plots <- ap[!(ap %in% except)]
      }
    }else{
      stop("the 'select' and 'except' arguments must not be equal to NULL at the same time")
    }
  }  
  
  nGraphics <- length(plots)
  
  if (multiple) {
    old.par <- par(no.readonly = TRUE)
    #on.exit(par(old.par))
    
    if(!is.null(mfrow)){
      rows <- mfrow[1] 
      cols <- mfrow[2]
      par(mfrow = c(rows, cols))
    }else{
        rows <- 2 
        cols <- 2
        par(mfrow = c(rows, cols))
    }
    
    nslots <- rows*cols
    plot_this <- plots
    lg_size <- ifelse(rows > 1, 1, 0.7)/rows
    
    for (i in 1:length(plot_this)) {
      plot.switch(plot_this[i], x, col.bar, grid, err.bounds, alpha, exp.benford, freq)
      if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
        legend.Berford(x, err.bounds, lg_size)
      }
    }
    
  }else{
    old.par <- par(no.readonly = TRUE)
    #on.exit(par(old.par))
    plot_this <- plots
    lg_size <- 0.7
    
    for (i in 1:length(plot_this)) {
      plot.switch(plot_this[i], x, col.bar, grid, err.bounds, alpha, exp.benford, freq)
      if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
        legend.Berford(x, err.bounds, lg_size)
      }
    }
  }
}

# Separate plots --------------------------------------------------------------------------------------

barplot.Benford <- function(x,
                              obs.freq = "digits",
                              main = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE,
                              freq = TRUE, ...){
  
  digits <- x[["bfd"]]$digits
  obs.freq <- tolower(obs.freq)
  
  if(freq){
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist.freq
             exp_freq <- x[["bfd"]]$benford.dist.freq
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist.freq
             exp_freq <- x[["bfd"]]$benford.so.dist.freq
           },
           "summation" = {
             obs_freq <- x[["bfd"]]$data.summation
             exp_freq <- rep(mean(obs_freq), length(digits))
           },
           "last two digits" = {
             digits <- 0:99
             obs_freq <- x$last.two.digits$data.dist.freq
             exp_freq <- rep(mean(obs_freq), length(digits))
           }
    )
  }else{
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist
             exp_freq <- x[["bfd"]]$benford.dist
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist
             exp_freq <- x[["bfd"]]$benford.so.dist.freq/sum(x[["bfd"]]$benford.so.dist.freq)
           },
           "summation" = {
             obs_freq <- x[["bfd"]]$data.summation
             exp_freq <- rep(mean(obs_freq), length(digits))
           },
           "last two digits" = {
             digits <- 0:99
             obs_freq <- x$last.two.digits$data.dist.freq/sum(x$last.two.digits$data.dist.freq)
             exp_freq <- rep(mean(obs_freq), length(digits))
           }
    )
  }
  
  if((err.bounds & !(obs.freq %in% c("summation", "second order")))){
    bounds <- compute.error.bounds(exp_freq, x$info$n, alpha, rootogram = FALSE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(0, max(c(obs_freq, exp_freq, ub))*1.1)
  }else{
    ylim <- c(0, max(c(obs_freq, exp_freq))*1.1)
  }
  
  if(is.null(xlab)){
    xlab <- c("First Digit", "First-Two Digits", "First-Three Digits", "First-Order Digits")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  }
  
  if(is.null(ylab)){
    ylab <- "Frequency"
  }
  
  xmarks <- seq(0.7, length(exp_freq)*1.2, 1.2)
  plot(xmarks, obs_freq,
       main = main,
       xlab = xlab, 
       ylab = ylab,
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = ylim,
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if(grid) {
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks,  labels = digits)
       }
  )
  
  barplot(obs_freq, col = col.bar, yaxt = "n", add = T)
  
  if(exp.benford){
    lines(xmarks, exp_freq, col = "red", lwd = 2)
  }
  if((err.bounds & !(obs.freq %in% c("summation", "second order")))){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
  }
  
  out <- list()
  if((err.bounds & !(obs.freq %in% c("summation", "second order")))){
    out$data <- data.frame(digits, obs_freq, exp_freq, bounds) 
  }else{
    out$data <- data.frame(digits, obs_freq, exp_freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


rootogram.Benford <- function(x,
                              obs.freq = "digits",
                              main = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE,
                              freq = TRUE, ...){
  digits <- x[["bfd"]]$digits
  obs.freq <- tolower(obs.freq)
  
  if(freq){
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist.freq
             exp_freq <- x[["bfd"]]$benford.dist.freq
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist.freq
             exp_freq <- x[["bfd"]]$benford.so.dist.freq
           }
    )
  }else{
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist
             exp_freq <- x[["bfd"]]$benford.dist
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist
             exp_freq <- x[["bfd"]]$benford.so.dist.freq/sum(x[["bfd"]]$benford.so.dist.freq)
           }
    )
  }
  
  if((err.bounds & !(obs.freq %in% "second order"))){
    bounds <- compute.error.bounds(exp_freq, x$info$n, alpha, rootogram = TRUE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(min(exp_freq - obs_freq, lb)*1.1, max(abs(exp_freq - obs_freq)*0.5, exp_freq, ub)*1.1)
  }else{
    ylim <- c(min(exp_freq - obs_freq)*1.1, max(abs(exp_freq - obs_freq)*0.5, exp_freq)*1.1)
  }
  
  if (is.null(xlab)) {
    xlab.options <- c("First Digit", "First-Two Digits", "First-Three Digits", "First-Order Digits")
    lab.picker <- ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)
    xlab <- xlab.options[lab.picker]
  }
  
  if (is.null(ylab)) {
    ylab <- "Frequency"
  }
  
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, obs_freq,
       main = main,
       xlab = xlab, ylab = ylab,
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = ylim,
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if (grid) {
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks,  labels = digits)
       }
  )
  rect(xleft = xmarks - 0.5, xright = xmarks + 0.5,
       ybottom = exp_freq, ytop = exp_freq - obs_freq,
       col = col.bar)
  abline(h = 0)
  if(exp.benford){
    lines(xmarks, exp_freq, col = "red", lwd = 2)
  }
  if((err.bounds & !(obs.freq %in% "second order"))){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    abline(h = 0, col = 'red')
  }
  out <- list()
  if((err.bounds & !(obs.freq %in% "second order"))){
    out$data <- data.frame(digits, obs_freq, exp_freq, bounds) 
  }else{
    out$data <- data.frame(digits, obs_freq, exp_freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


needle.Benford <- function(x,
                               discrepancy = "abs diff",
                               main = NULL,
                               xlab = NULL,
                               ylab = NULL,
                               grid = TRUE,
                               col = "blue", ...){
  
  digits <- c(NA, x[["bfd"]]$digits, NA)
  discrepancy <- tolower(discrepancy)
  
  disc <- switch(discrepancy,
                 "chi squared" = c(NA, x[["bfd"]]$squared.diff, NA),
                 "ex summation" = c(NA, x[["bfd"]]$abs.excess.summation, NA),
                 "abs diff" = c(NA, x[["bfd"]]$absolute.diff, NA)
  )
  
  if(is.null(xlab)){
    xlab <- c("First Digit", "First-Two Digits", "First-Three Digits", "First-Order Digits")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  }
  
  
  if(is.null(ylab)){
    ylab <- "Frequency"
  }
  
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, disc, 
       col = col,
       xlab = xlab,
       ylab = ylab, 
       main = main,
       xaxt = "n", xaxs = 'i', type = 'h',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 92, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       })
  points(xmarks, disc, pch = 19, col = col, cex = 0.5)
  
  out <- list()
  out$data <- data.frame(digits, disc)
  invisible(out)
}


xyplot.Berford <- function(x,
                               obs.freq = "digits",
                               main = "Expected vs observed frequencies",
                               xlab = NULL,
                               ylab = NULL,
                               grid = TRUE,
                               col = "blue",
                               freq = TRUE, ...){
  
  obs.freq <- tolower(obs.freq)
  
  if(freq){
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist.freq
             exp_freq <- x[["bfd"]]$benford.dist.freq
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist.freq
             exp_freq <- x[["bfd"]]$benford.so.dist.freq
           }
    )
  }else{
    switch(obs.freq,
           "digits" = {
             obs_freq <- x[["bfd"]]$data.dist
             exp_freq <- x[["bfd"]]$benford.dist
           },
           "second order" = {
             obs_freq <- x[["bfd"]]$data.second.order.dist
             exp_freq <- x[["bfd"]]$benford.so.dist.freq/sum(x[["bfd"]]$benford.so.dist.freq)
           }
    )
  }
  
  if(is.null(xlab)){
    xlab <- "Observed Frequency"
  }
  
  if(is.null(ylab)){
    ylab <- "Expected Frequency"
  }
  
  old.par <- par(pty="s")
  on.exit(par(old.par))
  
  lim <- c(min(c(obs_freq,exp_freq)),  max(c(obs_freq,exp_freq)))
  plot(obs_freq,
       exp_freq,
       pch = 19,
       col = col, 
       main = main,
       xlab = xlab,
       ylab = ylab,
       xlim = lim,
       ylim = lim,
       panel.first = {
         if(grid) grid(lty = 1, col = "gray90")
       })
  abline(a = 0, b = 1, col = "red", lty = 2)
}

plot.ordered.mantissa <- function(x, grid = TRUE, ...) {
  old.par <- par(pty="s")
  on.exit(par(old.par))
  plot(sort(x[["data"]]$data.mantissa), 
       pch = ".",
       col = "blue", 
       main = "Ordered Mantissa",
       xlab = "Ordered Observation",
       ylab = "Mantissa",
       yaxs = 'i', xaxs = 'i',
       panel.first = {
         if(grid) grid(lty = 1, col = "gray90")
       })
  abline(a = 0, b = 1/length(x[["data"]]$data.mantissa), col = "red", lty = 2)
}


## Generic functions -------------------------------------------------------------------------------------------

legend.Berford <- function(x, err.bounds, size) {
  if (err.bounds) {
    plot_colors <- c("lightblue","red","red")
    legend(x = "topright",
           inset = 0,
           legend = c("Observed Frequency", 
                      "Expected: Benford's Law", 
                      "Expected: Lower and Upper Bounds"), 
           col = plot_colors, 
           cex = size,
           lwd = c(rep(2, 2), 2),
           lty = c(rep(1, 2), 2))
  }else{
    plot_colors <- c("lightblue","red")
    legend(x = "topright",
           inset = 0,
           legend = c("Observed Frequency", 
                      "Expected: Benford's Law"), 
           col = plot_colors, 
           cex = size,
           lwd = 2,
           lty = rep(1, 2))
  }
}


check.plot.names <- function(x, y, ...){
  if (!all(x %in% y)) {
    idx <- which(!x %in% y)
    stop("Invalid plot name:", x[idx], "\nType ?plot.Benford for help.")
  }
}

plot.switch <- function(plot_this, x, col.bar, grid, err.bounds, alpha, exp.benford, freq){
  switch(plot_this,
         "digits" = barplot.Benford(x, obs.freq = "digits", main = "Digits distribution", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq),
         "rootogram digits" = rootogram.Benford(x, obs.freq = "digits", main = "Digits distribution - Rootogram", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq),
         "second order" = barplot.Benford(x,  obs.freq = "second order", main = "Digits distribution\nSecond Order Test", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq),
         "rootogram second order" = rootogram.Benford(x, obs.freq = "second order", main = "Digits distribution\nSecond Order Test - Rootogram", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq),
         "summation" = barplot.Benford(x,  obs.freq = "summation", main = "Summation Distribution by digits", xlab = NULL, ylab = "Summation", grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq),
         "mantissa" = plot.ordered.mantissa(x, grid),
         "chi squared" = needle.Benford(x, discrepancy = "chi squared", main = "Chi-Squared Difference", xlab = NULL, ylab = "Chi-squared", grid),
         "abs diff" = needle.Benford(x, discrepancy = "abs diff", main = "Absolute Difference", xlab = NULL, ylab = "Absolute Difference", grid),
         "ex summation" = needle.Benford(x, discrepancy = "ex summation", main = "Summation Difference", xlab = NULL, ylab = "Absolute Excess Summation", grid),
         "obs vs exp" = xyplot.Berford(x, obs.freq = "digits", main = "Expected vs observed frequencies", xlab = NULL, ylab = NULL, grid, freq),
         "last two digits" = barplot.Benford(x, obs.freq = "last two digits", main = "Last-Two Digits distribution", xlab = "Last-Two Digits", ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
         
  )  
}

compute.error.bounds <- function(exp.freq, n, alpha, rootogram = FALSE, freq = TRUE){
  if(freq){
    ub <- exp.freq + qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) + 1/2
    lb <- exp.freq - qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) - 1/2
  }else{
    ub <- exp.freq + qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq)/n) + 1/(2*n)
    lb <- exp.freq - qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq)/n) - 1/(2*n)
  }
  if(rootogram){
    data.frame(ub = ub - exp.freq, lb = lb - exp.freq)
  }else{
    data.frame(ub, lb)
  }
}

