
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
      switch.plot(plot_this[i], x, col.bar, grid, err.bounds, alpha, exp.benford, freq)
      if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
        draw.legend(x, err.bounds, lg_size)
      }
    }
    
  }else{
    old.par <- par(no.readonly = TRUE)
    #on.exit(par(old.par))
    plot_this <- plots
    lg_size <- 0.7
    
    for (i in 1:length(plot_this)) {
      switch.plot(plot_this[i], x, col.bar, grid, err.bounds, alpha, exp.benford, freq)
      if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
        draw.legend(x, err.bounds, lg_size)
      }
    }
  }
}

# Separate plots --------------------------------------------------------------------------------------

barplot.Benford <- function(x, y,
                            exp.freq,
                            number.of.digits,
                            main = NULL,
                            xlab = NULL,
                            ylab = NULL,
                            grid = TRUE,
                            col.bar = "lightblue",
                            err.bounds = FALSE,
                            alpha = 0.05,
                            exp.benford = TRUE,
                            freq = TRUE, ...){
  out <- list()
  if (err.bounds){
    bounds <- compute.error.bounds(exp.freq, length(x), alpha, rootogram = FALSE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    out$data <- data.frame(x, y, exp.freq, bounds) 
    out$params <- data.frame(alpha = alpha)
    ylim <- c(0, max(c(y, exp.freq, ub))*1.1)
  } else{
    out$data <- data.frame(x, y, exp.freq) 
    ylim <- c(0, max(c(y, exp.freq))*1.1)
  }
  
  xmarks <- seq(0.7, length(exp.freq)*1.2, 1.2)
  xlim <- c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)]))
  
  plot.base(x, xmarks, y, xlim, ylim, grid, type = "n")
  draw.barchart(y, col.bar)
  draw.error.bounds(xmarks, ub, lb, err.bounds)
  draw.line.benford(xmarks, exp.freq, exp.benford)
  main.and.labs(main, xlab, ylab, number.of.digits)
  
  invisible(out)
}

#barplot.Benford(bfd.cp$bfd$digits, bfd.cp$bfd$data.dist.freq, bfd.cp$bfd$benford.dist.freq, bfd.cp$info$number.of.digits, main="teste", err.bounds = T)


rootogram.Benford <- function(x, y,
                              exp.freq,
                              number.of.digits,
                              main = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE,
                              freq = TRUE, ...){

  out <- list()
  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, length(x), alpha, rootogram = TRUE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(min(exp.freq - y, lb)*1.1, max(abs(exp.freq - y)*0.5, exp.freq, ub)*1.1)
    out$data <- data.frame(digits, y, exp.freq, bounds) 
    out$params <- data.frame(alpha = alpha)
  }else{
    ylim <- c(min(exp.freq - y)*1.1, max(abs(exp.freq - y)*0.5, exp.freq)*1.1)
    out$data <- data.frame(digits, y, exp.freq)
  }
  
  xmarks <- seq(0.7, length(x)*1.2, 1.2)
  xlim <- c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)]))
  
  plot.base(x, xmarks, y, xlim, ylim, grid, type = "n")
  draw.rootogram(xmarks, y, exp.freq, col.bar)
  draw.line.benford(xmarks, exp.freq, exp.benford)
  draw.error.bounds(xmarks, ub, lb, err.bounds)
  main.and.labs(main, xlab, ylab, number.of.digits)
  
  invisible(out)
}

#rootogram.Benford(bfd.cp$bfd$digits, bfd.cp$bfd$data.dist.freq, bfd.cp$bfd$benford.dist.freq, bfd.cp$info$number.of.digits, main="teste", err.bounds = T)

needle.Benford <- function(x, y,
                           main = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           grid = TRUE,
                           col = "blue", ...){
  
  xmarks <- seq(0.7, length(x)*1.2, 1.2)
  plot.base(x, xmarks, y, xlim = NULL, ylim = NULL, grid, type = "h")
  points(xmarks, y, pch = 19, col = col, cex = 0.5)
  main.and.labs(main, xlab, ylab, number.of.digits)
  
  out <- list()
  out$data <- data.frame(x, y)
  invisible(out)
}


xyplot.Berford <- function(x, y,
                           exp.freq,
                           main = "Expected vs observed frequencies",
                           xlab = NULL,
                           ylab = NULL,
                           grid = TRUE,
                           col = "blue",
                           freq = TRUE, ...){
  
  if(is.null(xlab)){
    xlab <- "Observed Frequency"
  }
  
  if(is.null(ylab)){
    ylab <- "Expected Frequency"
  }
  
  old.par <- par(pty = "s")
  on.exit(par(old.par))
  
  axes.limits <- c(min(c(y, exp.freq)),  max(c(y, exp.freq)))
  plot(y,
       exp.freq,
       pch = 19,
       col = col, 
       main = main,
       xlab = xlab,
       ylab = ylab,
       xlim = axes.limits,
       ylim = axes.limits,
       panel.first = {
         if(grid) grid(lty = 1, col = "gray90")
       })
  abline(a = 0, b = 1, col = "red", lty = 2)
}

plot.ordered.mantissa <- function(x, grid = TRUE, ...) {
  old.par <- par(pty = "s")
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

plot.base <- function(digits, x, y, xlim, ylim, grid, type = 'n', ...){
  plot(x, y, xlim = xlim, ylim = ylim, type = type,
       main = "", xlab = "", ylab = "",
       yaxs = 'i', xaxs = 'i', xaxt = "n",
       panel.first = {
         if(grid) {
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           pickers <- seq(1, length(x), ifelse(length(digits) <= 90, 1, 10))
           axis(1, at = xmarks[pickers], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = x,  labels = digits)
       }
  )
}

draw.barchart <- function(y, col, ...){
  barplot(y, col = col, yaxt = "n", add = T)
}

draw.rootogram <- function(x, y, exp.freq, col, ...){
  rect(xleft = xmarks - 0.5,
       xright = xmarks + 0.5,
       ybottom = exp.freq,
       ytop = exp.freq - y,
       col = col)
  abline(h = 0)
}

draw.line.benford <- function(x, y, draw = TRUE, ...){
  if (draw) lines(x, y, col = "red", lwd = 2)
}

draw.error.bounds <- function(x, upper.bound, lower.bound, draw = TRUE, ...){
  if (draw){
    lines(upper.bound ~ x, lty = 2, col = "red")
    lines(lower.bound ~ x, lty = 2, col = "red")
  }
}

draw.legend <- function(x, err.bounds, size) {
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

main.and.labs <- function(main, xlab, ylab, n.digits, ...){
  if(is.null(xlab)){
    xlab.options <- c("First Digit", "First-Two Digits", "First-Three Digits", "First-Order Digits")
    lab.picker <- ifelse(number.of.digits <= 3, number.of.digits, 4)
    xlab <- xlab.options[lab.picker]
  }
  
  if(is.null(ylab)){
    ylab <- "Frequency"
  }
  
  title(main = main, xlab = xlab, ylab = ylab)
}


check.plot.names <- function(x, y, ...){
  if (!all(x %in% y)) {
    idx <- which(!x %in% y)
    stop("Invalid plot name:", x[idx], "\nType ?plot.Benford for help.")
  }
}

switch.plot <- function(plot_this, x, col.bar, grid, err.bounds, alpha, exp.benford, freq){
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

