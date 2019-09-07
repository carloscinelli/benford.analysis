#todo
#implemetar err.bounds no gráfico de frequencias esperadas vs observadas



# Main plot ---------------------------------------------------------------------------------------------

##' @title Plot method for Benford Analysis
##' @description The \code{plot} method for "Benford" objects.
##' 
##' @param  x a "Benford" object
##' @param select it specifies the order and which plots are going to be plotted. If NULL, the parameter except is used.
##' @param except it specifies which plots are not going to be plotted. If NULL, the parameter select is used.
##' Currently, you can choose from 13 plots: "digits", "rootogram digits", "obs vs exp", "second order", "rootogram second order", "last two digits", "summation",
##' "mantissa", "mantissa arc", "chi square", "abs diff", "ex summation", "diff vs ex summation". If you want to plot all, just
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
##' For more flexibility in plotting, use (`separate plots here`)...
##' @export
##' @importFrom graphics abline axis barplot legend lines par plot rect points arrows layout plot.new title box grid text mtext
##' @importFrom stats pchisq var
##' @importFrom utils head
##' @importFrom stats setNames qnorm


plot.Benford <-
function(x, select = c("digits", "obs vs exp", "summation", "diff vs ex summation",
                       "last two digits", "second order", "mantissa", "mantissa arc"), 
         except = NULL, 
         multiple = TRUE,
         col.bar = "lightblue",
         err.bounds = FALSE,
         alpha = 0.05,
         grid = TRUE,
         mfrow = NULL,
         exp.benford = TRUE,
         freq = TRUE, ...)
{
  
  if (class(x) != "Benford") stop("Class(x) must be 'Benford'")
  
  if(!(alpha > 0 & alpha < 1)) stop(paste0(alpha, " is not a valid value for 'alpha' parameter"))
  
  available.plots <- c("digits", "rootogram digits", "second order", "rootogram second order", "summation", "mantissa", "chi squared", "ex summation", "abs diff", "obs vs exp", "last two digits", "mantissa arc", "diff vs ex summation")
  
  if (!is.null(select)) {# if select is used, overrides the except parameter
    select <- tolower(select)
    check.plot.names(select, c(available.plots, "all"))
    if (all(select == "all")) plot_this <- available.plots
    else plot_this <- select
    
    if (!is.null(except))
      warning("the argument 'except' was ignored. See ?plot.Benford for more datails.")
    
  }else{
    
    if (!is.null(except)) {
      except <- tolower(except)
      check.plot.names(except, c(available.plots, "none"))
      if (all(except == "none")) {
        plot_this <- available.plots
      }else{
        ap <- available.plots
        plot_this <- ap[!(ap %in% except)] # remove plots listed in except
      }
    }else{
      stop("the 'select' and 'except' arguments must not be equal to NULL at the same time")
    }
  }
  
  nGraphics <- length(plot_this)
  # list of plots to draw without legend
  no.legend <- c("chi squared", "abs diff", "ex summation", "obs vs exp", "mantissa", "mantissa arc", "diff vs ex summation")
  
  if (multiple) {# multiple plots in a same panel
    old.par <- par(no.readonly = TRUE)
    #on.exit(par(old.par))
    
    if(!is.null(mfrow)){# mfrow is a parameter of the plot.Berford
      rows <- mfrow[1] 
      cols <- mfrow[2]
      par(mfrow = c(rows, cols))
    }else{
      if (nGraphics <= 2){
        rows <- 1
        cols <- nGraphics
        par(mfrow = c(rows, cols)) 
      }else{
        rows <- 2 
        cols <- 2
        par(mfrow = c(rows, cols)) 
      }
    }
    
    nslots <- rows*cols
    lg_size <- ifelse(rows > 1, 1, 0.7) / rows # legend size

    for (i in 1:length(plot_this)) {
      switch.plot(plot_this[i], bfd = x, col.bar = col.bar, grid = grid, err.bounds = err.bounds, alpha = alpha, exp.benford = exp.benford, freq = freq)
      if(!(plot_this[i] %in% no.legend))
        draw.legend(x, err.bounds, lg_size)
    }
    
  }else{ # plot individual graphs
    old.par <- par(no.readonly = TRUE)
    #on.exit(par(old.par))
    plot_this <- select
    lg_size <- 0.7
    
    for (i in 1:length(plot_this)) {
      switch.plot(plot_this[i], bfd = x, col.bar = col.bar, grid = grid, err.bounds = err.bounds, alpha = alpha, exp.benford = exp.benford, freq = freq)
      if(!(plot_this[i] %in% no.legend))
        draw.legend(x, err.bounds, lg_size)
    }
  }
}


# Separate plots --------------------------------------------------------------------------------------
utils::globalVariables(c("title", "digits", "y"))

barplot.Benford <-
function(x, y,
         exp.freq,
         ndigits,
         main = NULL,
         xlab = NULL,
         ylab = NULL,
         grid = TRUE,
         col.bar = "lightblue",
         err.bounds = FALSE,
         alpha = 0.05,
         exp.benford = TRUE,
         freq = TRUE, ...)
{
  
  out <- list()
  if (err.bounds){
    bounds <- compute.error.bounds(exp.freq, length(x), alpha, rootogram = FALSE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    out$data <- data.frame(x, y, exp.freq, bounds)
    out$params <- data.frame(alpha = alpha)
    ylim <- c(0, max(c(y, exp.freq, ub))*1.1)# y axis limits adjusted by upper error bounds
  } else{
    out$data <- data.frame(x, y, exp.freq)
    ylim <- c(0, max(c(y, exp.freq))*1.1)
  }
  
  xmarks <- seq(0.7, length(exp.freq)*1.2, 1.2) # ticks
  xlim <- c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])) # x axis limits
  
  # draw plot
  plot.base(x, xmarks, y, xlim, ylim, grid, type = "n")
  draw.barchart(y, col.bar)
  draw.error.bounds(xmarks, ub, lb, err.bounds)
  draw.line.benford(xmarks, exp.freq, exp.benford)
  main.and.labs(main, xlab, ylab, ndigits)
  
  invisible(out)
}

# barplot.Benford(bfd.cp$bfd$digits, bfd.cp$bfd$data.dist.freq, bfd.cp$bfd$benford.dist.freq, bfd.cp$info$number.of.digits, main="teste", err.bounds = T)


rootogram.Benford <-
function(x, y,
         exp.freq,
         ndigits,
         main = NULL,
         xlab = NULL,
         ylab = NULL,
         grid = TRUE,
         col.bar = "lightblue",
         err.bounds = FALSE,
         alpha = 0.05,
         exp.benford = TRUE,
         freq = TRUE, ...)
{

  out <- list()
  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, length(x), alpha, rootogram = TRUE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    # y axis limits adjusted by error bounds
    ylim <- c(min(exp.freq - y, lb)*1.1, max(abs(exp.freq - y)*0.5, exp.freq, ub)*1.1)
    out$data <- data.frame(x, y, exp.freq, bounds) 
    out$params <- data.frame(alpha = alpha)
  }else{
    ylim <- c(min(exp.freq - y)*1.1, max(abs(exp.freq - y)*0.5, exp.freq)*1.1)
    out$data <- data.frame(x, y, exp.freq)
  }
  
  xmarks <- seq(0.7, length(x)*1.2, 1.2)# ticks
  xlim <- c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)]))
  
  # draw plot
  plot.base(x, xmarks, y, xlim, ylim, grid, type = "n")
  draw.rootogram(xmarks, y, exp.freq, col.bar)
  draw.line.benford(xmarks, exp.freq, exp.benford)
  draw.error.bounds(xmarks, ub, lb, err.bounds)
  main.and.labs(main, xlab, ylab, ndigits)
  
  invisible(out)
}

#rootogram.Benford(bfd.cp$bfd$digits, bfd.cp$bfd$data.dist.freq, bfd.cp$bfd$benford.dist.freq, bfd.cp$info$number.of.digits, main="teste", err.bounds = T)

needle.Benford <-
function(x, y,
         ndigits,
         main = NULL,
         xlab = NULL,
         ylab = NULL,
         grid = TRUE,
         col = "blue", ...)
{
  
  xmarks <- seq(0.7, length(x)*1.2, 1.2)
  
  plot.base(x, xmarks, y, xlim = NULL, ylim = NULL, grid, type = "h", col = col)
  points(xmarks, y, pch = 19, col = col, cex = 0.5)
  main.and.labs(main, xlab, ylab, ndigits)
  
  out <- list()
  out$data <- data.frame(x, y)
  invisible(out)
}


xyplot.Berford <-
function(x, y,
         main = "Expected vs observed frequencies",
         xlab = NULL,
         ylab = NULL,
         grid = TRUE,
         col = "black",
         freq = TRUE, ...)
{
  
  if(is.null(xlab)){
    xlab <- "Observed Frequency"
  }
  
  if(is.null(ylab)){
    ylab <- "Expected Frequency"
  }
  
  old.par <- par(pty = "s")
  on.exit(par(old.par))
  
  axes.limits <- c(min(c(y, x)),  max(c(y, x)))
  plot(y,
       x,
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
  abline(a = 0, b = 1, col = "red", lwd = 2)
}

plot.ordered.mantissa <-
function(x, grid = TRUE, ...)
{
  
  old.par <- par(pty = "s")
  on.exit(par(old.par))
  plot(sort(x), 
       pch = ".",
       col = "blue", 
       main = "Ordered Mantissa",
       xlab = "Ordered Observation",
       ylab = "Mantissa",
       yaxs = 'i', xaxs = 'i',
       panel.first = {
         if(grid) grid(lty = 1, col = "gray90")
       })
  abline(a = 0, b = 1/length(x), col = "red", lty = 2)
}

plot.mantissa.arc <-
function(mantissa, L2, ...)
{
  
  x_coord <- mean(cos(2 * pi * mantissa))
  y_coord <- mean(sin(2 * pi * mantissa))
  l <- seq(0, 1, length.out = 100)
  x <- cos(2 * pi * l)
  y <- sin(2 * pi * l)
  oldpar <- par(pty = "s")
  on.exit(par(oldpar))
  plot(x, y, type = "l", xlab = "", ylab = "",
       panel.first = {
         grid(lty = 'solid')
         abline(h = 0, v = 0, col = 'gray40', asp = 1)
       })
  box(col = 'white')
  points(0, 0, col = "red", pch = "+", cex = 2)
  points(x_coord, y_coord, col = "blue", pch = 19, cex = 1.5)
  x_obs <- sqrt(L2) * cos(2 * pi * l)
  y_obs <- sqrt(L2) * sin(2 * pi * l)
  lines(x_obs, y_obs, col = "blue", lty = 2)
  
  pvalor <- exp(-L2 * length(mantissa))
  text(0.7, 1.05, labels = paste0("p-value: ", format.pval(pvalor)), col = "blue", cex = 0.7)
  text(x_coord + 0.1, y_coord - 0.1, labels = paste0("(", round(x_coord, 3), ", ", round(y_coord, 3), ")"), col = "blue", cex = 0.7)
}

# this function can be modular
plot.diff.vs.ex.summation <-
function(digits, diff.1, diff.2, grid, ...)
{
  
  out <- list()
  out$data <- data.frame(digits, diff.1, diff.2)
  xlimit <- max(abs(diff.1))
  ylimit <- max(abs(diff.2))
  plot(diff.1, diff.2,
       xlab = "Difference",
       ylab = "Excess Summation",
       xlim = c(-xlimit, xlimit),
       ylim = c(-ylimit, ylimit),
       pch = 19,
       cex = 1,
       panel.first = {
         if (grid) grid(lty = 1, col = "gray90")
       })
  abline(v = 0, h = 0)
  
  picker <- (out$data$diff.1 > 0) & (out$data$diff.2 > 0)
  if (any(picker))
    text(out$data$diff.1[picker] + xlimit*0.03, out$data$diff.2[picker] - ylimit*0.03, out$data$digits[picker], cex = .6)
}

## Generic functions -------------------------------------------------------------------------------------------

plot.base <-
function(digits, x, y, xlim, ylim, grid, type = 'n', col = "black", ...)
{
  
  plot(x, y, xlim = xlim, ylim = ylim, type = type, col = col,
       main = "", xlab = "", ylab = "",
       yaxs = 'i', xaxs = 'i', xaxt = "n",
       panel.first = {
         if(grid) {
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           pickers <- seq(1, length(x), 10)
           axis(1, at = x[pickers], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = x,  labels = digits)
       }
  )
}

draw.barchart <-
function(y, col, ...)
{
  barplot(y, col = col, yaxt = "n", add = T)
}

draw.rootogram <-
function(x, y, exp.freq, col, ...)
{
  rect(xleft = x - 0.5,
       xright = x + 0.5,
       ybottom = exp.freq,
       ytop = exp.freq - y,
       col = col)
  abline(h = 0)
}

draw.line.benford <-
function(x, y, draw = TRUE, ...)
{
  if (draw) lines(x, y, col = "red", lwd = 2)
}

draw.error.bounds <-
function(x, upper.bound, lower.bound, draw = TRUE, ...)
{
  if (draw){
    lines(upper.bound ~ x, lty = 2, col = "red")
    lines(lower.bound ~ x, lty = 2, col = "red")
  }
}

draw.legend <-
function(x, err.bounds, size)
{
  
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

main.and.labs <-
function(main = NULL, xlab = NULL, ylab = NULL, ndigits, ...)
{
  
  if(is.null(xlab)){
    xlab.options <- c("First Digit", "First-Two Digits", "First-Three Digits", "First-Order Digits")
    lab.picker <- ifelse(ndigits <= 3, ndigits, 4)
    xlab <- xlab.options[lab.picker]
  }
  
  if(is.null(ylab)){
    ylab <- "Frequency"
  }
  
  title(main = main, xlab = xlab, ylab = ylab)
}


check.plot.names <-
function(x, y, ...)
{
  if (!all(x %in% y)) {
    idx <- which(!x %in% y)
    stop("Invalid plot name:", x[idx], "\nType ?plot.Benford for help.")
  }
}

switch.plot <-
function(plot_this, bfd, col.bar, grid, err.bounds, alpha, exp.benford, freq)
{
  
  ds.bfd <- getBfd(bfd)
  y <- bfd$last.two.digits
  z <- getData(bfd)
  n.digits <- bfd$info$number.of.digits
  switch(plot_this,
         "digits" = {
           barplot.Benford(ds.bfd$digits, ds.bfd$data.dist.freq, ds.bfd$benford.dist.freq, n.digits, main = "Digits distribution", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           mtext(paste("X-squared: ", round(chisq(bfd)$statistic, 5), "\n\n ", sep = ""), line = 0, adj = 0, cex = 0.6)
           mtext(paste("KS: ", round(ks(bfd)$statistic, 5), "\n", sep = ""), line = 0, adj = 0, cex = 0.6)
           mtext(paste("MAD: ", round(MAD(bfd), 5), "- Conclusion: ", bfd$MAD.conformity, sep = ""), line = 0, adj = 0, cex = 0.6)
           
           },
         "rootogram digits" = {
           rootogram.Benford(ds.bfd$digits, ds.bfd$data.dist.freq, ds.bfd$benford.dist.freq, n.digits, main = "Digits distribution - Rootogram", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           },
         "second order" = {
           barplot.Benford(ds.bfd$digits, ds.bfd$data.second.order.dist.freq, ds.bfd$benford.so.dist.freq, n.digits, main = "Digits distribution\nSecond Order Test", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           },
         "rootogram second order" = {
           rootogram.Benford(ds.bfd$digits, ds.bfd$data.second.order.dist.freq, ds.bfd$benford.so.dist.freq, n.digits, main = "Digits distribution\nSecond Order Test - Rootogram", xlab = NULL, ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           },
         "summation" = {
           barplot.Benford(ds.bfd$digits, ds.bfd$data.summation, rep(mean(ds.bfd$data.summation), length(ds.bfd$digits)), n.digits, main = "Summation Distribution by digits", xlab = NULL, ylab = "Summation", grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           },
         "mantissa" = {
           plot.ordered.mantissa(z$data.mantissa, grid)
           },
         "chi squared" = {
           needle.Benford(c(NA, ds.bfd$digits, NA),  c(NA, ds.bfd$squared.diff, NA), n.digits, main = "Chi-Squared Difference", xlab = NULL, ylab = "Chi-squared", grid = grid, col = "blue")
           },
         "abs diff" = {
           needle.Benford(c(NA, ds.bfd$digits, NA),  c(NA, ds.bfd$absolute.diff, NA), n.digits, main = "Absolute Difference", xlab = NULL, ylab = "Absolute Difference", grid = grid, col = "blue")
           },
         "ex summation" = {
           needle.Benford(c(NA, ds.bfd$digits, NA),  c(NA, ds.bfd$abs.excess.summation, NA), n.digits, main = "Summation Difference", xlab = NULL, ylab = "Absolute Excess Summation", grid = grid, col = "blue")
           },
         "obs vs exp" = {
           xyplot.Berford(ds.bfd$data.dist.freq, ds.bfd$benford.dist.freq, main = "Expected vs observed frequencies", xlab = NULL, ylab = NULL, grid = grid)
           },
         "last two digits" = {
           barplot.Benford(y$last.two.digits, y$data.dist.freq, rep(mean(y$data.dist.freq), 100), n.digits, main = "Last-Two Digits distribution", xlab = "Last-Two Digits", ylab = NULL, grid = grid, col.bar = col.bar, err.bounds = err.bounds, alpha =  alpha, exp.berford = exp.benford, freq = freq)
           },
         "mantissa arc" = {
           plot.mantissa.arc(z$data.mantissa, marc(bfd)$statistic)
         },
         "diff vs ex summation" = {
           plot.diff.vs.ex.summation(ds.bfd$digits, ds.bfd$difference, ds.bfd$data.summation - mean(ds.bfd$data.summation), grid)
         }
  )
}


compute.error.bounds <-
function(exp.freq, n, alpha, rootogram = FALSE, freq = TRUE)
{
  
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

