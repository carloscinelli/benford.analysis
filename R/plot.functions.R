
# Main plot ---------------------------------------------------------------------------------------------

##' @title Plot method for Benford Analysis
##' @description The \code{plot} method for "Benford" objects.
##' 
##' @param  x a "Benford" object
##' @param select it specifies the order and which plots are going to be plotted. If NULL, the parameter except is used.
##' @param except it specifies which plots are not going to be plotted. If NULL, the parameter select is used.
##' Currently, you can choose from 9 plots: "digits", "rootogram digits", "second order", "rootogram second order", "summation",
##' "mantissa", "chi square", "abs diff", "ex summation". If you want to plot all, just
##' put except = "none". The default is not to plot the "mantissa" and "abs diff". If you want to plot all, just
##' put except = "all"
##' @param multiple if TRUE, all plots are grouped in the same window.
##' @param col.bar a color to be used to fill the bars. The default is lightblue.
##' @param err.bounds if TRUE, the upper and lower error bounds are draw. The error bounds indicate the binomial root mean square error.
##' @param alpha it specifies level of confidence interval. The defaults to 95 percent confidence interval,i.e., the error bounds will represent 1.96 standard error from the expected count by Benford's Law.
##' @param grid if TRUE, adds an rectangular grid to plot.
##' @param ... arguments to be passed to generic plot functions,
##' @return Plots the Benford object.
##' @export
##' @importFrom graphics abline axis barplot legend lines par plot rect points arrows layout plot.new
##' @importFrom stats pchisq var
##' @importFrom utils head
##' @importFrom stats setNames qnorm

plot.Benford <- function(x, 
                         select = c("digits", "second order", "summation", "chi squared", "ex summation"), 
                         except = NULL, 
                         multiple = TRUE,  
                         col.bar = "lightblue", 
                         err.bounds = FALSE, 
                         alpha = 0.05, 
                         grid = TRUE, ...){
  
  
  if (class(x) != "Benford") stop("Class(x) must be 'Benford'")
  
  if(!(alpha > 0 & alpha < 1)) stop(paste0(alpha, " is not a valid value for 'alpha' parameter"))
  
  available.plots <- c("digits", "rootogram digits", "second order", "rootogram second order", "summation", "mantissa", "chi squared", "ex summation", "abs diff", "none", "all")
  
  if (!is.null(select)) {
    check.plot.names(select, available.plots)
    select <- tolower(select)
    if (all(select == "all")) plots <- available.plots[1:9]
    else plots <- select
    
    if (!is.null(except)) {
      check.plot.names(except, available.plots)
      except <- tolower(except)
      if (all(except == "none")) {
        plots <- available.plots[1:9]
      }else{
        plots <- plots[!(plots %in% except)]
      }
    }
    
  }else{
    
    if (!is.null(except)) {
      check.plot.names(except, available.plots)
      except <- tolower(except)
      if (all(except == "none")) {
        plots <- available.plots[1:9]
      }else{
        ap <- available.plots[1:9]
        plots <- ap[!(ap %in% except)]
      }
    }else{
      stop("the 'select' and 'except' arguments must not be equal to NULL at the same time")
    }
  }  
  
  nGraphics <- length(plots)
  
  if (multiple | (nGraphics == 1)) {
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    
    if (nGraphics < 4) {
      rows = 1; 
      cols = nGraphics
    }
    
    if (nGraphics >= 4 & nGraphics <= 6) {
      rows = 2; 
      cols = 3
    }
    if (nGraphics > 6) {
      rows = 3; 
      cols = 3
    }
    
    nslots <- rows*cols
    plot_this <- c(rep("blank", nslots), "legend")
    plot_this[1:nGraphics] <- plots
    m <- matrix(c(1:nslots, rep(nslots + 1, cols)), nrow = rows + 1, ncol = cols,byrow = TRUE)
    layout(mat = m, heights = c(rep(0.9/rows, rows), 0.1)) 
    lg_size <- ifelse(rows > 1, 1, ifelse(err.bounds, 0.6, 0.7))
  }else{
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    plot_this <- vector("character", nGraphics*2)
    plot_this[seq(1, nGraphics*2, 2)] <- plots
    plot_this[seq(2, nGraphics*2, 2)] <- "legend"
    m <- matrix(c(1,2), nrow = 2, ncol = 1,byrow = TRUE)
    layout(mat = m, heights = c(0.9, 0.1)) 
    lg_size <- ifelse(err.bounds, 0.6, 0.7)
  }
  
  for (i in 1:length(plot_this)) {
    switch(plot_this[i],
           "digits" = plot.data.vs.benford(x, col.bar, grid, err.bounds, alpha, ...),
           "rootogram digits" = plot.rootogram.data.vs.benford(x, col.bar, grid, err.bounds, alpha, ...),
           "second order" = plot.second.order(x, col.bar, grid, ...),
           "rootogram second order" = plot.rootogram.second.order(x, col.bar, grid, ...),
           "summation" = plot.summation(x, col.bar, grid, ...),
           "mantissa" = plot.ordered.mantissa(x, grid, ...),
           "chi squared" = plot.chi_squared(x, grid, ...),
           "abs diff" = plot.abs.diff(x, grid, ...),
           "ex summation" = plot.ex.summation(x, grid, ...),
           "legend" = plot.legend(x, err.bounds, lg_size),
           "blank" = plot.new()
    ) 
  }
  
}


check.plot.names <- function(x, y, ...){
  if (!all(x %in% y)) {
    idx <- which(!x %in% y)
    stop("Invalid plot name:", x[idx], "\nType ?plot.Benford for help.")
  }
}

## Generic functions -------------------------------------------------------------------------------------------

# duas tarefas diferentes
## 1) criar funções genéricas e flexiveis para cada lógica de plot, indpendnetemente dos dados
## 2) pode criar função genérica de arrumar plots

# exportar para o usuário as funções genéricas

# rootogramplot <- function(obs.freq, exp.feq, tuning graphical parameters){
#   
# }
# 
# 
# # essa aqui talvez não ?
# plot.error.bounds <- function(bounds, type = c("lines", "arrows")){
#   if (lines){
#     lines()
#   }
# }

compute.error.bounds <- function(exp.freq, n, alpha, rootogram = FALSE){
  ub <- exp.freq + qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) + 1/2
  lb <- exp.freq - qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) - 1/2
  if(rootogram){
    data.frame(ub = ub - exp.freq, lb = lb - exp.freq)
  }else{
    data.frame(ub, lb)
  }
}


plot.frequency <- function(digits,
                           obs.freq,
                           exp.freq,
                           main = NULL,
                           xlab = "Digits",
                           ylab = "Frequency",
                           grid = TRUE,
                           col.bar = "lightblue",
                           err.bounds = FALSE,
                           alpha = 0.05){
  
  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, sum(obs.freq), alpha, rootogram = FALSE)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(0, max(c(obs.freq, exp.freq, ub))*1.1)
  }else{
    ylim <- c(0, max(c(obs.freq, exp.freq))*1.1)
  }
  
  xmarks <- seq(0.7, length(exp.freq)*1.2, 1.2)
  plot(xmarks, obs.freq,
       main = main,
       xlab = xlab, ylab = ylab,
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
  barplot(obs.freq, 
          col = col.bar, 
          yaxt = "n", add = T)
  lines(xmarks, exp.freq, col = "red", lwd = 2)
  if(err.bounds){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    invisible(bounds)
  }
}


plot.rootogram <- function(digits,
                           obs.freq,
                           exp.freq,
                           main = NULL,
                           xlab = "Digits",
                           ylab = "Frequency",
                           grid = TRUE,
                           col.bar = "lightblue",
                           err.bounds = FALSE,
                           alpha = 0.05){
  
  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, sum(obs.freq), alpha, rootogram = TRUE)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(min(exp.freq - obs.freq, lb)*1.1, max(abs(exp.freq - obs.freq)*0.5, exp.freq, ub)*1.1)
  }else{
    ylim <- c(min(exp.freq - obs.freq)*1.1, max(abs(exp.freq - obs.freq)*0.5, exp.freq)*1.1)
  }
  
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, obs.freq,
       main = main,
       xlab = xlab, ylab = ylab,
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = ylim,
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks,  labels = digits)
       }
  )
  rect(xleft = xmarks - 0.5,
       xright = xmarks + 0.5,
       ybottom = exp.freq, ytop = exp.freq - obs.freq, col = col.bar)
  abline(h = 0)
  lines(xmarks, exp.freq, col = "red", lwd = 2)
  if(err.bounds){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    abline(h = 0, col = 'red')
    invisible(bounds)
  }
  
}


plot.needle <- function(digits,
                                 discrepancy,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 grid = TRUE){
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, discrepancy, 
       col = "blue",
       xlab = xlab,
       ylab = ylab, 
       main = main,
       xaxt = "n",
       xaxs = 'i',
       type = 'h',
       cex.axis = 0.8,
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 92, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       })
  points(xmarks, discrepancy, pch = 19, col = "blue", cex = 0.5)
}

# Separate plots --------------------------------------------------------------------------------------

utils::globalVariables(c("x"))

plot.data.vs.benford <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, ...) {

  digits <- x[["bfd"]]$digits
  obs.freq <-x[["bfd"]]$data.dist.freq
  exp.freq <- x[["bfd"]]$benford.dist.freq
  out <- list()
  
  plot.data <- plot.frequency(digits, obs.freq, exp.freq, "Digits distribution", "Digits", "Frequency", grid, col.bar, err.bounds, alpha)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


plot.second.order <- function(x, col.bar = "lightblue", grid = TRUE, ...) {

  digits <- x[["bfd"]]$digits
  obs.freq <-x[["bfd"]]$data.second.order.dist.freq
  exp.freq <- x[["bfd"]]$benford.so.dist.freq
  out <- list()
  out$data <- data.frame(digits, obs.freq, exp.freq)
  
  plot.frequency(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test", "Digits", "Frequency", grid, col.bar)
  
  invisible(out)
}


plot.rootogram.data.vs.benford <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.dist.freq
  exp.freq <- x[["bfd"]]$benford.dist.freq
  out <- list()

  plot.data <- plot.rootogram(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test", "Digits", "Frequency", grid, col.bar, err.bounds, alpha)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


plot.rootogram.second.order <- function(x, col.bar = "lightblue", grid = TRUE, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.second.order.dist.freq
  exp.freq <- x[["bfd"]]$benford.so.dist.freq
  out <- list()
  out$data <- data.frame(digits, obs.freq, exp.freq)
  
  plot.rootogram(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test - Rootogram", "Digits", "Frequency", grid, col.bar)
  
  invisible(out)
}


plot.summation <- function(x, col.bar = "lightblue", grid = TRUE, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.summation
  exp.freq <- rep(mean(obs.freq), length(digits))
  out <- list()
  out$data <- data.frame(digits, obs.freq, exp.freq)
  
  plot.frequency(digits, obs.freq, exp.freq, "Summation Distribution by digits", "Digits", "Frequency", grid, col.bar)
  
  invisible(out)
}


plot.ordered.mantissa <- function(x, grid = TRUE, ...) {
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


plot.chi_squared <- function(x, grid = TRUE, ...) {
  squared.diff <- c(NA, x[["bfd"]]$squared.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, squared.diff)
  
  plot.needle(digits, squared.diff, "Chi-Squared Difference", "Digits", "Chi-squared")
  
  invisible(out)
}


plot.abs.diff <- function(x, grid = TRUE, ...) {
  absolute.diff <- c(NA, x[["bfd"]]$absolute.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, absolute.diff)
  
  plot.needle(digits, absolute.diff, "Absolute Difference", "Digits", "Absolute Difference")
  
  invisible(out)
}


plot.ex.summation <- function(x, grid = TRUE, ...) {
  abs.excess.summation <- c(NA, x[["bfd"]]$abs.excess.summation, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, abs.excess.summation)
  
  plot.needle(digits, abs.excess.summation, "Summation Difference", "Digits", "Absolute Excess Summation")
  
  invisible(out)
}


plot.legend <- function(x, err.bounds, size) {
  par(mar = c(0,0,0,0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
  if (err.bounds) {
    plot_colors <- c("lightblue","red","red")
    legend(x = "top",
           inset = 0,
           legend = c("Observed Frequency", 
                      "Expected: Benford's Law", 
                      "Expected: Lower and Upper Bounds"), 
           col = plot_colors, 
           cex = size,
           lwd = c(rep(2, 2), 2),
           lty = c(rep(1, 2), 2),
           horiz = TRUE)
  }else{
    plot_colors <- c("lightblue","red")
    legend(x = "top",
           inset = 0,
           legend = c("Observed Frequency", 
                      "Expected: Benford's Law"), 
           col = plot_colors, 
           cex = size,
           lwd = 2,
           lty = rep(1, 2),
           horiz = TRUE)
  }
  par(mar = c(5, 4, 4, 2) + 0.1)
}



