
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
##' @param mfrow A vector of the form c(`nr`, `nc`). Subsequent figures will be drawn in an `nr`-by-`nc` array on the device by columns (`mfcol`), or rows (`mfrow`), respectively. For more details see `?par()`.
##' @param col.bar a color to be used to fill the bars. The default is lightblue.
##' @param err.bounds if TRUE, the upper and lower error bounds are draw. The error bounds indicate the binomial root mean square error.
##' @param alpha it specifies level of confidence interval. The defaults to 95 percent confidence interval,i.e., the error bounds will represent 1.96 standard error from the expected count by Benford's Law.
##' @param grid if TRUE, adds an rectangular grid to plot.
##' @param ... arguments to be passed to generic plot functions,
##' @return Plots the Benford object.
##' @details If both \code{select} and \code{except} arguments have been provided, but only \code{select} will be considered and \code{except} ignored.
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
                         grid = TRUE,
                         mfrow = NULL, ...){
  
  
  if (class(x) != "Benford") stop("Class(x) must be 'Benford'")
  
  if(!(alpha > 0 & alpha < 1)) stop(paste0(alpha, " is not a valid value for 'alpha' parameter"))
  
  available.plots <- c("digits", "rootogram digits", "second order", "rootogram second order", "summation", "mantissa", "chi squared", "ex summation", "abs diff", "obs vs exp")
 
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
      }else{
        if (nGraphics < 4) {
          rows <- 1; 
          cols <- nGraphics
        }
        if (nGraphics >= 4 & nGraphics <= 6) {
          rows <- 2; 
          cols <- 3
        }
        if (nGraphics > 6) {
          rows <- 3; 
          cols <- 3
        }
      }
      
      par(mfrow = c(rows, cols))
      nslots <- rows*cols
      plot_this <- plots
      lg_size <- ifelse(rows > 1, 1, ifelse(err.bounds, 0.4, 0.7))/rows
      
      for (i in 1:length(plot_this)) {
        plot.switch(plot_this[i], x, col.bar, grid, err.bounds, alpha)
        if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
          plot.legend(x, err.bounds, lg_size)
        }
      }
      
    }else{
      old.par <- par(no.readonly = TRUE)
      #on.exit(par(old.par))
      plot_this <- plots
      lg_size <- ifelse(err.bounds, 0.4, 0.7)
      
      for (i in 1:length(plot_this)) {
        plot.switch(plot_this[i], x, col.bar, grid, err.bounds, alpha)
        if(!(plot_this[i] %in% c("chi squared", "abs diff", "ex summation", "obs vs exp"))){
          plot.legend(x, err.bounds, lg_size)
        }
      }
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

histogram.Benford <- function(digits,
                              obs.freq,
                              exp.freq,
                              main = NULL,
                              xlab = "Digits",
                              ylab = "Frequency",
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE){
  
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
  if(exp.benford)
    lines(xmarks, exp.freq, col = "red", lwd = 2)
  if(err.bounds){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    invisible(bounds)
  }
}


rootogram.Benford <- function(digits,
                              obs.freq,
                              exp.freq,
                              main = NULL,
                              xlab = "Digits",
                              ylab = "Frequency",
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE){
  
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
  if(exp.benford)
    lines(xmarks, exp.freq, col = "red", lwd = 2)
  if(err.bounds){
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    abline(h = 0, col = 'red')
    invisible(bounds)
  }
  
}


needle.Benford <- function(digits,
                           discrepancy,
                           main = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           grid = TRUE,
                           col = "blue"){
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, discrepancy, 
       col = col,
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
  points(xmarks, discrepancy, pch = 19, col = col, cex = 0.5)
}

xyplot.Berford <- function(obs.freq,
                           exp.freq,
                           main = "Expected vs observed frequencies",
                           xlab = "Observed Frequency",
                           ylab = "Expected Frequency",
                           grid = TRUE,
                           col = "blue",...){
  old.par <- par(pty="s")
  on.exit(par(old.par))
  lim <- c(min(c(obs.freq,exp.freq)),  max(c(obs.freq,exp.freq)))
  plot(obs.freq,
       exp.freq,
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

check.plot.names <- function(x, y, ...){
  if (!all(x %in% y)) {
    idx <- which(!x %in% y)
    stop("Invalid plot name:", x[idx], "\nType ?plot.Benford for help.")
  }
}

plot.switch <- function(plot_this, x, col.bar, grid, err.bounds, alpha){
  switch(plot_this,
         "digits" = plot.digits(x, col.bar, grid, err.bounds, alpha),
         "rootogram digits" = plot.rootogram.digits(x, col.bar, grid, err.bounds, alpha),
         "second order" = plot.second.order(x, col.bar, grid, err.bounds, alpha),
         "rootogram second order" = plot.rootogram.second.order(x, col.bar, grid, err.bounds, alpha),
         "summation" = plot.summation(x, col.bar, grid, err.bounds, alpha),
         "mantissa" = plot.ordered.mantissa(x, grid),
         "chi squared" = plot.chi_squared(x, grid),
         "abs diff" = plot.abs.diff(x, grid),
         "ex summation" = plot.ex.summation(x, grid),
         "obs vs exp" = plot.xy.digits(x, grid = TRUE, col = "blue")
  )  
}

compute.error.bounds <- function(exp.freq, n, alpha, rootogram = FALSE){
  ub <- exp.freq + qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) + 1/2
  lb <- exp.freq - qnorm(1 - alpha/2)*sqrt(exp.freq*(1 - exp.freq/n)) - 1/2
  if(rootogram){
    data.frame(ub = ub - exp.freq, lb = lb - exp.freq)
  }else{
    data.frame(ub, lb)
  }
}


# Separate plots --------------------------------------------------------------------------------------

utils::globalVariables(c("x", "err.bounds", "alpha", "exp.benford"))

plot.digits <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, exp.benford = TRUE, ...) {

  digits <- x[["bfd"]]$digits
  obs.freq <-x[["bfd"]]$data.dist.freq
  exp.freq <- x[["bfd"]]$benford.dist.freq
  out <- list()
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  plot.data <- histogram.Benford(digits, obs.freq, exp.freq, "Digits distribution", xlab, "Frequency", grid, col.bar, err.bounds, alpha, exp.benford)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


plot.second.order <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, exp.benford = TRUE, ...) {

  digits <- x[["bfd"]]$digits
  obs.freq <-x[["bfd"]]$data.second.order.dist.freq
  exp.freq <- x[["bfd"]]$benford.so.dist.freq
  out <- list()

  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  plot.data <- histogram.Benford(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test", xlab, "Frequency", grid, col.bar, err.bounds, alpha, exp.benford)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
  }
  
  invisible(out)
}


plot.rootogram.digits <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, exp.benford = TRUE, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.dist.freq
  exp.freq <- x[["bfd"]]$benford.dist.freq
  out <- list()

  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  plot.data <- rootogram.Benford(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test", xlab, "Frequency", grid, col.bar, err.bounds, alpha, exp.benford)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


plot.rootogram.second.order <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, exp.benford = TRUE, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.second.order.dist.freq
  exp.freq <- x[["bfd"]]$benford.so.dist.freq
  out <- list()

  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  plot.data <- rootogram.Benford(digits, obs.freq, exp.freq, "Digits distribution\nSecond Order Test - Rootogram", xlab, "Frequency", grid, col.bar, err.bounds, alpha, exp.benford)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
}


plot.summation <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, exp.benford = TRUE, ...) {
  digits <- x[["bfd"]]$digits
  obs.freq <- x[["bfd"]]$data.summation
  exp.freq <- rep(mean(obs.freq), length(digits))
  out <- list()

  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  plot.data <- histogram.Benford(digits, obs.freq, exp.freq, "Summation Distribution by digits", xlab, "Frequency", grid, col.bar, err.bounds, alpha, exp.benford)
  
  if(err.bounds){
    out$data <- data.frame(digits, obs.freq, exp.freq, plot.data) 
  }else{
    out$data <- data.frame(digits, obs.freq, exp.freq)
    out$params <- data.frame(alpha = alpha)
  }
  
  invisible(out)
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

plot.xy.digits <- function(x, grid = TRUE, col = "blue", ...) {
  
  obs.freq <- x[["bfd"]]$data.dist.freq
  exp.freq <- x[["bfd"]]$benford.dist.freq
  out <- list()
  
  xyplot.Berford(obs.freq, exp.freq, "Expected vs observed frequencies", "Observed Frequency", "Expected Frequency", grid, col)
  
  out$data <- data.frame(obs.freq, exp.freq)
  invisible(out)
}

plot.chi_squared <- function(x, grid = TRUE, col = "blue", ...) {
  squared.diff <- c(NA, x[["bfd"]]$squared.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, squared.diff)
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  needle.Benford(digits, squared.diff, "Chi-Squared Difference", xlab, "Chi-squared", grid, col)
  
  invisible(out)
}


plot.abs.diff <- function(x, grid = TRUE, col = "blue", ...) {
  absolute.diff <- c(NA, x[["bfd"]]$absolute.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, absolute.diff)
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  needle.Benford(digits, absolute.diff, "Absolute Difference", xlab, "Absolute Difference", grid, col)
  
  invisible(out)
}


plot.ex.summation <- function(x, grid = TRUE, col = "blue", ...) {
  abs.excess.summation <- c(NA, x[["bfd"]]$abs.excess.summation, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  out <- list()
  out$data <- data.frame(digits, abs.excess.summation)
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  needle.Benford(digits, abs.excess.summation, "Summation Difference", xlab, "Absolute Excess Summation", grid, col)
  
  invisible(out)
}


plot.legend <- function(x, err.bounds, size) {
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
           lty = c(rep(1, 2), 2),
           horiz = TRUE)
  }else{
    plot_colors <- c("lightblue","red")
    legend(x = "topright",
           inset = 0,
           legend = c("Observed Frequency", 
                      "Expected: Benford's Law"), 
           col = plot_colors, 
           cex = size,
           lwd = 2,
           lty = rep(1, 2),
           horiz = TRUE)
  }
}



