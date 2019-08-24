
# main plot ---------------------------------------------------------------



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
##' @importFrom graphics abline axis barplot legend lines par plot
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
  
  available.plots <- c("digits", "rootogram digits", "second order", "rootogram second order", "summation", "mantissa", "chi squared", "ex summation", "abs diff", "none", "all")
  
  if (!is.null(select)) {
    
    select <- tolower(select)
    
    if (!all(select %in% available.plots)) {
      idx <- which(!select %in% available.plots)
      stop("Invalid plot name:", select[idx], "\nType ?plot.Benford for help.")
    }
    
    if (all(select == "all")) {
      plots <- available.plots[1:9]
    }else{
      plots <- select
    }
    
  } else {
    if (!is.null(except)) {
      if (!all(except %in% available.plots)) {
        idx <- which(!except %in% available.plots)
        stop("Invalid plot name: ", except[idx], "\nType ?plot.Benford for help.")
      }
      except <- tolower(except)
      if (all(except == "none")) {
        plots <- available.plots[1:9]
      }else{
        ap <- available.plots[1:9]
        plots <- ap[!(ap %in% except)]
      }
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
           "digits" = plotting.data.vs.benford(x, col.bar, grid, err.bounds, alpha, ...),
           "rootogram digits" = plotting.rootogram.data.vs.benford(x, col.bar, grid, err.bounds, alpha, ...),
           "second order" = plotting.second.order(x, col.bar, grid, ...),
           "rootogram second order" = plotting.rootogram.second.order(x, col.bar, grid, ...),
           "summation" = plotting.summation(x, col.bar, grid, ...),
           "mantissa" = plotting.ordered.mantissa(x, grid, ...),
           "chi squared" = plotting.chi_squared(x, grid, ...),
           "abs diff" = plotting.abs.diff(x, grid, ...),
           "ex summation" = plotting.ex.summation(x, grid, ...),
           "legend" = plotting.legend(x, err.bounds, lg_size),
           "blank" = plot.new()
    ) 
  }
  
}


# separate plots ----------------------------------------------------------


# duas tarefas diferentes
## 1) criar funções genéricas e flexiveis para cada lógica de plot, indpendnetemente dos dados
## 2) pode criar função genérica de arrumar plots


# modular

compute.error.bounds <- function(ep, n, alpha){
  ub <- n*ep + qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) + 1/2
  lb <- n*ep - qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) - 1/2
  data.frame(ub, lb)
}


#### plot ####
#' @importFrom graphics rect points arrows layout plot.new

# exportar para o usuário as funções genéricas
frequencyplot <- function(obs.freq, exp.freq, alpha, err.bounds, tuning graphical parameters){
  
  out <- list()
  out$data <- data.frame(obs.freq, exp.freq, err.bounds)
  out$params <- list(alpha = alpha)
  invisible(out)
}


rootogramplot <- function(obs.freq, exp.feq, tuning graphical parameters){
  
}


# essa aqui talvez não ?
plot.error.bounds <- function(bounds, type = c("lines", "arrows")){
  if (lines){
    lines()
  }
}

frequencyplot(datadist, benfordist)
frequencyplot(second.datadist, benfordist)
frequencyplot(lastwo, uniformdist)



histogram


plotting.data.vs.benford <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, ...) {
  y <- x[["bfd"]]$data.dist.freq
  bdf <- x[["bfd"]]$benford.dist.freq
  digits <- x[["bfd"]]$digits
  xmarks <- seq(0.7, length(bdf)*1.2, 1.2)
  plot(xmarks, y,
       main = "Digits distribution\nBarchart",
       xlab = "Digits", ylab = "Frequency",
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = c(0, max(c(y, bdf))*1.1),
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if(grid) {
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks,  labels = digits)
       }
  )
  barplot(y, 
          col = col.bar, 
          yaxt = "n", add = T)
  lines(xmarks, bdf, col = "red", lwd = 2)
  if(err.bounds){
    n <- x$info$n
    ep <- x[["bfd"]]$benford.dist
    ub <- n*ep + qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) + 1/2
    lb <- n*ep - qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) - 1/2
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
  }
}



plotting.second.order <- function(x, col.bar = "lightblue", grid = TRUE, ...) {
  y <- x[["bfd"]]$benford.so.dist.freq
  bfd <- x[["bfd"]]$data.second.order.dist.freq
  digits <- x[["bfd"]]$digits
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y,
       main = "Digits distribution\nSecond Order Test - Barchart",
       xlab = "Digits", ylab = "Frequency",
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = c(0, max(c(bfd, y))*1.1),
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks,  labels = digits)
       }
  )
  barplot(bfd,
          col = col.bar, 
          yaxt = "n", add = T)
  lines(xmarks, y, col = "red", lwd = 2)
}




plotting.rootogram.data.vs.benford <- function(x, col.bar = "lightblue", grid = TRUE, err.bounds = FALSE, alpha = 0.05, ...) {
  y <- x[["bfd"]]$data.dist.freq
  bdf <- x[["bfd"]]$benford.dist.freq
  digits <- x[["bfd"]]$digits
  xmarks <- seq(0.7, length(bdf)*1.2, 1.2)
  plot(xmarks, y,
       main = "Digits distribution\nRootogram",
       xlab = "Digits", ylab = "Frequency",
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = c(min(bdf - y)*1.1, max(abs(bdf - y)*0.5, bdf)*1.1),
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
       ybottom = bdf, ytop = bdf - y, col = col.bar)
  abline(h = 0)
  lines(xmarks, bdf, col = "red", lwd = 2)
  if(err.bounds){
    n <- x$info$n
    ep <- x[["bfd"]]$benford.dist
    ub <- qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) + 1/2
    lb <- -qnorm(1 - alpha/2)*sqrt(n*ep*(1 - ep)) - 1/2
    lines(ub ~ xmarks, lty = 2, col = 'red')
    lines(lb ~ xmarks, lty = 2, col = 'red')
    abline(h = 0, col = 'red')
  }
}

plotting.rootogram.second.order <- function(x, col.bar = "lightblue", grid = TRUE, ...) {
  y <- x[["bfd"]]$data.second.order.dist.freq
  bdf <- x[["bfd"]]$benford.so.dist.freq
  digits <- x[["bfd"]]$digits
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y,
       main = "Digits distribution\nSecond Order Test - Rootogram",
       xlab = "Digits", ylab = "Frequency",
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = c(min(bdf - y)*1.1, max(abs(bdf - y)*0.5, bdf)*1.1),
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
       ybottom = bdf, ytop = bdf - y, col = col.bar)
  abline(h = 0)
  lines(xmarks, bdf, col = "red", lwd = 2)
}

plotting.summation <- function(x, col.bar = "lightblue", grid = TRUE, ...) {
  y <- x[["bfd"]]$data.summation
  digits <- x[["bfd"]]$digits
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y,
       main = "Summation Distribution by digits",
       xlab = "Digits", ylab = "Summation",
       xlim = c(floor(xmarks[1]), ceiling(xmarks[length(xmarks)])),
       ylim = c(0, max(c(y, y))*1.1),
       yaxs = 'i', xaxs = 'i', xaxt = "n", type = 'n',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 90, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       }
  )
  barplot(y,
          col = col.bar, 
          yaxt = "n", add = T)
  lines(xmarks, rep(mean(y), length(xmarks)), col = "red", lwd = 2)
}

plotting.ordered.mantissa <- function(x, grid = TRUE, ...) {
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

plotting.chi_squared <- function(x, grid = TRUE, ...) {
  y <- c(NA, x[["bfd"]]$squared.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y, 
       col = "blue",
       xlab = "Digits",
       ylab = "Chi-squared", 
       main = "Chi-Squared Difference",
       xaxt = "n",
       xaxs='i',
       type = 'h',
       cex.axis = 0.8,
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 92, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       })
  points(xmarks, y, pch = 19, col = "blue", cex = 1/x$info$number.of.digits)
}

plotting.abs.diff <- function(x, grid = TRUE, ...) {
  y <- c(NA, x[["bfd"]]$absolute.diff, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y,
       col = "blue",
       xlab = "Digits",
       ylab = "Absolute Difference", 
       main = "Absolute Difference",
       xaxt = "n",
       xaxs='i',
       type = 'h',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 92, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       })
  points(xmarks, y, pch = 19, col = "blue", cex = 1/x$info$number.of.digits)
}

plotting.ex.summation <- function(x, grid = TRUE, ...) {
  y <- c(NA, x[["bfd"]]$abs.excess.summation, NA)
  digits <- c(NA, x[["bfd"]]$digits, NA)
  xmarks <- seq(0.7, length(y)*1.2, 1.2)
  plot(xmarks, y,
       col = "blue",
       xlab = "Digits",
       ylab = "Absolute Excess Summation", 
       main = "Summation Difference",
       xaxt = "n",
       xaxs='i',
       type = 'h',
       panel.first = {
         if(grid){
           grid(nx = NA, ny = NULL, lty = 1, col = "gray90")
           axis(1, at = xmarks[seq(1, length(xmarks), ifelse(length(digits) <= 92, 1, 10))], tck = 1, col.ticks = "gray90", labels = F)
         }
         axis(1, at = xmarks, labels = digits)
       })
  points(xmarks, y, pch = 19, col = "blue", cex = 1/x$info$number.of.digits)
}

plotting.legend <- function(x, err.bounds, size) {
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



