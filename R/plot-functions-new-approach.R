

histogram.Benford.new <- function(x,
                              obs.freq = "digits",
                              freq = T,
                              main = NULL,
                              xlab = "Digits",
                              ylab = "Frequency",
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE, ...){
  
  digits <- x[["bfd"]]$digits
  
  if(freq){
    switch(obs.freq,
           "digits" = {
             obs.freq <- x[["bfd"]]$data.dist.freq
             exp.freq <- x[["bfd"]]$benford.dist.freq
           },
           "second order" = {
             obs.freq <- x[["bfd"]]$data.second.order.dist.freq
             exp.freq <- x[["bfd"]]$benford.so.dist.freq
           },
           "summation" = {
             obs.freq <- x[["bfd"]]$data.summation
             exp.freq <- rep(mean(obs.freq), length(digits))
           }
    )
  }else{
    switch(obs.freq,
           "digits" = {
             obs.freq <- x[["bfd"]]$data.dist
             exp.freq <- x[["bfd"]]$benford.dist
           },
           "second order" = {
             obs.freq <- x[["bfd"]]$data.second.order.dist
             exp.freq <- x[["bfd"]]$benford.so.dist
           },
           "summation" = {
             obs.freq <- x[["bfd"]]$data.summation
             exp.freq <- rep(mean(obs.freq), length(digits))
           }
    )
  }

  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, x$info$n, alpha, rootogram = FALSE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(0, max(c(obs.freq, exp.freq, ub))*1.1)
  }else{
    ylim <- c(0, max(c(obs.freq, exp.freq))*1.1)
  }
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  
  xmarks <- seq(0.7, length(exp.freq)*1.2, 1.2)
  plot(xmarks, obs.freq,
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

rootogram.Benford.new <- function(x,
                              obs.freq = "digits",
                              freq = TRUE,
                              main = NULL,
                              xlab = "Digits",
                              ylab = "Frequency",
                              grid = TRUE,
                              col.bar = "lightblue",
                              err.bounds = FALSE,
                              alpha = 0.05,
                              exp.benford = TRUE){
  digits <- x[["bfd"]]$digits
  
  if(freq){
    switch(obs.freq,
           "digits" = {
             obs.freq <- x[["bfd"]]$data.dist.freq
             exp.freq <- x[["bfd"]]$benford.dist.freq
           },
           "second order" = {
             obs.freq <- x[["bfd"]]$data.second.order.dist.freq
             exp.freq <- x[["bfd"]]$benford.so.dist.freq
           }
    )
  }else{
    switch(obs.freq,
           "digits" = {
             obs.freq <- x[["bfd"]]$data.dist
             exp.freq <- x[["bfd"]]$benford.dist
           },
           "second order" = {
             obs.freq <- x[["bfd"]]$data.second.order.dist
             exp.freq <- x[["bfd"]]$benford.so.dist
           }
    )
  }
  
  if(err.bounds){
    bounds <- compute.error.bounds(exp.freq, x$info$n, alpha, rootogram = TRUE, freq)
    ub <- bounds$ub
    lb <- bounds$lb
    ylim <- c(min(exp.freq - obs.freq, lb)*1.1, max(abs(exp.freq - obs.freq)*0.5, exp.freq, ub)*1.1)
  }else{
    ylim <- c(min(exp.freq - obs.freq)*1.1, max(abs(exp.freq - obs.freq)*0.5, exp.freq)*1.1)
  }
  
  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  
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

needle.Benford.new <- function(x,
                           discrepancy = "abs diff",
                           main = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           grid = TRUE,
                           col = "blue"){
  
  digits <- c(NA, x[["bfd"]]$digits, NA)
  
  disc <- switch(discrepancy,
           "chi squared" = c(NA, x[["bfd"]]$squared.diff, NA),
           "ex summation" = c(NA, x[["bfd"]]$abs.excess.summation, NA),
           "abs diff" = c(NA, x[["bfd"]]$absolute.diff, NA)
           )

  ord <- c("","-two","-tree", "-order")[ifelse(x$info$number.of.digits <= 3, x$info$number.of.digits, 4)]
  xlab <- paste0("First", ord, " Digits")
  
  xmarks <- seq(0.7, length(digits)*1.2, 1.2)
  plot(xmarks, disc, 
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
  points(xmarks, disc, pch = 19, col = col, cex = 0.5)
}
