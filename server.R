library(shiny)
library(rje)    ## for colours

plotFreqs <- function(freq) {
  gens <- length(freq)

  plotpoints <- function(f) {
    colours <- rep(1, gens)
    colours[f==0] <- 2
    colours[f==1] <- 3
    points((1:gens)-0.5, f, col=colours, cex=2, pch=18)
    points((1:gens)-0.5, f, type="c")
  }
  
  par(mar=c(2, 2, 0, 0), mgp=c(1, 0.5, 0))      ## fix margins
  plot(NULL, xlim=c(0.2, gens), ylim=c(0,1), axes=FALSE,xlab="",ylab="")
  
  lapply(c(0,0.5,1), function(y) segments(0, y,gens, y, col="lightgrey"))
  
  plotpoints(freq)

  axis(1, at=1:gens - 0.5, labels=1:gens, tick=FALSE, cex=1.2)
  axis(2, at=c(0,0.5,1))
}

plotSnakes <- function(n, gens, colours) {
  par(mar=c(0,2,0,0), mgp=c(0,0,0))      ## fix margins
  freq <- numeric(gens)
  plot(1, type="n", ylim=c(1-0.5, n+0.5), xlim=c(0.2, gens), axes=FALSE, ylab="", xlab="")
  ybottom <- seq((1:n))-0.3
  ytop <- ybottom + 0.6
  ## initial population
  rect(0.2, ybottom, 0.8, ytop, col=colours)
  prev <- 1:n
  freq[1] <- sum(prev <= n/2)/n
  for (i in 2:gens) {
    samp <- sort(sample(1:n, replace=TRUE))
    curr <- sort(prev[samp])
    if (gens<=20)
      arrows(i-1.18, samp , i-0.82, 1:n, lwd=2, length=0.1, col=grey(0.4))  
    rect(i-0.8, ybottom, i-0.2, ytop, col=colours[curr])
    prev <- curr
    freq[i] <- sum(curr<=n/2)/n
  }
  return(freq)
}



shinyServer(function(input, output) {
  a <- reactiveValues()
  a$time=Sys.time()
  
  output$showSnakes <- renderPlot({
    ## isolated input
    n <- as.integer(isolate(input$n))                 
    gens <- as.integer(isolate(input$gens))
    if (isolate(input$two_colours)) {
      cols <- sort(rep(cubeHelix(10)[c(3,9)], length.out=n))
    } else {
      cols <- cubeHelix(n)
    }
    ## reactive input
    input$run

    ## set up the plot layout
    layout(matrix(c(1,2), ncol=1), heights=c(3,1))
    
    f <- plotSnakes(n, gens, cols)
    plotFreqs(f)
    a$time <- c(isolate(a$time), Sys.time())
    print( a$time)
  })
})
