
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



### Plotfreqs should beony be called when we have
### either a) A new set of parameters and only one set of frequencies
### or b) We have added another set of data
# plotfreqs <- function() {
#   ngens <- length(freqs[[1]])
#   plot(1:ngens, freqs[[1]], type="l", axes=FALSE)
#   axis(1)
#   axis(2)
#   if (length(freqs)>1)
#     lapply(freqs[-1], function(x) lines(1:ngens, x) )
# }


library(shiny)
library(rje)    ## for colours

shinyServer(function(input, output) {
  
  v <- reactiveValues(doPlot=FALSE)
  
  output$showSnakes <- renderPlot({
    par(mar=c(0,2,0,0), mgp=c(0,0,0))      ## fix margins
    ## get parameters
    n <- as.integer(isolate(input$n))                 
    gens <- as.integer(isolate(input$gens))
    input$run
    if (isolate(input$two_colours)) {
      cols <- sort(rep(cubeHelix(10)[c(3,9)], length.out=n))
    } else {
      cols <- cubeHelix(n)
    }
    ## blank plots 
    layout(matrix(c(1,2), ncol=1), heights=c(3,1))
    freq <- numeric(gens)
    plot(1, type="n", ylim=c(1-0.5, n+0.5), xlim=c(0.2, gens), axes=FALSE, ylab="", xlab="")
    #axis(1, at=1:gens - 0.5, labels=1:gens, tick=FALSE, cex=1.2)
    ybottom <- seq((1:n))-0.3
    ytop <- ybottom + 0.6
    ## initial population
    rect(0.2, ybottom, 0.8, ytop, col=cols)
    prev <- 1:n
    freq[1] <- sum(prev <= n/2)/n
    for (i in 2:gens) {
      samp <- sort(sample(1:n, replace=TRUE))
      curr <- sort(prev[samp])
      if (gens<=20)
        arrows(i-1.18, samp , i-0.82, 1:n, lwd=2, length=0.1, col=grey(0.4))  
      rect(i-0.8, ybottom, i-0.2, ytop, col=cols[curr])
      prev <- curr
      freq[i] <- sum(curr<=n/2)/n
    }
    par(mar=c(2, 2, 0, 0), mgp=c(1, 0.5, 0))      ## fix margins
    colours <- rep(1, gens)
    colours[freq==0] <- 2
    colours[freq==1] <- 3
    plot(NULL, xlim=c(0.2, gens), ylim=c(0,1), axes=FALSE,xlab="",ylab="")
    segments(0,0,gens,0, col="lightgrey")
    segments(0,0.5,gens,0.5, col="lightgrey")
    segments(0, 1,gens, 1, col="lightgrey")
    points((1:gens)-0.5, freq, type="b",col=colours, cex=2, pch=18)

       axis(1, at=1:gens - 0.5, labels=1:gens, tick=FALSE, cex=1.2)
       
    axis(2, at=c(0,0.5,1))
  })
})
