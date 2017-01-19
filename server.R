
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rje)    ## for colours

shinyServer(function(input, output) {
  
  v <- reactiveValues(doPlot=FALSE)
  
  output$showSnakes <- renderPlot({
    par(mar=c(1,0,0,0), mgp=c(0,0,0))      ## fix margins
    ## get parameters
    n <- as.integer(isolate(input$n))                 
    gens <- as.integer(isolate(input$gens))
    input$run
    if (isolate(input$two_colours)) {
      cols <- sort(rep(cubeHelix(10)[c(3,9)], length.out=n))
    } else {
      cols <- cubeHelix(n)
    }
    ## blank plot 
    ## If I also want another plot of frequencies I could use layout
    ## layout(matrix(c(1,2), ncol=1), heights=c(4,1))
    plot(1, type="n", ylim=c(1-0.5, n+0.5), xlim=c(0,gens), axes=FALSE, ylab="", xlab="")
    axis(1, at=1:gens - 0.5, labels=1:gens, tick=FALSE, cex=1.2)
    ybottom <- seq((1:n))-0.3
    ytop <- ybottom + 0.6
    ## initial population
    rect(0.2, ybottom, 0.8, ytop, col=cols)
    prev <- 1:n
    for (i in 2:gens) {
      samp <- sort(sample(1:n, replace=TRUE))
      curr <- sort(prev[samp])
      arrows(i-1.18, samp , i-0.82, 1:n, lwd=2, length=0.1, col=grey(0.4))  
      rect(i-0.8, ybottom, i-0.2, ytop, col=cols[curr])
      prev <- curr
    }
  })
})
