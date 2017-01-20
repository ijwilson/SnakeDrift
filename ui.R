

library(shiny)

shinyUI(
  fluidPage(
  ## Application title
    title = "Snake Drift",
    fluidRow(
      h1("Snake Drift"),
      column(4,
      sliderInput("n",
                    "Population Size:",
                    min = 5,
                    max = 40,
                    value = 10) 
      ),
      column(4,
        sliderInput("gens",
                    "Generations:",
                    min = 5,
                    max = 40,
                    value = 10) 
      ),
      column(3,
             checkboxInput("two_colours", "Two Colours", value = FALSE)
      ),
      column(3,
             actionButton("run","Go!")
      )
    ),
    plotOutput("showSnakes", height="500px")
  )
)

