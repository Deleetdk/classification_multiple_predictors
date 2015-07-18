
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Classification with multiple predictors"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("X1",
                  "X1, beta",
                  min = -1,
                  max = 1,
                  value = .2,
                  step = .05),
      sliderInput("X2",
                  "X2, beta",
                  min = -1,
                  max = 1,
                  value = .3,
                  step = .05),
      sliderInput("X3",
                  "X3, beta",
                  min = -1,
                  max = 1,
                  value = .4,
                  step = .05)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot_X1", width = "50%"),
      plotOutput("plot_X2", width = "40%"),
      HTML("<p>Some example text. Blah blah blah. More blah. Long text. Paragraph. kekekek!</p>")
#       tabsetPanel(
#         tabPanel("Logistic plots",
#           plotOutput("plot_X1", width = "50%")
# #           plotOutput("plot_X2", width = "50%"),
# #           plotOutput("plot_X3")
#         ),
#         tabPanel("Table",
#                  DT::dataTableOutput("table")
#                  )
#       )
    )
  )
))
