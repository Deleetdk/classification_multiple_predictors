
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

min = -3
max = 3

shinyUI(fluidPage(

  # Application title
  titlePanel("Classification with multiple predictors"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("presets",
                  "Preset configurations",
                  choices = c("Hyde 2005 (default)" = "Hyde",
                              "Uniform",
                              "Large SD" = "Large_SD")),
      sliderInput("X1",
                  "X1 d",
                  min = min,
                  max = max,
                  value = .25,
                  step = .05),
      sliderInput("X2",
                  "X2 d",
                  min = min,
                  max = max,
                  value = -.20,
                  step = .05),
      sliderInput("X3",
                  "X3 d",
                  min = min,
                  max = max,
                  value = .05,
                  step = .05),
      sliderInput("X4",
                  "X4 d",
                  min = min,
                  max = max,
                  value = -.50,
                  step = .05),
      sliderInput("X5",
                  "X5 d",
                  min = min,
                  max = max,
                  value = .10,
                  step = .05),
      sliderInput("X6",
                  "X6 d",
                  min = -3,
                  max = 3,
                  value = -.15,
                  step = .05),
      sliderInput("X7",
                  "X7 d",
                  min = min,
                  max = max,
                  value = .1,
                  step = .05),
      sliderInput("X8",
                  "X8 d",
                  min = min,
                  max = max,
                  value = -.15,
                  step = .05),
      sliderInput("X9",
                  "X9 d",
                  min = min,
                  max = max,
                  value = 1,
                  step = .05)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      HTML("<p>Classification is a broad category statistics involving categorical dependent variables. In other words, it involves using some predictors to correctly assign units to <em>classes</em>, hence the name.</p>",
           "<p>If gender is viewed as a binary category, it is a classification problem with two groups. <a href='http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.374.1723&rep=rep1&type=pdf'>Hyde (2005)</a> has famously argued that the genders are generally similar, not different. She did this by doing a meta-analysis of meta-analyses of gender differences. Most of the differences reported were small to medium, a few were large. She doesn't calculate the mean and SD but they are .275 d and .318 (using absulate values).</p>",
           "<p>In this simulation we consider 9 traits in which two groups differ. This can be any two groups, but it is modeled on Hyde's gender analysis. The default values are set such that the mean abs. d is .278 and the SD .301, close to the actual values. The <em>Density plots</em> tab shows the 9 density plots by gender. Some show noticeable differences, others don't -- just like the real data.</p>",
           "<p>The <em>logistic plots</em> tab shows the result of a logistic regression on each variable. The colored line shows the probability that a person with the given level of the trait belongs to the blue group (Y = 1). We can note that the regression line is linear when the difference in the trait is small, but becomes increasingly non-linear as the difference increases.</p>",
           "<p>The <em>Accuracy table</em> tab shows the results of a logistic model using all 9 predictors to classify persons into the two groups. It is the proportion version of the contingency table. Each cell shows the proportion of cases belonging to that combination. The top-left cell has the proportion of cases where the model has correctly predicted that the person belongs to the red group. Adding the top-left and the bottom-right cells yields the overall accuracy rate. For the default data, this is about 72%.</p>",
           "<p>Try playing around with the values to get a feel for how group differences in multiple traits relates to the predictability of group membership. You can explore the three preset settings or try your own. Note that the three premade configurations have about the same mean abs. d. However, the configurations with a few large differences improve the predictability much more than it does having many smaller differences. In practice, this means that if there are just a few large differences, correctly predicting the gender of given person given their trait values becomes easy.</p>",
           ""),
      tabsetPanel(
        tabPanel("Density plots",
                 splitLayout(
                   plotOutput("den_X1", height = "150px"),
                   plotOutput("den_X2", height = "150px"),
                   plotOutput("den_X3", height = "150px")
                 ),
                 splitLayout(
                   plotOutput("den_X4", height = "150px"),
                   plotOutput("den_X5", height = "150px"),
                   plotOutput("den_X6", height = "150px")
                 ),
                 splitLayout(
                   plotOutput("den_X7", height = "150px"),
                   plotOutput("den_X8", height = "150px"),
                   plotOutput("den_X9", height = "150px")
                 )
        ),
        
        tabPanel("Logistic plots",
          splitLayout(plotOutput("logi_X1", height = "150px"),
                      plotOutput("logi_X2", height = "150px"),
                      plotOutput("logi_X3", height = "150px")
          ),
          splitLayout(plotOutput("logi_X4", height = "150px"),
                      plotOutput("logi_X5", height = "150px"),
                      plotOutput("logi_X6", height = "150px")
          ),
          splitLayout(plotOutput("logi_X7", height = "150px"),
                      plotOutput("logi_X8", height = "150px"),
                      plotOutput("logi_X9", height = "150px")
          )
        ),
        tabPanel("Accuracy table",
                 DT::dataTableOutput("accur_table"),
                 textOutput("accur_overall")
                 ),
        tabPanel("Effect size table",
                 DT::dataTableOutput("d_table"))
      )
    )
  )
))
