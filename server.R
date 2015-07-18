
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# functions ---------------------------------------------------------------

get_error_beta = function(x) {
  return(sqrt(1 - x^2))
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


# server ------------------------------------------------------------------

shinyServer(function(input, output) {
  
  reac_d = reactive({
    #sample
    n = 1e4
    
    #seed
    set.seed(45)
    
    #coefs
    X1 = input$X1
    X1_e = get_error_beta(X1)
    X2 = input$X2
    X2_e = get_error_beta(X2)
    X3 = input$X3
    X3_e = get_error_beta(X3)
    
    #generate
    {
      #target vector
      Y = c(rep(0, n/2), rep(1, n/2))
      Y = sample(Y)
      #pred 1
      X1 = Y * X1 + rnorm(n) * X1_e
      #pred 2
      X2 = Y * X2 + rnorm(n) * X2_e
      #pred 3
      X3 = Y * X3 + rnorm(n) * X3_e
    }
    
    d = data.frame(Y, X1, X2, X3)
    
    return(d)
  })
  
  reac_d2 = reactive({
    #fetch d
    d = reac_d()
    
    #fit
    fit = glm(Y ~ X1 + X2 + X3, data = d, family = "binomial")
    Y_hat = fit$fitted.values
    Y_hat_rounded = round(fit$fitted.values)
    prop_table = prop.table(table(Y_hat_rounded, d$Y))
    
    #df
    d2 = data.frame(permutations(2, 2, v = 0:1, repeats.allowed = T),
                    round(as.vector(prop_table), digits = 3))
    
    #fix names
    colnames(d2) = c("x", "y", "prob")
    
    #add more vars
    d2 = mutate(d2,
                x_pos = x - .5,
                y_pos = y - .5,
                prob_p = percent(prob, digits = 1))
    
    return(d2)
  })

  output$table <- DT::renderDataTable({
    #fetch data
    d = reac_d()
    
    #fit
    fit = glm(Y ~ X1 + X2 + X3, data = d, family = "binomial")
    
    #probable scores
    Y_hat = fit$fitted.values
    
    #most likely scores
    Y_hat_rounded = round(fit$fitted.values)
    
    #proportion table
    prop_table = prop.table(table(Y_hat_rounded, d$Y))
    
    #add margin sums
    prop_table = addmargins(prop_table)
    
    #as df
    prop_table = as.data.frame(matrix(as.vector(prop_table), nrow = 3))
    
    #names
    colnames(prop_table) = c("Y=0", "Y=1", "sum")
    rownames(prop_table) = c("Y pred.=0", "Y pred.=1", "sum")

    return(prop_table)
  },
  options = list(searching = F,
                 ordering = F,
                 paging = F,
                 info = F))
  
  output$plot_X1 = renderPlot({
    #fetch data
    d = reac_d()
    
    #fit
    fit_X1 = glm(Y ~ X1, data = d, family = "binomial")
    
    #X1
    d_X1 = data.frame(X1 = seq(min(d$X1), max(d$X1), sd(d$X1)/100))
    d_X1$fit = predict(fit_X1, newdata = d_X1, type = "link")
    d_X1$fit = fit_X1$family$linkinv(d_X1$fit) #rescale to 0-1
    
    #plot
    ggplot(d, aes(X1, Y)) +
      geom_point() +
      geom_line(data = d_X1, aes(x = X1, y = fit), color = "red", size = 1)
  })
  
  output$plot_X2 = renderPlot({
    #fetch data
    d = reac_d()
    
    #fit
    fit_X2 = glm(Y ~ X2, data = d, family = "binomial")
    
    #X2
    d_X2 = data.frame(X2 = seq(min(d$X2), max(d$X2), sd(d$X2)/100))
    d_X2$fit = predict(fit_X2, newdata = d_X2, type = "link")
    d_X2$fit = fit_X2$family$linkinv(d_X2$fit) #rescale to 0-1
    
    #plot
    ggplot(d, aes(X2, Y)) +
      geom_point() +
      geom_line(data = d_X2, aes(x = X2, y = fit), color = "blue", size = 1)
  })
  
  output$plot_X3 = renderPlot({
    #fetch data
    d = reac_d()
    
    #fit
    fit_X3 = glm(Y ~ X3, data = d, family = "binomial")
    
    #X3
    d_X3 = data.frame(X3 = seq(min(d$X3), max(d$X3), sd(d$X3)/100))
    d_X3$fit = predict(fit_X3, newdata = d_X3, type = "link")
    d_X3$fit = fit_X3$family$linkinv(d_X3$fit) #rescale to 0-1
    
    #plot
    ggplot(d, aes(X3, Y)) +
      geom_point() +
      geom_line(data = d_X3, aes(x = X3, y = fit), color = "green", size = 1)
  })
})
