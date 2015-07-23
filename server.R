
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

get_color = function(x, y) {
  c = brewer_pal("qual", palette = 3)(y)[x]
  return(c)
}

plot_logi = function(d, outcome_var) {
  #for easiness
  d$X = d[, outcome_var]
  
  #color
  color = get_color(as.numeric(str_sub(outcome_var, 2)), 9)
  
  #fit
  fit = glm(Y ~ X, data = d, family = "binomial")
  
  #curve
  d_curve = data.frame(X = seq(min(d$X), max(d$X), sd(d$X)/100) )
  d_curve$fit = predict(fit, newdata = d_curve, type = "link")
  d_curve$fit = fit$family$linkinv(d_curve$fit) #rescale to 0-1
  
  #plot
  ggplot(d, aes(X, Y)) +
    geom_point() +
    geom_line(data = d_curve, aes(x = X, y = fit),
              color = color,
              size = 1)
}

plot_den = function(d, outcome_var) {
  #set var
  d$X = d[, outcome_var]
  d = d[c(1, 11)]
  d$Y = as.factor(d$Y) #make factor
  
  #plot
  ggplot(d, aes(X, group = Y, color = Y)) +
    geom_line(data = d, aes(y = ..density..), stat = 'density') +
    scale_color_manual(values = c("blue", "red"), guide = F)
    
}

get_d = function(x, group) {
  library(psych)
  desc_x = describeBy(x, group)
  diff_mean_x = desc_x[[2]]$mean - desc_x[[1]]$mean
  wtd_sd_x = weighted.mean(c(desc_x[[1]]$sd, desc_x[[2]]$sd),
                           c(desc_x[[1]]$n, desc_x[[1]]$n))
  d_x = diff_mean_x / wtd_sd_x
  return(d_x)
}

# server ------------------------------------------------------------------

shinyServer(function(input, output, clientData, session) {
  
  reac_d = reactive({
    #sample
    n = 1e4
    
    #generate
    {
    #target vector
    Y = c(rep(1, n/2), rep(0, n/2))
    #pred 1
    X1 = c(rnorm(n/2, input$X1), rnorm(n/2, 0))
    X2 = c(rnorm(n/2, input$X2), rnorm(n/2, 0))
    X3 = c(rnorm(n/2, input$X3), rnorm(n/2, 0))
    X4 = c(rnorm(n/2, input$X4), rnorm(n/2, 0))
    X5 = c(rnorm(n/2, input$X5), rnorm(n/2, 0))
    X6 = c(rnorm(n/2, input$X6), rnorm(n/2, 0))
    X7 = c(rnorm(n/2, input$X7), rnorm(n/2, 0))
    X8 = c(rnorm(n/2, input$X8), rnorm(n/2, 0))
    X9 = c(rnorm(n/2, input$X9), rnorm(n/2, 0))
    }
    
    d = data.frame(Y, X1, X2, X3, X4, X5, X6, X7, X8, X9)
    
    #shuffle rows
    d = d[sample(nrow(d)), ]
    
    return(d)
  })
  
  reac_prop_table = reactive({
    #fetch data
    d = reac_d()
    
    #fit
    fit = glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = d, family = "binomial")
    
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
    colnames(prop_table) = c("Y = 0", "Y = 1", "sum")
    rownames(prop_table) = c("Y pred. = 0", "Y pred. = 1", "sum")
    
    return(prop_table)
  })
  
  observe({
    #get preset
    preset = input$presets
    
    print(preset)
    
    #Hyde
    if (preset == "Hyde") {
      vals = c(.25,
               -.20,
               .05,
               -.50,
               .1,
               .15,
               .1,
               .15,
               1)
      
      updateSliderInput(session,
                        "X1",
                        value = vals[1])
      updateSliderInput(session,
                        "X2",
                        value = vals[2])
      updateSliderInput(session,
                        "X3",
                        value = vals[3])
      updateSliderInput(session,
                        "X4",
                        value = vals[4])
      updateSliderInput(session,
                        "X5",
                        value = vals[5])
      updateSliderInput(session,
                        "X6",
                        value = vals[6])
      updateSliderInput(session,
                        "X7",
                        value = vals[7])
      updateSliderInput(session,
                        "X8",
                        value = vals[8])
      updateSliderInput(session,
                        "X9",
                        value = vals[9])
    }
    
    #Uniform
    if (preset == "Uniform") {
      print("if clause works")
      vals = rep(.25, 9)
      
      updateSliderInput(session,
                        "X1",
                        value = vals[1])
      updateSliderInput(session,
                        "X2",
                        value = vals[2])
      updateSliderInput(session,
                        "X3",
                        value = vals[3])
      updateSliderInput(session,
                        "X4",
                        value = vals[4])
      updateSliderInput(session,
                        "X5",
                        value = vals[5])
      updateSliderInput(session,
                        "X6",
                        value = vals[6])
      updateSliderInput(session,
                        "X7",
                        value = vals[7])
      updateSliderInput(session,
                        "X8",
                        value = vals[8])
      updateSliderInput(session,
                        "X9",
                        value = vals[9])
    }
    
    #Large SD
    if (preset == "Large_SD") {
      vals = c(1.25, -1.25, rep(0, 7))
      
      updateSliderInput(session,
                        "X1",
                        value = vals[1])
      updateSliderInput(session,
                        "X2",
                        value = vals[2])
      updateSliderInput(session,
                        "X3",
                        value = vals[3])
      updateSliderInput(session,
                        "X4",
                        value = vals[4])
      updateSliderInput(session,
                        "X5",
                        value = vals[5])
      updateSliderInput(session,
                        "X6",
                        value = vals[6])
      updateSliderInput(session,
                        "X7",
                        value = vals[7])
      updateSliderInput(session,
                        "X8",
                        value = vals[8])
      updateSliderInput(session,
                        "X9",
                        value = vals[9])
    }
    
    
  })
  
  output$accur_table = DT::renderDataTable({
    #fetch data
    d = reac_prop_table()
    
    return(d)
  },
  options = list(searching = F,
                 ordering = F,
                 paging = F,
                 info = F))
  
  output$accur_overall = renderText({
    #fetch data
    d = reac_prop_table()
    
    accur = d[1, 1] + d[2, 2]
    str = str_c("The overall accuracy is ", accur)
    return(str)
  })
  
  output$d_table = DT::renderDataTable({
    #fetch d
    d = reac_d()
    
    #calculate d for each trait
    trait_names = colnames(d)[-1] #skip Y
    
    #d values
    d_val = numeric(length = length(trait_names))
    for (n in seq_along(trait_names)) {
      d_val[n] = get_d(d[, n + 1], d$Y)
    }
    names(d_val) = colnames(d)[-1]
    d_val = d_val
    
    #r values
    r_val = cor(d)[, 1][-1]
    
    #r2 values
    r2_val = r_val^2
    
    #combine
    d_out = data.frame(d = d_val,
                       r = r_val,
                       r2 = r2_val)
    
    #desc. stats
    d_out["Mean (abs.)", ] = apply(d_out, 2, function(x) mean(abs(x)))
    d_out["SD (abs.)", ] = apply(d_out, 2, function(x) sd(abs(x)))
    
    #round
    d_out = round(d_out, 2)
    
    return(d_out)
  },
  options = list(searching = F,
                 ordering = F,
                 paging = F,
                 info = F))
  
  output$logi_X1 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X1")
  })
  
  output$logi_X2 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X2")
  })
  
  output$logi_X3 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X3")
  })
  
  output$logi_X4 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X4")
  })
  
  output$logi_X5 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X5")
  })
  
  output$logi_X6 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X6")
  })
  
  output$logi_X7 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X7")
  })
  
  output$logi_X8 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X8")
  })
  
  output$logi_X9 = renderPlot({
    #fetch data
    d = reac_d()
    
    #plot
    plot_logi(d, "X9")
  })
  
  # Density plots -----------------------------------------------------------  
  
  output$den_X1 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X1")
  })
  
  output$den_X2 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X2")
  })
  
  output$den_X3 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X3")
  })
  
  output$den_X4 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X4")
  })
  
  output$den_X5 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X5")
  })
  
  output$den_X6 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X6")
  })
  
  output$den_X7 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X7")
  })
  
  output$den_X8 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X8")
  })
  
  output$den_X9 = renderPlot({
    #fetch d
    d = reac_d()
    
    #plot
    plot_den(d, "X9")
  })
  
})



