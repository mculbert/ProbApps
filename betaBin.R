# Probability Apps: Shiny apps for exploring probability and statistics
# betaBin.R: Beta-Binomial Bayesian module
# Copyright 2016 Michael J. Culbertson <culbert1@illinois.edu>
# 
# Probability Apps is free software: you can redistribute it and/or modify it
# under the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your option)
# any later version.
# 
# Probability Apps is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License
# for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with Probability Apps.  If not, see <http://www.gnu.org/licenses/>.

library(dplyr)
library(shiny)
library(ggplot2)

betaBin.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, wellPanel(
        radioButtons(ns('Mode'), 'Mode', c('Manual', 'Simulate'), inline=T),
        conditionalPanel(condition=paste0("input['", ns('Mode'), "'] == 'Manual'"),
                         actionButton(ns('succ'), 'Success'),
                         actionButton(ns('fail'), 'Failure') ),
        conditionalPanel(condition=paste0("input['", ns('Mode'), "'] == 'Simulate'"),
                         numericInput(ns('p'), 'Success Probability', 0.5, 0, 1, width='6em'),
                         numericInput(ns('N'), 'Number of Trials', 10, 0, width='6em'),
                         actionButton(ns('run'), 'Simulate') ),
        #"Prior:",
          numericInput(ns('a'), 'a', 1, 0, width='6em'), 
          numericInput(ns('b'), 'b', 1, 0, width='6em'),
        actionButton(ns('clear'), 'Reset')
      )),
      column(6, textOutput(ns('MAP')), plotOutput(ns('plot')) ),
      column(2, uiOutput(ns('log')))
    ))
}

betaBin <- function(input, output, session, colors) {
  rv <- reactiveValues(
    data=NULL
  )

  # Clear the current data
  observeEvent(input$clear, { rv$data <- NULL })
  
  # Manual data
  observeEvent(input$succ, { rv$data <- c(rv$data, T) })
  observeEvent(input$fail, { rv$data <- c(rv$data, F) })
  
  # Simulate
  observeEvent(input$run, {
    if (input$p > 0 && input$p < 1 && input$N > 0) {
      rv$data <- c(rv$data, runif(input$N) < input$p)
    }
  })

  # Plot data
  output$MAP <- renderText({ 
    if (length(rv$data) > 0)
      sprintf('Posterior Mode = %.3f', (sum(rv$data)+input$a-1)/(length(rv$data)+input$b-1) )
    else ''
  })
  output$plot <- renderPlot({
    validate(need(input$Mode == 'Manual' || (input$p > 0 && input$p < 1),
                  "Success probability must be between 0 and 1"),
             need(input$Mode == 'Manual' || input$N > 0, 
                  "Number of trials must be greater than 0"),
             need(input$a > 0 && input$b > 0,
                  "a and b must be > 0"))
    ggplot(data.frame(x=(0:100)/100) %>% 
             mutate(prior=dbeta(x, 
                                ifelse(length(rv$data)>1, sum(rv$data[-length(rv$data)]), 0) + input$a,
                                ifelse(length(rv$data)>1, length(rv$data)-1-sum(rv$data[-length(rv$data)]), 0) + input$b),
                    posterior=dbeta(x, sum(rv$data)+input$a, length(rv$data)-sum(rv$data)+input$b))) +
      geom_line(aes(x, prior), linetype='dashed') +
      geom_line(aes(x, posterior), linetype=ifelse(length(rv$data) > 0, 'solid', 'blank')) +
      labs(x='p', y='Density') +
      theme(panel.background=element_blank(), axis.line=element_line('black'), 
            title=element_text('Lato', size=16), axis.text=element_text('Lato', size=12),
            axis.text.y=element_blank(), axis.ticks.y=element_blank() )
  })

  # Show outcomes list
  output$log <- renderUI({
    tags$textarea(paste(if (length(rv$data) > 0) c('Failure', 'Success')[1+rv$data],
                        collapse='\n'), rows=20, cols=10, readonly=T)
  })

}
