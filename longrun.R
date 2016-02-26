# Probability Apps: Shiny apps for exploring probability and statistics
# longrun.R: Long-run Probability module
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

longrun.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, wellPanel(
        numericInput(ns('p'), 'Success Probability', 0.5, 0, 1, width='6em'),
        numericInput(ns('N'), 'Number of Trials', 10, 0, width='6em'),
        actionButton(ns('run'), 'Simulate'),
        actionButton(ns('clear'), 'Reset'),
        checkboxInput(ns('animate'), 'Animate', value=T)
      )),
      column(6, textOutput(ns('cum')), plotOutput(ns('plot'))),
      column(2, uiOutput(ns('log')))
    ))
}

longrun <- function(input, output, session, colors) {
  rv <- reactiveValues(
    data=data.frame(x=logical(), cum=numeric(), i=integer(), cumProp=numeric())
  )

  # Clear the current data
  observe({
    input$clear ; input$p
    rv$data <- slice(isolate(rv$data), 0)
    rv$n <- 0
  })
  
  # Simulate
  observeEvent(input$run, {
    if (input$p > 0 && input$p < 1 && input$N > 0) {
      rv$data <- bind_rows(rv$data,
                           data.frame(x=(runif(input$N) < input$p)) %>%
                             mutate(cum=cumsum(x)+last(rv$data$cum, default=0),
                                    i=row_number()+last(rv$data$i, default=0),
                                    cumProp=cum/i) )
      if (!input$animate) rv$n <- nrow(rv$data)
    }
  })

  # Animate
  observe({
    if (isolate(rv$n) < nrow(rv$data)) {
      if (isolate(input$animate)) {
        rv$n <- isolate(rv$n) + 1
        invalidateLater(10)
      } else rv$n <- nrow(rv$data)
    }
  })
  
  # Plot data
  output$cum <- renderText({ 
    if (rv$n > 0)
      sprintf('Cumulative proportion = %g/%g = %.3f',
              rv$data$cum[rv$n], rv$n, rv$data$cumProp[rv$n])
    else ''
  })
  output$plot <- renderPlot({
    validate(need(input$p > 0 && input$p < 1,
                  "Success probability must be between 0 and 1"),
             need(input$N > 0, "Number of trials must be greater than 0"))
    ggplot(if (rv$n > 0) slice(rv$data, 1:rv$n) else rv$data, aes(i, cumProp)) + 
      geom_line() + ylim(0, 1) +
      geom_hline(yintercept=isolate(input$p), color=colors$mean) +
      labs(x='Trial Number', y='Cumulative Proportion') +
      theme(panel.background=element_blank(), axis.line=element_line('black'), 
            title=element_text('Lato', size=16), axis.text=element_text('Lato', size=12) )
  })

  # Show outcomes list
  output$log <- renderUI({
    tags$textarea(paste(if (rv$n > 0) c('Failure', 'Success')[1+rv$data$x[1:rv$n]],
                        collapse='\n'), rows=20, cols=10, readonly=T)
  })

}
