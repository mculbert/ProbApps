# Probability Apps: Shiny apps for exploring probability and statistics
# randNum.R: Random Numbers module
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

randNum.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    wellPanel(fluidRow(
      column(6, 
             selectInput(ns('dist'), label="Distribution", choices=c('Integers', 'Decimals', 'Urn', 'Binary', 'Normal'), selected='Integers'),
             conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Integers' || input['", ns('dist'), "'] == 'Text'"), checkboxInput(ns('replace'), 'With Replacement', value=T)),
             conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Integers'"), numericInput(ns('max'), 'Maximum', 10, min=1, width='6em')),
             conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Binary'"), numericInput(ns('p'), 'Success Probability', 0.5, 0, 1, width='6em')),
             conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Urn'"), textAreaInput(ns('outcomes'), 'Outcomes', cols=20, rows=5))
             ),
      column(6,
             numericInput(ns('N'), label="Sample Size", value=10, min=1, width='6em'),
             actionButton(ns('run'), "Simulate"),
             actionButton(ns('clear'), "Clear"),
             checkboxInput(ns('reset'), 'Reset', value=T)
      )
    )),
    uiOutput(ns('log'))
  )
}

randNum <- function(input, output, session, colors) {
  rv <- reactiveValues(
    data=NULL
  )

  # Clear the current data
  observe({
    input$clear
    rv$data <- NULL
  })
  
  # Simulate
  observeEvent(input$run, {
    if (input$reset) rv$data <- NULL
    if (input$dist == 'Integers') {
      if (!is.na(as.numeric(input$max)) && input$max > 0 && !is.na(as.numeric(input$N)) && input$N > 0)
        rv$data <- c(rv$data, sample.int(input$max, ifelse(input$replace, input$N, min(input$N, input$max)), input$replace))
    } else if (input$dist == 'Decimals') {
      if (!is.na(as.numeric(input$N)) && input$N > 0)
        rv$data <- c(rv$data, runif(input$N))
    } else if (input$dist == 'Urn') {
      if (input$outcomes != '' && !is.na(as.numeric(input$N)) && input$N > 0) {
        outcomes <- strsplit(input$outcomes, '\n')[[1]]
        rv$data <- c(rv$data, sample(outcomes, ifelse(input$replace, input$N, min(input$N, length(outcomes))), input$replace))
      }
    } else if (input$dist == 'Binary') {
      if (!is.na(as.numeric(input$p)) && input$p > 0 && input$p < 1 && !is.na(as.numeric(input$N)) && input$N > 0)
        rv$data <- c(rv$data, sample(c('Success', 'Failure'), input$N, T, c(input$p, 1-input$p)))
    } else {   # Normal
      if (!is.na(as.numeric(input$N)) && input$N > 0)
        rv$data <- c(rv$data, rnorm(input$N))
    }
  })

  # Show outcomes list
  output$log <- renderUI({
    tags$textarea(paste(rv$data, collapse='\n'), rows=20, cols=20, readonly=T)
  })

}
