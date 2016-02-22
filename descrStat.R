# Probability Apps: Shiny apps for exploring probability and statistics
# descrStat.R: Descriptive statistics module
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

descrStat.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns('plot'), click=ns('plot.click'), dblclick=ns('plot.dblclick')),
    plotOutput(ns('boxplot'), height=100),
    fluidRow(
      column(8, htmlOutput(ns('text'))),
      column(4, inputPanel(
        numericInput(ns('min'), 'Minimum', 0, width='5em'),
        numericInput(ns('max'), 'Maximum', 25, width='5em'),
        actionButton(ns('rand'), 'Random Data'),
        actionButton(ns('clear'), 'Clear Data')
      ))
    ))
}

descrStat <- function(input, output, session, colors) {
  rv <- reactiveValues(
    data=data.frame(x=numeric())
  )
  
  # Clear the current data
  observe({
    input$clear
    rv$data <- slice(isolate(rv$data), 0)
  })
  
  # Generate some random data
  observe({
    input$rand
    rv$data <- data.frame(x=runif(30, isolate(input$min), isolate(input$max)))
  })
  
  # Plot data
  gg <- NULL
  output$plot <- renderPlot({
    if (length(rv$data$x) == 0)
      gg <<- ggplot(rv$data) + geom_blank() + 
        xlim(c(input$min, input$max)) + ylim(c(0,10))
    else
      gg <<- ggplot(rv$data, aes(x)) +
        geom_dotplot(method='histodot', binwidth=abs(input$max-input$min)/30) +
        coord_cartesian(xlim=c(input$min, input$max))
    gg <<- gg + theme(panel.background=element_blank(), axis.line=element_line('black'), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_text('Lato', size=14) )
    gg
  })
  output$boxplot <- renderPlot({
    if (length(rv$data$x) == 0) ggplot(rv$data) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(rv$data, aes(x=factor(1), y=x)) +
      stat_boxplot(geom='errorbar', width=0.5) +
      geom_boxplot() +
      geom_boxplot(aes(ymin=..lower.., ymax=..upper..), color=colors$quartile, outlier.size=0, outlier.stroke=0) +
      geom_segment(aes(x=1, xend=1, y=mean(x)-sd(x)/2, yend=mean(x)+sd(x)/2), color=colors$stdev, linetype='dotted') +
      geom_boxplot(aes(y=median(x)), color=colors$median) +
      geom_boxplot(aes(y=mean(x)), color=colors$mean, linetype='dotted') +
      coord_flip(ylim=c(input$min, input$max)) +
      theme(panel.background=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank() )
  }, height=100)
  
  # Add a data point (from click)
  observe({ rv$data <- bind_rows(isolate(rv$data), data.frame(x=input$plot.click$x)) })
  
  # Remove a data point (from double-click)
  observe({
    bw <- ggplot_build(gg)$data[[1]]$binwidth[1]
    sel <- which.min(abs(isolate(rv$data$x) - input$plot.dblclick$x))
    if (length(sel) > 0 && abs(isolate(rv$data$x[sel]) - input$plot.dblclick$x) <= bw)
      rv$data <- slice(isolate(rv$data), -sel)
  })
  
  # Summary statistics
  output$text <- renderUI(tags$table(class='center',
    tags$tr(                  tags$td("Minimum:"),             if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", min(rv$data$x)))),
    tags$tr(class='quartile', tags$td("First Quartile:"),      if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$data$x, .25)))),
    tags$tr(class='median',   tags$td("Median:"),              if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", median(rv$data$x)))),
    tags$tr(class='mean',     tags$td("Mean:"),                if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", mean(rv$data$x)))),
    tags$tr(class='quartile', tags$td("Third Quartile:"),      if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$data$x, .75)))),
    tags$tr(                  tags$td("Maximum:"),             if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", max(rv$data$x)))),
    tags$tr(class='stdev',    tags$td('Std. Dev.:'),           if (length(rv$data$x) > 1) tags$td(style='text-align:right', sprintf("%.2f", sd(rv$data$x)))),
    tags$tr(class='quartile', tags$td('Interquartile Range:'), if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(quantile(rv$data$x, c(.25,.75)))))),
    tags$tr(                  tags$td('Range:'),               if (length(rv$data$x) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(range(rv$data$x)))))
  ))
  
}