# Probability Apps: Shiny apps for exploring probability and statistics
# samplePop.R: Sample from a Population module
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

samplePop.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    inputPanel(
      selectInput(ns('dist'), label="Distribution", choices=c('Uniform', 'Normal', 'Skewed', 'Binary'), selected='Normal'),  # 'Custom'
      conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Binary'"), numericInput(ns('p'), 'Success Probability', 0.5, 0, 1, width='6em')),
      numericInput(ns('N'), label="Sample Size", value=100, min=1),
      actionButton(ns('run'), "Simulate"),
      checkboxInput(ns('animate'), label="Animate", value=T)
    ),
    fluidRow(
      column(8,
        plotOutput(ns('popPlot'), height=300),
        plotOutput(ns('popBoxplot'), height=100)),
      htmlOutput(ns('popDescr'))
    ),
    fluidRow(
      column(8,
        plotOutput(ns('sampPlot'), height=300),
        plotOutput(ns('sampBoxplot'), height=100)),
      htmlOutput(ns('sampDescr'))
    ))
}

samplePop.get.pop <- function(input) {
  if (input$dist == 'Normal')
    data.frame(x=1:100) %>% mutate(p=dnorm(x, 50, 15), p=p/sum(p))
  else if (input$dist == 'Skewed')
    data.frame(x=1:100) %>% mutate(p=dchisq(x/10, 4), p=p/sum(p))
  else if (input$dist == 'Binary' && input$p > 0 && input$p < 1)
    data.frame(x=c(0,100), p=c(1-input$p, input$p))
  else
    data.frame(x=1:100, p=.01)
}

samplePop.sim <- function(pop, N) sample(pop$x, N, T, pop$p)

samplePop <- function(input, output, session, colors) {
  rv <- reactiveValues(
    pop=NULL,
    popDescr=NULL,
    fullData=NULL,
    data=NULL,
    N=0
  )
  
  # Set the population distribution
  observe({
    rv$pop <- samplePop.get.pop(input)
    rv$popDescr <- data.frame(ymin=rv$pop$x[which(rv$pop$p > 0)[1]],
                              ymax=rv$pop$x[max(which(rv$pop$p > 0))],
                              q1=rv$pop$x[min(which(cumsum(rv$pop$p) >= .25))],
                              q2=rv$pop$x[min(which(cumsum(rv$pop$p) >= .5))],
                              q3=rv$pop$x[min(which(cumsum(rv$pop$p) >= .75))],
                              ymean=sum(rv$pop$x * rv$pop$p),
                              ysd=sqrt(sum(rv$pop$x**2 * rv$pop$p) - sum(rv$pop$x * rv$pop$p)**2)
    )
  })

  # Simulate
  observe({
    input$run
    rv$fullData <- samplePop.sim(rv$pop, isolate(input$N))
    rv$N <- 0
  })
  
  # Animate
  observe({
    if (isolate(rv$N) < length(rv$fullData)) {
      if (isolate(input$animate)) {
        rv$N <- isolate(rv$N) + 1
        rv$data <- rv$fullData[1:isolate(rv$N)]
        invalidateLater(10)
      } else {
        rv$data <- rv$fullData
        rv$N <- length(rv$fullData)
      }
    }
  })

  # Plot data
  output$popPlot <- renderPlot({
    ggplot(rv$pop) + geom_bar(aes(x, p), stat='identity', width=1) +
      theme(panel.background=element_blank(), axis.line=element_line('black'), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_text('Lato', size=14) )
  }, height=300)
  output$popBoxplot <- renderPlot({
    ggplot(rv$popDescr, aes(x=factor(1))) +
      geom_boxplot(aes(ymin=ymin, ymax=ymax, lower=q1, middle=q2, upper=q3), stat='identity') +
      geom_boxplot(aes(ymin=q1, ymax=q3, lower=q1, middle=q2, upper=q3), color=colors$quartile, outlier.size=0, outlier.stroke=0, stat='identity') +
      geom_segment(aes(x=1, xend=1, y=ymean-ysd/2, yend=ymean+ysd/2), color=colors$stdev, linetype='dotted') +
      geom_boxplot(aes(ymin=q2, ymax=q2, lower=q2, middle=q2, upper=q2), color=colors$median, stat='identity') +
      geom_boxplot(aes(ymin=ymean, ymax=ymean, lower=ymean, middle=ymean, upper=ymean), color=colors$mean, linetype='dotted', stat='identity') +
      coord_flip(ylim=c(0, 100)) +
      theme(panel.background=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank() )
  }, height=100)
  output$sampPlot <- renderPlot({
    if (length(rv$data) == 0) ggplot(rv$data) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$data)) + geom_bar(aes(x), width=1) + xlim(-1, 101) + 
      theme(panel.background=element_blank(), axis.line=element_line('black'), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_text('Lato', size=14) )
  }, height=300)
  output$sampBoxplot <- renderPlot({
    if (length(rv$data) == 0) ggplot(rv$data) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$data), aes(x=factor(1), y=x)) +
      #stat_boxplot(geom='errorbar', width=0.5) +
      geom_boxplot() +
      geom_boxplot(aes(ymin=..lower.., ymax=..upper..), color=colors$quartile, outlier.size=0, outlier.stroke=0) +
      geom_segment(aes(x=1, xend=1, y=mean(x)-sd(x)/2, yend=mean(x)+sd(x)/2), color=colors$stdev, linetype='dotted') +
      geom_boxplot(aes(y=median(x)), color=colors$median) +
      geom_boxplot(aes(y=mean(x)), color=colors$mean, linetype='dotted') +
      coord_flip(ylim=c(0, 100)) +
      theme(panel.background=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank() )
  }, height=100)
  
  # Descriptive statistics
  output$popDescr <- renderUI(tags$table(class='center',
                                         tags$tr(tags$td(colspan=2, tags$strong("Population Distribution"))),
                                         tags$tr(                  tags$td("Minimum:"),             tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$ymin))),
                                         tags$tr(class='quartile', tags$td("First Quartile:"),      tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$q1))),
                                         tags$tr(class='median',   tags$td("Median:"),              tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$q2))),
                                         tags$tr(class='mean',     tags$td("Mean:"),                tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$ymean))),
                                         tags$tr(class='quartile', tags$td("Third Quartile:"),      tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$q3))),
                                         tags$tr(                  tags$td("Maximum:"),             tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$ymax))),
                                         tags$tr(class='stdev',    tags$td('Std. Dev.:'),           tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$ysd))),
                                         tags$tr(class='quartile', tags$td('Interquartile Range:'), tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$q3-rv$popDescr$q1))),
                                         tags$tr(                  tags$td('Range:'),               tags$td(style='text-align:right', sprintf("%.2f", rv$popDescr$ymax-rv$popDescr$ymin)))
  ))
  output$sampDescr <- renderUI(tags$table(class='center',
                                          tags$tr(tags$td(colspan=2, tags$strong("Data Distribution"))),
                                          tags$tr(                  tags$td("Minimum:"),             if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", min(rv$data)))),
                                          tags$tr(class='quartile', tags$td("First Quartile:"),      if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$data, .25)))),
                                          tags$tr(class='median',   tags$td("Median:"),              if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", median(rv$data)))),
                                          tags$tr(class='mean',     tags$td("Mean:"),                if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", mean(rv$data)))),
                                          tags$tr(class='quartile', tags$td("Third Quartile:"),      if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$data, .75)))),
                                          tags$tr(                  tags$td("Maximum:"),             if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", max(rv$data)))),
                                          tags$tr(class='stdev',    tags$td('Std. Dev.:'),           if (length(rv$data) > 1) tags$td(style='text-align:right', sprintf("%.2f", sd(rv$data)))),
                                          tags$tr(class='quartile', tags$td('Interquartile Range:'), if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(quantile(rv$data, c(.25,.75)))))),
                                          tags$tr(                  tags$td('Range:'),               if (length(rv$data) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(range(rv$data)))))
  ))
  
}
