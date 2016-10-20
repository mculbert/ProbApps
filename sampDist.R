# Probability Apps: Shiny apps for exploring probability and statistics
# sampDist.R: Sampling Distributions module
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

sampDist.UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    inputPanel(
      selectInput(ns('dist'), label="Distribution", choices=c('Uniform', 'Normal', 'Skewed', 'Binary'), selected='Normal'),  # 'Custom'
      conditionalPanel(condition=paste0("input['", ns('dist'), "'] == 'Binary'"), numericInput(ns('p'), 'Success Probability', 0.5, 0, 1, width='6em')),
      numericInput(ns('N'), label="Sample Size", value=100, min=1),
      numericInput(ns('B'), label="Number of Simulations", value=10, min=1),
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
    ),
    fluidRow(
      column(8,
             plotOutput(ns('meansPlot'), height=300),
             plotOutput(ns('meansBoxplot'), height=100)),
      htmlOutput(ns('meansDescr'))
    ))
}

sampDist.get.pop <- function(input) {
  if (input$dist == 'Normal')
    data.frame(x=1:100) %>% mutate(p=dnorm(x, 50, 15), p=p/sum(p))
  else if (input$dist == 'Skewed')
    data.frame(x=1:100) %>% mutate(p=dchisq(x/10, 4), p=p/sum(p))
  else if (input$dist == 'Binary' && !is.na(as.numeric(input$p)) && input$p > 0 && input$p < 1)
    data.frame(x=c(0,100), p=c(1-input$p, input$p))
  else
    data.frame(x=1:100, p=.01)
}

sampDist.sim <- function(pop, N) sample(pop$x, N, T, pop$p)

sampDist <- function(input, output, session, colors) {
  rv <- reactiveValues(
    pop=NULL,
    popDescr=NULL,
    sample=NULL,
    means=NULL,
    N=0
  )
  
  # Set the population distribution
  observe({
    rv$pop <- sampDist.get.pop(input)
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
    rv$N <- isolate(input$B)
    rv$means <- NULL
  })
  
  # Animate
  observe({
    input$run
    if (isolate(rv$N) > 0) {
      if (isolate(input$animate)) {
        rv$sample <- sampDist.sim(isolate(rv$pop), isolate(input$N))
        rv$means <- c(isolate(rv$means), mean(isolate(rv$sample)))
        rv$N <- isolate(rv$N) - 1
        invalidateLater(10)
      } else {
        rv$means <- c(isolate(rv$means), 
                      sapply(1:(isolate(rv$N)-1), function(i) mean(sampDist.sim(isolate(rv$pop), isolate(input$N)))))
        rv$sample <- sampDist.sim(isolate(rv$pop), isolate(input$N))
        rv$means <- c(isolate(rv$means), mean(isolate(rv$sample)))
        rv$N <- 0
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
    if (length(rv$sample) == 0) ggplot(rv$sample) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$sample)) + geom_bar(aes(x), width=1) + xlim(-1, 101) + 
      theme(panel.background=element_blank(), axis.line=element_line('black'), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_text('Lato', size=14) )
  }, height=300)
  output$sampBoxplot <- renderPlot({
    if (length(rv$sample) == 0) ggplot(rv$sample) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$sample), aes(x=factor(1), y=x)) +
      #stat_boxplot(geom='errorbar', width=0.5) +
      geom_boxplot() +
      geom_boxplot(aes(ymin=..lower.., ymax=..upper..), color=colors$quartile, outlier.size=0, outlier.stroke=0) +
      geom_segment(aes(x=1, xend=1, y=mean(x)-sd(x)/2, yend=mean(x)+sd(x)/2), color=colors$stdev, linetype='dotted') +
      geom_boxplot(aes(y=median(x)), color=colors$median) +
      geom_boxplot(aes(y=mean(x)), color=colors$mean, linetype='dotted') +
      coord_flip(ylim=c(0, 100)) +
      theme(panel.background=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank() )
  }, height=100)
  output$meansPlot <- renderPlot({
    if (length(rv$means) == 0) ggplot(rv$means) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$means)) + geom_bar(aes(x), width=1) + xlim(-1, 101) + 
      theme(panel.background=element_blank(), axis.line=element_line('black'), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x=element_text('Lato', size=14) )
  }, height=300)
  output$meansBoxplot <- renderPlot({
    if (length(rv$means) == 0) ggplot(rv$means) + geom_blank() + theme(panel.background=element_blank())
    else ggplot(data.frame(x=rv$means), aes(x=factor(1), y=x)) +
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
                                          tags$tr(tags$td(colspan=2, tags$strong("Data Distribution (Most Recent Sample)"))),
                                          tags$tr(                  tags$td("Minimum:"),             if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", min(rv$sample)))),
                                          tags$tr(class='quartile', tags$td("First Quartile:"),      if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$sample, .25)))),
                                          tags$tr(class='median',   tags$td("Median:"),              if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", median(rv$sample)))),
                                          tags$tr(class='mean',     tags$td("Mean:"),                if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", mean(rv$sample)))),
                                          tags$tr(class='quartile', tags$td("Third Quartile:"),      if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$sample, .75)))),
                                          tags$tr(                  tags$td("Maximum:"),             if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", max(rv$sample)))),
                                          tags$tr(class='stdev',    tags$td('Std. Dev.:'),           if (length(rv$sample) > 1) tags$td(style='text-align:right', sprintf("%.2f", sd(rv$sample)))),
                                          tags$tr(class='quartile', tags$td('Interquartile Range:'), if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(quantile(rv$sample, c(.25,.75)))))),
                                          tags$tr(                  tags$td('Range:'),               if (length(rv$sample) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(range(rv$sample)))))
  ))
  output$meansDescr <- renderUI(tags$table(class='center',
                                           tags$tr(tags$td(colspan=2, tags$strong("Distribution of Sample Means"))),
                                           tags$tr(                  tags$td("Minimum:"),             if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", min(rv$means)))),
                                           tags$tr(class='quartile', tags$td("First Quartile:"),      if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$means, .25)))),
                                           tags$tr(class='median',   tags$td("Median:"),              if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", median(rv$means)))),
                                           tags$tr(class='mean',     tags$td("Mean:"),                if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", mean(rv$means)))),
                                           tags$tr(class='quartile', tags$td("Third Quartile:"),      if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", quantile(rv$means, .75)))),
                                           tags$tr(                  tags$td("Maximum:"),             if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", max(rv$means)))),
                                           tags$tr(class='stdev',    tags$td('Std. Dev.:'),           if (length(rv$means) > 1) tags$td(style='text-align:right', sprintf("%.2f", sd(rv$means)))),
                                           tags$tr(class='quartile', tags$td('Interquartile Range:'), if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(quantile(rv$means, c(.25,.75)))))),
                                           tags$tr(                  tags$td('Range:'),               if (length(rv$means) > 0) tags$td(style='text-align:right', sprintf("%.2f", diff(range(rv$means)))))
  ))
  
}
