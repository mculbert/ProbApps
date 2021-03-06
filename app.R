# Probability Apps: Shiny apps for exploring probability and statistics
# app.R: Main Shiny app
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

library(shiny)

# Match CSS colors
colorList <- list(
  mean=rgb(0xFF, 0x00, 0x00, maxColorValue=0xFF),    # red
  median=rgb(0xFF, 0x8C, 0x00, maxColorValue=0xFF),  # dark orange
  quartile=rgb(0x0, 0x80, 0x00, maxColorValue=0xFF), # green
  stdev=rgb(0x00, 0x00, 0xFF, maxColorValue=0xFF)    # blue
)

# Load component modules
source('descrStat.R')
source('longrun.R')
source('samplePop.R')
source('sampDist.R')
source('randNum.R')
# source('regr.R')
source('betaBin.R')


ui <- shinyUI(fluidPage(
  tags$head(includeCSS('probapps.css'), includeScript('probapps.js')),
  navbarPage(
  "Probability Apps",

  tabPanel(
    "Descriptive Statistics",
    includeHTML('www/descrStat-instructions.html'),
    descrStat.UI('descrStat'),
    includeHTML('www/descrStat-explorations.html'),
    value='descrStat'
  ),
  
  tabPanel(
    "Long-run Probability",
    includeHTML('www/longrun-instructions.html'),
    longrun.UI('longrun'),
    includeHTML('www/longrun-explorations.html'),
    value='longrun'
  ),
  
  tabPanel(
    "Sample from Population",
#    includeHTML('www/samplePop-instructions.html'),
    samplePop.UI('samplePop'),
#    includeHTML('www/samplePop-explorations.html'),
    value='samplePop'
  ),
  
  tabPanel(
    "Sampling Distributions",
#    includeHTML('www/sampDist-instructions.html'),
    sampDist.UI('sampDist'),
#    includeHTML('www/sampDist-explorations.html'),
    value='sampDist'
  ),
  
  tabPanel(
    "Random Numbers",
#    includeHTML('www/randNum-instructions.html'),
    randNum.UI('randNum'),
#    includeHTML('www/randNum-explorations.html'),
    value='randNum'
  ),
  
  tabPanel(
    "Regression by Eye",
    "Coming soon!",
    value='regr'
  ),
  
  tabPanel(
    "Beta-Binomial",
#    includeHTML('www/betaBin-instructions.html'),
    betaBin.UI('betaBin'),
#    includeHTML('www/betaBin-explorations.html'),
    value='betaBin'
  ),

tabPanel(
    "About the Apps",
    includeHTML('www/about.html'),
    value='about'
  ),
  
  id="tab", collapsible=T
)))


server <- shinyServer(function(input, output, session) {

  callModule(descrStat, 'descrStat', colorList)
  callModule(longrun, 'longrun', colorList)
  callModule(samplePop, 'samplePop', colorList)
  callModule(sampDist, 'sampDist', colorList)
  callModule(randNum, 'randNum', colorList)
  callModule(betaBin, 'betaBin', colorList)
  
  # Start on About tab
  updateNavbarPage(session, 'tab', 'about')
    
  # If interactive, end after one session
  if (interactive()) session$onSessionEnded(function() { stopApp() })
})


shinyApp(ui=ui, server=server)
