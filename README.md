# Probability Apps

*Interactive simulations to explore probability and statistics concepts, by Michael J. Culbertson*

Probability Apps was inspired by a set of Java applets provided with the second edition of Agresti and Franklin's textbook [Statistics: The Art and Scinece of Learning from Data](https://www.goodreads.com/book/show/2398581.Statistics). For a while, I incorporated the use of these applets into my introductory statistics course. But, the aging applets perpetually caused difficulty as new versions of Java were released and web security increased, overall. Instead of continuing to push through with the original applets, I decided to recreate the simulations with [Shiny](http://shiny.rstudio.com). If you find these apps useful or integrate them into your own course, I'd be delighted to hear about it.

## Quick Start

Probability Apps can be accessed online via shinyapps.io at: https://mculbert.shinyapps.io/ProbApps

## App Descriptions

* **Descriptive Statistics:** Explore how the quantiles, mean, and standard deviation of a sample relate to data in a sample
* **Long-run Probability:** Watch how the cumulative proportion approaches a theoretical probability as the sample size increases
* **Sample from Population:** Examine the relationship between population and sample as the sample size increases
* **Sampling Distribution:** Investigate how the distribution of sample statistics changes as a function of sample size
* **Random Numbers:** Draw a set of random numbers for offline simulations or experiments
* **Regression by Eye:** Explore the relationship between bivariate data and regression lines

## Dependencies

Probability Apps runs on [R](https://www.r-project.org) and requires the following R libraries:
```
install.packages(c('dplyr', 'shiny', 'ggplot2'))
```
Note that `shiny` must be at least version 0.14.

## Running Locally

You can launch Probability Apps locally in R directly from the GitHub repository with the command:
```
shiny::runGitHub("mculbert/ProbApps")
```

Alternatively, if you download the code, you can launch Probability Apps from R with:
```
setwd('/path/to/parent/directory')
shiny::runApp('ProbApps')
```

## License

Probability Apps is free software released under the [GNU Affero General Public License](http://www.gnu.org/licenses/agpl.html) (AGPL). You are free to redistribute and modify Probability Apps, but you must make any modified code available to users according to the terms of the AGPL.

## Change Log

* GitHub Repository
  * Initial version of Random Numbers
  * Initial version of Sampling Distributions
  * Initial version of Sample from Population
  * Initial version of Long-run Probability
* Version 0.1 (2016-02-21)
  * Initial version of Descriptive Statistics
