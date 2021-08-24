# COVID-19: Data Viz in R (ggplot2) & Shiny

There's a misconception within the data community that [R](https://www.r-project.org/) is only good for statistical analysis. And yes, R is great for that, but it can produce some pretty stunning data visualizations too! Here I present an interactive dashboard for the early spread of COVID-19<sup>\*</sup> built using R and [Shiny](https://shiny.rstudio.com/).

<p align="center">
  <img src="assets/A3_COV19_RViz1_Sample.gif" alt="animated"/>
</p>

<em><sup>\*</sup>This visualisations presented here are meant for illustrative purposes only, and the data pertaining to COVID-19 is not up-to-date. The data was last updated on 29-Apr-2019.</em>

## Background

Ask any data scientist and they'll tend to agree, there is a recipe to studying data, and it's this:

1. Clean
2. Visualize
3. Analyze (or model)
4. Productionalise (or share)
5. Monitor (and update)

There are some T&Cs. You can swap the order of points 2. and 3. or even combine them (ever heard of EDA?). You might also jump between these points - the analysis of any data is often an iterative process. That said, the above recipe is generally a good starting point.

When it comes to choosing a language or software package for data visualization it should score pretty high at executing each of these steps. So what are your options? PowerBI is great for visualizations and shareability. It's pretty bad at data wrangling though (and quite simply horrendous at statistical analysis without applying a great deal of effort). Ditto for Tableau, if you're wondering. Other popular options include [Python](https://www.python.org/)<sup>\*</sup>, or even a combination of MS Excel & Powerpoint. I decided to give [R](https://www.r-project.org/) (and it's IDE, [RStudio](https://www.rstudio.com/)) a go.

1. Clean - I used [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8).
2. Visualize - I used [ggplot2](https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5).
3. Analyze (or model) - Again, I used [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8).
4. Productionalize (or share) - I used [shiny](https://shiny.rstudio.com/).

(Quick tip: check out the [tidyverse](https://www.tidyverse.org/packages/) package.)

<em><sup>\*</sup>I'm not going to pretend that Python isn't better than R in almost every possible way. For starters, the community is much larger - and this matters. A lot. You'll find more tutorials, more courses. You'll have better luck finding sample code online. You'll have a greater selection of packages to choose from. In fact, Python is so popular it's very likely the company you're at already has Python integrated into their stack, and this makes productionalizing anything you come up with much smoother.</em>

## Installation

Visit the official site for [RStudio](https://www.rstudio.com/products/rstudio/) and follow the installation instructions. It's a simple "download and double-click" type of installation. You'll also have to do the same for [R](https://www.r-project.org/). Open a new notebook, and paste this into the first cell or block. That's it! RStudio will find the packages automatically (if you haven't already got them).

```R

## shiny
if(!require(shiny)) {install.packages("shiny"); library(shiny)}

## data frame manipulation
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)) {install.packages("lubridate"); library(lubridate)}

## interactive data tables 
if(!require(DT)) {install.packages("DT"); library(DT)}

## plots
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if(!require(gghighlight)) {install.packages("gghighlight"); library(gghighlight)}
if(!require(plotly)) {install.packages("plotly"); library(plotly)}

## plots - mapdata
if(!require(rnaturalearth)) {install.packages("rnaturalearth"); library(rnaturalearth)}
if(!require(rnaturalearthdata)) {install.packages("rnaturalearthdata"); library(rnaturalearthdata)}
if(!require(sf)) {install.packages("sf"); library(sf)}

### plots - animation
if(!require(gganimate)) {install.packages("gganimate"); library(gganimate)}
if(!require(gifski)) {install.packages("gifski"); library(gifski)}
if(!require(transformr)) {install.packages("transformr"); library(transformr)}

### plots - custom labels
if(!require(directlabels)) {install.packages("directlabels"); library(directlabels)}
if(!require(ggrepel)) {install.packages("ggrepel"); library(ggrepel)}
if(!require(scales)) {install.packages("scales"); library(scales)}
    
```