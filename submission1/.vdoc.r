#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes, fixest,
               scales, gganimate, gapminder, gifski, png, tufte, plotly, OECD,
               ggrepel, survey, foreign, devtools, pdftools, kableExtra, modelsummary,
               kableExtra)
#
#
#
#
knitr::opts_chunk$set(warning = FALSE)
#
#
#
#
#| include: false
#| eval: true
load("Hw3_workspace.Rdata")
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: taxchange
#| fig-cap: Question 1 Graph

question1_graph
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: avgtax2012
#| fig-cap: Question 2 Graph

question2_graph
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: priceincrease
#| fig-cap: Question 3 Graph

question3_graph
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: priceincreaselow
#| fig-cap: Question 4 Graph

question4_graph
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: logs
#| tbl-cap: "Log Regression"

library(kableExtra)
options(knitr.kable.NA = 0)
knitr::kable(regression_table, 
             col.names=c("Coefficients","Standard Errors"),
             format.args=list(big.mark=","), booktabs = TRUE) %>%
             kable_styling(latex_options=c("scale_down"))

#
#
#
#
#
#
#
#
#
#| echo: false
#| label: logs
#| tbl-cap: "Log Regression"

library(kableExtra)
options(knitr.kable.NA = 0)
knitr::kable(iv_model, 
             col.names=c("Coefficients","Standard Errors"),
             format.args=list(big.mark=","), booktabs = TRUE) %>%
             kable_styling(latex_options=c("scale_down"))

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
