
# load packages

# install packages from CRAN
p_needed <- c(
  'rmarkdown',
  'ggplot2',
  'ineq',
  'tidyverse',  
  'readxl',  
  'reldist',
  'reshape2',
  'plyr',
  'data.table',
  'corrplot',
  'GGally',
  'stargazer',
  'caret',
  'kableExtra',
  'gridExtra',
  'qwraps2',
  'xtable',
  'knitr',
  'kableExtra',
  'tseries',
  'stats',
  'car',
  'stargazer',
  'png',
  'broom',
  'lmtest'
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
