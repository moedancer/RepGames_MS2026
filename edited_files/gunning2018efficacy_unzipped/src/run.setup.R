## common libraries
## sql connection
library(sqldf)
library(plyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
library(lme4)
## glm.nb
library(MASS)
## glht
#require(multcomp)
##### NOTE MD: 
##### Function 'cld' not available in version 2.30-2, need to install version 2.27-62
##### This function is e.g. used in line 270 of mk.helpers.R
library(lsmeans)
## for cld in lsmeans
library(multcompView)
library(car)
