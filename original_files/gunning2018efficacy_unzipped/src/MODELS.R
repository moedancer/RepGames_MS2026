## depends on ../import.R
## for data & persistent sqldf.connection

## constants 
.nboot <- 1e3
.do.bootstrap <- FALSE

## helper functions
## also calls run.setup.R
source('mk.helpers.R')

## cleanup crazy spray records
source('run.spray.fix.R')
## load in all data, cleanup
source('mod.prep.R')
## baseline C1 models
source('mod.base.R')
## model counts for each time period / sector
source('mod.circuit2.R')
## model proportions for each time period / sector
source('mod.logis.R')
## control cage data
source('run.cage.R')

## bootstraps of circuit2
## run if requested or cache not present
source('run.boot.load.R')
