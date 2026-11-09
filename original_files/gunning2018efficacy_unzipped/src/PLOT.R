source('mk.helpers.R')
source('plot.prep.R')

## for conditional code specific to dryad release
.in.dryad <- TRUE

## tables - results used by some plots...
source('tab.counts.total.R')
tab.count.circuit2
## by sector
tab.count
## margins
tab.count.tot

## model results tables: circuit x sector
## with Tukey HSD groups
source('tab.hsd.R')
.hsd.spray
## container
.hsd.cont
.hsd.infest
## parity
.hsd.par
## contrasts

## model contrasts:
## between-time, between-sector, between prior-spray
source('tab.contr.R')
## results:
lcontr$adult$tab.contr.time
lcontr$adult$tab.contr.tx
## see also lcontr$infest$...
tab.contr.spray

## build ggplot2 objects
## see results under source

## baseline survey adult counts
source('plot.baseline.R')
.plot.baseline

## bootstrap timeseries of observations
source('plot.ts.boot.R')
.plot.ts.boot.count
.plot.ts.boot.count.full
## zoom in on experimental spraying
.plot.ts.boot.zoom
## proportions
.plot.ts.boot.pr

## histogram of sprays per house 
source('plot.spray.hist.R')
## separate histogram for each experiment
.plot.spray.hist.range 
.plot.spray.hist.perturb 

## spray coverage by cycle
source('plot.spray.cycle.R')
.plot.spray.cycle

## model results for counts
source('plot.hypoth.circuit2.R')
.plot.hypoth.circuit2
.plot.hypoth.circuit2.full
## model contrasts
.plot.contrast 

## model results for proportions
source('plot.hypoth.logis.R')
## 
.plot.hypoth.logis
## proportion infested houses
.plot.hypoth.infest

## control cages - 24 hr mortality, 1hr knockdown
source('plot.cage.R')
.tab.cage
.plot.cage
