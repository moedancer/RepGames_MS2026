require(reshape2)
require(ggplot2)

source('mk.helpers.R')
## depends on mod.prep.R
## See also SOURCE.R

.dat.cage.m <- melt(.dat.cage, measure.vars=c('kd_1hr','kill_24hr'))
## cleanup names
.lvl <- c(
    kill_24hr='Kill (24 Hours)', 
    kd_1hr='Knockdown (1 Hour)'
)
.dat.cage.m$variable <- factor(.dat.cage.m$variable, 
    levels=names(.lvl),
    labels=.lvl
)
.dat.cage.mean <- .dat.cage.m[, 
    by=c('zone', 'cycle', 'location_code', 'variable'),
    j=within(list(),{
        mean <- mean(value)
        sd <- sd(value)
    })
]
## cleanup
.dat.cage.mean$zone <- mk.revalue.zone(.dat.cage.mean$zone)

## room effect, difference from house mean
.dat.cage.room <- .dat.cage[,
    by=c('zone', 'location_code'), 
    j=list(
        kill_diff = kill_24hr - mean(kill_24hr)
    )
]
