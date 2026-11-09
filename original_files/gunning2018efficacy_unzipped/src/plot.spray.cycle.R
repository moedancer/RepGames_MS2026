## histogram of number of sprays per house 
source('mk.helpers.R')

.dat.spray.cycle <- sqldf('
    select * from v_all_spray
')
.dat.spray.cycle <- data.table(.dat.spray.cycle)[,
    keyby=c('zone', 'cycle', 'status', 'spray_record'),
    j=list(nspray=length(unique(location_code)))
]

## compute percentages
.dat.spray.cycle <- .dat.spray.cycle[, 
    keyby=c('zone', 'cycle', 'spray_record'),
    j=within(list(), {
        status <- status
        nres <- sum(nspray)
        prop <- nspray/nres
        per.spray <- sprintf('%1.1f%%',prop*100)
        per.spray[!grepl('^Fum',status)] <- NA
    })
]

## cleanup for plotting
.dat.spray.cycle <- within(.dat.spray.cycle, {
    zone <- mk.revalue.zone(zone)
    spray_record <- mk.revalue.spray_record(spray_record)
    status <- sub('Fumigated', 'Sprayed', status)
    ## set sprayed as last
    status <- factor(status,
        levels=rev(c('Sprayed', 'Closed', 'No Access', 'Vacant', 'No Data'))
    ) 
})

## reorder - spray on bottom (last)
.dat.spray.cycle <- .dat.spray.cycle[
    order(-as.numeric(status))
]
## plot labels
.spray.record <- tolower(levels(.dat.spray.cycle$spray_record))
#.house.tot <- data.table(tab.count)[
#    ,by='Experiment', 
#    j=list(nres=sum(`Uniq Res`), spray_record='DIRESA Supplemental')
#]
#.house.tx <- data.table(subset(tab.count, Treatment=='Spray'))[
#    ,by='Experiment', 
#    j=list(nres=sum(`Uniq Res`), spray_record='NIH')
#]
#.house.both <- rbind(.house.tot, .house.tx)
##
#.dat.spray.cycle <- join(.dat.spray.cycle,
    #.house.both,
    #by=c('Experiment', 'spray_record')
#)
#
#.dat.spray.cycle <- within( .dat.spray.cycle, {
    #prop.spray <- nspray/nres
#}) 

## plotting
.margin.top <- .mk.unit(c(0,3,0,3))
.theme.spray.cycle <- .gg.theme_bw + 
theme(
    plot.margin=.margin.top,
    legend.position=c(0.01,0.975),
    legend.justification=c(0,1),
    legend.background=.leg.back,
    legend.box.just='top'
) 


.plot.spray.cycle <- (
    ggplot(.dat.spray.cycle, 
        aes(x=factor(cycle),y=prop, fill=status, label=per.spray)
    )
    + facet_grid(spray_record ~ zone , space='free', scales='free_x')
    + geom_bar(stat='identity', position='stack', color='black', size=0.1 )
    + geom_text(y=0.2, size=3)
    + .gg.theme_bw
    + .theme.spray.cycle
    + xlab('Spray Cycle')
    + scale_y_continuous(labels=percent)
    + ylab('Percent Coverage')
    + scale_fill_discrete('Spray Status')
    + guides(fill=guide_legend(ncol=2))
)
