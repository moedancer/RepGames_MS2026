## see run.cage.R
## boxplot of all cages by measure and cycle
## facet by zone 

.plot.cage <- (
    ggplot(.dat.cage.mean,
        aes(y=mean, x=factor(cycle))
        #aes(y=value, x=factor(cycle), fill=location_code)
    ) 
    + facet_grid(variable ~ zone)
    + geom_boxplot()
    + xlab('Spray Cycle')
    + ylab('Proportion Mosquitoes')
    #+ scale_fill_discrete('Measure')
    + scale_fill_discrete(guide=guide_legend(title=NULL))
    + theme_bw()
    + ylim(c(-0.055, 1.0))
    + theme(
        legend.justification=c(1,0),
        legend.position=c(1.018,-0.03),
        legend.background=element_rect(color='#DDDDDD')
    )
)


## facet by zone 
## boxplot of house means by machine
.plot.cage.machine <- (
    ggplot(.dat.cage.mean, 
        aes(y=mean, x=machine, fill=variable)
        #aes(y=value, x=factor(cycle), fill=location_code)
    ) 
    + facet_grid(.~ zone, scales='free_x', space='free_x')
    + geom_boxplot()
    + xlab('Machine Type')
    + ylab('Proportion Mosquitoes')
    + scale_fill_discrete('Measure')
    
    + theme_bw()
    + ylim(c(-0.055, 1.0))
    + theme(
        legend.justification=c(1,0),
        legend.position=c(1.018,-0.03),
        legend.background=element_rect(color='#DDDDDD')
    )
)

## boxplot - room effect
.plot.cage.room <- (
    ggplot(.dat.cage.room, 
        aes(y=kill.diff, x=zone, fill=cage)
        #aes(y=value, x=factor(cycle), fill=location_code)
    )
    #+ facet_grid(.~ zone)
    + geom_boxplot()
    + xlab('Experiment')
    + ylab('Difference from location mean')
    + scale_fill_discrete('Cage Location')
    + theme_bw()
    #+ ylim(c(-0.055, 1.0))
    + theme(
        legend.justification=c(1,0),
        legend.position=c(1.0,-0.025),
        legend.background=element_rect(color='#DDDDDD')
    )
)

## model table
aa <- within(.dat.cage.m, {
    ## set order
    zone <- factor(zone, levels=c('rangefind','perturb'))
    zone.cycle <- paste(zone,cycle,sep='.')
    loc.cage <- paste(location_code,cage,sep='.')
})

.tab.cage = dlply(aa, 'variable', function(.df){ 
    mod <- glmer(
        #value ~ zone + ( 1 | cycle/location_code/cage), 
        #value ~ zone + ( 1 | zone.cycle/location_code/cage),
        value ~ zone + ( 1 | zone.cycle/location_code),
        family = binomial(link = "logit"),
        weight=Nobs, .df
    )
    ret <- mk.cld(mod, 
        .spec=~zone,    
        .dat=.df, .model.frame=mod@frame,
        .est.col='prob',
        ## all columns, order later
        .col.order=T
    )
    ret <- within(ret, {
        Assay <- unique(.df$variable)
    }) 
    .col.order=c('Experiment','Assay','nObs', 'Group', 'Est', 'SE', '95\\% CI')
    ret <- ret[,.col.order]
    ret
})
.tab.cage <- do.call(rbind,.tab.cage)
