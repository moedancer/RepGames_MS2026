## setup sql, common functions and libraries
source('mk.helpers.R')

.dat.baseline <- subset(.dat.prep,
    (response_shortname == 'aa_total') & grepl('1_', circuit2),
    select=c(zone, project_code, response)
)

#.hist.breaks=c(0,1,4, 8, 16, 24, 32, 48, 64, 96, 128)
.hist.breaks=c(0,1,2,4,10, max(.dat.baseline$response) +2
)
## labels - single numbers, or range of included
.hist.lab = paste0(
    head(.hist.breaks,-1), 
    c('','',paste0('-',tail(.hist.breaks,-3)-1))
)

.dat.baseline <- within(.dat.baseline, {
    bin <- cut(response, .hist.breaks, right=F)
})
## custom sector name cleanup
.dat.baseline <- within(.dat.baseline, {
    project_code <- mk.revalue.project_code(project_code, suffix = ' Sector')
    zone <- mk.revalue.zone(zone)
})
## compute proportions
.prop.baseline = ddply(.dat.baseline, c('project_code', 'zone'), function(.df) {
    .bin = levels(.df$bin)
    ret = data.frame( 
        ## order bins 
        ## make first bin named zero
        bin=factor(.bin, levels=.bin, labels=.hist.lab),
        prop=summary(.df$bin)/nrow(.df)
    )
    ret
})

.plot.baseline <- (
    ggplot(.prop.baseline, aes(x=bin, y=prop))#, fill=variable))
    + theme_bw()
    ## need to transform breaks to go along w/x-axis - confusing
    + geom_bar(stat='identity',position='dodge')
    + facet_grid(project_code ~ zone, scales='free')
    + ylab('Proportion of Observations')
    + xlab('AA Adults per House (Observed)') # limits=c(0,200), 
    #+ aes(y=..density..)
    + theme(
        legend.position=c(1,1),
        legend.justification=c(1,1)
    )
    #+ scale_fill_brewer('Sex', palette='Set1') 
)
