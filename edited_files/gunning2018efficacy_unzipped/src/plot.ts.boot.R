source('plot.prep.R')
## depends on run.boot.circuit.R

## nudge top to the right a bit
.margin.boot.top <- .mk.unit(.gg.plot.margin)
.margin.boot.bottom <- .mk.unit(.gg.plot.margin + c(2, 0,0, -2))
## legend across top of counts plot
.boot.theme.top <- .gg.theme_bw + 
theme(
    plot.margin=.margin.boot.top,
    legend.position='top',
    #legend.position=c(0.105,1.02),
    #legend.justification=c(0,1),
    legend.box='horizontal',
    legend.background=.leg.back#,
    #legend.key.width=unit(1.5,'cm')
)

.boot.theme.bottom <- .boot.theme.top + theme(
    legend.position=c(5,5),
    plot.margin=.margin.boot.bottom
)
## subset response_name == proportion or counts
.has.prop <- grepl('^Pr',.dat.boot.ci$response_name)
.dat.boot.ci.count <- na.omit(.dat.boot.ci[!.has.prop,])
.dat.boot.ci.pr <- na.omit(.dat.boot.ci[.has.prop,])

## define base plot, swap in data later
## This is all counts, for SI
.plot.ts.boot.count.base <- ( 
    ggplot(.dat.boot.ci.count, 
        aes(x=as.Date(date_min), 
            y=response_mean, ymin=lower, ymax=upper,
            color=Treatment, linetype=Treatment #, color=project_code
    ))
    + facet_grid(response_name~zone, scales='free', space='free_x') 
    + ylab('Adults/HSE (weekly mean)')
    + xlab('') 
    + custom.date.scales
    + .boot.theme.top
    + geom_errorbar(aes(size=Treatment), width=3, alpha=0.65, size=0.5, linetype=1)
    #+ geom_linerange(alpha=0.25, size=1.75, linetype=1)
    #+ geom_line(alpha=0.8, size=0.5) 
    + geom_point(aes(size=n_sid),alpha=0.35)
    + geom_point(aes(shape=Treatment), size=1, color='black', alpha=0.8)
    + scale_size_continuous('# Sampled Houses', 
        range=c(1.25,6)#, breaks=c(200, 600)
    ) 
    + .scale.2col(.region.label)
    + scale_shape_discrete(.region.label, solid=F)
    + scale_linetype_manual(.region.label, breaks=c('Buffer', 'Spray'), values=c(1,2,2,3)) 
)

## add spray lines
.plot.ts.boot.count.full <- ( 
    .plot.ts.boot.count.base 
    + spray_ggplot
    #+ .spray.linetype
)

## Just aedes adults, legend on top
.plot.ts.boot.count <- (
    .plot.ts.boot.count.full
    %+% subset(.dat.boot.ci.count, 
        response_name == response.map$aa_total
    )
)


## zoom-in on weekly AA adults
## for week (week-of-year), see psql query:
## select zone, circuit2, min(week), max(week) from v_all_house group by zone, circuit2 order by zone, circuit2;
## 
## duplicate of bufer for S-2013 week 20??

.dat.boot.zoom <- subset(.dat.boot.ci.count, 
    response_name == response.map$aa_total
    & (
        (zone == 'S-2013' & (week <= 27))
        |
        (zone == 'L-2014' & (week >= 16) & (week <= 25))
    )
)


.plot.ts.boot.zoom <- (
    (
        .plot.ts.boot.count.base 
        + facet_grid(~zone, scales='free', space='free_x') 
        + ylab('AA/HSE (weekly mean)')
        + spray_ggplot_exper 
        + scale_size_continuous('# Sampled Houses', 
            range=c(1.25,6), breaks=c(100, 300)
        )
    ) 
    %+% .dat.boot.zoom
)

## change out data - proportions
.plot.ts.boot.pr <- ( 
    .plot.ts.boot.count 
    %+% .dat.boot.ci.pr
    + ylab('Proportion (weekly mean)')
    ## move all scales out of the way
    + .boot.theme.bottom
)
