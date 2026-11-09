## histogram of number of sprays per house 
source('mk.helpers.R')

## get number of times each house is sprayed w/each campaign
## exclude sprays from cards (DISA)
## join to points to include zeros
.dat.spray.count <- sqldf("
    select p.zone, p.project_code, 
        p.location_code, r.spray_record, s.date 
    from all_points p 
    cross join
    (select distinct spray_record from v_all_spray) r
    left join
    (select * from v_all_spray where status = 'Fumigated') s
    using (location_code, spray_record)
    where p.obs_class = 'seen'
", stringsAsFactors=TRUE)

## cleanup colnames from SQLite
if (.in.dryad) {
    colnames(.dat.spray.count) <- sub('^[prs][.]','',colnames(.dat.spray.count)) 
}

.dat.spray.count <- data.table(.dat.spray.count)[,
    by=.(zone, project_code, spray_record, location_code),
    j=list(nspray=length(na.omit(date)))
]

## Clean up zeros
.dat.spray.count <- within(.dat.spray.count, {
    spray_record <- mk.revalue.spray_record(spray_record)
    ## prettify project_code
    project_code <- mk.revalue.project_code(project_code, suffix = ' Sector')
    zone <- mk.revalue.zone(zone)
})

## subset out separate experiments
.dat.spray.count.perturb <- droplevels(subset(.dat.spray.count,
    grepl('L-2014', zone) &
    ## don't need to show zeros for buffer / NIH
    !( 
        grepl('Exper', spray_record)
        & grepl('Buffer',project_code)
    )
))

## no diresa spraying for rangefinding
.dat.spray.count.range <- droplevels(subset(.dat.spray.count, 
    grepl('S-2013', zone)
    & grepl('Exper', spray_record)
    & !grepl('Buffer',project_code)
))

.plot.spray.hist.range <- ( 
    ggplot(.dat.spray.count.range, aes(x=factor(nspray, levels=0:max(nspray))))
    + facet_grid(spray_record ~ project_code, scales='free_y')
    + geom_bar(stat="count")
    + theme_bw()
    + ylab('Houses')
    + xlab('Number of Spray Events Per House')
    + scale_x_discrete(drop=FALSE)
)

.plot.spray.hist.perturb <- .plot.spray.hist.range %+% 
    .dat.spray.count.perturb

