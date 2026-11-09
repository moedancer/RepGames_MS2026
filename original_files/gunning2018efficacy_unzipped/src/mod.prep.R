## setup sql, common functions and libraries
source('mk.helpers.R')

## prepare for linear models
.dat.prep <- sqldf("
    select sid, zone, project_code, location_code, circuit2, circuit as subcircuit,
    aa_total, aa_females, na_total, pc,
    rooms, rooms_surveyed, ct as containers,
    block_number, kitchen, roof_house, floor, construction, collector
    from v_all_house where project_code is not null
", stringsAsFactors=T)

## omit problem locations (split combine locations??)
## needs attention
## in run.spray.fix.R
.dat.prep <- subset(.dat.prep,
    !(location_code %in% spray.problem.locs)
)

## discard mosquitoes above this threshold
.aa_total_thresh <- Inf

## Define operational levels of physical characteristics
## as ordered factors - low level = less developed
##?? move this to mk.read.files or run.postgres?
.dat.prep <- within(.dat.prep, {
    ## observation id, unused
    #id <- 1:length(block_number)
    ## use dep var name to select model family
    ##
    ## family=binomial
    #aa_total[aa_total>.aa_total_thresh ] <- .aa_total_thresh 
    aa_infest <- aa_total > 0
    na_infest <- na_total > 0
    block_number <- factor(block_number)
    build <- factor(
        grepl('wood', construction, ignore.case=TRUE), 
        levels=c( FALSE, TRUE),
        labels=c( 'No Wood', 'Wood')
    )
    floor <- factor(
        grepl('land', floor, ignore.case=TRUE), 
        levels=c(FALSE, TRUE),
        labels=c('No Dirt', 'Dirt')
    )
    kitchen <- factor(
        grepl('wood', kitchen, ignore.case=TRUE), 
        levels=c( FALSE, TRUE),
        labels=c( 'No Wood', 'Wood')
    )
    roof <- factor(
        grepl('closed', roof_house, ignore.case=TRUE), 
        ## rev order
        levels=c( TRUE, FALSE),
        labels=c( 'Closed', 'No Closed')
    )
    pos_containers <- factor(
        pc != 0,
        levels=c(FALSE, TRUE),
        labels=c('No Positives', 'Positives')
    )
})
.dat.prep <- melt(.dat.prep, 
    measure.vars=c('aa_total', 'na_total', 'aa_females', 'aa_infest', 'na_infest'),
    variable.name='response_shortname', value.name='response'
)
.dat.prep$response_name <- mk.revalue.response_name(.dat.prep$response_shortname )

##  parity by circuit2
.dat.parity <- sqldf('
    select zone, circuit2, project_code, location_code,
        n_dissect, n_null, 
        n_dissect - n_null as n_not_null
    from v_join_house_par_cont
', stringsAsFactors=T)

#.dat.parity <- subset(.dat.parity, 
#    !(sid %in% spray.sid.unsprayed),
#)

.min.obs <- 5
## remove blocks with too few obs
.dat.parity <- ddply(.dat.parity, c('zone','circuit2', 'project_code'), function(.df) {
    if (nrow(.df) < .min.obs) { 
        .df$n_null <- NA
        .df$n_not_null <- NA
    }
    return(.df)
})
.bycols <- c('zone','circuit2', 'project_code', 'location_code')
.dat.parity <- data.table(.dat.parity, key=.bycols)

## pos containers
.dat.cont <- sqldf("
    select zone, project_code, circuit2, circuit, date
    sid, location_code,  container_id,
    larvae, pupae, positive_containers
    from v_all_cont
", stringsAsFactors=T)
## now in cleanup
## where circuit2 != '2_FUM'

## container surveys
.dat.cont <- data.table(.dat.cont, key=.bycols)
## compute pos/neg cont for each circuit2 
.dat.cont.perhouse <- .dat.cont[, keyby=.bycols, 
    j=within(list(), {
        n_cont <- length(unique(.SD$container_id))
        n_pos <- sum(.SD$positive_containers)
        n_neg <- n_cont - n_pos
    })
]


## observation counts
dat.count <- sqldf('
    select * from v_agg_survey_counts where project_code is not null
')

## fix zone factor levels / order
dat.count <- within(dat.count,{
    zone <- as.character(mk.revalue.zone(zone))
    project_code <- as.character(mk.revalue.project_code(project_code))
})

## details by circuit
dat.count.circuit2 <- sqldf('
    select zone, project_code, circuit2, location_code, sid, n_cont from v_join_house_par_cont
')
dat.count.circuit2 <- as.data.table(mk.fix.all(dat.count.circuit2))
setkeyv(dat.count.circuit2, c('Experiment', 'Circuit'))


##??
## uses successful sprays from 2013
## use to exclude spurious control cage data 
## (e.g. records for unsprayed houses)
.dat.cage <- sqldf("
    select * from v_all_cage 
")
.dat.cage <- data.table(.dat.cage)

## time series of surveys
## for bootstrap
.dat.ts.adult <- sqldf('
    select * from v_join_house_par_cont 
', stringsAsFactors=T)

.dat.ts.adult <- within(.dat.ts.adult, {
    date <- as.Date(date)
    prop_null <- n_null / n_dissect
    prop_cont_pos <- n_cont_pos / n_cont
    rm(n_null)
    rm(n_dissect)
    rm(n_cont)
    rm(n_cont_pos)
})

## approx dates of spraying 
.spray_dates <- sqldf('select * from v_spray_dates')
.spray_dates <- within(.spray_dates,
    date <- as.Date(date)
)
