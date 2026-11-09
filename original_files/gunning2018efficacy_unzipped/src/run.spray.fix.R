source('run.setup.R')

## inner: both house surveys and spray records
.dat.tx <- sqldf("
    select * from v_join_house_tx
")
## 

## tag locations as 
## having been sprayed before or after survey
## (within same week)
## 
## assume not sprayed until proven otherwise
## can't do length-1 assign w/reuse in within({})
.names <- list(
    good='Prior Spray', 
    bad='No Prior Spray',
    unclear='Timing Unclear',
    ugly='No Record'
)
.dat.tx$spray.status <- .names$bad
.dat.tx = within(.dat.tx, {
    subcircuit <- circuit
    circuit <- NULL
    ## text code
    status = factor(status)
    ## ever sprayed
    spray.any <- (status == 'Fumigated')
    ## timing codes
    spray.status[spray.any] <- .names$good
    spray.status[spray.any & (date_diff_min <= 0 ) ] <- .names$unclear
    ## closed
    ## no spray record
    spray.status[is.na(status)] <- .names$ugly
    spray.status = factor(spray.status, levels=.names)
})

.dat.tx <- as.data.table(.dat.tx)
    
## prop0 for each status group
spray.summary <- .dat.tx[,
    keyby=c('zone','spray.status'), 
    j=list(
        nloc = length(location_code), 
        prop0=sum(aa_total==0)/length(aa_total)
    )
]

## unsprayed locations in treatment sector / time
.sid.spray.uncertian = unique(
    subset(.dat.tx, !grepl('^sprayed', spray.status)
    )$sid
)

.key <- c('zone','date_diff_min')
## worst case time difference: early spray, late survey
.timing.2014 <- .dat.tx[,
    keyby=.key, j=list(count=length(sid))
]
.timing.2014.up <- .dat.tx[,
    keyby=.key, j=list(count=length(sid))
]


## locations in treatment sector 
## with no record of visit by spray team
## problems w/spit & combine of houses?
## Check?
spray.problem.locs = unique(
    subset(.dat.tx, grepl('^no record', spray.status)
    )$location_code
)

## cleanup?
.dat.tx <- rename(.dat.tx, c(spray.status='Spray.status'))
.dat.tx$dummy <- 'dummy'

## GLM model, spray status by zone 
.mods.spray.fix <- mk.models(
    .dat.tx, .mod.form=aa_total ~ zone*Spray.status,
    .by.vars='dummy',
    .hypoth.form=~Spray.status*zone,
    .coef.unlog=T, .fast.ci=T
)
