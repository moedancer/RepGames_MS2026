## setup sql, common functions and libraries
source('mk.helpers.R')

.dat.ts.adult <- as.data.table(.dat.ts.adult)
.bycols <- c('zone', 'circuit2', 'week', 'project_code', 'sid')
setkeyv(.dat.ts.adult, .bycols) 

## bootstrap function for data.table
## .nboot defined in child-00init.Rnw
## .boot.cachefile defined in run.boot.load.R
## gets data from each circuit2 
## draw .nboot sets of sid
## join to data, summarize w/in index and week
## NOTE - bootstrap by zone, circuit2 (below
## aggregate by .in.by 
mk.boot.circuit2 <- function(
    .sid.dt, .dat.dt, .nboot,
    .in.by=c('week','project_code','response_name'),
    .ci.alpha = 0.05
) {
    .ci.probs <- c(.ci.alpha/2, 1-.ci.alpha/2)
    ## simple progress indicator
    cat('.')
    ## merge (subset) .sid.dt to .dt zone/circuit2
    .sid <- .sid.dt$sid
    .nsid <- length(.sid)
    .draw <- data.table(
        index=1:.nboot,
        sid=sample(.sid, .nsid*.nboot, replace=T)
    )
    setkey(.draw, sid)
    ## merge (subset) draws by sid 
    .draw.dat <- merge(.draw, .dat.dt, allow.cartesian=T)
    #.draw.parity <- merge(.draw, .parity.dt)
    ## get mean for each combo of response & draw (index)
    ret <- .draw.dat[, 
        keyby=c(.in.by, 'index'),
        j=within(list(), {
            .dat <- na.omit(response)
            nObs_boot <- length(.dat)
            boot_mean <- mean(.dat)
            rm(.dat)
        })
    ]
    #ret$.dat <- NULL
    ## get ci for each response (marginalize over draw /index
    ret <- ret[, keyby=.in.by,
        j=data.table(
            lower=quantile(boot_mean, .ci.alpha/2, na.rm=T),
            upper=quantile(boot_mean, 1-.ci.alpha/2, na.rm=T),
            lower_nobs=quantile(nObs_boot, .ci.alpha/2 ),
            upper_nobs=quantile(nObs_boot, 1-.ci.alpha/2 )
        )
    ]
    return(ret)
}

.bycols.boot <- c('zone', 'circuit2', 'sid')
## just sid for draws (resample by house survey)
.dat.ts.sid <- subset(.dat.ts.adult, select=.bycols.boot)

## to long-form after sid subset, before bootstrap
.dat.ts.adult <- melt(.dat.ts.adult, 
    measure.vars = c('aa_total','na_total', 'aa_females', 'prop_null', 'prop_cont_pos'),
    variable.name='response_name', 
    value.name='response'
)

## rekey
.dat.ts.adult <- data.table(.dat.ts.adult, 
    key=c(.bycols.boot, 'response_name')
)

.msg <- sprintf("### Running bootstrap with %d draws per level (one . per level) ###\n", .nboot)
cat(.msg)
## the actual draws
## the outer .dat.ts.adult provides data
.dat.boot.ci <- .dat.ts.sid[,
    ## these are treatment / sample / bootstrap blocks 
    by=c('zone', 'circuit2'),  
    j=mk.boot.circuit2(.SD, .dat.dt=.dat.ts.adult, .nboot=.nboot)
]
setkey(.dat.boot.ci, zone, circuit2, week)

## summarize by week
## summarize observation timing, counts (x-axis, dot size)
.by.group <- c('zone','week', 'project_code')
.dat.ts.sample <- .dat.ts.adult[, 
    by=.by.group,
    j=data.table(
        date_min=min(.SD$date),
        n_sid=length(unique(.SD$sid))
    )
]
## summarize observations 
.dat.ts.obs <- .dat.ts.adult[,
    keyby=c(.by.group,'response_name'),
    j=within(list(), {
        .dat <- na.omit(response)
        nObs <- length(.dat)
        response_mean <- mean(.dat)
        rm(.dat)
    })
]
        
## combine observation means w/sample times
.dat.ts.obs <- merge(.dat.ts.obs, .dat.ts.sample,
    by=.by.group
)

## then combine bootstrap results
.dat.boot.ci <- merge(.dat.ts.obs, .dat.boot.ci,
    by=c(.by.group, 'response_name')
)

save(.dat.boot.ci, file=.boot.cachefile)
