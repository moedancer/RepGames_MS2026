## setup sql, common functions and libraries
source('mk.helpers.R')
source('run.setup.R')

## helper
.nfun = function(x) length(unique(x))

tab.count.circuit2 <- dat.count.circuit2[,
    by=c('Experiment','Circuit','Weeks'), j=list(
        #nweek = .nfun(week),
        ## collapse out the blank treatment
        Treatment = paste(unique(Treatment), collapse=''),
        n_loc = .nfun(location_code),
        n_visit = .nfun(sid),
        n_full = sum(!is.na(n_cont)),
        n_visit_buff = .nfun(subset(.SD, Sector == 'Buffer')$sid),
        n_visit_spray = .nfun(subset(.SD, Sector == 'Spray')$sid)
    )
]

tab.count.circuit2 <- subset(tab.count.circuit2,
    select=c(Experiment, Circuit, Weeks, Treatment,
        n_loc, n_visit, n_full, n_visit_buff, n_visit_spray
    )
)

tab.count.circuit2  <- rename(tab.count.circuit2, c(
    n_loc='Houses', 
    n_visit='Surveys',
    n_full='Full Surveys',
    n_visit_buff='Buffer',
    n_visit_spray='Spray'
))

## do this with melt/cast?
tab.count.tot <- as.data.table(dat.count)[, 
    by=zone, j=list(
        house=sum(n_loc), 
        adult=sum(n_adult),
        cont=sum(n_cont),
        dissect=sum(n_dissect)
    )
]
        
tab.count <- dat.count
## simulate double-row colnames
colnames(tab.count) <- c('Experiment', 'Sector', 'Houses', 'Surveys', '~Sampled','Adults:~', '~Sampled','Containers:~', '~Dissected', 'Females:~')
## formatting.
tab.count.txt <- rbind(c('','','', '', 'Female', 'Total', 'Nulliparous', 'Total'), tab.count)
