## Full model table results
## hsd = Honest significant difference


## tab_hsd xtable col alignments
## nObs should be r, others can be c
.hsd.align <- c('r', circuit='c', weeks='c', tx='c', sector='c', nobs='r', 'c','c','c','c')
.hsd.align.cont <- c('r', circuit='c', weeks='c', sector='c', nobs='r', 'c','c','c','c')

## function to print - escape hashes, hline in
## middle
.print.tab.hsd <- function(.xtab, .size=.tab.size, .float=T) {
    .hline.add <- ifelse('Sector' %in% colnames(.xtab),
        ## last row of buffer
        max(grep('Buffer', .xtab$Sector)),
        ## otherwise just repeat zero
        0
    )
    ## add hline between tx groups
    .hline <- c( -1, 0, nrow(.xtab), .hline.add )
    print(.xtab, 
        floating=.float,
        size=.size, 
        hline.after=.hline,
        sanitize.text.function = .san.hash
    )
}

## helper functions
## prepare xtable w/ label & caption, 
## print & return xtable
.my.tab.hsd <- function(
    x, leg, label, first=FALSE, .align=.hsd.align, 
    ... ## xtable args
) {
    xtab <- xtable(x, 
        align=.align,
        caption=leg[[label]],
        label=label, ...
    )
    ## first table of pair
    if (first) {
        ## latex macro to manipulate counters, 
        ## see child-60SI
        cat('\\tabLetter\n')
        .print.tab.hsd(xtab)
        cat('\\tabNumber\n')
    } else{
        .print.tab.hsd(xtab)
    }
    return(xtab)
}

## model results tables
## prep a caption, then build the data  
## make xtable and print asis in final doc

## model is list, named by zone.measure
## gen list names
.which.aedes <- with(.mods.circuit2$hypoth.df,
    sprintf('%s.%s', levels(zone), response.map$aa_total)
)
.tmp.mods <- .mods.circuit2$models[.which.aedes]
.hsd.aedes <- lapply(.tmp.mods, mk.cld)

##### NOTE MD: We want to generate equivalent tables for mixed model results
##### EDIT START #####
# 1) Negative binomial model with random effect for location
.which.aedes.mixed1 <- with(.mods.circuit2.mixed1$hypoth.df,
    sprintf('%s.%s', levels(zone), response.map$aa_total)
)
.tmp.mods.mixed1 <- .mods.circuit2.mixed1$models[.which.aedes.mixed1]
.hsd.aedes.mixed1 <- lapply(.tmp.mods.mixed1, mk.cld)

# 2) Poisson model with random effect for location
.which.aedes.mixed2 <- with(.mods.circuit2.mixed2$hypoth.df,
    sprintf('%s.%s', levels(zone), response.map$aa_total)
)
.tmp.mods.mixed2 <- .mods.circuit2.mixed2$models[.which.aedes.mixed2]
.hsd.aedes.mixed2 <- lapply(.tmp.mods.mixed2, mk.cld)
##### EDIT STOP #####

.hsd.spray <- mk.cld( .mods.spray.fix$models[[1]], 
    .spec = ~zone+Spray.status,
    .col.order = c('Experiment',"Spray.status",'nObs', 'Group', 'Est', 'SE', '95\\% CI')
)
.hsd.spray <- data.table(.hsd.spray, key='Experiment')

##### NOTE MD: We want to generate equivalent tables for mixed model results
##### EDIT START #####
# 1) Negative binomial model
.hsd.spray.mixed1 <- mk.cld( .mods.spray.mixed1$models[[1]], 
    .spec = ~zone+Spray.status,
    .col.order = c('Experiment',"Spray.status",'nObs', 'Group', 'Est', 'SE', '95\\% CI')
)
.hsd.spray.mixed1 <- data.table(.hsd.spray.mixed1, key='Experiment')

# 2) Poisson model
#    ATTENTION: We report rate ratios instead of response ratios!
.hsd.spray.mixed2 <- mk.cld( .mods.spray.mixed2$models[[1]], 
    .spec = ~zone+Spray.status,
    .col.order = c('Experiment',"Spray.status",'nObs', 'Group', 'Est', 'SE', '95\\% CI')
)
.hsd.spray.mixed2 <- data.table(.hsd.spray.mixed2, key='Experiment')
##### EDIT STOP #####

## binomial models from here down
.hsd.infest <- lapply(.mods.infest$models, mk.cld, .est.col='prob')
##### NOTE MD: We want to generate equivalent tables for mixed model results
##### EDIT START #####
.hsd.infest.mixed1 <- lapply(.mods.infest.mixed1$models, mk.cld, .est.col='prob')
##### EDIT STOP #####

## ests are small, use more digits
.hsd.cont <- lapply(.mods.cont$models, mk.cld,
    .est.col='prob',
    .ci.fmt='%1.3f-%1.3f'
)
.hsd.cont <- lapply(.hsd.cont, subset, select=-Treatment)
##### NOTE MD: We want to generate equivalent tables for mixed model results
##### EDIT START #####
# 1) Only household-specific random intercept
.hsd.cont.mixed1 <- lapply(.mods.cont.mixed1$models, mk.cld, .est.col='prob',
    .ci.fmt='%1.3f-%1.3f')
.hsd.cont.mixed1 <- lapply(.hsd.cont.mixed1, subset, select=-Treatment)

# 2) Household x Circuit-specific random intercept
.hsd.cont.mixed2 <- lapply(.mods.cont.mixed2$models, mk.cld, .est.col='prob',
    .ci.fmt='%1.3f-%1.3f')
.hsd.cont.mixed2 <- lapply(.hsd.cont.mixed2, subset, select=-Treatment)

# 3) Combination of 1) and 2)
.hsd.cont.mixed3 <- lapply(.mods.cont.mixed3$models, mk.cld, .est.col='prob',
    .ci.fmt='%1.3f-%1.3f')
.hsd.cont.mixed3 <- lapply(.hsd.cont.mixed3, subset, select=-Treatment)
##### EDIT STOP #####

# pc per house = breteau index
.hsd.bi <- lapply(.mods.bi$models, mk.cld,
    .ci.fmt='%1.3f-%1.3f'
)
.hsd.bi <- lapply(.hsd.bi, subset, select=-Treatment)
##### NOTE MD: We want to generate equivalent table for mixed model
#####            with Poisson instead of negative binomial
##### EDIT START #####
.hsd.bi.mixed <- lapply(.mods.bi.mixed$models, mk.cld,
    .ci.fmt='%1.3f-%1.3f'
)
.hsd.bi.mixed <- lapply(.hsd.bi.mixed, subset, select=-Treatment)
##### EDIT STOP #####


.hsd.par <- lapply(.mods.parity$models, mk.cld, .est.col='prob')

##### NOTE MD: We want to generate equivalent tables for mixed model results
##### EDIT START #####
# 1) Only household-specific random intercept
.hsd.par.mixed1 <- lapply(.mods.parity.mixed1$models, mk.cld, .est.col='prob')
# 2) Household x Circuit-specific random intercept
.hsd.par.mixed2 <- lapply(.mods.parity.mixed2$models, mk.cld, .est.col='prob')
# 3) Combination of 1) and 2)
.hsd.par.mixed3 <- lapply(.mods.parity.mixed3$models, mk.cld, .est.col='prob')
##### EDIT STOP #####
