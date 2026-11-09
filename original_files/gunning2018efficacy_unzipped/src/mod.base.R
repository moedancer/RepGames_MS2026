## GLM of baseline state of system: 
## response of Aedes / Non-aedes to physical characteristics
##
## for a nice discussion of negbin vs quasipois
## see www.statpower.net/Content/MLRM/Lecture%20Slides/Overdispersion.pdf

#source('mod.prep.R')

.dat.base <- subset(.dat.prep, 
    grepl('^1_', circuit2) & response_name %in% with(response.map, c(aa_total, na_total))
)

## normalize counts to median # counts 
## (e.g. intercept = counts for "average" house
.dat.base <- ddply(.dat.base, 'zone', function(.df){
    .df <- within(.df, {
        rooms <- rooms - median(rooms, na.rm=T)
        rooms_surveyed <- rooms_surveyed - median(rooms_surveyed, na.rm=T)
        containers <- containers - median(containers, na.rm=T)
    })
    return(.df)
})
## prep formula
    ## not signif in either: kitchen + rooms_surveyed +
    ## block correlates w/ses
    ## collector as random effect
    ## block_number + collector + project_code + 
    ## 

## model formula 
.mod.spec <- 'response ~  build + floor + roof + containers + rooms + rooms_surveyed' # + collector' # + kitchen '
## inspect one at a time
#.mod.spec <- 'response ~  roof' 
## collector is significant (randeff model?)
#.mod.spec <- 'response ~  collector' 
## look at subset(.mods.base$goodness, family=='negbin')
## why is Perturb aa_total rsq so low??

## glmer
#.mod.spec <- '%s ~  build + floor + roof + kitchen + containers + rooms + rooms_surveyed + (1|collector)' #+ (1|location_code)'
#.mod.spec <- '%s ~  build + floor + roof +  rooms + containers + (1|collector)' #+ (1|location_code)'
## pos_containers could cause or be caused by adults - not fair?
#.mod.spec <- '%s ~  build + floor + roof +  rooms + containers + pos_containers  + (1|collector)' #+ (1|location_code)'
#.mod.spec <- '%s ~  build + floor + roof +  rooms + containers + pos_containers  + (1|collector) + (1|id)' #+ (1|location_code)'
#.mod.spec <- '%s ~  build + floor + roof +  rooms + containers + pos_containers  + (1|collector) + (1|location_code)'

#.base.p.thresh <- 0.01
.mods.base <- mk.models(
    .dat.base, .mod.spec, 
    .coef.unlog=T, .do.hypoth=F, .fast.ci=T
)

## pull out for Anova?
.mods.base.aedes <- .mods.base$models[c(1,3)]
.mods.base.nonaedes <- .mods.base$models[c(2,4)]


## correlation of predictors
.mods.base$cor <- dlply(.dat.base, 'zone', function(.df) {
    .dat <- subset(.df, select=c(build, floor, roof))
    .dat <- sapply(.dat, as.numeric)
    ret <- cor(.dat)
    ret
})
