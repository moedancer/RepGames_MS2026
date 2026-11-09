## depends on:
#source('mod.prep.R')
# and 
#source('run.spray.fix.R')
## See also SOURCE.R
##
## setup sql, common functions and libraries
source('mk.helpers.R')

.dat.circuit2 <- subset(.dat.prep, 
    ## not infest
    !grepl('PrIH', response_name),
    ## xian 2016-08-09: include all here,
    ## seperate model for effect of not spraying
    ## 
    ## prev comment: 
    ## remove surveys that might have occured before spraying
    ## spray.uncertain.sid in run.spray.fix.R
    #!(sid %in% .sid.spray.uncertain), 
    select=c(
        zone, project_code, location_code, subcircuit, circuit2, response, response_name, collector
    #,na_infest, aa_infest
))
## from above, has NAs:
## kitchen, ct as containers, rooms_surveyed, pc, rooms

## breteau index (pc/100 house)
## negative binomial glm of #pc/house
.mods.bi <- mk.models(.dat.cont.perhouse, 
    .mod.form='n_pos ~ circuit2  * project_code',
    .by.vars='zone', 
    .hypoth.form=~project_code|circuit2,
    .coef.unlog=T, .fast.ci=T
)

#.dat.circuit2 <- na.omit(.dat.circuit2)

## other model specs
## randef: '+ (1|collector)'
## block correlates w/a lot
##
## test RE:
## pchisq(2*(logLik(fitted_model)-logLik(fitted_model_without_RE)), df=1,lower.tail=FALSE)/2

.mods.circuit2 <- mk.models(
    .dat.circuit2, .mod.form='response ~  circuit2 * project_code',
    .hypoth.form=~project_code|circuit2,
    .coef.unlog=T, .fast.ci=T
)

if (F) {
    ## model by (sub)circuit
    .tmp.dat <- subset(.dat.circuit2, grepl('^AA Adults', response_name))
    .tmp.dat$subcircuit.fac <- factor(.tmp.dat$subcircuit)
    ## use infrastructure to build model
    ## only care about hypoth.df
    .mod.subcircuit <- mk.models(.tmp.dat,
        response ~ subcircuit.fac * project_code,
        .hypoth.form=~project_code*subcircuit.fac,
        .by.vars='response_name', .coef.unlog=T
    )$hypoth.df

    ## add back factor as number
    .mod.subcircuit <- within(.mod.subcircuit, {
        subcircuit <- as.numeric(as.character(subcircuit.fac))
    })
}

if(F) {
## add wilcoxon tests between each pair of tx/buffer 
.tmp.df <- subset(.mods.circuit2$mod.df)
.mods.circuit2$t.tests <- dlply(.tmp.df, c('zone', 'response_name'), function(.df.out) {
    dlply(.df.out, 'Circuit', function(.df) {
        #mod <- glm.nb(
        .dat <- dlply(.df, 'Sector', function(.df.in) .df.in$response)
        ret <- wilcox.test(x=.dat[[1]], y=.dat[[2]], conf.int=T) 
        return(ret)
    })
})
}
