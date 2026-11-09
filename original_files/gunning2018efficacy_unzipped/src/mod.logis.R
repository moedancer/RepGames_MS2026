## depends on:
#source('mod.prep.R')
## See also SOURCE.R
source('mk.helpers.R')
##
## logistic regression:
## parity and pos_cont binomial glm

.mods.parity <- mk.models(.dat.parity, 
    .mod.form='cbind(n_null, n_not_null) ~ circuit2  * project_code',
    .by.vars='zone', .coef.unlog=T,
    .hypoth.form=~project_code|circuit2,
    .mod.fun=glm, family=binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)
    
.mods.cont <- mk.models(.dat.cont.perhouse, 
    .mod.form='cbind(n_pos, n_neg) ~ circuit2  * project_code',
    .by.vars='zone', .coef.unlog=T,
    .hypoth.form=~project_code|circuit2,
    .mod.fun=glm, family=binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)

.dat.infest <- subset(.dat.prep, 
    response_shortname=='aa_infest',
    select=c(zone, circuit2, project_code, response)
)
.dat.infest <- data.table(.dat.infest)[,
    by=c('zone','circuit2','project_code'),
    j=list(
        n_infest=as.integer(response),
        n_not=1-response
    )
]

.mods.infest <- mk.models(.dat.infest, 
    .mod.form='cbind(n_infest, n_not) ~ circuit2  * project_code',
    .by.vars='zone', .coef.unlog=T,
    .hypoth.form=~project_code|circuit2,
    .mod.fun=glm, family=binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)
    

## combine binom glms
.hypoth.logis <- rbind(
    cbind(.mods.parity$hypoth.df, response_name=response.map$prop_null),
    cbind(.mods.cont$hypoth.df, response_name=response.map$prop_cont_pos),
    cbind(.mods.infest$hypoth.df, response_name=response.map$aa_infest)
)
