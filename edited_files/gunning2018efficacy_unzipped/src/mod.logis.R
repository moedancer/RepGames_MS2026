## depends on:
#source('mod.prep.R')
## See also SOURCE.R
source('mk.helpers.R')
##
## logistic regression:
## parity and pos_cont binomial glm

.mods.parity <- mk.models(
    .dat.parity,
    .mod.form = 'cbind(n_null, n_not_null) ~ circuit2  * project_code',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glm,
    family = binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)

##### NOTE MD: We want an equivalent model with additional random effects:
#####          1) Household-specific random intercepts
#####          2) Random effect for each combination of household and circuit
#####          3) RE from 1) and 2) combined
#####          We have to use our enhanced version of mk.models for that
##### EDIT START #####
.mods.parity.mixed1 <- mk.models(
    .dat.parity,
    .mod.form = 'cbind(n_null, n_not_null) ~ circuit2  * project_code + (1|location_code)', #added RE
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)#,
        #tol = 1e-6
    ) #added, otherwise convergence warnings
)

.mods.parity.mixed2 <- mk.models(
    .dat.parity,
    .mod.form = 'cbind(n_null, n_not_null) ~ circuit2  * project_code + (1|location_code:circuit2)', #added RE
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)#,
        #tol = 1e-6
    ) #added, otherwise convergence warnings
)

.mods.parity.mixed3 <- mk.models(
    .dat.parity,
    .mod.form = 'cbind(n_null, n_not_null) ~ circuit2  * project_code + (1|location_code) + (1|location_code:circuit2)', #added RE
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)#,
        #tol = 1e-6
    ) #added, otherwise convergence warnings
)
##### EDIT STOP #####

.mods.cont <- mk.models(
    .dat.cont.perhouse,
    .mod.form = 'cbind(n_pos, n_neg) ~ circuit2  * project_code',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glm,
    family = binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)

##### NOTE MD: We want an equivalent model with additional random effects:
#####          1) Household-specific random intercepts
#####          2) Random effect for each combination of household and circuit
#####          3) RE from 1) and 2) combined
#####          We have to use our enhanced version of mk.models for that
##### EDIT START #####
.mods.cont.mixed1 <- mk.models(
    .dat.cont.perhouse,
    .mod.form = 'cbind(n_pos, n_neg) ~ circuit2  * project_code + (1|location_code)',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5),
        tol = 1e-6
    )
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)

.mods.cont.mixed2 <- mk.models(
    .dat.cont.perhouse,
    .mod.form = 'cbind(n_pos, n_neg) ~ circuit2  * project_code + (1|location_code:circuit2)',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5),
        tol = 1e-6
    )
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)
.mods.cont.mixed3 <- mk.models(
    .dat.cont.perhouse,
    .mod.form = 'cbind(n_pos, n_neg) ~ circuit2  * project_code + (1|location_code) + (1|location_code:circuit2)',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5),
        tol = 1e-6
    )
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)
##### EDIT STOP #####

.dat.infest <- subset(
    .dat.prep,
    response_shortname == 'aa_infest',
    ##### NOTE MD: We also need 'location_code' to identify results from same houshold
    ##### EDIT START #####
    select = c(zone, circuit2, project_code, response, location_code)
    ##### EDIT STOP #####
)
.dat.infest <- data.table(.dat.infest)[,
    ##### NOTE MD: We also need 'location_code' to identify results from same houshold
    ##### EDIT START #####
    by = c('zone', 'circuit2', 'project_code', 'location_code'),
    ##### EDIT STOP #####
    j = list(
        n_infest = as.integer(response),
        n_not = 1 - response
    )
]

.mods.infest <- mk.models(
    .dat.infest,
    .mod.form = 'cbind(n_infest, n_not) ~ circuit2  * project_code',
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glm,
    family = binomial
    ## log binomial model, estimates relative risk
    #.mod.fun=glm, family=binomial(link=log)
)

##### NOTE MD: We want an equivalent model with additional random effect:
#####          1) Household-specific random intercepts
#####          As there is only one observation within each circuit of each household,
#####            further random effects are not meaningful!
#####          We have to use our enhanced version of mk.models for that
##### EDIT START #####
.mods.infest.mixed1 <- mk.models(
    .dat.infest,
    .mod.form = 'cbind(n_infest, n_not) ~ circuit2  * project_code + (1|location_code)', #added RE
    .by.vars = 'zone',
    .coef.unlog = T,
    .hypoth.form = ~ project_code | circuit2,
    .mod.fun = glmer,
    family = binomial, #changed to glmer
    control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)#,
        #tol = 1e-6
    ) #added, otherwise convergence warnings
)
##### EDIT STOP #####


## combine binom glms
.hypoth.logis <- rbind(
    cbind(.mods.parity$hypoth.df, response_name = response.map$prop_null),
    cbind(.mods.cont$hypoth.df, response_name = response.map$prop_cont_pos),
    cbind(.mods.infest$hypoth.df, response_name = response.map$aa_infest)
)

##### NOTE MD: We also want to create summary tables for mixed models:
#####          1) Household-specific random intercepts
#####          2) Random effect for each combination of household and circuit
#####          3) RE from 1) and 2) combined
#####          For the "infest" model, only 1) is applcable.
#####          Hence, we add it to all other summary tables too.
##### EDIT START #####
.hypoth.logis.mixed1 <- rbind(
    cbind(.mods.parity.mixed1$hypoth.df, response_name = response.map$prop_null),
    cbind(.mods.cont.mixed1$hypoth.df, response_name = response.map$prop_cont_pos),
    cbind(.mods.infest.mixed1$hypoth.df, response_name = response.map$aa_infest)
)

.hypoth.logis.mixed2 <- rbind(
    cbind(.mods.parity.mixed2$hypoth.df, response_name = response.map$prop_null),
    cbind(.mods.cont.mixed2$hypoth.df, response_name = response.map$prop_cont_pos),
    cbind(.mods.infest.mixed1$hypoth.df, response_name = response.map$aa_infest)
)

.hypoth.logis.mixed3 <- rbind(
    cbind(.mods.parity.mixed3$hypoth.df, response_name = response.map$prop_null),
    cbind(.mods.cont.mixed3$hypoth.df, response_name = response.map$prop_cont_pos),
    cbind(.mods.infest.mixed1$hypoth.df, response_name = response.map$aa_infest)
)
##### EDIT STOP #####
