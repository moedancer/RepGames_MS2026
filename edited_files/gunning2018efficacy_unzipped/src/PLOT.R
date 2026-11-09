source('mk.helpers.R')
source('plot.prep.R')
library(knitr)

## for conditional code specific to dryad release
.in.dryad <- TRUE

## tables - results used by some plots...
source('tab.counts.total.R')
tab.count.circuit2
## by sector
tab.count

##### NOTE MD: Table headings do not align with heading in Table 1 of manuscript
##### START EDIT #####
tab.count.edit <- rbind(
  c(
    '',
    '',
    '',
    '',
    'Female',
    'Total',
    'Positive',
    'Total',
    'Nulliparous',
    'Total'
  ),
  tab.count
)
tab.count.edit
##### END EDIT #####

## margins
tab.count.tot

## model results tables: circuit x sector
## with Tukey HSD groups
source('tab.hsd.R')
.hsd.spray
## container
.hsd.cont
.hsd.infest
## parity
.hsd.par
## contrasts

##### NOTE MD: We also want to report results from corresponding mixed effects models
##### EDIT START #####
# Counterparts to Table S5 (only household-specific random intercept meaningful)
kable(.hsd.spray.mixed1) # Negative binomial model
cat(
  "p-value for random effect:",
  anova(
    .mods.spray.mixed1$models[[1]],
    .mods.spray.fix$models[[1]]
  )$`Pr(>Chisq)`[2]
)
kable(.hsd.spray.mixed2) # Poisson model

# Counterpart to Table S7 (only household-specific random intercept meaningful)
kable(.hsd.infest.mixed1)
ranef_p <- sapply(
  names(.mods.infest.mixed1$models),
  function(z) {
    anova(
      .mods.infest.mixed1$models[[z]],
      .mods.infest$models[[z]]
    )$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)

# Counterparts to Table S8
kable(.hsd.par.mixed1) # + (1|location_code)
ranef_p <- sapply(
  names(.mods.parity.mixed1$models),
  function(z) {
    anova(
      .mods.parity.mixed1$models[[z]],
      .mods.parity$models[[z]]
    )$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)

kable(.hsd.par.mixed2) # + (1|location_code:circuit2)
ranef_p <- sapply(
  names(.mods.parity.mixed2$models),
  function(z) {
    anova(
      .mods.parity.mixed2$models[[z]],
      .mods.parity$models[[z]]
    )$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)

kable(.hsd.par.mixed3) # + (1|location_code) + (1|location_code:circuit2)
ranef_p <- sapply(names(.mods.parity.mixed3$models), function(z) {
  anova(
    .mods.parity.mixed3$models[[z]],
    .mods.parity.mixed1$models[[z]]
  )$`Pr(>Chisq)`[2]
})
cat(
  "p-values for radding household x phase effect to household effect:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)
ranef_p <- sapply(names(.mods.parity.mixed3$models), function(z) {
  anova(
    .mods.parity.mixed3$models[[z]],
    .mods.parity.mixed2$models[[z]]
  )$`Pr(>Chisq)`[2]
})
cat(
  "p-values for radding household effect to household x phase effect:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)

# Counterpart to Table S10

kable(.hsd.cont.mixed1) # + (1|location_code)
ranef_p <- sapply(
  names(.mods.cont.mixed1$models),
  function(z) {
    anova(.mods.cont.mixed1$models[[z]], .mods.cont$models[[z]])$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)
cat("AIC (2013 model):", AIC(.mods.cont.mixed1$models[["rangefind"]]))
cat("AIC (2014 model):", AIC(.mods.cont.mixed1$models[["perturb"]]))

.hsd.cont.mixed2 # + (1|location_code:circuit2)
ranef_p <- sapply(
  names(.mods.cont.mixed2$models),
  function(z) {
    anova(.mods.cont.mixed2$models[[z]], .mods.cont$models[[z]])$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)
cat("AIC (2013 model):", AIC(.mods.cont.mixed2$models[["rangefind"]]))
cat("AIC (2014 model):", AIC(.mods.cont.mixed2$models[["perturb"]]))

.hsd.cont.mixed3 # + (1|location_code) + (1|location_code:circuit2)
ranef_p <- sapply(
  names(.mods.cont.mixed3$models),
  function(z) {
    anova(
      .mods.cont.mixed3$models[[z]],
      .mods.cont.mixed1$models[[z]]
    )$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for adding household x phase effect to household effect:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)
ranef_p <- sapply(
  names(.mods.cont.mixed3$models),
  function(z) {
    anova(
      .mods.cont.mixed3$models[[z]],
      .mods.cont.mixed2$models[[z]]
    )$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for adding household effect to household x phase effect:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)

##### EDIT STOP #####

##### NOTE MD: So far, there is no output created that shows data from Table S9
#####          However, the results can maybe be found in .hsd.bi (created in tab.hsd.R).
#####          We also want to generate robustness result (with mixed model)
##### EDIT START #####
.hsd.bi
cat(
  "AICs:",
  sapply(names(.mods.bi$models), function(z) AIC(.mods.bi$models[[z]]))
)

# We can only provide results of the Poisson models here
# Negative binomial models suffer from convergence issues
# (see 'mod.circuit2.R')
kable(.hsd.bi.mixed)
cat(
  "AICs:",
  sapply(names(.mods.bi.mixed$models), function(z) {
    AIC(.mods.bi.mixed$models[[z]])
  })
)
##### EDIT STOP #####

##### NOTE MD: So far, there is no output created that shows data from Table S6
#####          However, the results can maybe be found in .hsd.aedes (created in tab.hsd.R)
#####          We also want to generate robustness result (with mixed model)
##### EDIT START #####
.hsd.aedes
cat(
  "AICs:",
  sapply(names(.mods.circuit2$models)[c(3, 6)], function(z) {
    AIC(.mods.circuit2$models[[z]])
  })
)

kable(.hsd.aedes.mixed1) # Negative binomial model
ranef_p <- sapply(
  names(.mods.circuit2.mixed1$models)[c(3, 6)],
  function(z) {
    anova(.mods.circuit2.mixed1$models[[z]], .mods.circuit2$models[[z]])$`Pr(>Chisq)`[2]
  }
)
cat(
  "p-values for random effects:\n",
  sprintf("%s: %.3g\n", names(ranef_p), ranef_p),
  sep = ""
)
cat(
  "AICs:",
  sapply(names(.mods.circuit2.mixed1$models)[c(3, 6)], function(z) {
    AIC(.mods.circuit2.mixed1$models[[z]])
  })
)

kable(.hsd.aedes.mixed2) # Poisson model
cat(
  "AICs:",
  sapply(names(.mods.circuit2.mixed2$models)[c(3, 6)], function(z) {
    AIC(.mods.circuit2.mixed2$models[[z]])
  })
)
##### EDIT STOP #####

## model contrasts:
## between-time, between-sector, between prior-spray
source('tab.contr.R')
## results:
lcontr$adult$tab.contr.time
lcontr$adult$tab.contr.tx
## see also lcontr$infest$...
tab.contr.spray
cat("AIC:", AIC(.mods.spray.fix$models[[1]]))

##### NOTE MD: We also want to report results from corresponding mixed effects models
##### EDIT START #####
# 1) Negative binomial model for AA/HSE with random effect for households
kable(lcontr.mixed1$adult$tab.contr.time)
kable(lcontr.mixed1$adult$tab.contr.tx)
kable(tab.contr.spray.mixed1)
cat(
  "p-value for random effect:\n",
  anova(.mods.spray.mixed1$models[[1]], .mods.spray.fix$models[[1]])$`Pr(>Chisq)`[2]
)
cat("AIC:", AIC(.mods.spray.mixed1$models[[1]]))

# 2) Poisson model for AA/HSE with random effect for households
kable(lcontr.mixed2$adult$tab.contr.time)
kable(lcontr.mixed2$adult$tab.contr.tx)
kable(tab.contr.spray.mixed2)
cat("AIC:", AIC(.mods.spray.mixed2$models[[1]]))

##### EDIT STOP #####

## build ggplot2 objects
## see results under source

## baseline survey adult counts
source('plot.baseline.R')
.plot.baseline

## bootstrap timeseries of observations
source('plot.ts.boot.R')
.plot.ts.boot.count
.plot.ts.boot.count.full
## zoom in on experimental spraying
.plot.ts.boot.zoom
## proportions
.plot.ts.boot.pr

## histogram of sprays per house
source('plot.spray.hist.R')
## separate histogram for each experiment
.plot.spray.hist.range
.plot.spray.hist.perturb

## spray coverage by cycle
source('plot.spray.cycle.R')
.plot.spray.cycle

## model results for counts
source('plot.hypoth.circuit2.R')
.plot.hypoth.circuit2
.plot.hypoth.circuit2.full
## model contrasts
.plot.contrast

##### NOTE MD: We also want to show plots for corresponding mixed effects models
##### EDIT START #####
# 1) Negative binomial model for AA/HSE with random effect for households
.plot.hypoth.circuit2.mixed1
ggsave("../plots/Fig4A_RC1.jpeg", device = "jpeg", width = 2700, height = 1000, units = "px")
.plot.hypoth.circuit2.full.mixed1
ggsave("../plots/FigS5A_RC1.jpeg", device = "jpeg", width = 2700, height = 2700, units = "px")
## model contrasts
.plot.contrast.mixed1
ggsave("../plots/Fig5_RC1.jpeg", device = "jpeg", width = 2700, height = 1600, units = "px")


# 2) Poisson model for AA/HSE with random effect for households
# NOTE: For the "PC/HSE" analysis, no convergence for the negative binomial model was achieved.
#       Hence, we also display results for the Poisson model here.
.plot.hypoth.circuit2.mixed2
ggsave("../plots/Fig4A_RC2.jpeg", device = "jpeg", width = 2700, height = 1000, units = "px")
.plot.hypoth.circuit2.full.mixed2
ggsave("../plots/FigS5A_RC2.jpeg", device = "jpeg", width = 2700, height = 2700, units = "px")
## model contrasts
.plot.contrast.mixed2
ggsave("../plots/Fig5_RC2.jpeg", device = "jpeg", width = 2700, height = 1600, units = "px")

##### EDIT STOP #####

## model results for proportions
source('plot.hypoth.logis.R')
##
.plot.hypoth.logis
##### NOTE MD: We also want to show the plot for the corresponding mixed effects models
##### EDIT START #####
.plot.hypoth.logis.mixed1
ggsave("../plots/FigS5B_RC1.jpeg", device = "jpeg", width = 2700, height = 1500, units = "px")
.plot.hypoth.logis.mixed2
ggsave("../plots/FigS5B_RC2.jpeg", device = "jpeg", width = 2700, height = 1500, units = "px")
.plot.hypoth.logis.mixed3
ggsave("../plots/FigS5B_RC3.jpeg", device = "jpeg", width = 2700, height = 1500, units = "px")
##### EDIT STOP #####

## proportion infested houses
.plot.hypoth.infest
##### NOTE MD: We also want to show the plot for the corresponding mixed effects model
##### EDIT START #####
.plot.hypoth.infest.mixed1
ggsave("../plots/Fig4B_RC1.jpeg", device = "jpeg", width = 2700, height = 1200, units = "px")
##### EDIT STOP #####

## control cages - 24 hr mortality, 1hr knockdown
source('plot.cage.R')
.tab.cage
.plot.cage
