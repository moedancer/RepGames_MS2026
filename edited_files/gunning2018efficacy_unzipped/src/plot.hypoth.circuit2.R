.dat.hypoth.circuit2 <- mk.fix.all(.mods.circuit2$hypoth.df)

## breteau index
#bi.nhouse <- 100
.dat.hypoth.bi <- mk.fix.all(.mods.bi$hypoth.df)
.dat.hypoth.bi <- within(.dat.hypoth.bi, {
    response_name <- response.map$pc_house
})
## get missing weeks for BI and set to NA
.wks.missing <- subset(.dat.hypoth.circuit2, (
    response_name == response.map$aa_total
    & !(Weeks %in% .dat.hypoth.bi$Weeks)
))
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$pc_house
    response <- asymp.LCL <- asymp.UCL <- NA
})
.dat.hypoth.bi  <- rbind(.dat.hypoth.bi, .wks.missing) 

.dat.hypoth.circuit2 <- rbind(.dat.hypoth.circuit2, .dat.hypoth.bi)

.plot.hypoth.circuit2.full <- (
    ggplot(.dat.hypoth.circuit2,
        aes(color=Sector, shape=Sector, x=Circuit, y=response, 
            ymax=asymp.UCL, ymin=asymp.LCL
        )
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + .scale.2col(.region.label)
    + scale_shape_discrete(.region.label)
    ## no xlab because logis goes below
    + xlab('')
    + ylab('Count')
    ## disable x-axis labels
    + .theme.hypoth.top
    + .theme.no.xlabs
)

.plot.hypoth.circuit2 <- (
    .plot.hypoth.circuit2.full
    %+% subset(.dat.hypoth.circuit2, 
        response_name == response.map$aa_total
    )
    %+% ylab(response.map$aa_total)
    %+% facet_grid( ~ Experiment , scales='free', space='free_x')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        data=lcontr$adult$.dat.signif,
        y=-0.05, vjust=0,
        mapping=aes(x=Circuit, label=signif),
        color='black', alpha=1,
        fontface='bold',
        inherit.aes=F
    )
    + expand_limits(y=c(-0.05))
)


.plot.contrast <- (
    ggplot(lcontr$adult$.dat.contrast)
    + aes(
        x=Circuit, y=response, 
        ymax=asymp.UCL, ymin=asymp.LCL, 
        label=signif
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + geom_hline(linetype=2, yintercept=1)
    + .gg.theme_bw
    + scale_x_discrete()
    + ylab('Contrast Ratio')
    + xlab('Circuit')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        y=-0.1, color='darkblue', 
        size=7,
        fontface='bold'
    )
    + expand_limits(y=c(0))
)

##### NOTE MD: We want to create the same plots for our results based on mixed models.
#####          These can be directly compared to the results from fixed effects models.
##### EDIT START #####
# 1) Negative binomial model for AA/HSE with random effect for households
.dat.hypoth.circuit2.mixed1 <- mk.fix.all(.mods.circuit2.mixed1$hypoth.df)

## breteau index
#bi.nhouse <- 100
.dat.hypoth.bi.mixed1 <- mk.fix.all(.mods.bi.mixed$hypoth.df)
.dat.hypoth.bi.mixed1 <- within(.dat.hypoth.bi.mixed1, {
    response_name <- response.map$pc_house
})
names(.dat.hypoth.circuit2.mixed1)[which(names(.dat.hypoth.circuit2.mixed1) == "prob")] <- "response"
names(.dat.hypoth.bi.mixed1)[which(names(.dat.hypoth.bi.mixed1) == "prob")] <- "response"

## get missing weeks for BI and set to NA
.wks.missing <- subset(.dat.hypoth.circuit2.mixed1, (
    response_name == response.map$aa_total
    & !(Weeks %in% .dat.hypoth.bi.mixed1$Weeks)
))
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$pc_house
    response <- asymp.LCL <- asymp.UCL <- NA
})
.dat.hypoth.bi.mixed1  <- rbind(.dat.hypoth.bi.mixed1, .wks.missing) 

.dat.hypoth.circuit2.mixed1 <- rbind(.dat.hypoth.circuit2.mixed1, .dat.hypoth.bi.mixed1)

.plot.hypoth.circuit2.full.mixed1 <- (
    ggplot(.dat.hypoth.circuit2.mixed1,
        aes(color=Sector, shape=Sector, x=Circuit, y=response, 
            ymax=asymp.UCL, ymin=asymp.LCL
        )
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + .scale.2col(.region.label)
    + scale_shape_discrete(.region.label)
    ## no xlab because logis goes below
    + xlab('')
    + ylab('Count')
    ## disable x-axis labels
    + .theme.hypoth.top
    + .theme.no.xlabs
)

.plot.hypoth.circuit2.mixed1 <- (
    .plot.hypoth.circuit2.full.mixed1
    %+% subset(.dat.hypoth.circuit2.mixed1, 
        response_name == response.map$aa_total
    )
    %+% ylab(response.map$aa_total)
    %+% facet_grid( ~ Experiment , scales='free', space='free_x')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        data=lcontr.mixed1$adult$.dat.signif,
        y=-0.05, vjust=0,
        mapping=aes(x=Circuit, label=signif),
        color='black', alpha=1,
        fontface='bold',
        inherit.aes=F
    )
    + expand_limits(y=c(-0.05))
)

.plot.contrast.mixed1 <- (
    ggplot(lcontr.mixed1$adult$.dat.contrast)
    + aes(
        x=Circuit, y=response, 
        ymax=asymp.UCL, ymin=asymp.LCL, 
        label=signif
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + geom_hline(linetype=2, yintercept=1)
    + .gg.theme_bw
    + scale_x_discrete()
    + ylab('Contrast Ratio')
    + xlab('Circuit')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        y=-0.1, color='darkblue', 
        size=7,
        fontface='bold'
    )
    + expand_limits(y=c(0))
)

# 2) Negative binomial model for AA/HSE with random effect for households
# NOTE: For the "PC/HSE" analysis, no convergence for the negative binomial model was achieved.
#       Hence, we also display results for the Poisson model here.
.dat.hypoth.circuit2.mixed2 <- mk.fix.all(.mods.circuit2.mixed2$hypoth.df)

## breteau index
#bi.nhouse <- 100
.dat.hypoth.bi.mixed2 <- mk.fix.all(.mods.bi.mixed$hypoth.df)
.dat.hypoth.bi.mixed2 <- within(.dat.hypoth.bi.mixed2, {
    response_name <- response.map$pc_house
})
names(.dat.hypoth.circuit2.mixed2)[which(names(.dat.hypoth.circuit2.mixed2) == "prob")] <- "response"
names(.dat.hypoth.bi.mixed2)[which(names(.dat.hypoth.bi.mixed2) == "prob")] <- "response"

## get missing weeks for BI and set to NA
.wks.missing <- subset(.dat.hypoth.circuit2.mixed2, (
    response_name == response.map$aa_total
    & !(Weeks %in% .dat.hypoth.bi.mixed2$Weeks)
))
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$pc_house
    response <- asymp.LCL <- asymp.UCL <- NA
})
.dat.hypoth.bi.mixed2  <- rbind(.dat.hypoth.bi.mixed2, .wks.missing) 

.dat.hypoth.circuit2.mixed2 <- rbind(.dat.hypoth.circuit2.mixed2, .dat.hypoth.bi.mixed2)

.plot.hypoth.circuit2.full.mixed2 <- (
    ggplot(.dat.hypoth.circuit2.mixed2,
        aes(color=Sector, shape=Sector, x=Circuit, y=response, 
            ymax=asymp.UCL, ymin=asymp.LCL
        )
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + .scale.2col(.region.label)
    + scale_shape_discrete(.region.label)
    ## no xlab because logis goes below
    + xlab('')
    + ylab('Count')
    ## disable x-axis labels
    + .theme.hypoth.top
    + .theme.no.xlabs
)

.plot.hypoth.circuit2.mixed2 <- (
    .plot.hypoth.circuit2.full.mixed2
    %+% subset(.dat.hypoth.circuit2.mixed2, 
        response_name == response.map$aa_total
    )
    %+% ylab(response.map$aa_total)
    %+% facet_grid( ~ Experiment , scales='free', space='free_x')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        data=lcontr.mixed2$adult$.dat.signif,
        y=-0.05, vjust=0,
        mapping=aes(x=Circuit, label=signif),
        color='black', alpha=1,
        fontface='bold',
        inherit.aes=F
    )
    + expand_limits(y=c(-0.05))
)

.plot.contrast.mixed2 <- (
    ggplot(lcontr.mixed2$adult$.dat.contrast)
    + aes(
        x=Circuit, y=response, 
        ymax=asymp.UCL, ymin=asymp.LCL, 
        label=signif
    )
    + facet_grid( response_name ~ Experiment, scales='free', space='free_x') 
    ## plot spray bars in background
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    ## CI
    + geom_pointrange(position=position_dodge(width=0.15))
    + geom_hline(linetype=2, yintercept=1)
    + .gg.theme_bw
    + scale_x_discrete()
    + ylab('Contrast Ratio')
    + xlab('Circuit')
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        y=-0.1, color='darkblue', 
        size=7,
        fontface='bold'
    )
    + expand_limits(y=c(0))
)

##### EDIT STOP #####
