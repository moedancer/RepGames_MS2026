# source('plot.prep.R')
## depends on mod.logis.R

.tmp.df <- mk.fix.all(.hypoth.logis)
.null.df <- subset(.tmp.df,
    grepl(response.map$prop_null, response_name)
)
## just infest
.infest.df <- subset(.tmp.df,
    grepl(response.map$aa_infest, response_name, fixed=T)
)


## just cont
.cont.df <- subset(.tmp.df,
    grepl(response.map$prop_cont_pos, response_name)
)


## add back missing cont week labels 
## null.df has full record
.wks.missing <- subset(.null.df, !(Weeks %in% .cont.df$Weeks))
##
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$prop_cont_pos
    prob <- asymp.LCL <- asymp.UCL <- NA
})
.cont.df <- rbind(.cont.df, .wks.missing)

## just l-glm
.logis.df <- rbind(.cont.df, .null.df)

## substract 1? 
.plot.hypoth.logis <- (
    ggplot(.logis.df,
        aes(x=xlab, y=prob, ymax=asymp.UCL, ymin=asymp.LCL, color=Sector, shape=Sector)
    ) 
    + facet_grid( response_name ~ Experiment , scales='free', space='free_x') 
    #+ geom_hline(yintercept=1-.diff, linetype=2, col='black', size=0.3)
    ## plot spray bars in background
    + scale_x_discrete()
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    + geom_pointrange(position=position_dodge(width=0.15))
    #+ geom_point(size=3, alpha=0.8)
    ## CI
    #+ geom_linerange()
    #+ scale_y_continuous(labels=percent)
    #+ scale_y_sqrt()
    #+ scale_color_discrete('Measure')
    + xlab('Circuit')
    + .scale.2col('Treatment')
    + scale_shape_discrete('Treatment')
    + ylab('Proportion')
    + .theme.hypoth.bottom
    #+ ylab('Odds Ratio of Success (Current / Baseline')
)
## 
#.plot.hypoth.cont <- .plot.hypoth.null %+% .cont.df
.plot.hypoth.infest <- (
    .plot.hypoth.logis 
    %+% .infest.df
    %+% facet_grid( ~ Experiment , scales='free', space='free_x')
    %+% ylab(response.map$aa_infest)
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        data=lcontr$infest$.dat.signif,
        y=-0.05, vjust=0,
        mapping=aes(x=Circuit, label=signif),
        color='black', alpha=1,
        fontface='bold',
        inherit.aes=F
    )
    + expand_limits(y=c(-0.05))
)

##### NOTE MD: We want to create the same plots for our results based on mixed models.
#####          These can be directly compared to the results from fixed effects models.
#####          1) Household-specific random intercepts
#####          2) Random effect for each combination of household and circuit
#####          3) RE from 1) and 2) combined
#####          For the "infest" model, only 1) is applcable.
##### EDIT START #####

# 1) Household-specific random intercepts

.tmp.mixed1.df <- mk.fix.all(.hypoth.logis.mixed1)
.null.mixed1.df <- subset(.tmp.mixed1.df,
    grepl(response.map$prop_null, response_name)
)
## just infest
.infest.mixed1.df <- subset(.tmp.mixed1.df,
    grepl(response.map$aa_infest, response_name, fixed=T)
)
## just cont
.cont.mixed1.df <- subset(.tmp.mixed1.df,
    grepl(response.map$prop_cont_pos, response_name)
)
## add back missing cont week labels 
## null.df has full record
.wks.missing <- subset(.null.mixed1.df, !(Weeks %in% .cont.mixed1.df$Weeks))
##
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$prop_cont_pos
    prob <- asymp.LCL <- asymp.UCL <- NA
})
.cont.mixed1.df <- rbind(.cont.mixed1.df, .wks.missing)

## just l-glm
.logis.mixed1.df <- rbind(.cont.mixed1.df, .null.mixed1.df)

## substract 1? 
.plot.hypoth.logis.mixed1 <- (
    ggplot(.logis.mixed1.df,
        aes(x=xlab, y=prob, ymax=asymp.UCL, ymin=asymp.LCL, color=Sector, shape=Sector)
    ) 
    + facet_grid( response_name ~ Experiment , scales='free', space='free_x') 
    #+ geom_hline(yintercept=1-.diff, linetype=2, col='black', size=0.3)
    ## plot spray bars in background
    + scale_x_discrete()
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    + geom_pointrange(position=position_dodge(width=0.15))
    #+ geom_point(size=3, alpha=0.8)
    ## CI
    #+ geom_linerange()
    #+ scale_y_continuous(labels=percent)
    #+ scale_y_sqrt()
    #+ scale_color_discrete('Measure')
    + xlab('Circuit')
    + .scale.2col('Treatment')
    + scale_shape_discrete('Treatment')
    + ylab('Proportion')
    + .theme.hypoth.bottom
    #+ ylab('Odds Ratio of Success (Current / Baseline')
)
## 
#.plot.hypoth.cont <- .plot.hypoth.null %+% .cont.df
.plot.hypoth.infest.mixed1 <- (
    .plot.hypoth.logis.mixed1 
    %+% .infest.mixed1.df
    %+% facet_grid( ~ Experiment , scales='free', space='free_x')
    %+% ylab(response.map$aa_infest)
    ## add text indicating significance 
    ## of specific contrasts
    + geom_text(
        data=lcontr.mixed1$infest$.dat.signif,
        y=-0.05, vjust=0,
        mapping=aes(x=Circuit, label=signif),
        color='black', alpha=1,
        fontface='bold',
        inherit.aes=F
    )
    + expand_limits(y=c(-0.05))
)

# 2) Random effect for each combination of household and circuit

.tmp.mixed2.df <- mk.fix.all(.hypoth.logis.mixed2)
.null.mixed2.df <- subset(.tmp.mixed2.df,
    grepl(response.map$prop_null, response_name)
)
## just cont
.cont.mixed2.df <- subset(.tmp.mixed2.df,
    grepl(response.map$prop_cont_pos, response_name)
)
## add back missing cont week labels 
## null.df has full record
.wks.missing <- subset(.null.mixed2.df, !(Weeks %in% .cont.mixed2.df$Weeks))
##
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$prop_cont_pos
    prob <- asymp.LCL <- asymp.UCL <- NA
})
.cont.mixed2.df <- rbind(.cont.mixed2.df, .wks.missing)

## just l-glm
.logis.mixed2.df <- rbind(.cont.mixed2.df, .null.mixed2.df)

## substract 1? 
.plot.hypoth.logis.mixed2 <- (
    ggplot(.logis.mixed2.df,
        aes(x=xlab, y=prob, ymax=asymp.UCL, ymin=asymp.LCL, color=Sector, shape=Sector)
    ) 
    + facet_grid( response_name ~ Experiment , scales='free', space='free_x') 
    #+ geom_hline(yintercept=1-.diff, linetype=2, col='black', size=0.3)
    ## plot spray bars in background
    + scale_x_discrete()
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    + geom_pointrange(position=position_dodge(width=0.15))
    #+ geom_point(size=3, alpha=0.8)
    ## CI
    #+ geom_linerange()
    #+ scale_y_continuous(labels=percent)
    #+ scale_y_sqrt()
    #+ scale_color_discrete('Measure')
    + xlab('Circuit')
    + .scale.2col('Treatment')
    + scale_shape_discrete('Treatment')
    + ylab('Proportion')
    + .theme.hypoth.bottom
    #+ ylab('Odds Ratio of Success (Current / Baseline')
)

# 3) RE from 1) and 2) combined

.tmp.mixed3.df <- mk.fix.all(.hypoth.logis.mixed3)
.null.mixed3.df <- subset(.tmp.mixed3.df,
    grepl(response.map$prop_null, response_name)
)
## just cont
.cont.mixed3.df <- subset(.tmp.mixed3.df,
    grepl(response.map$prop_cont_pos, response_name)
)
## add back missing cont week labels 
## null.df has full record
.wks.missing <- subset(.null.mixed3.df, !(Weeks %in% .cont.mixed3.df$Weeks))
##
.wks.missing <- within(.wks.missing, {
    response_name <- response.map$prop_cont_pos
    prob <- asymp.LCL <- asymp.UCL <- NA
})
.cont.mixed3.df <- rbind(.cont.mixed3.df, .wks.missing)

## just l-glm
.logis.mixed3.df <- rbind(.cont.mixed3.df, .null.mixed3.df)

## substract 1? 
.plot.hypoth.logis.mixed3 <- (
    ggplot(.logis.mixed3.df,
        aes(x=xlab, y=prob, ymax=asymp.UCL, ymin=asymp.LCL, color=Sector, shape=Sector)
    ) 
    + facet_grid( response_name ~ Experiment , scales='free', space='free_x') 
    #+ geom_hline(yintercept=1-.diff, linetype=2, col='black', size=0.3)
    ## plot spray bars in background
    + scale_x_discrete()
    + scale_alpha_manual(guide=F, values=.spray.alphas)
    + .plot.spray.bars
    + geom_pointrange(position=position_dodge(width=0.15))
    #+ geom_point(size=3, alpha=0.8)
    ## CI
    #+ geom_linerange()
    #+ scale_y_continuous(labels=percent)
    #+ scale_y_sqrt()
    #+ scale_color_discrete('Measure')
    + xlab('Circuit')
    + .scale.2col('Treatment')
    + scale_shape_discrete('Treatment')
    + ylab('Proportion')
    + .theme.hypoth.bottom
    #+ ylab('Odds Ratio of Success (Current / Baseline')
)

##### EDIT STOP #####
