## depends on run.spray.fix.R
##
# use visreg2d to look at status x circuit

.theme.spray.fix <- (
    .gg.theme_bw
    + theme(
    )
)

.dat.plot <- .mods.spray.fix$hypoth.df 
.dat.plot$zone <- mk.revalue.zone(.dat.plot$zone)
.plot.spray.fix <- (
    ggplot(.dat.plot,
        aes(x=`Spray.status`, y=response, 
            ymax=asymp.UCL, ymin=asymp.LCL
        )
    )
    + facet_grid( . ~ zone)
    + geom_pointrange(position=position_dodge(width=0.15))
    + ylab('Adults per House (NB model)')
    + xlab('Spray Record Category\n(Only locations in treatment sector during treatment time)')
    + .theme.spray.fix
)

