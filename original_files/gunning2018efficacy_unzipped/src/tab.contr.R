## table of glm contrasts - between time & between region
## depends on results from 
# mod.circuit2.R
# run.spray.fix.R
# mod.logis.R
source('mk.helpers.R')

## table setup
## used by xtable, see .xtab.contr in file child-02
.contr.align <- c('l','l','l','l','r','r','r','r')
.contr.display <- c('s','s','s','s','s','f','f','g')

## grab names of models to output
.iadult <- grep(paste0('[.]', response.map$aa_total), names(.mods.circuit2$lsmeans))
.tmp.adult <- .mods.circuit2$lsmeans[.iadult]
names(.tmp.adult) <- gsub('[.].*','', names(.tmp.adult))

.l.in <- list(
    adult=.tmp.adult, 
    infest=.mods.infest$lsmeans
)

lcontr <- lapply(.l.in, function(.lsm) {within(list(), {
    ## get just adult models, fix names (just year)
    ##

    ## between time
    ## pairwise contrasts between circuits for spray region 
    tab.contr.time <- ldply(.lsm, function(.yr) {
        ret <- mk.contrast(.yr, by='project_code', method='trt.vs.ctrl')
        ## only return spray
        ret <- subset(ret, grepl('treatment',project_code))
        ## strip of baseline circuit
        ret$contrast <- sub(' - .*','', ret$contrast)
        ret
    })
    ## final name cleanup
    ## here, need to manually set contrast to circuit2
    tab.contr.time <- mk.cleanup.contr(
        tab.contr.time, do.rename.contr=T
    )

    ## pairwise contrasts between regions
    tab.contr.tx <- ldply(.lsm, function(.yr) {
        ret <- mk.contrast(.yr, 'revpairwise')
        ret$project_code <- 'treatment'
        return(ret)
    })
    ## final name cleanup
    message('Rename warning here ok:')
    tab.contr.tx <- mk.cleanup.contr(tab.contr.tx)

    .dat.contrast <- rbind(
        cbind(tab.contr.time, response_name='Between Times'),
        cbind(tab.contr.tx, response_name='Between Sectors')
    )
    ## mark signif diff between
    .dat.contrast <- within(.dat.contrast, {
        signif <- ifelse(p.value < 0.05, '*','')
        response <- Ratio
    })
    ## join for spray bars
    .dat.contrast <- {
        ## get and cleanup join table
        .join <- subset(.join_circuit2, 
            select=c(zone, circuit2_name, circuit2_id, spray_type)
        )
        .join$zone <- mk.revalue.zone(.join$zone)
        .join <- mk.rename.cols(.join)
        ret <- merge(.dat.contrast, .join, by=c('Experiment','Circuit'))
        ret
    }

    .dat.signif <- with(tab.contr.tx, data.frame(
        Experiment, Circuit, is.tx = p.value < 0.05
    ))
    .dat.signif <- merge(.dat.signif, subset(tab.contr.time, select = c(Experiment, Circuit, p.value)))
    .dat.signif <- within(.dat.signif, {
        is.time <- p.value < 0.05
        p.value <- NULL
        signif <- factor(is.time + 2*is.tx,
            levels=c(0,1,2,3), labels=c('', 't', 's', 'st')
        )
    })
})})

tab.contr.spray <- mk.contrast(
    .mods.spray.fix$lsmeans[[1]], 'trt.vs.ctrl', by='zone'
)

tab.contr.spray <- within(tab.contr.spray, {
    Experiment <- mk.revalue.zone(zone)
    Contrast <- sub('-', '/', contrast)
    Ratio <- response.ratio
})
## remove missing level, reorder for final output
tab.contr.spray <- data.table(
    na.omit(subset(tab.contr.spray,
        select=c(Experiment, Contrast, Ratio, SE, p.value)
    )), key='Experiment'
)
tab.contr.spray <- mk.bold.contr(tab.contr.spray)
