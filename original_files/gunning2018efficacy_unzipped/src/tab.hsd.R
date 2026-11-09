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

.hsd.spray <- mk.cld( .mods.spray.fix$models[[1]], 
    .spec = ~zone+Spray.status,
    .col.order = c('Experiment',"Spray.status",'nObs', 'Group', 'Est', 'SE', '95\\% CI')
)
.hsd.spray <- data.table(.hsd.spray, key='Experiment')

## binomial models from here down
.hsd.infest <- lapply(.mods.infest$models, mk.cld, .est.col='prob')


## ests are small, use more digits
.hsd.cont <- lapply(.mods.cont$models, mk.cld,
    .est.col='prob',
    .ci.fmt='%1.3f-%1.3f'
)
.hsd.cont <- lapply(.hsd.cont, subset, select=-Treatment)

# pc per house = breteau index
.hsd.bi <- lapply(.mods.bi$models, mk.cld,
    .ci.fmt='%1.3f-%1.3f'
)
.hsd.bi <- lapply(.hsd.bi, subset, select=-Treatment)

.hsd.par <- lapply(.mods.parity$models, mk.cld, .est.col='prob')
