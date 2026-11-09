source('run.setup.R')

## convenience function for knitr
## add warning to the stackmod. 
## telling which file was sourced
my.source <- function(.fn, .debug=FALSE) {
    if (.debug) cat(sprintf('Source file %s\n', .fn))
    source(.fn)
}

## today's date as text
my.date = function() format(Sys.time(), '%Y_%m_%d')

## deprecated, useful reference
## custom table printing 
## controlled by print.num 
## (see .do.figs in child-00)
my.print.tab <- function(x, print.num, ...) {
    ## pull out table caption / label
    .attr <- attributes(x)
    if(print.num==1) {
    ## normal
        print(x, ...)
    } else if (print.num ==2) {
    ## label + cap
        .tab <- sprintf('\\begin{table}[h!]\n\\caption{%s}\n\\label{%s}\n\\end{table}\n', .attr$caption, .attr$label 
        )
        cat(.tab)
    } else if (print.num ==3) {
    ## just label
        .tab <- sprintf('\n\\refstepcounter{table}\\label{%s}\n\\textbf{Table \\ref{%s} goes here}.\n', .attr$label, .attr$label)
        cat(.tab)
    } else {
    }
    return(invisible())
}

####################################
## convenience cleanup for printing
####################################
## 
mk.rename.cols <- function(.df, 
    .namevec=c(
        circuit2_name='Circuit', project_code='Sector',
        wk_label='Weeks', zone='Experiment'
    )
){
    .df <- rename(.df, .namevec)
    .df
}

## reassign levels of project_code (tx)
mk.revalue.project_code <- function(.tx, suffix=''){
    ret <- factor(.tx,
        levels=c('buffer', 'treatment'),
        labels=paste0(c('Buffer', 'Spray'), suffix)
    )
} 

## rangefind comes first
mk.revalue.zone <- function(.vec){
    .vec <- factor(.vec,
        levels=c('rangefind', 'perturb'),
        labels= c('S-2013', 'L-2014')
    )
    return(.vec)
}


response.map <- within(list(), {
        aa_total <- 'AA/HSE'
        aa_females <- 'AA Females/HSE'
        na_total <- 'Non-AA Adults/HSE'
        pc_house <- 'PC/HSE'
        prop_null <- 'PrNF'
        prop_cont_pos <- 'PrPC'
        aa_infest <- 'PrIH (AHI)'
        na_infest <- 'Non-AA PrIH'
})

## like above, pass factor rather than whole .df
mk.revalue.response_name <- function(.resp, .map=response.map){
    ret <- factor(.resp,
        levels=names(.map),
        labels=.map
    )
    return(droplevels(ret))
} 

mk.revalue.spray_record <- function(.text) {
    ret <- factor(.text, 
        levels=c('DIRESA Supplemental', 'NIH'),
        labels=c('Emergency (Citywide)', 'Experimental')
    )
    return(ret)
}
    
## bad??
## get numeric from factor
## extra is a factor level 
mk.circuit2_id.name <- function(.vec, prefix='C') {
    .vec <- paste0(prefix, as.integer(.vec))
    .vec
}

## join table - production labels for weeks, sectors, etc.
## use v_agg_survey to minimize dependencies
.join_circuit2 <- sqldf("
    select zone, project_code, circuit2, circuit2_id, wk_label, spray_type
    from all_circuit2
    cross join (
        select distinct project_code from v_agg_survey_counts
    ) sub
")
.join_circuit2 <- within(.join_circuit2, {
    circuit2_name <- mk.circuit2_id.name(circuit2_id)
    xlab <- circuit2_name
    xlab <- factor(xlab, levels=unique(xlab))
    ## order by occurance, not lexical order
    ## becomes Weeks
    wk_label <- factor(wk_label, levels=unique(wk_label))
    ## spray labels
    spray_type[spray_type=='# ' & project_code=='buffer'] <- ''
    Treatment <- revalue(factor(spray_type), c(`# ` = 'Exper. spray', `## `='Citywide spray'))
})

## fix up names and levels all at once
## (week labels from above)
mk.fix.all <- function(.df, .df.join=.join_circuit2) {
    ## circuit2_id
    if( 'circuit2' %in% colnames(.df)) {
        .df <- join(.df, .df.join)
    }
    if( 'project_code' %in% colnames(.df)) {
        .df <- within(.df, {
            ## mark 
            project_code <- mk.revalue.project_code(project_code)
        })
    }
    .df$zone <- mk.revalue.zone(.df$zone)
    .df <- mk.rename.cols(.df)
    .df
}

## write plot to pdf, doing nothing if .plot=F
## set title to filename if missing
mk.pdf <- function(.plotobj, .filename, .plot,
    .width=9, .height=8, .title=NULL
){
    ## do nothing
    if (!.plot) return()
    if (is.null(.title)) .title <- .filename
    pdf(.filename, width=.width, height=.height,
        title = .title
    )
    plot(.plot)
    dev.off()
    return()
}

my.ldply2 <- function(.l, fun) {
    ## 2-level list to data.frame
    ldply(.l, function(.ll) ldply(.ll, function(.lll) fun(.lll) ))
}

## the kitchen sink
## much of this is outdated, really only need
## the model, & final product hypoth.df
mk.models <- function(
    .dat.all, .mod.form,
    .by.vars=c('zone', 'response_name'),
    .thresh=1, .coef.unlog=F, .fast.ci=F,
    ## use lsmeans cld
    .do.hypoth=T, .adjust='tukey',
    .hypoth.form=~circuit2*project_code, .sort=F,
    #.contrast.meth='revpairwise',
    .mod.fun=glm.nb, ...
){
    ## run model for each combo of .by.vars in .dat.all, 
    ## model specified by .mod.form
    ## .thresh = set coef w/p-val >.tresh to NA
    ## 1 disables
    ## returns named list of "report" data.frames
    ## and list of models
    ## 
    ## for each experiment and predictor, build model
    ret.tmp <- dlply(.dat.all, .by.vars, function(.df) {
        ## get first row for experimental condition
        ## make sure to keep as data.frame even if only one .by.vars
        .cond.df <- .df[1,][.by.vars] 
        ## prevent warning in cbind
        rownames(.cond.df) <- NULL
        ## pack return objects into list
        ret.in <- within(list(), {
            ## for each predictor .y construct model form
            ## args for glm/glm.nb
            .dat <- droplevels(.df)
            #.args$data <- droplevels(na.omit(dat))
            ## construct formula w/response var
            ## formula uses current environmet , needs dat
            ## though this goes awry for glm?
            .form <- formula(.mod.form)
            ## use glm.nb for negbin response
            ## glm.nb to ignore family arg
            #print(.df[1,])
            mod <- .mod.fun(.form, .dat, ...
                #,control=glm.control(epsilon = 1e-2, maxit = 100, trace=T)
            )
            mod$data <- .dat
            .summary <- summary(mod)
            #mod <- step(mod)
            #mod <- do.call('glm', .args)
            #mod <- step(mod)
            ## goodness of fit measures as df
            goodness <- data.frame(
                rsq = 1 - with(mod, deviance/null.deviance),
                deviance = mod$deviance,
                AIC = AIC(mod)
            )
            anova <- Anova(mod)
            anova <- cbind(coef=rownames(anova), as.data.frame(anova))
            ## extract coef into table
            .tab <- .summary$coef[,c(1,4)]
            ## for clarity, set coef w/high p val = NA
            .tab <- aaply(.tab, 1, function(x) {
                if (x[2] > .thresh ) { 
                    x[1:2] = NA 
                } 
                x
            })
            coef.tab <- data.frame(
                Parameter=rownames(.tab),   
                Estimate=.tab[,1], `P-value`=.tab[,2]
            )
            ## estimated dispersion from glm.nb
            if ('theta' %in% names(.summary)) {
                .theta <- data.frame(
                    Parameter='Theta',  
                    Estimate=summary(mod)$theta, 
                    `P-value`=NA
                )
                coef.tab <- rbind(.theta, coef.tab)
            }
            coef.tab <- cbind(.cond.df, coef.tab)
            
            ## get model as data.frame
            ## add helpful vars to model df
            mod.df <- cbind( mod$model, .cond.df)
            #mod.df$response_name <- .cond.df$response_name 
            ## fit in orig units
            mod.df$fit <- fitted(mod)
            ## various residuals
            mod.df$resid_response <- residuals(mod, type='response')
            mod.df$resid_pearson <- residuals(mod, type='pearson')

            ## bind together model specs, coef ests, and CI
            .coef <- coef(mod)
            ## optionally, unlog coefs / CI
            if (.coef.unlog) {
                coef.tab$Estimate <- exp(coef.tab$Estimate )
                .coef <- exp(.coef)
            }
            if( .do.hypoth) {
                if (.coef.unlog) {
                    .type='response'
                } else {
                    .type='link'
                }
                mod.lsm <- lsmeans(mod, .hypoth.form, adjust=.adjust, data=.dat)
                .cld.df <- cld(mod.lsm, type=.type,
                    sort=.sort, adjust=.adjust
                )
                #.mod.contr <- contrast(mod.lsm, .contrast.meth)
                #hypoth.df <- cbind(
                    #as.data.frame(confint(.mod.contr, type=.type)),
                    #P.value=test(.mod.contr, type=.type)$p.value
                #)
                hypoth.df <- cbind( .cond.df, .cld.df)
            }

        }) ## end within(list(), 
    return(ret.in)
    })
    ## final return object
    ret <- within(list(), {
        ## pull report dfs out of nested list and combin
        confint <- ldply(ret.tmp, function(.l) .l$confint)
        goodness <- ldply(ret.tmp, function(.l) .l$goodness)
        coef.tab <- ldply(ret.tmp, function(.l) .l$coef.tab)
        mod.df <- ldply(ret.tmp, function(.l) .l$mod.df)
        anova <- ldply(ret.tmp, function(.l) .l$anova)
        hypoth.df <- ldply(ret.tmp, function(.l) .l$hypoth.df)
        ## list of model objects
        models <- llply(ret.tmp, function(.l) .l$mod)
        lsmeans <- llply(ret.tmp, function(.l) .l$mod.lsm)
    })
    return(ret)
}

## plot helper - name file by date
.mk.fn <- function(.base, .date=Sys.Date(), .ext='.pdf') {
    sprintf('%s-%s%s', .base, .date, .ext)
}

## used below - format results from lsmeans
mk.lsm.tab <- function(.lsm, 
    .est.col, .ci.fmt
){
    ret <- as.data.frame(.lsm)
    ret <- within(ret, {
        Est <- ret[[.est.col]]
        ci <- sprintf(.ci.fmt, asymp.LCL, asymp.UCL)
    })
    ret
}

## model estimates by treatment block
## with significance groups
mk.cld <- function(.mod, 
    .spec = ~circuit2 + project_code,
    .dat = .mod$data,
    .model.frame = .mod$model,
    .type='response', .sort=F, .Letters=letters,
    .raw=F, .adjust='tukey',
    #.est.fmt='%1.2f',
    ## prob for logistic
    .est.col = 'response',
    .ci.fmt='%1.2f-%1.2f',
    .col.order=c('Circuit','Weeks', 'Treatment', 'Sector', 'nObs', 'Group', 'Est', 'SE', '95\\% CI')
) {
    ## explicitly pass data through to ref.grid
    ## sometimes it gets lost...
    .lsm <- lsmeans(.mod, .spec, data=.dat)
    ret <- cld(.lsm, type=.type, sort=.sort, Letters=.Letters, adjust=.adjust)
    ## return computations
    if (.raw) { 
        return(ret)
    }
    ## otherwise format for pretty printing
    ret <- mk.lsm.tab(ret, .est.col, .ci.fmt)
    ret <- within(ret, {
        ## monospace to get vertical alignment in table
        Group <- sprintf('\\texttt{%s}',gsub(' ', '~', .group))
    })
    ## get nobs
    .counts <- as.data.frame(xtabs(.spec, .model.frame))
    ## merge nobs, keep order
    ret <- join(ret, .counts)
    ## get grouping columns by pos, the rest by name
    ## cleanup
    ret <- rename(ret, c(Freq='nObs', ci='95\\% CI'))
    ret <- mk.fix.all(ret)
    ret <- ret[,.col.order]
    #ret <- na.omit(ret)
    ret
}

## convenience function
## use results from lsmeans
## compute contrast, return as df
## combine p.value of test with confidence intervals
mk.contrast <- function(
    x, ..., type='response', adjust='tukey',
    strip=NULL
) {
    contr <- contrast(x,...)
    ret <- as.data.frame(confint(contr, type=type, adjust=adjust))
    ret$p.value <- as.data.frame(summary(contr, type=type, adjust=adjust))$p.value
    ret
}

## mult comparison??
mk.bold.contr <- function(ret, p.sig=0.05) {
    ## get circuit # from contrast after stripping 
    ## optionally adjusting offset
    ## find sig to highlight
    .ip <- (ret$p.value < p.sig)
    ## bold text of contrast
    #ret[[.col]] <- as.character(ret[[.col]] )
    ret <- within(ret, {
        p.value <- sprintf('%1.3g', p.value)
        p.value[.ip] <- sprintf('\\textbf{%s}',p.value[.ip])
    })
    ret
}

## helper function
## cleanup colnames for final display
mk.cleanup.contr <- function(.tab, 
    col.order = c('Experiment','Circuit','Weeks','Treatment','Ratio', 'SE', 'p.value', 'asymp.LCL', 'asymp.UCL'),
    ## for by-time contrast
    do.rename.contr=F,
    ## name changes needed for mk.fix.all
    col.rename = c(.id='zone', response.ratio='Ratio', odds.ratio='Ratio')
) {
    if (do.rename.contr) {
        col.rename = c(col.rename, contrast='circuit2')
    }
    ret <- rename(.tab, col.rename)
    ret <- mk.fix.all(ret)
    ret <- data.table(ret, key=c('Experiment','Circuit'))
    ret[,col.order, with=F]
}
