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

##### NOTE MD: We need a function to compute marginal estimates from generalised mixed models
#####          Code has been provided by Claude Opus 5.5
##### EDIT START #####
marg.emm <- function(.cld, .mod, .est.name = 'prob') {
    ## erwartet cld/summary-Ausgabe auf LINK-Skala
    .d <- as.data.frame(.cld)
    ## Schätzspalte heißt je nach Paketversion 'lsmean' oder 'emmean'
    .eta.col <- intersect(c('emmean', 'lsmean'), names(.d))[1]
    if (is.na(.eta.col)) stop('Keine Schätzspalte (emmean/lsmean) gefunden - type="link" verwendet?')

    ## Gesamt-SD der Random Intercepts (mehrere Intercepts addieren sich)
    .vc <- lme4::VarCorr(.mod)
    if (any(sapply(.vc, nrow) > 1))
        stop('marg.emm unterstützt nur Modelle mit reinen Random Intercepts')
    .sig <- sqrt(sum(sapply(.vc, function(v) v[1, 1])))

    ## Link-Funktion aus dem Modell (funktioniert auch für probit etc.)
    .fam <- family(.mod)
    ## E[f(eta + sig*Z)], Z ~ N(0,1), numerisch integriert
    .E <- function(eta, f) sapply(eta, function(e) {
        if (.fam$link == 'log') {
            ## geschlossene Form: E[exp(eta + sig*Z)] = exp(eta + sig^2/2)
            ## (gilt für linkinv und mu.eta, beide = exp beim Log-Link)
            exp(e + .sig^2 / 2)
        } else {
            ## endliche Grenzen: Masse von N(0,1) außerhalb ±10 ist vernachlässigbar
            integrate(function(z) f(e + .sig * z) * dnorm(z), -10, 10)$value
        }
    })
    ## SE per Delta-Methode (zuerst, solange eta noch auf Link-Skala ist)
    .d$SE <- .E(.d[[.eta.col]], .fam$mu.eta) * .d$SE
    ## CI-Grenzen transformieren (g ist monoton)
    for (.cl in intersect(c('asymp.LCL', 'asymp.UCL', 'lower.CL', 'upper.CL'), names(.d)))
        .d[[.cl]] <- .E(.d[[.cl]], .fam$linkinv)
    ## Schätzer
    .d[[.eta.col]] <- .E(.d[[.eta.col]], .fam$linkinv)
    names(.d)[names(.d) == .eta.col] <- .est.name
    .d
}
##### EDIT STOP #####

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
    .mod.fun=glm.nb, 
    ##### NOTE MD: Argument added to choose between marginal and conditional estimates for generalised mixed models
    ##### EDIT START #####
    .marginal = TRUE, 
    ##### EDIT STOP #####
    ...
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

            ##### NOTE MD: mk.models could not deal with mixed models in previous form
            #####          Changes were supported by Claude Opus 5.5

            ##### EDIT START #####
            .dat <- droplevels(.df)
            .form <- formula(.mod.form)
            mod <- .mod.fun(.form, .dat, ...)
            .is.mer <- inherits(mod, 'merMod')
            ## S3-Modelle: data wie bisher anhängen; bei S4 nicht möglich/nötig
            if (!isS4(mod)) mod$data <- .dat
            .summary <- summary(mod)

            ## goodness of fit
            if (.is.mer) {
                .rsq <- NA   # siehe Hinweis unten
            } else {
                .rsq <- 1 - mod$deviance / mod$null.deviance
            }
            goodness <- data.frame(
                rsq = .rsq,
                deviance = deviance(mod),
                AIC = AIC(mod)
            )
            anova <- Anova(mod)
            anova <- cbind(coef=rownames(anova), as.data.frame(anova))

            .tab <- .summary$coefficients[, c(1,4), drop=FALSE]
            .tab <- aaply(.tab, 1, function(x) {
                if (x[2] > .thresh) x[1:2] <- NA
                x
            })
            coef.tab <- data.frame(
                Parameter=rownames(.summary$coefficients),
                Estimate=.tab[,1], `P-value`=.tab[,2]
            )
            ## estimated dispersion: glm.nb bzw. glmer.nb
            .theta.val <- NULL
            if ('theta' %in% names(.summary)) {
                .theta.val <- .summary$theta
            } else if (.is.mer && grepl('Negative Binomial', family(mod)$family)) {
                .theta.val <- lme4::getME(mod, 'glmer.nb.theta')
            }
            if (!is.null(.theta.val)) {
                .theta <- data.frame(Parameter='Theta', Estimate=.theta.val, `P-value`=NA)
                coef.tab <- rbind(.theta, coef.tab)
            }
            coef.tab <- cbind(.cond.df, coef.tab)

            ## model.frame() statt mod$model
            mod.df <- cbind(model.frame(mod), .cond.df)
            mod.df$fit <- fitted(mod)
            mod.df$resid_response <- residuals(mod, type='response')
            mod.df$resid_pearson <- residuals(mod, type='pearson')

            ## feste Effekte
            .coef <- if (.is.mer) lme4::fixef(mod) else coef(mod)
            if (.coef.unlog) {
                coef.tab$Estimate <- exp(coef.tab$Estimate)
                .coef <- exp(.coef)
            }
            ##### EDIT STOP #####

            ##### NOTE MD: Edit code to compute marginal estimates for generalised mixed models
            #####          Code generated by Claude Opus 5.5
            ##### EDIT START #####
            if (.do.hypoth) {
                if (.coef.unlog) {
                    .type <- 'response'
                } else {
                    .type <- 'link'
                }
                mod.lsm <- lsmeans(mod, .hypoth.form, adjust=.adjust, data=.dat)
                if (.is.mer && .marginal && .type == 'response') {
                    ## marginal: auf Link-Skala rechnen, dann RE herausintegrieren
                    .cld.link <- cld(mod.lsm, type='link', sort=.sort, adjust=.adjust)
                    .cld.df <- marg.emm(.cld.link, mod, .est.name='prob')
                } else {
                    .cld.df <- cld(mod.lsm, type=.type, sort=.sort, adjust=.adjust)
                }
                hypoth.df <- cbind(.cond.df, .cld.df)
            }
            ##### EDIT STOP #####

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
##### NOTE MD: Previous version was not able to handel mixed models
#####          More flexible code provided by Claude Opus 5.5
##### EDIT START #####
mk.cld <- function(.mod, 
    .spec = ~circuit2 + project_code,
    .dat = NULL,
    .model.frame = model.frame(.mod),
    .type='response', .sort=F, .Letters=letters,
    .raw=F, .adjust='tukey',
    .est.col = 'response',
    .ci.fmt='%1.2f-%1.2f',
    .col.order=c('Circuit','Weeks', 'Treatment', 'Sector', 'nObs', 'Group', 'Est', 'SE', '95\\% CI'),
    ##### NOTE MD: Get marginal (or conditional) values from generalised mixed models
    ##### EDIT START #####
    .marginal = TRUE 
    ##### EDIT STOP #####
) {
    ## Daten bestimmen: S3 (glm/glm.nb) wie bisher, S4 (merMod) über model.frame
    if (is.null(.dat)) {
        .dat <- if (isS4(.mod)) model.frame(.mod) else .mod$data
    }
    .lsm <- lsmeans(.mod, .spec, data=.dat)
    
    ##### NOTE MD: Output marginal values from generalised mixed models if required
    #####          Code generated by Claude Opus 5.5
    ##### EDIT START #####
    .lsm <- lsmeans(.mod, .spec, data=.dat)
    if (isS4(.mod) && .marginal && .type == 'response') {
        ret <- marg.emm(
            cld(.lsm, type='link', sort=.sort, Letters=.Letters, adjust=.adjust),
            .mod, .est.name=.est.col
        )
    } else {
        ret <- cld(.lsm, type=.type, sort=.sort, Letters=.Letters, adjust=.adjust)
    }
    ##### EDIT STOP #####
    
    if (.raw) { 
        return(ret)
    }
    ret <- mk.lsm.tab(ret, .est.col, .ci.fmt)
    ret <- within(ret, {
        Group <- sprintf('\\texttt{%s}', gsub(' ', '~', .group))
    })
    .counts <- as.data.frame(xtabs(.spec, .model.frame))
    ret <- join(ret, .counts)
    ret <- rename(ret, c(Freq='nObs', ci='95\\% CI'))
    ret <- mk.fix.all(ret)
    ret <- ret[,.col.order]
    ret
}
##### EDIT STOP #####

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
    ##### NOTE MD: We want all result columns for different models to be named 'Ratio'
    ##### EDIT START #####
    col.rename = c(.id='zone', response.ratio='Ratio', odds.ratio='Ratio', rate.ratio='Ratio')
    ##### EDIT STOP #####
) {
    if (do.rename.contr) {
        col.rename = c(col.rename, contrast='circuit2')
    }
    ret <- rename(.tab, col.rename)
    ret <- mk.fix.all(ret)
    ret <- data.table(ret, key=c('Experiment','Circuit'))
    ret[,col.order, with=F]
}
