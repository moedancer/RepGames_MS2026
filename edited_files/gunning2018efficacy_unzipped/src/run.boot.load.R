## save results to cache file
.boot.cachefile <- sprintf('cache.boot.ci-%s.RData',.nboot )
## re-bootstrap if asked, or cache file does not exist
if (.do.bootstrap ||(length(dir(patt=.boot.cachefile)) ==0)) {
    ## draw, save to cache file
    source('run.boot.circuit.R')
} else {
    load(.boot.cachefile)
}

## then cleanup
## fix zone factor levels / order
.dat.boot.ci <- within(.dat.boot.ci, {
    zone <- mk.revalue.zone(zone)
    Treatment <- mk.revalue.project_code(project_code)
    response_name <- mk.revalue.response_name(response_name )
})
## in a few cases, a small number of samples are in the wrong week??
.dat.boot.ci <- subset(.dat.boot.ci, upper_nobs>10)
#))
