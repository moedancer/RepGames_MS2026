library(sqldf)
library(RSQLite)
## create persistent connection
dbcon <- dbConnect(RSQLite::SQLite(), ":memory:")
## explicitly set sqldf options
options(
    sqldf.driver="SQLite",
    sqldf.connection=dbcon
)


#source('run.setup.R')
## location of csv.gz files
indir <- "./data/"
## 
tab.names <- read.csv(
    'table_list.csv',
    comment.char='#', 
    stringsAsFactors=FALSE
)

## read each table into R
## push to SQLite db
not_used <- lapply(tab.names$name, function(tab) {
    fn <- sprintf('%s/%s.csv.gz', indir, tab)
    dat <- read.csv(fn)
    dbWriteTable(dbcon, tab, dat)
    invisible()
})
