## read data files into SQLite db,
## set persistent sqldf.connection
source('import.R')
## use above, build all models
source('src/MODELS.R', chdir=TRUE)
## use above, build all tables & plots
source('src/PLOT.R', chdir=TRUE)
