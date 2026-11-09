# DataDryad README
* Data from: Efficacy of Aedes aegypti control by indoor Ultra Low Volume (ULV) insecticide spraying in Iquitos, Peru
* DOI: https://doi.org/10.5061/dryad.160023v
* Prepared by CE Gunning: https://www.x14n.org

## Dependencies (R packages)
* To load data only:
    - sqldf
    - RSQLite
* To run models / build figures:
    - see `src/run.setup.R` for a complete list

## Data Tables
* Included as gzipped csv files in dir `data/`
* Listed in `table_list.csv` (see comments there)
* Load all tables into R: `source("import.R")`

## Analysis: (load data and run models)
* Run everything: `source("MAKE.R")`
    - loads data: `import.R`
    - builds models: `src/MODELS.R`
    - builds tables & plots: `src/PLOT.R`
