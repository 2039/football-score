library(optparse)
library(jsonlite)

exports <- c("options", "save", "load")

# [o] Split into json and RDS saver/loader (maybe not json loader)
# [o] Include toggle for saving as json or RDS

options_list = list(
    optparse::make_option(c("-s", "--save"), action="store_true", default=FALSE,
    type="logical", help="store the parameters to the given filename")
,   optparse::make_option(c("-l", "--load"), action="store", default=NULL,
    type="character", help="load the parameters from the given filename")
,   optparse::make_option(c("-m", "--model"), action="store", default=NULL,
    type="character", help="select the model")
,   optparse::make_option(c("-f", "--format"), action="store", default="dat",
    type="character", help="file format")
)

options = optparse::parse_args(optparse::OptionParser(option_list=options_list))

data_path <- function(name, ext) paste0("data/", name, ".", ext)



# store data

.json_save <- function(data, filename)
    jsonlite::write_json(data, path=data_path(filename, "json"))

.RDS_save <- function(data, filename)
    saveRDS(data, file=data_path(filename, "dat"))

.csv_save <- function(data, filename)
    write.csv(data, file=data_path(filename, "csv"), row.names=F, quote=F)

save <- function(data, format=options$format, filename=options$save)
    switch(format,
        json=.json_save,
        dat=.RDS_save,
        csv=.csv_save
    )(data, filename)



# load data

.json_load <- function(filename)
    jsonlite::read_json(path=data_path(filename, "json"))

.RDS_load <- function(filename)
    readRDS(file=data_path(filename, "dat"))


load <- function(format=options$format, filename=options$load)
    switch(format, json=.json_load, dat=.RDS_load)(filename)
