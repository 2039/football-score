library(optparse)

exports <- c("options", "save", "load")

options_list = list(
    optparse::make_option(c("-s", "--save"), action="store", default=NULL,
    type="character", help="store the parameters to the given filename")
,   optparse::make_option(c("-l", "--load"), action="store", default=NULL,
    type="character", help="load the parameters from the given filename")
,   optparse::make_option(c("-m", "--model"), action="store", default=NULL,
    type="character", help="select the model")
)

options = optparse::parse_args(optparse::OptionParser(option_list=options_list))

save <- function(data)
    # paste0 == paste(sep="")
    saveRDS(data, file=paste0("data/", options$save, ".dat"))


load <- function()
    # paste0 == paste(sep="")
    readRDS(file=paste0("data/", options$load, ".dat"))
