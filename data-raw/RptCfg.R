RptCfg <- read.csv("data-raw/RptCfg.csv", as.is = TRUE)
Abbr <- read.csv("data-raw/Abbr.csv", as.is = TRUE)

# These need to be used externally.
devtools::use_data(RptCfg, overwrite = TRUE) # Used externally

# These need to be used internally.
devtools::use_data(RptCfg, Abbr, overwrite = TRUE, internal = TRUE) # Used internally
