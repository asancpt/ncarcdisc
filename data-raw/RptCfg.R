RptCfg <- read.csv("data-raw/RptCfg.csv", as.is = TRUE)

# write.csv(RptCfg, "data-raw/RptCfg.csv", row.names = FALSE)

# This RptCfg is need to be used externally and internally
devtools::use_data(RptCfg, overwrite = TRUE) # Used externally
devtools::use_data(RptCfg, overwrite = TRUE, internal = TRUE) # Used internally
