RptCfg <- read.csv("data-raw/RptCfg.csv", as.is = TRUE)

write.csv(RptCfg, "data-raw/RptCfg.csv", row.names = FALSE)
devtools::use_data(RptCfg, overwrite = TRUE)
