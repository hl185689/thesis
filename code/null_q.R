working.dir <- "C:\\Users\\hl185689\\Documents\\Thesis\\Data\\tsmart_um_mt_analytic_20200210\\"
d <- readr::read_tsv(paste0(working.dir,"tsmart_um_mt_analytic_20200210.csv"))


colSums(is.na(d))
