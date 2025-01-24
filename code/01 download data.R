rm(list=ls())

#install.packages(c("readxl", "httr"))
library(readxl)
library(httr)
options(timeout=1000)


BAs <- c("YAD", "AZPS", "DEAA", "AECI", "AVRN", "AVA", "BANC", "BPAT", "CISO", "HST", "TPWR", "TAL", "SCEG", "DUK", "FPC", "CPLE", "CPLW", "EPE", "EEI", "ERCO", "FMPP", "FPL", "GVL", "GRMA", "GLHB", "GRID", "GRIF", "ISNE", "IPCO", "IID", "JEA", "LDWP", "LGEE", "MISO", "GWA", "WWA", "NEVP", "HGMA", "NYIS", "NWMT", "OVEC", "PJM", "DOPD", "PACE", "PACW", "PGE", "AEC", "PSCO", "PNM", "CHPD", "GCPD", "PSEI", "SRP", "SCL", "SEC", "SC", "SEPA", "SOCO", "SWPP", "SPA", "TEC", "TVA", "TEPC", "TIDC", "NSB", "WALC", "WACM", "WAUW")

#Code to restart partway through since this download process isn't super reliable
#index <- match("SPA", BAs); BAs <- BAs[index:length(BAs)]

for(ba in BAs){
  url <- paste0('https://www.eia.gov/electricity/gridmonitor/knownissues/xls/',ba,'.xlsx')
  file <- paste0('data/',ba,'.xlsx')
  download.file(url, file, mode='wb')
  Sys.sleep(10)
}
