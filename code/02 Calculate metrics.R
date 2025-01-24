rm(list=ls())
library(readxl)
library(dplyr)
library(lubridate)


BAs <- c("YAD", "AZPS", "DEAA", "AECI", "AVRN", "AVA", "BANC", "BPAT", "CISO", "HST", "TPWR", "TAL", "SCEG", "DUK", "FPC", "CPLE", "CPLW", "EPE", "EEI", "ERCO", "FMPP", "FPL", "GVL", "GRMA", "GLHB", "GRID", "GRIF", "ISNE", "IPCO", "IID", "JEA", "LDWP", "LGEE", "MISO", "GWA", "WWA", "NEVP", "HGMA", "NYIS", "NWMT", "OVEC", "PJM", "DOPD", "PACE", "PACW", "PGE", "AEC", "PSCO", "PNM", "CHPD", "GCPD", "PSEI", "SRP", "SCL", "SEC", "SC", "SEPA", "SOCO", "SWPP", "SPA", "TEC", "TVA", "TEPC", "TIDC", "NSB", "WALC", "WACM", "WAUW")
sort(BAs) -> BAs

colnames <- c( "Total", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023",'gen_only?','avg_demand')
id <- data.frame(matrix(ncol = length(colnames), nrow = 68))
colnames(id) <- colnames
rownames(id) <- BAs
id$`gen_only?`<- "N"

hhi <- data.frame(matrix(ncol = length(colnames), nrow = 68))
colnames(hhi) <- colnames
rownames(hhi) <- BAs
hhi$`gen_only?`<- "N"

BA <- 'PSCO'
for(BA in BAs){
  file <- paste0('data/BAs/',BA,'.xlsx')
  df <- read_xlsx(file,  guess_max=100000)
  if(df[[1,'Generation only?']]=='Y'){
    id[rownames(id)==BA,'gen_only?'] <- hhi[rownames(id)==BA,'gen_only?'] <- "Y"
    next
    }
  logical_cols <- sapply(df, is.logical)
  df[, logical_cols] <- lapply(df[, logical_cols, drop = FALSE], function(x) as.numeric(as.character(x)))
  
  # Drop unneeded vars
  drop <- c('DF','D','NG','TI','Imputed D','Imputed NG','Imputed TI')
  df <- df[,!(names(df) %in% drop)]
  df <- df %>% select(-starts_with("NG:"))                    
  df <- df %>% select(-starts_with("Imputed"))              
  df <- df %>% select(-starts_with("CO2"))         
  
  #Create Mountain Time Zone variable
  df$`UTC time` <- as.POSIXct(df$`UTC time`, tz = "UTC")
  df$mtn_time <- with_tz(df$`UTC time`, tzone='America/Denver')

  df$year <- year(df$mtn_time)
  
  df[is.na(df$`Adjusted TI`),'Adjusted TI'] <- df[is.na(df$`Adjusted TI`),'Adjusted NG'] - df[is.na(df$`Adjusted TI`),'Adjusted D']
  
  id[rownames(id)==BA,'avg_demand'] <- hhi[rownames(id)==BA,'avg_demand'] <- sum(df$`Adjusted D`, na.rm = TRUE)/nrow(df)
  
  # Import dependency
  df %>% mutate(imports = ifelse(`Adjusted TI` < 0, -`Adjusted TI`, 0)) -> df
  NAsD <- df[is.na(df$`Adjusted D`),]
  NAsTI <- df[is.na(df$`Adjusted TI`),]
  df <- df %>% filter(!is.na(`Adjusted D`))
  
  id[rownames(id)==BA,'TI_hrs_missing'] <- nrow(NAsTI)
  id[rownames(id)==BA,'D_hrs_missing'] <- nrow(NAsD)
  id[rownames(id)==BA,'Total'] <- sum(df$imports, na.rm=TRUE)/sum(df$`Adjusted D`, na.rm=TRUE)
  
  
  for(yr in 2016:2023){
    id[rownames(id)==BA, as.character(yr)] <- sum(df[df$year==yr,'imports'], na.rm=TRUE) / sum(df[df$year==yr,'Adjusted D'], na.rm=TRUE)
  }

  # Import Diversity
  # Columns in between "Adjusted..." and "CO2..." or "Subregion..."
  index1 <- max(which(grepl("^Adjusted", names(df)))) + 1
  index2 <- min(which(grepl("^(CO2|Subregion|Positive Generation)", names(df)))) - 1
  hdf <- df
  df[,c(index1:index2)] %>% mutate_all(~replace(.,.>0,0)) %>%
    mutate_all(~ replace(., is.na(.),0)) -> hdf[,c(index1:index2)]
  hdf <- hdf[,c(1:8, index1:index2)]
  hdf[,9:ncol(hdf)] <- hdf[,9:ncol(hdf)]*(-1)
  hdf$`UTC time` <- as.POSIXct(hdf$`UTC time`, tz = "UTC")
  hdf$mtn_time <- with_tz(hdf$`UTC time`, tzone='America/Denver')
  hdf$year <- year(hdf$mtn_time)
  NAsD <- hdf[is.na(hdf$`Adjusted D`),]
  hhi[rownames(hhi)==BA,'D_hrs_missing'] <- nrow(NAsD) 
  hdf[hdf$year>=2016 & hdf$year<=2023,] -> hdf
  
  #Calculate Import diversity: Each individual BA's imports (total and by year) is a share of total imports in that time period
  hhi[rownames(hhi)==BA,'Total'] <- 0
  for(col in 9:(ncol(hdf)-2)){
    p <- (sum(hdf[,col], na.rm=TRUE)/sum(hdf[,9:(ncol(hdf)-2)], na.rm=TRUE))
    if(p==0 | is.nan(p)){next}
    hhi[rownames(hhi)==BA,'Total'] <- hhi[rownames(hhi)==BA,'Total'] + p * log(p)
    #hhi[rownames(hhi)==BA,'Total'] <- hhi[rownames(hhi)==BA,'Total'] + ((sum(hdf[,col], na.rm=TRUE)/sum(hdf[,'Adjusted D'], na.rm=TRUE))*100)^2
  }
  hhi[rownames(hhi)==BA,'Total'] <- -1 * hhi[rownames(hhi)==BA,'Total']
  
  for(yr in 2016:2023){
    hhi[rownames(hhi)==BA, as.character(yr)] <- 0
    for(col in 9:(ncol(hdf)-2)){
      #p <- (sum(hdf[hdf$year==yr,col], na.rm=TRUE)/sum(hdf[hdf$year==yr,'Adjusted D'], na.rm=TRUE))
      p <- (sum(hdf[hdf$year==yr,col], na.rm=TRUE)/sum(hdf[hdf$year==yr,9:(ncol(hdf)-2)], na.rm=TRUE))
      if(p==0 | is.nan(p)){next}
      hhi[rownames(hhi)==BA, as.character(yr)] <- hhi[rownames(hhi)==BA, as.character(yr)] + p * log(p)
    }
    hhi[rownames(hhi)==BA, as.character(yr)] <- -1 * hhi[rownames(hhi)==BA, as.character(yr)]
  }
}

# Share of total imports into PGE, PSEI, and TPWR by source.


write.csv(id, "data/import dependency.csv")
write.csv(hhi, 'data/import concentration.csv')

