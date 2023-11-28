library(readxl)
root <- "D:/OneDrive - CGIAR/SA_Team/"
#fcs
read_fcs <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, FCS))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
fcs_2019 <- read_fcs(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_FCS")
fcs_2020 <- read_fcs(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_FCS")
fcs_2021 <- read_fcs(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_FCS")
View(fcs_2020)
#get unique county names
counties <- unique(fcs_2019$County)

calculate_mean <- function(df, counties, name){
  final_df <- data.frame(county=character(), name=numeric())
  for (county in counties){
    county_df <- df[df$County == county, ]
    mean <- mean(county_df$max)
    final_df <- rbind(final_df, c(county, mean))
  }
  colnames(final_df)[1] <- "County"
  colnames(final_df)[2] <- name
  return(final_df)
}
final_fcs_2019 <- calculate_mean(fcs_2019, counties, "fcs_2019")
final_fcs_2020 <- calculate_mean(fcs_2020, counties, "fcs_2020")
final_fcs_2021 <- calculate_mean(fcs_2021, counties, "fcs_2021")
View(final_fcs_2019)
#2022
fcs_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="FCS_2022")
fcs_months <- subset(fcs_2022,select=-1)
View(final_fcs_2020)
fcs_months <- fcs_months*100
fcs_2022$max_poor <- pmax(fcs_months$jan_poor,fcs_months$april_poor, fcs_months$may_poor, fcs_months$june_poor, na.rm=TRUE)
fcs_2022$max_borderline <- pmax(fcs_months$jan_borderline,fcs_months$april_borderline, fcs_months$may_borderline, fcs_months$june_borderline, na.rm=TRUE)
fcs_2022$fcs_2022 <- rowMeans(fcs_2022[,c("max_poor","max_borderline")])
final_fcs_2022 <- subset(fcs_2022, select=c("County", "fcs_2022"))
#average fcs over the years
fcs_19_20 <- merge(final_fcs_2019,final_fcs_2020, by = "County")
fcs_21_22 <- merge(final_fcs_2021,final_fcs_2022, by = "County")
View(fcs)
fcs <- merge(fcs_19_20,fcs_21_22, by = "County")
fcs$fcs_2019 <- as.numeric(fcs$fcs_2019)
fcs$fcs_2020 <- as.numeric(fcs$fcs_2020)
fcs$fcs_2021 <- as.numeric(fcs$fcs_2021)
fcs$fcs_2022 <- as.numeric(fcs$fcs_2022)
fcs$fcs <- round(rowMeans(fcs[,c("fcs_2019","fcs_2020","fcs_2021","fcs_2022")]),1)
fcs <- subset(fcs, select=c("County","fcs"))

#rCSI
read_rcsi <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, rCSI))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
rcsi_2019 <- read_rcsi(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_rCSI")
rcsi_2020 <- read_rcsi(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_rCSI")
rcsi_2021 <- read_rcsi(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_rCSI")
View(rcsi)
final_rcsi_2019 <- calculate_mean(rcsi_2019, counties,"rcsi_2019")
final_rcsi_2020 <- calculate_mean(rcsi_2020, counties,"rcsi_2020")
final_rcsi_2021 <- calculate_mean(rcsi_2021, counties,"rcsi_2021")
rcsi_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="RCSI_2022")
rcsi_months <- subset(rcsi_2022,select=-1)
rcsi_months <- rcsi_months * 100
rcsi_2022$max_stressed <- pmax(rcsi_months$jan_stressed,rcsi_months$april_stressed,rcsi_months$may_stressed,rcsi_months$june_stressed,na.rm=TRUE)
rcsi_2022$max_crisis <- pmax(rcsi_months$jan_crisis,rcsi_months$april_crisis,rcsi_months$may_crisis,rcsi_months$june_crisis,na.rm=TRUE)
rcsi_2022$rcsi_2022 <- rowMeans(rcsi_2022[,c("max_stressed","max_crisis")])
final_rcsi_2022 <- subset(rcsi_2022, select=c("County", "rcsi_2022"))
rcsi_19_20 <- merge(final_rcsi_2019,final_rcsi_2020, by="County")
rcsi_21_22 <- merge(final_rcsi_2021,final_rcsi_2022, by="County")
rcsi <- merge(rcsi_19_20, rcsi_21_22, by="County")
rcsi$rcsi_2019 <- as.numeric(rcsi$rcsi_2019)
rcsi$rcsi_2020 <- as.numeric(rcsi$rcsi_2020)
rcsi$rcsi_2021 <- as.numeric(rcsi$rcsi_2021)
rcsi$rcsi_2022 <- as.numeric(rcsi$rcsi_2022)
rcsi$rcsi <- round(rowMeans(rcsi[,c("rcsi_2019","rcsi_2020","rcsi_2021","rcsi_2022")]),1)
rcsi <- subset(rcsi, select=c("County","rcsi"))

#livelihood
read_livelihood <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, Coping))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
lh_2019 <- read_livelihood(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_Livelihood")
lh_2020 <- read_livelihood(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_Livelihood")
lh_2021 <- read_livelihood(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_Livelihood")
View(lh)
final_lh_2019 <- calculate_mean(lh_2019,counties,"lh_2019")
final_lh_2020 <- calculate_mean(lh_2020,counties,"lh_2020")
final_lh_2021 <- calculate_mean(lh_2021,counties,"lh_2021")
lh_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="Livelihood_coping")
lh_months <- subset(lh_2022,select=-1)
lh_months <- lh_months * 100
lh_2022$max_crisis <- pmax(lh_months$april_crisis,lh_months$may_crisis,lh_months$june_crisis,lh_months$jan_crisis,na.rm=TRUE)
lh_2022$max_emerg <- pmax(lh_months$april_emergencies,lh_months$may_emergencies,lh_months$june_emergencies,lh_months$jan_emergencies,na.rm=TRUE)
lh_2022$lh_2022 <- rowMeans(lh_2022[,c("max_crisis","max_emerg")])
final_lh_2022 <- subset(lh_2022, select=c("County","lh_2022"))
lh_19_20 <- merge(final_lh_2019,final_lh_2020, by="County")
lh_21_22 <- merge(final_lh_2021,final_lh_2022, by="County")
lh <- merge(lh_19_20,lh_21_22, by="County")
lh$lh_2019 <- as.numeric(lh$lh_2019)
lh$lh_2020 <- as.numeric(lh$lh_2020)
lh$lh_2021 <- as.numeric(lh$lh_2021)
lh$lh_2022 <- as.numeric(lh$lh_2022)
lh$lh <- round(rowMeans(lh[,c("lh_2019","lh_2020","lh_2021","lh_2022")]))
lh <- subset(lh, select=c("County","lh"))
