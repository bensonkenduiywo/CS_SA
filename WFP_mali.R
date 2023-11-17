library(haven)
path <- 'D:/OneDrive - CGIAR/SA_Team/Data/Mali/'
df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
temp <- df1[df1$ADMIN0Name=='MALI', ]
temp <- aggregate(FCS~YEAR+ADMIN2Name, data=temp,mean, na.rm=T)
View(temp[,c('YEAR',"FCS", "ADMIN2Name")])
#write.csv(df1, paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.csv'))
df1 <- aggregate(FCS~YEAR+ADMIN2Name, data=df1,mean, na.rm=T)
df2 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience_stratebmz.sav'))
#write.csv(df2, paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience_stratebmz.csv'))
View(df2[,c('YEAR',"FCS", "ADMIN2Name")])