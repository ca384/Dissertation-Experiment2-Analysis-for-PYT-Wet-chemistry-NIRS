library(readxl)
library(tidyverse)
# The NIRS scans for 2019/2020
umu_freshRep1_20<-read_excel("/Users/ca384/Downloads/chiPYT_Um.xlsx") 
head(umu_fresh20[1:6,1:6])
tail(umu_fresh20[1:6,1:6])
dim(umu_freshRep1_20)

Sel_umu_freshRep1_20 = umu_freshRep1_20[,-c(2:4)]

colnames(Sel_umu_freshRep1_20)[colnames(Sel_umu_freshRep1_20)=="Accession name"] <- "unique.id"                                               
Sel_umu_freshRep1_20 = as.data.frame(Sel_umu_freshRep1_20)
head(Sel_umu_freshRep1_20[1:15,1:15])
Mean_umu_freshRep1_20 <-Sel_umu_freshRep1_20 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)
head(Mean_umu_freshRep1_20[1:15,1:15])
dim(Mean_umu_freshRep1_20) 
rownames(Mean_umu_freshRep1_20) # has 176 genotypes


umu_freshRep2_20 <-read_excel("/Users/ca384/Downloads/ChiPYTR2.xlsx") 
head(umu_freshRep2_20[1:6,1:6])
tail(umu_freshRep2_20[1:6,1:6])
dim(umu_freshRep2_20)

Sel_umu_freshRep2_20 = umu_freshRep2_20[,-c(2:4)]

colnames(Sel_umu_freshRep2_20)[colnames(Sel_umu_freshRep2_20)=="Plot"] <- "unique.id"                                               
Sel_umu_freshRep2_20 = as.data.frame(Sel_umu_freshRep2_20)
head(Sel_umu_freshRep2_20[1:15,1:15])
Mean_umu_freshRep2_20 <-Sel_umu_freshRep2_20 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)
head(Mean_umu_freshRep2_20[1:15,1:15])
dim(Mean_umu_freshRep2_20) 
rownames(Mean_umu_freshRep2_20) # has 173 genotypes




oto_freshRep1_20<-read_excel("/Users/ca384/Downloads/ChiPYTOtR1.xlsx") 
head(oto_freshRep1_20[1:6,1:6])
tail(oto_freshRep1_20[1:6,1:6])
dim(oto_freshRep1_20)

Sel_oto_freshRep1_20 = oto_freshRep1_20[,-c(2:4)]

colnames(Sel_oto_freshRep1_20)[colnames(Sel_oto_freshRep1_20)=="Accession name"] <- "unique.id"                                               
Sel_oto_freshRep1_20 = as.data.frame(Sel_oto_freshRep1_20)
head(Sel_oto_freshRep1_20[1:15,1:15])
Mean_oto_freshRep1_20 <-Sel_oto_freshRep1_20 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)
head(Mean_oto_freshRep1_20[1:15,1:15])
dim(Mean_oto_freshRep1_20) 
rownames(Mean_oto_freshRep1_20) # has 166 genotypes


oto_freshRep2_20<-read_excel("/Users/ca384/Downloads/ChiPYTOtR2.xlsx") 
head(oto_freshRep2_20[1:6,1:6])
tail(oto_freshRep2_20[1:6,1:6])
dim(oto_freshRep2_20)
Sel_oto_freshRep2_20 = oto_freshRep2_20[,-c(2:4)]


colnames(Sel_oto_freshRep2_20)[colnames(Sel_oto_freshRep2_20)=="Accession name"] <- "unique.id"                                               
Mean_oto_freshRep2_20 <-Sel_oto_freshRep2_20 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)
head(Mean_oto_freshRep2_20[1:15,1:15])
Mean_oto_freshRep2_20=as.data.frame(Mean_oto_freshRep2_20)

dim(Mean_oto_freshRep2_20) 
rownames(Mean_oto_freshRep2_20) # has 175 genotypes


Otobi_garri_20 <-read_excel("/Users/ca384/Downloads/2021 chichi otobi garri.xlsx") 
Otobi_garri_20<- as.data.frame(Otobi_garri_20)
head(Otobi_garri_20[1:15,1:15])
head(Otobi_garri_20[1:6,1:6])
Sel_oto_gari_20 = Otobi_garri_20[,-c(2:5)]

colnames( Sel_oto_gari_20)[colnames(Sel_oto_gari_20)=="sample _id"] <- "unique.id"
Mean_oto_gari_20 <- Sel_oto_gari_20 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)
head(Mean_oto_gari_20[1:15,1:15])
dim(Mean_oto_gari_20) # 42 genotypes



# 2021 Nirs data
Otobi_garri_21 <-read_excel("/Users/ca384/Downloads/2021 chichi otobi garri.xlsx") 

