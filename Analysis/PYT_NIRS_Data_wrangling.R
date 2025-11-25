packages_used <- c("readxl","devtools","tidyverse", "ggplot2","magrittr",
                   "stringr" , "prospectr","tidyr", "dplyr", "tibble",
                   "waves", "reshape2")
ip <- installed.packages()
all_packages_installed <- TRUE
for (package in packages_used){
  if (!(package %in% ip[,"Package"])){
    print(paste("Please install package", package))
    install.packages(package)
    all_packages_installed <- FALSE
  } else {
    library(package, character.only = T) # load required package
  } # end else statement
}#END packages_used
if (!all_packages_installed) stop(" install packages")

# oto_garri_nirs20 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/NIRS/2019_2020/Otobi/2019-2020-PYT-GARRI-OTOBI.csv")
# 
# oto_garri_nirs21<-read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/NIRS/2020_2021/Otobi/otobi-garri2020-2021.csv")
# summary(oto_garri_nirs21)

All_chips <- read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/Wet_chemistry/PYT/All_chips_no_DMC.csv")
All_dmc <- read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/Wet_chemistry/PYT/All_dmc.csv")
all_garri<-read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/Wet_chemistry/PYT/All_garri_chem_func.csv")
all_garri2<-all_garri %>%
  dplyr::select( "Location","Year","Genotype","SI",      
                 "SP" , "WAC" , "BD", "AMY",     
                 "CF" , "STC","SC")

oto_garri=all_garri2$Location =="Otobi"
oto_garri <- all_garri2[all_garri2$Year == 2021 & all_garri2$Location == "Otobi", ]

umu_garri <- all_garri2[all_garri2$Year == 2021 & all_garri2$Location == "Umudike", ]


##roots2019/2020
otoR1_fresh_nirs21 <-read_excel("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/otobi_2020_2021/2021 chichi otobi fresh root R1.xlsx",
                                sheet = 1, col_names = T)

head(otoR1_fresh_nirs21[,1:10])
dim(otoR1_fresh_nirs21)
str(otoR1_fresh_nirs21)
unique(otoR1_fresh_nirs21$unique_id)
sp1 <- 350:(348+dim(otoR1_fresh_nirs21)[2])
length(sp1)
sp1X <- c()
for(i in sp1){
  xx <- paste(c("X",i),collapse = "")
  sp1X <- c(sp1X,xx)
}

colnames(otoR1_fresh_nirs21) <- c("unique_id","portion", sp1X)
# head(otoR1_fresh_nirs21[,1:10])
# len <- dim(otoR1_fresh_nirs21)[2]
# head(otoR1_fresh_nirs21[,(len-4):len])
#view(otoR1_fresh_nirs21) # Assign row names using dimnames attribute
library(readxl)
otoR2_fresh_nirs21 <-read_excel("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/otobi_2020_2021/2021 chichi otobi fresh root R2.xlsx",
                                sheet = 1, col_names = T )

Oto_garri_20<-read_excel ("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/otobi_2020_2021/2021_otobi gari flour.xlsx", sheet = 1, col_names = T)

head(otoR2_fresh_nirs21[,1:10])
dim(otoR2_fresh_nirs21)
str(otoR2_fresh_nirs21)
unique(otoR2_fresh_nirs21$unique_id)
sp2 <- 350:(348+dim(otoR2_fresh_nirs21)[2]) # the raw data had no spectrum number so this code is adding the number to the length of the data frame
length(sp2)
sp2X <- c()
for(i in sp2){
  xx1 <- paste(c("X",i),collapse = "")
  sp2X <- c(sp2X,xx1) # adding x to the spectrum number
}


colnames(otoR2_fresh_nirs21) <- c("unique_id","portion", sp2X)
# head(otoR2_fresh_nirs21[,1:10])

oto_nirs21<-rbind(otoR1_fresh_nirs21, otoR2_fresh_nirs21)
str(oto_nirs21[,1:20])


oto_nirs21$unique_id= as.factor(oto_nirs21$unique_id)
#otoR1_fresh_nirs21 <- otoR1_fresh_nirs21 %>% 
#dplyr::rename(plot = code)
oto_nirs21<- oto_nirs21 %>%
  dplyr::rename("unique.id" = "unique_id" )

head(oto_nirs21[,1:10])
oto_nirs21_Agg <- oto_nirs21[,-2] %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean) # aggrgate all to have one genotype that is removing the portion

oto_nirs21_Agg <- as.data.frame(oto_nirs21_Agg)


# view the spectra
plot_spectra(oto_nirs21_Agg,
             num.col.before.spectra = 1,
             detect.outliers = FALSE,
             alternate.title = "oto_fresh_spectra")

library(factoextra)
library(FactoMineR)

head(oto_nirs21_Agg[,1:10])
oto_nirs21_Agg_pca=oto_nirs21_Agg
rownames(oto_nirs21_Agg_pca) <- oto_nirs21_Agg_pca$unique.id
oto_nirs21_Agg_pca = oto_nirs21_Agg_pca[,-1]
pca_spec = PCA(oto_nirs21_Agg_pca,scale.unit = T)# pca of spectra data to see if there are outliers
pc_cov = pca_spec$ind$coord
library(prospectr)
spec_NIR <-savitzkyGolay(
  X = oto_nirs21_Agg_pca,
  m = 1,
  p = 1,
  w = 15,
  delta.wav = 2
)

head(spec_NIR[,1:10])
spec_NIR2 <- spec_NIR
rownames(spec_NIR2) <- NULL
dim(spec_NIR2)
colnames(spec_NIR2) <- colnames(spec_NIR)
colnames(spec_NIR2) <- gsub(pattern = "X", replacement = "", colnames(spec_NIR2))
matplot(as.numeric(colnames(spec_NIR2)),
        t(spec_NIR2),
        type = "l",
        xlab = "Wavelength /nm",
        ylab = "1st derivative"
)

library(rrBLUP)
library(dplyr)
specMat =  (spec_NIR2%*% t(spec_NIR2))/ncol(spec_NIR2)
spec_NIR_scale  <- scale(spec_NIR2, center = T, scale = T)
specMat <- A.mat(X = spec_NIR_scale)
dim(specMat)
colnames(specMat) <- rownames(spec_NIR)
rownames(specMat) <- rownames(spec_NIR)


garri_data7<-read.csv("/Users/ca384/Documents/ChinedoziRepo/GarriQuality/output/data7_garri.csv")
garri_data7a = garri_data7
garri_data7a= garri_data7%>%
  dplyr::select("Location","Year",  "Genotype", "SI", 
                "SP", "WAC"  ,  "BD",  "AMY", "CF" , "STC" , "SC")

garri_data7a_oto <- garri_data7a[garri_data7a$Location=="Otobi" & garri_data7a$Year==2021,]

specMat[1:10,1:10]
idsp <- which(garri_data7a_oto$Genotype  %in% rownames(specMat))
BLUEMean_Gar_sel  <- garri_data7a_oto[idsp,]
idm <- which(rownames(specMat) %in% BLUEMean_Gar_sel$Genotype)
specMat1 <- specMat[idm,idm]
ans <-  kin.blup(data =BLUEMean_Gar_sel, geno ="Genotype", pheno = "SC", 
                 K =  specMat1  )
cor(as.vector(ans$pred), BLUEMean_Gar_sel$WAC, use="pairwise")
head(garri_data7a_oto)


################
#using garri to predict garri

head(Oto_garri_20[,1:10])
dim(Oto_garri_20)
str(Oto_garri_20)
unique(Oto_garri_20$NR17C2aF120P002)
sp4 <- 350:(348+dim(Oto_garri_20)[2]) # the raw data had no spectrum number so this code is adding the number to the length of the data frame
length(sp4)
sp4X <- c()
for(i in sp4){
  xx4 <- paste(c("X",i),collapse = "")
  sp4X <- c(sp4X,xx4) # adding x to the spectrum number
}


colnames(Oto_garri_20) <- c("unique_id", sp4X)
head(Oto_garri_20[,1:10])


Oto_garri_20$unique_id= as.factor(Oto_garri_20$unique_id)
#otoR1_fresh_nirs21 <- otoR1_fresh_nirs21 %>% 
#dplyr::rename(plot = code)
Oto_garri_20=Oto_garri_20 %>%
  dplyr::rename("unique.id" = "unique_id" )

head(Oto_garri_20[,1:10])
Oto_garri_20_Agg <- Oto_garri_20[,-2] %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean) # aggrgate all to have one genotype that is removing the portion

Oto_garri_20_Agg  <- as.data.frame(Oto_garri_20_Agg )
head(Oto_garri_20_Agg[,1:10])

# view the spectra
plot_spectra(Oto_garri_20_Agg,
             num.col.before.spectra = 1,
             detect.outliers = FALSE,
             alternate.title = "oto_garri_spectra")

library(factoextra)
library(FactoMineR)

head(Oto_garri_20_Agg[,1:10])
Oto_garri_20_Agg_pca=Oto_garri_20_Agg
rownames(Oto_garri_20_Agg_pca) <- Oto_garri_20_Agg_pca$unique.id
Oto_garri_20_Agg_pca = Oto_garri_20_Agg_pca[,-1]
pca_spec_garri = PCA(Oto_garri_20_Agg_pca,scale.unit = T)# pca of spectra data to see if there are outliers
pc_cov_garri = pca_spec_garri$ind$coord
library(prospectr)
spec_NIR_garri <-savitzkyGolay(
  X = Oto_garri_20_Agg_pca,
  m = 1,
  p = 1,
  w = 15,
  delta.wav = 2
)

head(spec_NIR_garri[,1:10])
spec_NIR3 <- spec_NIR_garri
rownames(spec_NIR3) <- NULL
dim(spec_NIR3)
colnames(spec_NIR3) <- colnames(spec_NIR_garri)
colnames(spec_NIR3) <- gsub(pattern = "X", replacement = "", colnames(spec_NIR3))
matplot(as.numeric(colnames(spec_NIR3)),
        t(spec_NIR3),
        type = "l",
        xlab = "Wavelength /nm",
        ylab = "1st derivative"
)

specMat_garri =  (spec_NIR3%*% t(spec_NIR3))/ncol(spec_NIR3)
spec_NIR_scale_garri  <- scale(spec_NIR3, center = T, scale = T)
specMat_garri <- A.mat(X = spec_NIR_scale_garri)
dim(specMat_garri)
colnames(specMat_garri) <- rownames(spec_NIR_garri)
rownames(specMat_garri) <- rownames(spec_NIR_garri)


specMat_garri[1:10,1:10]
idsp_garri <- which(garri_data7a_oto$Genotype  %in% rownames(specMat_garri))
BLUEMean_Gar_sel_gar  <- garri_data7a_oto[idsp_garri,]

idm_garri <- which(rownames(specMat_garri) %in% BLUEMean_Gar_sel_gar$Genotype)
specMat2 <- specMat_garri[idm_garri,idm_garri]
ans <-  kin.blup(data =BLUEMean_Gar_sel_gar, geno ="Genotype", pheno = "BD", 
                 K =  specMat2)
cor(as.vector(ans$pred), BLUEMean_Gar_sel_gar$BD, use="pairwise")

ans <-  kin.blup(data =BLUEMean_Gar_sel_gar, geno ="Genotype", pheno = "SP", 
                 K =  specMat2)
cor(as.vector(ans$pred), BLUEMean_Gar_sel_gar$SP, use="pairwise")

head(garri_data7a_oto)







############################

# pretreatment 
oto_21 <- Oto_garri_20_Agg %>% 
  dplyr::rename(unique.id = unique.id)%>%
  pretreat_spectra(pretreatment = 1:13) %>% # exclude pretreatment 1 (raw data)
  bind_rows(.id = "pretreatment") %>%
  gather(key = "wl",
         value = "s.value",
         tidyselect::starts_with("X")) %>%
  mutate(wl = as.numeric(readr::parse_number(.data$wl)),
         pretreatment = as.factor(pretreatment)) %>%
  drop_na(s.value) %>%
  ggplot(data = ., aes(x = wl, y = s.value, group = unique.id)) +
  geom_line(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Pretreated spectra",
       x = "Wavelength",
       y = "Spectral Value") +
  
  facet_wrap( ~ pretreatment, scales = "free")

#attach the reference data with the aggregate nirs data



all_garri2<-all_garri %>%
  dplyr::select( "Location","Year","Genotype","SI",      
                 "SP" , "WAC" , "BD", "AMY",     
                 "CF" , "STC","SC")



#oto_garri <- all_garri2[all_garri2$Year == 2020 & all_garri2$Location == "Otobi", ]
#oto_garri2 <- all_garri2[all_garri2$Location == "Otobi" & all_garri2$Year == 2021,]

#umu_garri2 <- all_garri2[all_garri2$Location == "Umudike", ]

#oto_garri3 = oto_garri2 %>%
#  dplyr::rename("unique.id"= "Genotype")

#umu_garri3 = umu_garri2%>%
# dplyr::rename("unique.id" = "Genotype")

#merge the nirs and lab data for otobi
oto_Nirs_lab <- merge(oto_garri3, Oto_garri_20_Agg, by = "unique.id", all = TRUE)


quartiles_WAC <- quantile(oto_Nirs_lab$WAC, probs=c(.25, .75), na.rm = TRUE) # 
IQR_WAC <- IQR(oto_Nirs_lab$WAC, na.rm = TRUE) # interquatile range  

Lower_WAC <- quartiles_WAC[1] - 1.5*IQR_WAC
Upper_WAC <- quartiles_WAC[2] + 1.5*IQR_WAC

clean_WAC <- subset(oto_Nirs_lab, oto_Nirs_lab$WAC > Lower_WAC & oto_Nirs_lab$WAC < Upper_WAC)


# take turns to select the reference data by excluding other lab data
oto_Nirs_lab.prepped <- clean_WAC  %>%
  dplyr::rename(reference = WAC ) %>%
  dplyr::select(unique.id, dplyr::everything(),-Location, -Year, -SP ,  -SI , -BD, -AMY , -CF ,-STC, -SC) %>%
  na.omit() 

filtered.df <- oto_Nirs_lab.prepped %>%
  filter_spectra(
    df = .,
    filter = TRUE,
    return.distances = TRUE,
    num.col.before.spectra = 2,
    window.size = 15
  )

results.list <- filtered.df%>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  test_spectra(
    train.data = .,
    tune.length = 20,
    num.iterations = 50,
    pretreatment = 1:13
  )
results.list$summary.model.performance
results.list$importance
results.list$predictions

filtered.df1 <- filtered.df[,-c(2,2154)]
head(filtered.df1[,1:10])
filtered.df1$study.name <- "Garrispec"
colnames(filtered.df1)[1] <- "sample.id"
dim(filtered.df1)
filtered.df1 <- filtered.df1[,c(2153,1:2152)]
head(filtered.df1[,1:10])
pretreat_wac= pretreat_spectra(
  filtered.df1,
  test.data = NULL,
  pretreatment = 7,
  preprocessing.method = "pretreatment"
  
)

pretreated.val <- ikeogu.2017.prepped %>%
  filter(study.name == "C16Mval") %>%
  pretreat_spectra(pretreatment = 8)

pretreated.val.mx <- pretreated.val %>%
  dplyr::select(starts_with("X")) %>%
  as.matrix()

best.ncomp <- model.to.save$best.model.stats$best.ncomp_mode


overall.range <- c(min(c(pretreated.val$reference, predicted.values)),
                   max(c(pretreated.val$reference, predicted.values)))
cbind(unique.id = pretreated.val$unique.id,
      observed = pretreated.val$reference,
      predicted = predicted.values) %>%
  as_tibble() %>%
  mutate(observed = as.numeric(observed),
         predicted = as.numeric(predicted)) %>%
  ggplot(aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "gray80") +
  geom_point() +
  coord_fixed(xlim = overall.range,
              ylim = overall.range) +
  labs(title = "Example dry matter content predictions",
       x = "Observed",
       y = "Predicted") +
  theme_bw()





#Bulk density
quartiles_BD <- quantile(oto_nirs_lab$BD, probs=c(.25, .75), na.rm = TRUE) # 
IQR_BD <- IQR(oto_nirs_lab21$BD, na.rm = TRUE) # interquatile range  

Lower_BD <- quartiles_BD[1] - 1.5*IQR_BD
Upper_BD <- quartiles_BD[2] + 1.5*IQR_BD

clean_BD <- subset(oto_nirs_lab21, oto_nirs_lab21$BD > Lower_BD & oto_nirs_lab21$BD < Upper_BD)
hist(clean_BD$BD)

# take turns to select the reference data by excluding other lab data
oto_nirs_lab21.prepped <- clean_BD  %>%
  dplyr::rename(reference = BD ) %>%
  dplyr::select(unique.id, dplyr::everything(),-Location, -Year, -SP ,  -SI , -AMY , -CF ,-STC, -SC) %>%
  na.omit() 

filtered.BD <- oto_nirs_lab21.prepped %>%
  filter_spectra(
    df = .,
    filter = TRUE,
    return.distances = TRUE,
    num.col.before.spectra = 2,
    window.size = 15
  )

results.BD <- filtered.BD%>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  test_spectra(
    train.data = .,
    tune.length = 5,
    num.iterations = 50,
    pretreatment = 1:13
  )
results.BD$summary.model.performance
results.BD$importance






##


Umu_nirs21 <- read_excel("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/umudike/2021 chichi umudike fresh root.xlsx", sheet = 1, col_names = F)
dim(Umu_nirs21)
str(Umu_nirs21)
unique(Umu_nirs21$...1)
sp3 <- 350:(348+dim(Umu_nirs21)[2])
length(sp3)
sp3X <- c()
for(i in sp3){
  xx2 <- paste(c("X",i),collapse = "") 
  sp3X <- c(sp3X,xx2)
}

colnames(Umu_nirs21) <- c("unique.id", sp3X)

Umu_nirs21_Agg <- Umu_nirs21 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)

Umu_nirs21_Agg <- as.data.frame(Umu_nirs21_Agg)


# view the spectra
plot_spectra(Umu_nirs21_Agg,
             num.col.before.spectra = 1,
             detect.outliers = FALSE,
             alternate.title = "umu_fresh_spectra")


umu_nirs21 <- Umu_nirs21_Agg %>% 
  pretreat_spectra(pretreatment = 1:13) %>% # exclude pretreatment 1 (raw data)
  bind_rows(.id = "pretreatment") %>%
  gather(key = "wl",
         value = "s.value",
         tidyselect::starts_with("X")) %>%
  mutate(wl = as.numeric(readr::parse_number(.data$wl)),
         pretreatment = as.factor(pretreatment)) %>%
  drop_na(s.value) %>%
  ggplot(data = ., aes(x = wl, y = s.value, group = unique.id)) +
  geom_line(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Pretreated spectra",
       x = "Wavelength",
       y = "Spectral Value") +
  
  facet_wrap( ~ pretreatment, scales = "free")




#sort out data for pretreatment SNV and SNV_SG
SNV<-umu_nirs21$data[umu_nirs21$data$pretreatment=='SNV',]
SNV_SG<-umu_nirs21$data[umu_nirs21$data$pretreatment=='SNVSG',]

umu_SNV_wide<-dcast(data = SNV, formula = unique.id ~ wl, 
                    value.var = "s.value", fun.aggregate = mean)
umu_SNV_wide$location <- "umudike"
# unq1 <- c()
# for(i in 1:nrow(umu_SNV_wide)){
#   xx <- paste(c("umudike",i),collapse = "_")
#   unq1 <- c(unq1,xx)
# }
umu_SNV_wide$location <-unq1
dim(umu_SNV_wide[,2153])
umu_SNV_wide<- umu_SNV_wide[,c(1,2153,2:2152)] # bring the last column(location) to the 2nd colunm
head(umu_SNV_wide[,1:5])

head(Umu_nirs21[, 1:10])

library(magrittr)
test.model <- Umu_nirs21 %>%
  save_model(
    df = .,
    write.model = FALSE,
    pretreatment = 1:13,
    model.name = "my_prediction_model",
    tune.length = 3,
    num.iterations = 3
  )
summary(test.model$best.model)
test.model$best.model.stats


#################
#using genomic prediction script
traits<-c("SI", "SP" , "WAC" , "BD", "AMY",     
       "CF" , "STC", "SC")

BLUEMean_Gar_sel_gar
BLUEMean_Gar_sel_gar$Genotype <- as.factor(BLUEMean_Gar_sel_gar$Genotype)
specMat_garri
GRM <- read.csv("/Users/ca384/Documents/ChinedoziRepo/GarriQuality/output/GRM.csv")
head(GRM[, 1:10])
rownames(GRM) <- GRM$X
GRM <- GRM[,-1]

idm <- which(rownames(GRM) %in%rownames(specMat_garri))
length(idm)
GRM1 <- GRM[idm, idm]
ids <- which(rownames(specMat_garri) %in% rownames(GRM1))
specMat_garri1 <- specMat_garri[ids,ids]
idsr <- which(rownames(specMat_garri1) %in% unique(BLUEMean_Gar_sel_gar$Genotype) )
specMat_garri2 <- specMat_garri1[idsr,idsr]
all(rownames(GRM1) %in% rownames(specMat_garri1))
unique(BLUEMean_Gar_sel_gar$Genotype)


idm <- which(rownames(GRM1) %in% BLUEMean_Gar_sel_gar$Genotype )
GRM2 <- GRM1[idm,idm]
colnames(GRM2) <- rownames(GRM2)
head(GRM2[,1:19])
GRM2 <- as.matrix(GRM2)
rownames(GRM2) %in% BLUEMean_Gar_sel_gar$Genotype

library(CovCombR)
?CovComb
comMspec <- CovComb(Klist = list(GRM2,specMat_garri2))
head(comMspec[,1:10])
library(sommer)

Acc_pred_NIR_gari <- tibble()
for(Trait in traits) {
  # Creating a folder that contain 5 subset with 5times with a total of 5*5*8(traits)= 200
  fold5 = caret::createMultiFolds(y = unique(BLUEMean_Gar_sel_gar$Genotype), k = 5, times = 5)
  
  
  for(i in 1:length(fold5)){
    index = fold5[[i]] # the index of the sample for training set
    #subset the phenotypic data, 
    train_geno <- droplevels(unique(BLUEMean_Gar_sel_gar$Genotype)[index])
    train_geno_ind <- which(BLUEMean_Gar_sel_gar$Genotype %in% train_geno)
    train.data <- droplevels(BLUEMean_Gar_sel_gar %>%
                               filter(row_number() %in% train_geno_ind)) # subset the training set
    dim(train.data)
    test.data <- droplevels(BLUEMean_Gar_sel_gar %>%
                              filter(!row_number() %in% train_geno_ind)) # subset the testing set
    dim(test.data)
    test.data1 <- test.data
    test.data[,traits] = NA # change the grain yield of the training set to NA value
    
    mod_dat2<- rbind(train.data, test.data) # combine the the data set for analysis
    mod_dat1 <- droplevels(mod_dat1)
    str(mod_dat1)
    #####################
    
    
    eval(parse(text = paste("ans4 <- mmer(",Trait,"~1,
  random=~ vsr(Genotype,  Gu=  comMspec) ,
      rcov=~units,
      data= mod_dat2[,-c(1:2)])")))
    str(mod_dat1)
    pblup = as.data.frame(ans4$U$`u:Genotype`)
    pblup = cbind(rownames(pblup),pblup)
    colnames(pblup)[1] = "Genotype"
    testind = which(pblup$Genotype %in% unique(test.data$Genotype))
    testpred = pblup[testind,]
    testpred$Genotype %in% unique(test.data$Genotype)
    testpred = testpred[order(testpred$Genotype),]
    
    obs_test =  test.data1 %>% group_by(Genotype) %>%
      summarise_at(.vars = Trait, .funs = mean)
    obs_test= obs_test[order(obs_test$Genotype),]
    r = cbind( Trait, predictability = round(cor(testpred[,Trait],obs_test[,Trait], use = "pairwise.complete.obs"),3))
    colnames(r)[2] <- c("predictability")
    Acc_pred_NIR_gari = rbind(Acc_pred_NIR_gari,r)
    
    
  }
}
#}
Acc_pred_NIR_gari$predictability= as.numeric(Acc_pred_NIR_gari$predictability)
ggplot(Acc_pred_NIR_gari, mapping = aes(x = Trait, y = predictability, fill = Trait)) +
  geom_boxplot() + theme_bw()




###############
Oto_garri_nirs21<-read_excel("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/otobi_2020_2021/2021 chichi otobi gari flour.xlsx",
                             sheet = 1, col_names = F )

dim(Oto_garri_nirs21)
str(Oto_garri_nirs21)
unique(Oto_garri_nirs21$...1)
sp4 <- 350:(348+dim(Oto_garri_nirs21)[2])
length(sp4)
sp4X <- c()
for(i in sp4){
  xx4 <- paste(c("X",i),collapse = "") 
  sp4X <- c(sp4X,xx4)
}

colnames(Oto_garri_nirs21) <- c("unique.id", sp4X)

Oto_garri_nirs21_Agg <- Oto_garri_nirs21 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)

#Oto_garri_nirs21_Ag <- as.data.frame(Oto_garri_nirs21_Agg)


# view the spectra
plot_spectra(Oto_garri_nirs21_Agg,
             num.col.before.spectra = 1,
             detect.outliers = FALSE,
             alternate.title = "oto_garri_spectra")


Oto_garri_nirs_pretrt <- Oto_garri_nirs21_Agg %>% 
  pretreat_spectra(pretreatment = 1:13) %>% # exclude pretreatment 1 (raw data)
  bind_rows(.id = "pretreatment") %>%
  gather(key = "wl",
         value = "s.value",
         tidyselect::starts_with("X")) %>%
  mutate(wl = as.numeric(readr::parse_number(.data$wl)),
         pretreatment = as.factor(pretreatment)) %>%
  drop_na(s.value) %>%
  ggplot(data = ., aes(x = wl, y = s.value, group = unique.id)) +
  geom_line(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Pretreated spectra",
       x = "Wavelength",
       y = "Spectral Value") +
  facet_wrap( ~ pretreatment, scales = "free")

Oto_garri_nirs_pretrt


Umu_garri_nirs21 = read_excel("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/umudike/2021 chichi umudike gari flour.xlsx", sheet = 1, col_names = F)

dim(Umu_garri_nirs21)
str(Umu_garri_nirs21)
unique(Umu_garri_nirs21$...1 )
sp5 <- 350:(348+dim(Umu_garri_nirs21)[2])
length(sp5)
sp5X <- c()
for(i in sp5){
  xx5 <- paste(c("X",i),collapse = "") 
  sp5X <- c(sp5X,xx5)
}

colnames(Umu_garri_nirs21) <- c("unique.id", sp5X)

Umu_garri_nirs21_Agg <- Umu_garri_nirs21 %>% 
  group_by(unique.id) %>%
  summarise_all(.funs = mean)

#Umu_garri_nirs21_Ag <- as.data.frame(Umu_garri_nirs21_Agg)


# view the spectra
plot_spectra(Umu_garri_nirs21_Agg,
             num.col.before.spectra = 1,
             detect.outliers = FALSE,
             alternate.title = "umu_garri_spectra")


Umu_garri_nirs_pretrt <- Umu_garri_nirs21_Agg %>% 
  pretreat_spectra(pretreatment = 1:13) %>% # exclude pretreatment 1 (raw data)
  bind_rows(.id = "pretreatment") %>%
  gather(key = "wl",
         value = "s.value",
         tidyselect::starts_with("X")) %>%
  mutate(wl = as.numeric(readr::parse_number(.data$wl)),
         pretreatment = as.factor(pretreatment)) %>%
  drop_na(s.value) %>%
  ggplot(data = ., aes(x = wl, y = s.value, group = unique.id)) +
  geom_line(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Pretreated spectra",
       x = "Wavelength",
       y = "Spectral Value") +
  facet_wrap( ~ pretreatment, scales = "free")

Umu_garri_nirs_pretrt


#################################### Data wit no outlier from the PYT study
garri_All_garri_pyt <- read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/Wet_chemistry/PYT/garri_2years_2loc_no_outlier.csv")
garri_All_garri_pyt2<- garri_All_garri_pyt %>%
  dplyr::select("Location" , "Year" , "Genotype" , "SI" ,"SP" ,"WAC","BD", "AMY" , "CF", "STC" , "SC")
only_oto_garri <- all_garri2[all_garri2$Year == 2021 & all_garri2$Location == "Otobi", ]

only-umu_garri <- all_garri2[all_garri2$Year == 2021 & all_garri2$Location == "Umudike", ]


