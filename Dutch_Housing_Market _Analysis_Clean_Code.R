## ---- Script Dutch Housing Market Analysis - Jasper Kars (2021) ------------##

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(Amelia)
library(mice)
library(cbsodataR)
library(janitor)
library(e1071)
library(simputation)
library(magrittr)
library(GGally)
library(mlbench)
library(stringr) 
library(car) 
library(rpart) 
library(rpart.plot) 
library(gdata) 
library(psych) 
library(kableExtra)
library(ggpubr)s
library(openxlsx)
library(FactoMineR)
library(janitor)
library(VIM)
library(simputation)
library(devtools)
library(readr)
library(REdaS)
library(stats)
library(reshape2)
library(gdata)
library(stargazer)
library(caTools)
library(ISLR)
library(tidyr)
library(pls)
library(MLmetrics)
library(Metrics)
library(randomForest)
library(class)
library(FNN)   
library(MVN)
library(dbscan)
library(mvnTest)
library(vip)
library(factoextra)
library(corrplot)
library(doMC)
library(foreach)
library(iterators)
library(parallel)
library(arm) 
library(Hmisc)
library(ggfortify)
library(jtools)
library(party)
library(rminer)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(sf)
library(tmap)
library(githubinstall)

## Step 1 (data preparation ) --------------------------------------------------

# 4.1 load datasets cbs --------------------------------------------------------
toc <- cbsodataR::cbs_get_toc()

# load 2017 cbs ----------------------------------------------------------------
buurten_17_NL <- cbs_get_data('83765NED') %>%
  cbs_add_label_columns() %>%
  cbs_add_date_column() %>%
  clean_names()

# select neighborhoods as unit of analysis 
buurten_17_NL_clean <- buurten_17_NL  %>%
  mutate(soort_regio_2 = str_trim(soort_regio_2)) %>%
  mutate(Jaar = 2017) %>%
  filter(soort_regio_2 == "Buurt")

# load 2016 cbs ----------------------------------------------------------------
buurten_2016_NL <- cbs_get_data('83487NED') %>%
  cbs_add_label_columns() %>%
  cbs_add_date_column() %>%
  clean_names()

# select neighborhoods as unit of analysis 
buurten_16_NL_clean <- buurten_2016_NL %>% 
  mutate(soort_regio_2 = str_trim(soort_regio_2)) %>%
  filter(soort_regio_2 == "Buurt") %>%
  mutate(Jaar = 2016)  

# first data check -------------------------------------------------------------
# filtering all 0 (inhabitants variable) out of df (aantal_inwoners)
buurten_17_NL_clean <- filter(buurten_17_NL_clean, aantal_inwoners_5  > 0)
buurten_16_NL_clean <- filter(buurten_16_NL_clean, aantal_inwoners_5  > 0)

# dropna test with housing value
buurten_17_NL_clean <- buurten_17_NL_clean %>% 
  drop_na(gemiddelde_woningwaarde_35) %>% 
  drop_na(koopwoningen_40) %>% 
  drop_na(personen_per_soort_uitkering_bijstand_74)  

buurten_16_NL_clean<- buurten_16_NL_clean %>%
  drop_na(gemiddelde_woningwaarde_35) 

# fit 2016 with 2017 dataset ---------------------------------------------------
buurten_16_fitted <- buurten_16_NL_clean %>% 
  rename(
    motorfietsen_93 = motorfietsen_94,
    afstand_tot_huisartsenpraktijk_94 = afstand_tot_huisartsenpraktijk_95,
    afstand_tot_grote_supermarkt_95 =   afstand_tot_grote_supermarkt_96,
    afstand_tot_kinderdagverblijf_96 =  afstand_tot_kinderdagverblijf_97,
    afstand_tot_school_97 = afstand_tot_school_98,
    scholen_binnen3km_98 = scholen_binnen3km_99,
    oppervlakte_totaal_99 = oppervlakte_totaal_100,
    oppervlakte_land_100 = oppervlakte_land_101,
    oppervlakte_water_101 =  oppervlakte_water_102,
    meest_voorkomende_postcode_102 = meest_voorkomende_postcode_103,
    dekkingspercentage_103 = dekkingspercentage_104, 
    mate_van_stedelijkheid_104 = mate_van_stedelijkheid_105,
    omgevingsadressendichtheid_105 = omgevingsadressendichtheid_106,
    totaal_diefstal_uit_woning_schuur_ed_106 = totaal_diefstal_uit_woning_schuur_ed_107,
    vernieling_misdrijf_tegen_openbare_orde_107 = vernieling_misdrijf_tegen_openbare_orde_108,
    gewelds_en_seksuele_misdrijven_108 = gewelds_en_seksuele_misdrijven_109
  )

# delete variable not in 2017
buurten_16_fitted$bedrijfsmotorvoertuigen_93 <- NULL

# 4.2 joining 2016 and 2017  ---------------------------------------------------
combined_ds_1617 <- rbind(buurten_17_NL_clean, buurten_16_fitted)

# dropna in distances variabeles 
combined_ds_1617 <- combined_ds_1617 %>% 
  drop_na(afstand_tot_school_97, afstand_tot_huisartsenpraktijk_94, 
          totaal_diefstal_uit_woning_schuur_ed_106)

summary(combined_ds_1617)       

# 4.2 train/ test split (stratified 70/30) -------------------------------------

set.seed(123)
train.index1 <- createDataPartition(combined_ds_1617$gemiddelde_woningwaarde_35, 
                                    p = 0.7, list = FALSE)
train_combined <- combined_ds_1617[ train.index1,]
test_combined  <- combined_ds_1617[-train.index1,]

# 4.2 prepare for imputation  --------------------------------------------------
# check out missing values % 
# Enders ( 2003 ) stated that a missing rate of 15% to 20% was common 

pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(combined_ds_1617,2,pMiss)

aggr_plot <- aggr(combined_ds_1617, col=c('blue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(combined_ds_1617), 
                  cex.axis=.35, gap=3, 
                  ylab=c("Histogram of missing data","Pattern")
                  
# prepare for imputation - drop variables with too much na (> 20) --------------
train_combined$appartement_48 <- NULL
train_combined$tussenwoning_49 <- NULL
train_combined$hoekwoning_50 <- NULL 
train_combined$twee_onder_een_kap_woning_51 <- NULL
train_combined$vrijstaande_woning_52 <- NULL
train_combined$huurwoning_53 <- NULL
train_combined$eigen_woning_54  <- NULL 
train_combined$appartement_56 <- NULL
train_combined$tussenwoning_57  <- NULL
train_combined$hoekwoning_58  <- NULL
train_combined$twee_onder_een_kap_woning_59  <- NULL
train_combined$vrijstaande_woning_60 <- NULL
train_combined$huurwoning_61 <- NULL
train_combined$meest_voorkomende_postcode_102 <- NULL
train_combined$dekkingspercentage_103 <- NULL

# prepare for imputation - drop variables with too much na (> 20) --------------
test_combined$appartement_48 <- NULL
test_combined$tussenwoning_49 <- NULL
test_combined$hoekwoning_50 <- NULL 
test_combined$twee_onder_een_kap_woning_51 <- NULL
test_combined$vrijstaande_woning_52 <- NULL
test_combined$huurwoning_53 <- NULL
test_combined$eigen_woning_54  <- NULL 
test_combined$appartement_56 <- NULL
test_combined$tussenwoning_57  <- NULL
test_combined$hoekwoning_58  <- NULL
test_combined$twee_onder_een_kap_woning_59  <- NULL
test_combined$vrijstaande_woning_60 <- NULL
test_combined$huurwoning_61 <- NULL
test_combined$meest_voorkomende_postcode_102 <- NULL
test_combined$dekkingspercentage_103 <- NULL

# assign NAs gemiddeld_aardgasverbruik_totaal_55 to 0 (following CBS policy) ---
train_combined <- train_combined %>%
  mutate_at(vars(gemiddeld_aardgasverbruik_totaal_55), ~replace_na(., 0))

test_combined <- test_combined %>%
  mutate_at(vars(gemiddeld_aardgasverbruik_totaal_55), ~replace_na(., 0))

# match gemiddeld_aardgasverbruik_totaal_55 met eigen_woning_62
train_combined$eigen_woning_62 <- ifelse(train_combined$gemiddeld_aardgasverbruik_totaal_55 %in% c("0"), 
                                         0, train_combined$eigen_woning_62)

test_combined$eigen_woning_62 <- ifelse(test_combined$gemiddeld_aardgasverbruik_totaal_55 %in% c("0"), 
                                        0, test_combined$eigen_woning_62)

# 4.2 execute pmm --------------------------------------------------------------
train_combined_ready <- mice(train_combined [7:95],m=5,maxit=50,meth='pmm',seed=123)
test_combined_ready  <- mice(test_combined[7:95],m=5,maxit=50,meth='pmm',seed=123)

# select appropriate candidates (#1 )
train_combined_ready  <- complete(train_combined_ready, 1)
test_combined_ready  <- complete(test_combined_ready, 1)

# re-establish label variables to dataset 
train_combined_ready <- cbind(train_combined_ready,
                              codering_3= train_combined$codering_3)
train_combined_ready <- cbind(train_combined_ready,
                              wijken_en_buurten_label= train_combined$wijken_en_buurten_label)
train_combined_ready <- cbind(train_combined_ready,
                              gemeentenaam_1= train_combined$gemeentenaam_1)
train_combined_ready <- cbind(train_combined_ready,
                              Jaar= train_combined$Jaar)

test_combined_ready <- cbind(test_combined_ready,
                             codering_3= test_combined$codering_3)
test_combined_ready <- cbind(test_combined_ready,
                             wijken_en_buurten_label= test_combined$wijken_en_buurten_label)
test_combined_ready <- cbind(test_combined_ready,
                             gemeentenaam_1= test_combined$gemeentenaam_1)
test_combined_ready <- cbind(test_combined_ready,
                             Jaar= test_combined$Jaar)

# set y (average property value) to end of the dataset 
train_combined_ready <- cbind(train_combined_ready,
                              gemiddelde_woningwaarde_35= train_combined$gemiddelde_woningwaarde_35)

test_combined_ready <- cbind(test_combined_ready,
                             gemiddelde_woningwaarde_35= test_combined$gemiddelde_woningwaarde_35)

train_combined_ready <- train_combined_ready[-31]
test_combined_ready <- test_combined_ready[-31]

# saving DF's ------------------------------------------------------------------
saveRDS(train_combined_ready, "train_combined_ready.rds")
saveRDS(test_combined_ready, "test_combined_ready.rds")

# bind for purpose of normality and statistical tests 
combi_ds_tr_tst <- rbind(train_combined_ready ,test_combined_ready)
saveRDS(combi_ds_tr_tst, "combi_ds_tr_tst.rds")

## Step 2 (Data exploration - EDA) ---------------------------------------------

# load packages ----------------------------------------------------------------
train_combined_ready <- readRDS(file = "train_combined_ready.rds")
test_combined_ready <- readRDS(file = "test_combined_ready.rds")

# copy of cleaned complete dataset just for some EDA purposes 
combi_ds_tr_tst <- readRDS(file = "combi_ds_tr_tst.rds")

# 4.3 EDA ----------------------------------------------------------------------
#source: https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
describe(combi_ds_tr_tst)
summary(combi_ds_tr_tst)

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
  
}

basic_eda <- basic_eda(combi_ds_tr_tst)

#export table with summary stats 
neigborhoods_ss <- descr(combi_ds_tr_tst)
View(neigborhoods_ss)
kable(neigborhoods_ss) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# plot all integer variables on average property value (y)
# create a file 
dir.create('plots')

buurten <- combi_ds_tr_tst

for(name in (names(buurten))){
  if(is.integer(buurten[[name]])){
    print(name)
    tabel <- buurten %>% 
      ggplot(aes(x = .data[[name]], y = gemiddelde_woningwaarde_35)) +
      geom_jitter(alpha = .5, ) +
      labs(title = paste('Property Value vs', name),
           x = name, 
           y = 'Average property value')
    ggplot2::ggsave(filename = paste0('plots/', name, '.jpeg'))
  }
}

# 4.3.1 - Plots
plot.1 <- combi_ds_tr_tst  %>%
  mutate(Income_Class = ifelse(gemiddeld_inkomen_per_inwoner_66 < 34.678, "6. low",
                               ifelse(gemiddeld_inkomen_per_inwoner_66 >  34.678  & gemiddeld_inkomen_per_inwoner_66 < 38.690, "5. medium low",
                                      ifelse(gemiddeld_inkomen_per_inwoner_66 > 38.690 & gemiddeld_inkomen_per_inwoner_66  < 44.360, "4. medium",
                                             ifelse(gemiddeld_inkomen_per_inwoner_66  > 44.360 & gemiddeld_inkomen_per_inwoner_66 < 52.500, "3. medium high",
                                                    ifelse(gemiddeld_inkomen_per_inwoner_66 > 52.500 & gemiddeld_inkomen_per_inwoner_66< 70.000 , "2. 2x high",
                                                           ifelse(gemiddeld_inkomen_per_inwoner_66  > 70.000, "1. highest", ""))))))) %>%
  ggplot(aes(x = huishoudens_totaal_28 , y = gemiddelde_woningwaarde_35, col = Income_Class)) +
  geom_boxplot() + 
  theme_classic() +
  facet_wrap(~ Income_Class) +
  labs(title = 'Property Value vs Nr. of Households',
       x = 'Households Total', 
       y = 'Average Property Value') +
  theme(text = element_text(size = 20))  

plot.2 <- combi_ds_tr_tst %>%
  mutate(Income_Class = ifelse(gemiddeld_inkomen_per_inwoner_66 < 34.678, "6. low",
                               ifelse(gemiddeld_inkomen_per_inwoner_66 >  34.678  & gemiddeld_inkomen_per_inwoner_66 < 38.690, "5. medium low",
                                      ifelse(gemiddeld_inkomen_per_inwoner_66 > 38.690 & gemiddeld_inkomen_per_inwoner_66  < 44.360, "4. medium",
                                             ifelse(gemiddeld_inkomen_per_inwoner_66  > 44.360 & gemiddeld_inkomen_per_inwoner_66 < 52.500, "3. medium high",
                                                    ifelse(gemiddeld_inkomen_per_inwoner_66 > 52.500 & gemiddeld_inkomen_per_inwoner_66< 70.000 , "2. 2x high",
                                                           ifelse(gemiddeld_inkomen_per_inwoner_66  > 70.000, "1. highest", ""))))))) %>%
  ggplot(aes(x = woningvoorraad_34 , y = gemiddelde_woningwaarde_35, col = Income_Class)) +
  geom_jitter(alpha = 5) +
  facet_wrap(~ Income_Class) +
  theme_classic() +
  labs(title = 'Property Value vs Housing Stock',
       x = 'Housing Stock', 
       y = 'Average Property Value') +
  theme(text = element_text(size = 20))  

plot.3 <- combi_ds_tr_tst %>%
  ggplot(aes(x = mate_van_stedelijkheid_104, y = gemiddelde_woningwaarde_35)) +
  geom_jitter(alpha = .5, ) +
  theme_classic() +
  labs(title = paste('Average Property Value vs Degree of Urbanization'),
       x = 'Degree of Urbanization', 
       y = 'Average property value') +
  theme(text = element_text(size = 20))  

plot.4 <- combi_ds_tr_tst %>%
  mutate(Income_Class = ifelse(gemiddeld_inkomen_per_inwoner_66 < 34.678, "6. low",
                               ifelse(gemiddeld_inkomen_per_inwoner_66 >  34.678  & gemiddeld_inkomen_per_inwoner_66 < 38.690, "5. medium low",
                                      ifelse(gemiddeld_inkomen_per_inwoner_66 > 38.690 & gemiddeld_inkomen_per_inwoner_66  < 44.360, "4. medium",
                                             ifelse(gemiddeld_inkomen_per_inwoner_66  > 44.360 & gemiddeld_inkomen_per_inwoner_66 < 52.500, "3. medium high",
                                                    ifelse(gemiddeld_inkomen_per_inwoner_66 > 52.500 & gemiddeld_inkomen_per_inwoner_66< 70.000 , "2. 2x high",
                                                           ifelse(gemiddeld_inkomen_per_inwoner_66  > 70.000, "1. highest", ""))))))) %>%
  ggplot(aes(x = huishoudens_totaal_28 , y = gemiddelde_woningwaarde_35, col = Income_Class)) +
  geom_jitter(alpha = 5) +
  facet_wrap(~ Income_Class) +
  labs(title = 'Property Value vs Nr. of Households',
       x = 'Households Total', 
       y = 'Average Property Value') +
  theme(text = element_text(size = 20))  

# 4.3.2 Checking for normality of distribution ---------------------------------
# HZ Test 
HZ.test(combi_ds_tr_tst, qqplot = FALSE)

# qqplot
qqnorm(combi_ds_tr_tst$gemiddelde_woningwaarde_35, pch = 1, frame = FALSE)
qqline(combi_ds_tr_tst$gemiddelde_woningwaarde_35, col = "steelblue", lwd = 2) +
 
# 4.4.1 Scaling / standardization ----------------------------------------------
train_combined_ready[1:88] <- scale(train_combined_ready[1:88], 
                                   center = TRUE, scale = TRUE) 
test_combined_ready[1:88] <- scale(test_combined_ready[1:88],
                                  center = TRUE, scale = TRUE)

combi_ds_tr_tst [1:88] <-  scale(combi_ds_tr_tst[1:88], 
                                center = TRUE, scale = TRUE)

# 4.3.3 Statistical tests ------------------------------------------------------
# Correlation plot 
# source: http://www.sthda.com/upload/rquery_cormat.r

cor(combi_ds_tr_tst, method = c("spearman"))
corrplot(cor(combi_ds_tr_tst))

# KMO 
# source: https://methods.sagepub.com/dataset/howtoguide/kmo-nilt-2012-r
kmo <- function(x) { x <- subset(x, complete.cases(x)) # Omit missing values
r <- cor(x) # Correlation matrix
r2 <- r^2   # Squared correlation coefficients
i <- solve(r) # Inverse matrix of correlation matrix
d <- diag(i) # Diagonal elements of inverse matrix
p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
KMO <- sum(r2)/(sum(r2)+sum(p2)) #Equation for KMO test
MSA <- colSums(r2)/(colSums(r2)+colSums(p2)) #Equation for individual MSA
return(list(KMO=KMO, MSA=MSA))
}

# KMO scores 
kmo(combi_ds_tr_tst)

# Bartlett's Test
nieuw <- combi_ds_tr_tst %>% 
  pivot_longer(cols = 4:ncol(combi_ds_tr_tst), names_to = 'group', 
               values_to = 'value')

bartlett.test(nieuw$value, nieuw$group)

# Bartlett's Test scores 
bartlett.test(value ~ group, nieuw)

# Drop after KMO test (below .5)
train_combined_ready$oppervlakte_water_101 <- NULL
train_combined_ready$eigendom_onbekend_44 <- NULL
train_combined_ready$a_landbouw_bosbouw_en_visserij_79 <- NULL

test_combined_ready$oppervlakte_water_101 <- NULL
test_combined_ready$eigendom_onbekend_44 <- NULL
test_combined_ready$a_landbouw_bosbouw_en_visserij_79 <- NULL

## Step 3 (Feature extraction / selection) -------------------------------------

# PCA 4.4.2 --------------------------------------------------------------------
# source: #source: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

set.seed(123)
train_combined_ready_pca <- train_combined_ready
PCA_1 <- PCA(train_combined_ready_pca[1:85], graph = FALSE)
summary(PCA_1)


# check out eigenvalues (also considering acceptable range)
eig.val_1 <- get_eigenvalue(PCA_1)
eig.val_1

summary(eig.val_1)

# compute scree plot 
fviz_eig(PCA_1, addlabels = TRUE, ylim = c(0, 50)) +
  theme(text = element_text(size = 20))  

# compute results of PCA
var_PCA1 <- get_pca_var(PCA_1)
var_PCA1 

# plot PCA
head(var_PCA1$coord)
head(var_PCA1$cos2)
head(var_PCA1$contrib)

head(var_PCA1$coord, 4)

# plot correlation circle
fviz_pca_var(PCA_1, col.var = "black")

head(var_PCA1$cos2, 5)

# quality of representation 
head(var_PCA1$cos2, 10)

fviz_cos2(PCA_1, choice = "var", axes = 1:2)

fviz_pca_var(PCA_1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# change the transparency by cos2 values
fviz_pca_var(PCA_1, alpha.var = "cos2")

# contributions of variables to PC4
head(var_PCA1$contrib, 85)
fviz_contrib(PCA_1, choice = "var", axes =4, top = 5)

# most important variables in plot
fviz_pca_var(PCA_1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# most signifcant variables 
res.desc <- dimdesc(PCA_1, axes = c(1,2), proba = 0.05)

# Description of dimension 1
res.desc$Dim.1

# quality 
ind <- get_pca_ind(PCA_1)
ind
# Ccordinates of individuals
head(ind$coord)
# quality of individuals
head(ind$cos2)
# contributions of individuals
head(ind$contrib)
fviz_pca_ind(PCA_1)

# final scree plot
prin_compt <- prcomp(train_combined_ready_pca[-31], scale. = F)
names(prin_compt)
prin_compt$rotation[1:85,5:7]
std_dev <- prin_compt$sdev
pr_var <- std_dev^22
pr_var[1:12]
prop_varex <- pr_var/sum(pr_var)

biplot(prin_compt,cex=.05)

# plot for in report section 5.1
plotpca <- prin_compt$rotation[1:85,1:10]
plotpca_round <- round(plotpca,digits=3)
write.xlsx(plotpca_round, 'plotpca_round1.xlsx')

pca_a <- prin_compt$rotation[1:85,1:2]
# scree plot
plot(prop_varex , xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# set up PCA for test/ train ---------------------------------------------------
set.seed(123)
# pca train set 
pca = preProcess(x = train_combined_ready[1:85], method = 'pca', 
                 pcaComp = 9)
pca_train <- predict(pca, train_combined_ready)
pca_train <- pca_train[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]

# pca test set 
pca_test <- predict(pca, test_combined_ready)
# we need to put the columns back in order we want
pca_test <- pca_test[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]

# save for back-up purposes
saveRDS(pca_train, "pca_train.rds")
saveRDS(pca_test, "pca_test.rds")

# Apply MINREM strategy (selecting stable-important variables) -----------------
# 4.4.3 MINREM -----------------------------------------------------------------

#Source: Perez Rave et al. (2019): 
# https://www.researchgate.net/publication/350790001_Tech_report_MINREM_DEMO_REDUCED_illustrativepdf?channel=doi&linkId=60722bdb92851c8a7bba914d&showFulltext=true

# MINREM function 
min.rem_red_demo<-function(base.dat, prop.entrena,nrep, secu.ini,p.fin,
                           delta,Ndepura, pvali,y.price){
  names(base.dat[y.price])<-"gemiddelde_woningwaarde_35"
  conteo<-function(x){length(x[!is.na(x) & x==T])}
  levis<-function(x){length(levels(factor(x)))}
  secu.fin = round(nrow(base.dat)*prop.entrena*p.fin,0)
  set.seed(123)
  entrena<-sample(x=1:nrow(base.dat),
                  size=round(nrow(base.dat)*prop.entrena),0)
  valida<-(-entrena)
  #INITIAL MODEL
  mod.ini<-lm(log(gemiddelde_woningwaarde_35)~., data=base.dat[entrena,])
  sum.mod.ini<-summary(mod.ini)
  coef.ini<-as.data.frame(summary(mod.ini)$coefficients)
  extens<-length(coef.ini$Estimate)
  atri.ini<-(rownames(coef.ini))
  coef.ini<- rownames(coef.ini[coef.ini[,4]<pvali,])
  prop.glob<-data.frame(ID=1:extens)
  secu<-seq(from=secu.ini, to=secu.fin, by=delta)
  set.seed(456)
  for (i in (1:length(secu))){
    resum.frame<-data.frame(ID=1:extens)
    rownames(resum.frame)<-atri.ini
    for (k in 1:nrep){
      muestra<-(sample(rownames(base.dat[entrena,]),
                       secu[i],replace=T))
      d.test<-base.dat[muestra,]
      set.mod<-colnames(mod.ini$model)
      set.mod<-colnames(d.test)%in%set.mod
      d.test<-d.test[,set.mod]
      d.test<-gdata::drop.levels(d.test)
      sde<-apply(d.test, 2,levis)
      d.test<-d.test[,sde>1]
      mod<-lm(log(gemiddelde_woningwaarde_35)~., data=base.dat[muestra,])
      val.p=round(summary(mod)$coefficients[,c(4)],5)
      if(length(val.p)<nrow(resum.frame)){
        atri.muestra<-names(val.p)
        w<-rep(NA,nrow(resum.frame))
        
        for(r in 1:length(atri.muestra)){
          w[rownames(resum.frame)%in%atri.muestra[r]]=val.p[r]}
        tam.n=rep(secu[i],length(w))
        resumen<-data.frame(val.p=w, tam.n=tam.n)
      }else{
        tam.n=rep(secu[i],length(val.p))
        resumen<-data.frame(val.p, tam.n)}
      resum.frame<-data.frame(resum.frame, resumen)
    }
    serie<-seq(from=2, to=ncol(resum.frame), by=2)
    prop<-resum.frame[,serie]<pvali
    w<-apply(prop,1,conteo)/nrep
    prop<-data.frame(w)
    colnames(prop)<-c(secu[i])
    prop.glob<-data.frame(prop.glob,prop)
  }
  prop.glob<-prop.glob[-1,-1]
  #DESCRIBING
  t0=data.frame(prop.glob, param=rownames(prop.glob))
  t0<-(melt(t0, id="param"))
  colnames(t0)<-c("Param", "Tam.muestra", "Prop.Re.Ho")
  t<-t0[order(t0$Param),]
  t$Tam.muestra<-as.numeric(substr(t$Tam.muestra,2,10))
  sec1<-seq(0,round(secu.fin*100/100000,0),0.5)
  g1<-ggplot(data=t,aes(x=t$Tam.muestra/1000,
                        y=t$Prop.Re.Ho,
                        group=Param,
                        colour = Param))+
    geom_line()+geom_point()+theme_bw()+
    ylab("Prop. cases with p-val<0.05 ~ sample size")+
    ggtitle("Evol. prop. cases with p-val<0.05 ~ sample size")+
    xlab("sample size (in thousands)")+
    scale_x_continuous(breaks = sec1)+
    geom_vline(xintercept = Ndepura/1000,
               lty=2,colour="gray20")+
    geom_hline(yintercept = 0.95,lty=2, colour="gray20")+
    theme(legend.title=element_blank())
  sec1<-seq(0,round(secu.fin*100/100000,0),1)
  g2<-ggplot(data=t,aes(x=t$Tam.muestra/1000,
                        y=t$Prop.Re.Ho,
                        group=Param))+
    geom_line()+geom_point()+theme_bw()+
    theme(strip.background =element_rect(fill="azure2"))+
    ylab("Prop. cases with p-val<0.05 ~ sample size")+
    xlab("Sample size (in thousands)")+
    
    ggtitle("Evol. prop. cases with p-val<0.05 ~ sample size")+
    facet_wrap(~Param, ncol = 3)+
    scale_x_continuous(breaks = sec1)+
    scale_y_continuous(breaks=c(0,0.5,0.95),
                       limits = c(0,1.03))+
    geom_vline(xintercept = c(Ndepura/1000),
               colour="brown2")+
    geom_hline(yintercept = 0.95, colour="brown2")
  sec1<-seq(0,round(secu.fin*100/100000,0)*1000,500)
  max.muestra<-function(x){names(x[max(which((x<0.95)))])}
  n.max.ReHo<-suppressWarnings(apply(prop.glob,
                                     1,max.muestra))
  nom.param<-names(n.max.ReHo)
  n.max.ReHo<-as.numeric(substr(n.max.ReHo,2,10))
  n.max.ReHo<-data.frame(nom.param, n.max.ReHo)
  g3 <-ggplot(n.max.ReHo, aes(nom.param, n.max.ReHo))+
    geom_bar(stat = "identity", aes(fill = nom.param),
             na.rm = T)+
    coord_flip()+guides(fill=FALSE)+
    xlab("Variables")+
    ylab("Tam. muestra")+
    ggtitle("Sample size with indep.variable NO-significant")+
    scale_y_continuous(breaks = sec1,
                       limits = c(0,max(n.max.ReHo$n.max.ReHo,
                                        na.rm = T)))
  #FINAL MODEL
  n.max.ReHo$n.max.ReHo[is.na(n.max.ReHo$n.max.ReHo)]<-0
  elegidos<-as.character(n.max.ReHo$nom.param[n.max.ReHo$n.max.ReHo<=(Ndepura-delta)])
  ele.depura<-NULL
  for (i in 1:length(elegidos)){
    za<- names(base.dat)[names(base.dat)==elegidos[i]]
    if(length(za)==0){
      za<-substr(elegidos[i],1,nchar(elegidos[i])-1)
      simb<-grep(paste(substr(za,nchar(za)-1,
                              nchar(za)-1),"\\.", sep=""), za)
      if(length(simb)>0){za<-gsub('[[:punct:] ]+',' ',za)}
    }
    ele.depura<-c(ele.depura,za)
  }
  elegidos<-unique(ele.depura)
  elegidos<-c("gemiddelde_woningwaarde_35", elegidos)
  elegidos<-names(base.dat)%in%elegidos
  mod.fin<-lm(log(gemiddelde_woningwaarde_35)~.,data=base.dat[entrena,
                                                              elegidos])
  mod.fin.int2<-lm(log(gemiddelde_woningwaarde_35)~.^2,
                   data=base.dat[entrena,elegidos])
  sum.mod.fin.int2=summary(mod.fin.int2)
  sum.mod.fin<-summary(mod.fin)
  coef.fin<-as.data.frame(summary(mod.fin)$coefficients)
  coef.fin<-rownames(coef.fin[coef.fin[,4]<pvali,])
  #CONSOLIDATING RESULTS
  mi.list<-list(mod.ini=mod.ini, sum.mod.ini=sum.mod.ini,coef.ini=coef.ini,
                prop.glob=prop.glob,
                g1=g1, g2=g2, g3=g3, n.max.ReHo=n.max.ReHo,
                mod.fin=mod.fin, sum.mod.fin=sum.mod.fin,coef.fin=coef.fin,
                mod.fin.int2=mod.fin.int2, sum.mod.fin.int2=sum.mod.fin.int2)
}

# dataset copy for MINREM
train_combined_ready_MINREM <- train_combined_ready

# compute MINREM 
set.seed(123)
resul.obs.ml <-min.rem_red_demo(base.dat=train_combined_ready_MINREM,prop.entrena=0.7,nrep=100,
                                secu.ini=200,p.fin=.2,delta=100,Ndepura=1000,pvali=0.05,
                                y.price="gemiddelde_woningwaarde_35")

# results of MINREM 
names(resul.obs.ml)
resul.obs.ml$mod.ini
resul.obs.ml$sum.mod.ini
resul.obs.ml$mod.fin
resul.obs.ml$sum.mod.fin
resul.obs.ml$mod.fin.int2
resul.obs.ml$sum.mod.fin.int2
resul.obs.ml$coef.ini
resul.obs.ml$coef.fin
head(resul.obs.ml$prop.glob)
resul.obs.ml$g1
resul.obs.ml$g2
resul.obs.ml$g3
resul.obs.ml$n.max.ReHo

# Check stable-important variables for inferential purposes --------------------
mod.elegi.estima <- resul.obs.ml$mod.fin
resul <-round(data.frame(coef=coef(mod.elegi.estima),confint(mod.elegi.estima)),2)
colnames(resul)<-c("Coef", "CI95.inf", "CI95.sup")
p.cambio<-round((exp(resul$Coef)-1)*100)
p.cambio[1]<-NA
resul<-data.frame(resul,p.cambio)
kable(resul) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


plot.minrem <- train_combined_ready %>%
  ggplot(aes(x =   k_40huishoudens_met_laagste_inkomen_70, 
             y = gemiddelde_woningwaarde_35)) +
  geom_jitter(alpha = .5, ) +
  labs(title = paste('45 to 65 years old'),
       x = '45 to 65 years old', 
       y = 'Average property value')

# select stable-important variables under MINREM for train and test

# select stable-important variables under MINREM for train and test
MINREM_model_train <- train_combined_ready %>% 
  select(k_45tot65jaar_11,
         gemiddeld_elektriciteitsverbruik_totaal_47,
         k_40personen_met_laagste_inkomen_67,
         k_40huishoudens_met_laagste_inkomen_70,   
         k_20huishoudens_met_hoogste_inkomen_71,
         gemiddelde_woningwaarde_35,
         codering_3,
         wijken_en_buurten_label,
         gemeentenaam_1,
         Jaar)

MINREM_model_test <- test_combined_ready %>%
  select(k_45tot65jaar_11, gemiddeld_elektriciteitsverbruik_totaal_47,
         k_40personen_met_laagste_inkomen_67,
         k_40huishoudens_met_laagste_inkomen_70,   
         k_20huishoudens_met_hoogste_inkomen_71,
         gemiddelde_woningwaarde_35,
         codering_3,
         wijken_en_buurten_label,
         gemeentenaam_1,
         Jaar) 
 
# save for back-up purposes
saveRDS(MINREM_model_train, "MINREM_model_train.rds")
saveRDS(MINREM_model_test, "MINREM_model_test.rds")  

## Step 4 (Set up baseline MLR models) -----------------------------------------

# 5.2 Baseline MLR models  
set.seed(123)

MLR_model <- lm(formula = gemiddelde_woningwaarde_35 ~ ., 
                data = train_combined_ready)
summary(MLR_model)
test_set_MLR <- predict(MLR_model, newdata = test_combined_ready)

#r2 check function 
r2_check <- function (x, y) cor(x, y) ^ 2
r2_MLR <- r2_check(test_combined_ready$gemiddelde_woningwaarde_35, test_set_MLR)
corr_MLR <- cor(test_combined_ready$gemiddelde_woningwaarde_35, 
                test_set_MLR, method="spearman")
RMSE(test_combined_ready$gemiddelde_woningwaarde_35, test_set_MLR)

# set categories of variables for MLR 
MLR_total <- train_combined_ready %>% 
  select(mannen_6, vrouwen_7, k_0tot15jaar_8,k_15tot25jaar_9, 
         k_25tot45jaar_10,                        
         k_45tot65jaar_11, k_65jaar_of_ouder_12, ongehuwd_13, 
         gehuwd_14, gescheiden_15,                  
         verweduwd_16,westers_totaal_17,
         gemiddeld_elektriciteitsverbruik_totaal_47, 
         gemiddeld_aardgasverbruik_totaal_55, eigen_woning_62,
         percentage_woningen_met_stadsverwarming_63,
         gemiddeld_inkomen_per_inkomensontvanger_65,   
         gemiddeld_inkomen_per_inwoner_66, k_40personen_met_laagste_inkomen_67, 
         k_20personen_met_hoogste_inkomen_68, actieven1575jaar_69, 
         k_40huishoudens_met_laagste_inkomen_70, 
         k_20huishoudens_met_hoogste_inkomen_71,
         afstand_tot_kinderdagverblijf_96, afstand_tot_school_97, 
         scholen_binnen3km_98, mate_van_stedelijkheid_104, 
         omgevingsadressendichtheid_105, 
         totaal_diefstal_uit_woning_schuur_ed_106, 
         gemiddelde_huishoudensgrootte_32,
         bevolkingsdichtheid_33, woningvoorraad_34,mannen_6, vrouwen_7, 
         k_0tot15jaar_8,k_15tot25jaar_9, k_25tot45jaar_10, k_45tot65jaar_11, 
         k_65jaar_of_ouder_12, ongehuwd_13, gehuwd_14, gescheiden_15,     
         verweduwd_16,westers_totaal_17, gemiddelde_woningwaarde_35)   

MLR_total_test <- test_combined_ready %>% 
  select(mannen_6, vrouwen_7, k_0tot15jaar_8,k_15tot25jaar_9, 
         k_25tot45jaar_10,                        
         k_45tot65jaar_11, k_65jaar_of_ouder_12, ongehuwd_13, 
         gehuwd_14, gescheiden_15,                  
         verweduwd_16,westers_totaal_17,
         gemiddeld_elektriciteitsverbruik_totaal_47, 
         gemiddeld_aardgasverbruik_totaal_55, eigen_woning_62,
         percentage_woningen_met_stadsverwarming_63,
         gemiddeld_inkomen_per_inkomensontvanger_65,   
         gemiddeld_inkomen_per_inwoner_66, k_40personen_met_laagste_inkomen_67, 
         k_20personen_met_hoogste_inkomen_68, actieven1575jaar_69, 
         k_40huishoudens_met_laagste_inkomen_70, 
         k_20huishoudens_met_hoogste_inkomen_71,
         afstand_tot_kinderdagverblijf_96, afstand_tot_school_97, 
         scholen_binnen3km_98, mate_van_stedelijkheid_104, 
         omgevingsadressendichtheid_105, 
         totaal_diefstal_uit_woning_schuur_ed_106, 
         gemiddelde_huishoudensgrootte_32,
         bevolkingsdichtheid_33, woningvoorraad_34,mannen_6, vrouwen_7, 
         k_0tot15jaar_8,k_15tot25jaar_9, k_25tot45jaar_10, k_45tot65jaar_11, 
         k_65jaar_of_ouder_12, ongehuwd_13, gehuwd_14, gescheiden_15,     
         verweduwd_16,westers_totaal_17, gemiddelde_woningwaarde_35)  

# compute total MLR model 
MLR_total_MLR <- lm(formula = gemiddelde_woningwaarde_35 ~ ., 
                    data = MLR_total)

summary(MLR_total_MLR)

MLR_model_1 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., data =  MLR_total)
summary(MLR_model_1)
pred_MLRtotal <- predict(MLR_total_MLR, newdata = MLR_total_test)

r2_char_pop <- r2_check(MLR_total_test$gemiddelde_woningwaarde_35, pred_MLRtotal)
MAE(MLR_total_test$gemiddelde_woningwaarde_35, pred_MLRtotal)
RMSE(MLR_total_test$gemiddelde_woningwaarde_35, pred_MLRtotal)
cor(MLR_total_test$gemiddelde_woningwaarde_35, pred_MLRtotal)

# different individual factors 
char_pop <- train_combined_ready %>% 
  select(mannen_6, vrouwen_7, k_0tot15jaar_8,k_15tot25jaar_9, k_25tot45jaar_10,                        
         k_45tot65jaar_11, k_65jaar_of_ouder_12, ongehuwd_13, gehuwd_14, gescheiden_15,                  
         verweduwd_16,westers_totaal_17,
         gemiddelde_woningwaarde_35)      

energy_con <- train_combined_ready %>% 
  select(gemiddeld_elektriciteitsverbruik_totaal_47, 
         gemiddeld_aardgasverbruik_totaal_55, eigen_woning_62,
         percentage_woningen_met_stadsverwarming_63,
         gemiddelde_woningwaarde_35)

socio_eco <- train_combined_ready %>% 
  select(gemiddeld_inkomen_per_inkomensontvanger_65,   
         gemiddeld_inkomen_per_inwoner_66, k_40personen_met_laagste_inkomen_67, 
         k_20personen_met_hoogste_inkomen_68, actieven1575jaar_69, 
         k_40huishoudens_met_laagste_inkomen_70, 
         k_20huishoudens_met_hoogste_inkomen_71,
         gemiddelde_woningwaarde_35)       

environmental_char <- train_combined_ready %>%
  select(afstand_tot_kinderdagverblijf_96, afstand_tot_school_97, 
         scholen_binnen3km_98, mate_van_stedelijkheid_104, 
         omgevingsadressendichtheid_105, 
         totaal_diefstal_uit_woning_schuur_ed_106, 
         gemiddelde_huishoudensgrootte_32,
         bevolkingsdichtheid_33, woningvoorraad_34, gemiddelde_woningwaarde_35) 

char_pop_test <- test_combined_ready %>% 
  select(mannen_6, vrouwen_7, k_0tot15jaar_8,k_15tot25jaar_9, k_25tot45jaar_10,                        
         k_45tot65jaar_11, k_65jaar_of_ouder_12, ongehuwd_13, gehuwd_14, gescheiden_15,                  
         verweduwd_16,westers_totaal_17,
         gemiddelde_woningwaarde_35)      

energy_con_test <- test_combined_ready %>% 
  select(gemiddeld_elektriciteitsverbruik_totaal_47, 
         gemiddeld_aardgasverbruik_totaal_55, eigen_woning_62,
         percentage_woningen_met_stadsverwarming_63,
         gemiddelde_woningwaarde_35)

socio_eco_test <- test_combined_ready %>% 
  select(gemiddeld_inkomen_per_inkomensontvanger_65,   
         gemiddeld_inkomen_per_inwoner_66, k_40personen_met_laagste_inkomen_67, 
         k_20personen_met_hoogste_inkomen_68, actieven1575jaar_69, 
         k_40huishoudens_met_laagste_inkomen_70, 
         k_20huishoudens_met_hoogste_inkomen_71,
         gemiddelde_woningwaarde_35)       

environmental_char_test <- test_combined_ready %>%
  select(afstand_tot_kinderdagverblijf_96, afstand_tot_school_97, 
         scholen_binnen3km_98, mate_van_stedelijkheid_104, 
         omgevingsadressendichtheid_105, 
         totaal_diefstal_uit_woning_schuur_ed_106, 
         gemiddelde_huishoudensgrootte_32,
         bevolkingsdichtheid_33, woningvoorraad_34, gemiddelde_woningwaarde_35)  

# MLR1: char_pop  
MLR_model_1 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., data = char_pop)
summary(MLR_model_1)

#remove char_pop and check improvement
char_pop$gescheiden_15 <- NULL
char_pop_test$gescheiden_15 <- NULL

MLR_model_1 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., data = char_pop)
summary(MLR_model_1)

pred_MLR1 <- predict(MLR_model_1, newdata = char_pop_test)

r2_char_pop <- r2_check(char_pop_test$gemiddelde_woningwaarde_35, pred_MLR1)
RMSE(char_pop_test$gemiddelde_woningwaarde_35, pred_MLR1)

# MLR2: energy_con  
MLR_model_2 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., data = energy_con)
summary(MLR_model_2)

pred_MLR2 <- predict(MLR_model_2, newdata = energy_con_test)

r2_energy_con <- r2_check(energy_con_test$gemiddelde_woningwaarde_35, pred_MLR2)
RMSE (energy_con_test$gemiddelde_woningwaarde_35, pred_MLR2)

# MLR3: socio_eco
MLR_model_3 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., data = socio_eco)
summary(MLR_model_3)

pred_MLR3 <- predict(MLR_model_3, newdata = socio_eco_test)

r2_socio_eco <- r2_check(socio_eco_test$gemiddelde_woningwaarde_35, pred_MLR3)
RMSE(socio_eco_test$gemiddelde_woningwaarde_35, pred_MLR3)

# plot MLR3: socio_eco 
plot(MLR_model_3)

ggplot(data =  socio_eco_test, aes(x = gemiddeld_inkomen_per_inwoner_66, 
                                   y = gemiddelde_woningwaarde_35)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

#plot - check for linearity with residuals vs. fits plot
autoplot(MLR_model_3, 1) +
  theme_classic() 
plot(MLR_model_3, 1)

# MLR4: environmental_char
MLR_model_4 <- lm(formula = gemiddelde_woningwaarde_35 ~ ., 
                  data = environmental_char)
summary(MLR_model_4)

pred_MLR4 <- predict(MLR_model_4, newdata = environmental_char_test)

r2_socio_eco <- r2_check(environmental_char_test$gemiddelde_woningwaarde_35, 
                         pred_MLR4)
RMSE(environmental_char_test$gemiddelde_woningwaarde_35, pred_MLR4)

## Step 5 (Train and test (non)linear algorithms) ------------------------------

#load train/test set -----------------------------------------------------------
pca_train_1 <- readRDS(file = "pca_train.map.rds")
pca_test_1 <- readRDS(file = "pca_test.map.rds")

pca_train <- pca_train_1[5:14]
pca_test <- pca_test_1[5:14]

# PCR --------------------------------------------------------------------------
# source: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/152-principal-component-and-partial-least-squares-regression-essentials/ 
set.seed(123)
model_pcr_1 <- pcr(gemiddelde_woningwaarde_35~., data = pca_train, 
                   scale = FALSE)

summary(model_pcr_1)
# plot model r2 + RMSE vs different values of components
 validationplot(model_pcr_1, val.type="R2")
 validationplot(model_pcr_1, val.type="RMSE")
 
pcr_pred1 <- predict(model_pcr_1, pca_train, ncomp =9)
pcr_pred <- predict(model_pcr_1, pca_test, ncomp =9)

summary(pcr_pred)
# train
mae_pcr_train_1 <- MAE(pca_train$gemiddelde_woningwaarde_35, pcr_pred1)
rmse_pcr_train_1 <-RMSE(pca_train$gemiddelde_woningwaarde_35, pcr_pred1)
r2_pcr_train_1 <- r2_check(pca_train$gemiddelde_woningwaarde_35, pcr_pred1)
corr_pcr_train_1 <- cor(pca_train$gemiddelde_woningwaarde_35, pcr_pred1, method="spearman") 

# test 
mae_pcr_1 <- MAE(pca_test$gemiddelde_woningwaarde_35,pcr_pred )
rmse_pcr_1 <-RMSE(pca_test$gemiddelde_woningwaarde_35,pcr_pred )
r2_pcr_1 <- r2_check(pca_test$gemiddelde_woningwaarde_35, pcr_pred )
corr_pcr_1 <- cor(pca_test$gemiddelde_woningwaarde_35,pcr_pred , method="spearman")

mae_pcr_1
rmse_pcr_1 
r2_pcr_1
corr_pcr_1

# find most important features 
vip(model_pcr_1 , num_features = 9, method = "model") +
  theme_classic() +
  theme(text = element_text(size = 20))  

# SVR --------------------------------------------------------------------------
# source: https://blogs.fu-berlin.de/reseda/svm-regression/
# find optimal parameters for SVR (gamma, cost and epsilon)
set.seed(123)
tuneSVR1 <- tune.svm(pca_train,pca_train$gemiddelde_woningwaarde_35)

tuneSVR1$best.model

# set up SVR regressor with optimal parameters, using radial due to non-linearity 
regressor = svm(formula = gemiddelde_woningwaarde_35 ~ .,
                data =  pca_train,  
                type = "eps-regression",
                kernel = "radial",
                gamma = 0.1 , cost = 1, epsilon = 0.1)

# Fitting the  regression model
# add a new column in the dataframe
# Plot
ggplot() +
  geom_point(aes(x = pca_test$PC3, y = pca_test$gemiddelde_woningwaarde_35), colour = "red") +
  geom_line(aes(x = pca_test$PC3, y = predict(regressor, newdata = pca_test)), colour = "blue")+
  ggtitle("Predicted versus real property neighborhood values(SVR)") +
  theme_classic() +
  xlab("PCA3") +
  ylab("Property Value")

# compute train and test 
y_predTR = predict(regressor, newdata = pca_train[-1])
y_predTR
cmTR = table(pca_train[, 1], y_predTR)
cmTR

y_pred_SVR = predict(regressor, newdata = pca_test[-1])
y_pred_SVR
cm = table(pca_test[, 1], y_pred_SVR)
cm

describe(y_pred_SVR)
#Scatter Plot
plot(pca_train)
points(pca_train$PC1, y_predTR, col = "red", pch=16)

# important variables finding 
# source: https://stackoverflow.com/questions/34781495/how-to-find-important-factors-in-support-vector-machine
cat('SVM model case:\n')
fit2 <- svm(gemiddelde_woningwaarde_35 ~ ., data=pca_train, type = "eps-regression",
            kernel = "radial",  gamma = 0.1 , cost = 1, epsilon = 0.1)
w <- t(fit2$coefs) %*% fit2$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

w_round <- round(w,digits=2)
write.xlsx(w_round, 'svm_weights.xlsx')

# evaluation 
mae_svr_train <- MAE(pca_train$gemiddelde_woningwaarde_35, y_predTR )
rmse_svr_train <-RMSE(pca_train$gemiddelde_woningwaarde_35, y_predTR )
r2_svr_train <- r2_check(pca_train$gemiddelde_woningwaarde_35, y_predTR )
corr_svr_train <- cor(pca_train$gemiddelde_woningwaarde_35, y_predTR, 
                      method="spearman")

mae_svr <- MAE(pca_test$gemiddelde_woningwaarde_35, y_pred_SVR)
rmse_svr <-RMSE(pca_test$gemiddelde_woningwaarde_35, y_pred_SVR)
r2_svr <- r2_check(pca_test$gemiddelde_woningwaarde_35, y_pred_SVR)
corr_svr <- cor(pca_test$gemiddelde_woningwaarde_35, y_pred_SVR, method="spearman")

mae_svr
rmse_svr 
r2_svr
corr_svr

# RF ---------------------------------------------------------------------------
# source: https://rpubs.com/phamdinhkhanh/389752
# source: https://www.rpubs.com/aprasar/293450

# find optimal number of variables at each split
set.seed(123)

train_control_rf <- trainControl(method = "repeatedcv", 
                                 number = 10,
                                 repeats = 2,
                                 verboseIter = TRUE,
                                 allowParallel = TRUE)

rf_tuned <- train(gemiddelde_woningwaarde_35 ~ .,
                  data = pca_train,
                  method = "rf",
                  metric = "RMSE",
                  trControl = train_control_rf)
plot(rf_tuned)
# optimal mtry= 5

# test number of trees 
forest_train <- cforest(gemiddelde_woningwaarde_35~ ., 
                        data=pca_train, controls=cforest_control(mtry=5, mincriterion=0))
# optimal at 500

# set up model 
forest <- randomForest(gemiddelde_woningwaarde_35 ~ ., data = pca_train,
                       mtry=5, ntree=500)

str(forest, 1)
getTree(forest,1)
varImp(forest)
importance(forest)
varImpPlot(forest)
plot(forest)

# plot importance of variables 
par(mfrow=c(1,2))
varImpPlot(forest,main='Variable Importance Plot: RF',pch=16,col='blue')

out_of_bag = predict(forest)
out_of_bag 

y_pred_RF = predict(forest, newdata =  pca_test[-1])
y_pred_RF

describe(y_pred_RF)
# evaluation 
mse_rf <- mean(( pca_test$gemiddelde_woningwaarde_35-y_pred_RF)^2)
mae_rf <- MAE( pca_test$gemiddelde_woningwaarde_35, y_pred_RF)
rmse_rf <-RMSE( pca_test$gemiddelde_woningwaarde_35, y_pred_RF)
r2_check <- function (x, y) cor(x, y) ^ 2
r2_rf <- r2_check( pca_test$gemiddelde_woningwaarde_35, y_pred_RF)
corr_rf <- cor( pca_test$gemiddelde_woningwaarde_35, y_pred_RF, method="spearman")

mae_rf
rmse_rf 
r2_rf
corr_rf

# k-NN -------------------------------------------------------------------------
# source: https://online.stat.psu.edu/stat857/node/129/
# https://bookdown.org/tpinto_home/Regression-and-Classification/k-nearest-neighbours-regression.html
# https://rpubs.com/anshulkumar/KNN-DecTree-RandForest

#find optimal k with CV 
set.seed(123)
trControl <- trainControl(method  = "cv",
                          number  = 10)

best_fit_knn <- train(gemiddelde_woningwaarde_35 ~ .,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:300),
                      trControl  = trControl,
                      metric     = "RMSE",
                      res        = "eucl",
                      data       = pca_train) 
# best fit k= 4 
plot(best_fit_knn)  
 
  
# Find the 4-NN distance for each observation (see k-NN)
k_nn_test_mod <- as.matrix(pca_train[,1:10])
kNNdist(k_nn_test_mod , k = 4)

# Get matrix with distances to the 1st, 2nd, 3th and 4th NN.
kNNdist(k_nn_test_mod, k = 4, all = TRUE)
kNNdistplot(k_nn_test_mod, k = 4)
cl <- dbscan(k_nn_test_mod, eps = .7, minPts = 5)
pairs(k_nn_test_mod, col = cl$cluster+1L)

# first fit k-NN model 
knnFit <- train(gemiddelde_woningwaarde_35 ~ ., data = pca_train, method = "knn")

#Output of kNN fit
knnImp <- varImp(knnFit)
dotPlot(knnImp)
k_nn_test_mod <- as.matrix(k_nn_test_mod[,1:4])

# train final knn model 
reg_pred_train <- pca_train[-1]
reg_pred_test <- pca_test[-1]
abs_outcome_train <- pca_train[1]
abs_outcome_test <- pca_test[1]

reg_results_train <- knn.reg(reg_pred_train, reg_pred_train, abs_outcome_train, 
                             k = 4)

reg_results <- knn.reg(reg_pred_train, reg_pred_test, abs_outcome_train,
                       k = 4)
str(reg_results)
          
# gaining insights 
dtest.y.results <- pca_test$gemiddelde_woningwaarde_35
dtest.y.results$k4.pred <- reg_results$pred

head(dtest.y.results,dim = 5843 (dtest.y.results))

# plot results
plot(pca_test$gemiddelde_woningwaarde_35, reg_results$pred, xlab="y", ylab=expression(hat(y)))
plot(pca_test$PC6, pca_test$gemiddelde_woningwaarde_35)  
lines(seq(-10,10), reg_results$pred)   
describe(reg_results$pred)

# evaluation 

mae_knn_train <- MAE(pca_train$gemiddelde_woningwaarde_35, reg_results_train$pred)
rmse_knn_train <-RMSE(pca_train$gemiddelde_woningwaarde_35, reg_results_train$pred)
r2_knn_train <- r2_check(pca_train$gemiddelde_woningwaarde_35, reg_results_train$pred)
corr_knn_train <- cor(pca_train$gemiddelde_woningwaarde_35, reg_results_train$pred, 
                      method="spearman")

mae_knn <- MAE(pca_test$gemiddelde_woningwaarde_35, reg_results$pred)
rmse_knn <-RMSE(pca_test$gemiddelde_woningwaarde_35, reg_results$pred)
r2_check <- function (x, y) cor(x, y) ^ 2
r2_knn <- r2_check(pca_test$gemiddelde_woningwaarde_35, reg_results$pred)
corr_knn <- cor(pca_test$gemiddelde_woningwaarde_35,reg_results$pred, method="spearman")

mae_knn
rmse_knn 
r2_knn
corr_knn  

# Interpretation via plots------------------------------------------------------
plot(pca_test$gemiddelde_woningwaarde_35, pcr_pred)
plot(pca_test$gemiddelde_woningwaarde_35, y_pred_SVR)
plot(pca_test$gemiddelde_woningwaarde_35,  y_pred_RF)
plot(pca_test$gemiddelde_woningwaarde_35, reg_results$pred)

# pcr
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=pcr_pred,
                 color=pcr_pred-gemiddelde_woningwaarde_35),alpha=0.7) +
  scale_color_gradient2(low = 'red', mid = 'lightgreen', high = 'darkorange') +
  labs(color = "Prediction Error") +
  theme_classic() +
  theme(text = element_text(size = 20))
 
# svr
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=y_pred_SVR,
                 color=y_pred_SVR-gemiddelde_woningwaarde_35),alpha=0.7) +
  scale_color_gradient2(low = 'red', mid = 'lightgreen', high = 'darkorange') +
  labs(color = "Prediction Error") +
  theme_classic() +
  theme(text = element_text(size = 20))

# RF 
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=y_pred_RF,
                 color=y_pred_RF-gemiddelde_woningwaarde_35),alpha=0.7) +
  scale_color_gradient2(low = 'red', mid = 'lightgreen', high = 'darkorange') +
  labs(color = "Prediction Error") +
  theme_classic() +
  theme(text = element_text(size = 20))

# k-NN
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=reg_results$pred,
                 color=reg_results$pred-gemiddelde_woningwaarde_35),alpha=0.7) +
  scale_color_gradient2(low = 'red', mid = 'lightgreen', high = 'darkorange') +
  labs(color = "Prediction Error") +
  theme_classic() +
  theme(text = element_text(size = 20))

# Export for maps --------------------------------------------------------------

# pcr
test_perf_map_pcr_fm <- cbind(pca_test_1,
                           y_pred_pcr= pcr_pred)
test_perf_map_pcr_fm  <- cbind(test_perf_map_pcr_fm,
                            codering_3= pca_test_1$codering_3)
test_perf_map_pcr_fm <- cbind(test_perf_map_pcr_fm,
                            wijken_en_buurten_label= pca_test_1$wijken_en_buurten_label)
test_perf_map_pcr_fm <- cbind(test_perf_map_pcr_fm,
                           gemeentenaam_1= pca_test_1$gemeentenaam_1)
test_perf_map_pcr_fm <- cbind(test_perf_map_pcr_fm,
                                     Jaar=pca_test_1$Jaar)
test_perf_map_pcr_fm <-test_perf_map_pcr_fm[1:15]

# svr
test_perf_map_svr_fm <- cbind(pca_test_1,
                           y_pred_SVR= y_pred_SVR)
test_perf_map_svr_fm  <- cbind(test_perf_map_svr_fm,
                            codering_3= pca_test_1$codering_3)
test_perf_map_svr_fm  <- cbind(test_perf_map_svr_fm,
                            wijken_en_buurten_label= pca_test_1$wijken_en_buurten_label)
test_perf_map_svr_fm <- cbind(test_perf_map_svr_fm,
                           gemeentenaam_1= pca_test_1$gemeentenaam_1)
test_performance_map_svr_fm <- cbind(test_perf_map_svr_fm,
                                  Jaar=pca_test_1$Jaar)
test_performance_map_svr_fm <- test_performance_map_svr_fm[1:15]

# rf
test_perf_map_pcr_rf <- cbind(pca_test_1,
                              y_pred_RF = y_pred_RF)
test_perf_map_pcr_rf  <- cbind(test_perf_map_pcr_rf,
                               codering_3= pca_test_1$codering_3)
test_perf_map_pcr_rf <- cbind(test_perf_map_pcr_rf,
                              wijken_en_buurten_label= pca_test_1$wijken_en_buurten_label)
test_perf_map_pcr_rf <- cbind(test_perf_map_pcr_rf,
                              gemeentenaam_1= pca_test_1$gemeentenaam_1)
test_perf_map_pcr_rf <- cbind(test_perf_map_pcr_rf,
                              Jaar=pca_test_1$Jaar)
test_perf_map_pcr_rf <- test_performance_map_rf_fm[1:15]

# k-nn
test_perf_map_pcr_knn <- cbind(pca_test_1,
                              reg_resultsKNN = reg_results$pred)
test_perf_map_pcr_knn  <- cbind(test_perf_map_pcr_knn,
                               codering_3= pca_test_1$codering_3)
test_perf_map_pcr_knn <- cbind(test_perf_map_pcr_knn,
                              wijken_en_buurten_label= pca_test_1$wijken_en_buurten_label)
test_perf_map_pcr_knn <- cbind(test_perf_map_pcr_knn,
                              gemeentenaam_1= pca_test_1$gemeentenaam_1)
test_perf_map_pcr_knn <- cbind(test_perf_map_pcr_knn,
                              Jaar=pca_test_1$Jaar)
test_perf_map_pcr_knn <- test_perf_map_pcr_knn[1:15]

View(test_perf_map_pcr_knn)
 
## Step 6 (Train and test stable-important vars with (non)linear algorithms) ---

# 5.3 Relevant variables under MINREM selected for model training --------------
MINREM_train_1 <- readRDS(file = "MINREM_model_train.rds")
MINREM_test_1 <-readRDS(file = "MINREM_model_test.rds")

MINREM_train <- MINREM_train_1[1:6]
MINREM_test  <- MINREM_test_1[1:6]

# MLR --------------------------------------------------------------------------
MLR_MINREM <- lm(gemiddelde_woningwaarde_35~ ., data = MINREM_train)

pred_MINREM_MLR <- predict(MLR_MINREM, newdata = MINREM_test)

mae_MINREM_MLR <- MAE(MINREM_test$gemiddelde_woningwaarde_35,  pred_MINREM_MLR)
rmse_MINREM_MLR <-RMSE(MINREM_test$gemiddelde_woningwaarde_35, pred_MINREM_MLR)
r2_MINREM_MLR <- r2_check(MINREM_test$gemiddelde_woningwaarde_35, pred_MINREM_MLR)
corr_MINREM_MLR <- cor(MINREM_test$gemiddelde_woningwaarde_35, pred_MINREM_MLR, 
                       method="spearman")

# important variables 
vip(MLR_MINREM , num_features = 5, method = "model")

mae_MINREM_MLR
rmse_MINREM_MLR
r2_MINREM_MLR
corr_MINREM_MLR

# SVR --------------------------------------------------------------------------
set.seed(123)
tuneSVR_MINREM <- tune.svm(MINREM_train,MINREM_train$gemiddelde_woningwaarde_35)
tuneSVR_MINREM$best.model

# set up SVR regressor with optimal parameters 
regressor = svm(formula = gemiddelde_woningwaarde_35 ~ .,
                data =  MINREM_train,  
                type = "eps-regression",
                kernel = "radial",
                gamma =  0.1666667 , cost = 1, epsilon =  0.1)

# compute test and train 
y_pred_MINREM_svr = predict(regressor, newdata = MINREM_test[-6])
y_pred_MINREM_svr

y_predTR_MINREM_svr = predict(regressor, newdata = MINREM_train[-6])
y_predTR_MINREM_svr

mae_MINREM_svr <- MAE(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_svr)
rmse_MINREM_svr <-RMSE(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_svr)
r2_MINREM_svr <- r2_check(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_svr)
corr_MINREM_svr <- cor(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_svr, method="spearman")

mae_MINREM_svr 
rmse_MINREM_svr 
r2_MINREM_svr 
corr_MINREM_svr 

# RT ---------------------------------------------------------------------------
set.seed(123)
train_MINREM_rf <- trainControl(method = "repeatedcv", 
                                number = 10,
                                repeats = 2,
                                verboseIter = TRUE,
                                allowParallel = TRUE)

rf_tuned_MINREM <- train(gemiddelde_woningwaarde_35 ~ .,
                         data = MINREM_train,
                         method = "rf",
                         metric = "RMSE",
                         trControl = train_MINREM_rf )

plot(rf_tuned_MINREM)

# optimal mtry= 2
forest_MINREM <- randomForest(gemiddelde_woningwaarde_35 ~ ., data = MINREM_train,
                              mtry=2, ntree=128)

str(forest_MINREM, 1)
getTree(forest_MINREM ,1)
varImp(forest_MINREM)
importance(forest_MINREM)
varImpPlot(forest_MINREM)

plot(forest_MINREM)

# plot RF 
# Create a decision tree model
tree <- rpart(gemiddelde_woningwaarde_35~., data=MINREM_train, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

y_pred_MINREM_rf = predict(forest_MINREM, newdata = MINREM_test[-6])
y_pred_MINREM_rf

# evaluation 
mae_rf_MINREM <- MAE(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_rf)
rmse_rf_MINREM  <-RMSE(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_rf)
r2_rf_MINREM  <- r2_check(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_rf)
corr_rf_MINREM  <- cor(MINREM_test$gemiddelde_woningwaarde_35, y_pred_MINREM_rf, method="spearman")

mae_rf_MINREM 
rmse_rf_MINREM 
r2_rf_MINREM 
corr_rf_MINREM

# K-nn -------------------------------------------------------------------------
set.seed(123)

#find optimal k with CV 
trControl <- trainControl(method  = "cv",
                          number  = 10)
best_fit_knn <- train(gemiddelde_woningwaarde_35 ~ .,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:100),
                      trControl  = trControl,
                      metric     = "RMSE",
                      data       = MINREM_train) 
# best fit k= 8
best_fit_knn

# train knn model 
reg_pred_train_MINREM <-  MINREM_train[-6]
reg_pred_test_MINREM <-  MINREM_test[-6]

abs_outcome_train_MINREM <- MINREM_train[6]
abs_outcome_test_MINREM <- MINREM_test[6]

reg_results_MINREM <- knn.reg(reg_pred_train_MINREM, reg_pred_test_MINREM, 
                              abs_outcome_train_MINREM, 
                              k = 8)
reg_results_MINREM

plot(MINREM_test$gemiddelde_woningwaarde_35, reg_results_MINREM$pred, xlab="y", 
     ylab=expression(hat(y)))

# evaluation 
mae_knn_MINREM <- MAE(MINREM_test$gemiddelde_woningwaarde_35, reg_results_MINREM$pred)
rmse_knn_MINREM <-RMSE(MINREM_test$gemiddelde_woningwaarde_35, reg_results_MINREM$pred)
r2_knn_MINREM <- r2_check(MINREM_test$gemiddelde_woningwaarde_35, reg_results_MINREM$pred)
corr_knn_MINREM <- cor(MINREM_test$gemiddelde_woningwaarde_35,reg_results_MINREM$pred, method="spearman")

mae_knn_MINREM
rmse_knn_MINREM 
r2_knn_MINREM
corr_knn_MINREM

# Interpretation via plots------------------------------------------------------

# MLR 
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=pred_MINREM_MLR,
                 color=pred_MINREM_MLR-gemiddelde_woningwaarde_35),alpha=0.7) +
  ggtitle('Plotting Error')  

# svr
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=y_pred_MINREM_svr,
                 color=y_pred_MINREM_svr-gemiddelde_woningwaarde_35),alpha=0.7) +
  ggtitle('Plotting Error')  

# RF 
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=y_pred_MINREM_rf,
                 color=y_pred_MINREM_rf-gemiddelde_woningwaarde_35),alpha=0.7) +
  ggtitle('Plotting Error')  
 


# KNN
ggplot(pca_test) +
  geom_point(aes(x=gemiddelde_woningwaarde_35,y=reg_results_MINREM$pred,
                 color=reg_results_MINREM$pred-gemiddelde_woningwaarde_35),alpha=0.7) +
  ggtitle('Plotting Error')  
  

# Export for maps --------------------------------------------------------------

# MLR
test_performance_map_MLR <- cbind(MINREM_test_1,
                                 y_pred_MINREM_rf= pred_MINREM_MLR)
test_performance_map_MLR  <- cbind(test_performance_MAP_MLR,
                                  codering_3= MINREM_test_1$codering_3)
test_performance_map_MLR  <- cbind(test_performance_MAP_MLR,
                                  wijken_en_buurten_label= MINREM_test_1$wijken_en_buurten_label)
test_performance_map_MLR <- cbind(test_performance_MAP_MLR,
                                 gemeentenaam_1= MINREM_test_1$gemeentenaam_1)
test_performance_map_MLR <- cbind(test_performance_MAP_MLR,
                                 Jaar= MINREM_test_1$Jaar)
test_performance_map_MLR <- test_performance_MAP_MLR[1:11]

# SVR
test_performance_map_SVR <- cbind(MINREM_test_1,
                                 y_pred_MINREM_rf= y_pred_MINREM_svr)
test_performance_map_SVR  <- cbind(test_performance_map_SVR,
                                  codering_3= MINREM_test_1$codering_3)
test_performance_map_SVR  <- cbind(test_performance_map_SVR,
                                  wijken_en_buurten_label= MINREM_test_1$wijken_en_buurten_label)
test_performance_map_SVR <- cbind(test_performance_map_SVR,
                                 gemeentenaam_1= MINREM_test_1$gemeentenaam_1)
test_performance_map_SVR <- cbind(test_performance_map_SVR,
                                 Jaar= MINREM_test_1$Jaar)
test_performance_map_SVR <- test_performance_map_SVR[1:11]

# RF
test_performance_map_RF <- cbind(MINREM_test_1,
                                 y_pred_MINREM_rf= y_pred_MINREM_rf)
test_performance_map_RF  <- cbind(test_performance_map_RF,
                                  codering_3= MINREM_test_1$codering_3)
test_performance_map_RF  <- cbind(test_performance_map_RF,
                                  wijken_en_buurten_label= MINREM_test_1$wijken_en_buurten_label)
test_performance_map_RF <- cbind(test_performance_map_RF,
                                 gemeentenaam_1= MINREM_test_1$gemeentenaam_1)
test_performance_map_RF <- cbind(test_performance_map_RF,
                                 Jaar= MINREM_test_1$Jaar)
test_performance_map_RF <- test_performance_map_RF[1:11]

# k-NN
test_performance_MAP_KNN <- cbind(MINREM_test_1,
                                 y_pred_MINREM_rf= reg_results_MINREM$pred)
test_performance_MAP_KNN  <- cbind(test_performance_MAP_KNN,
                                  codering_3= MINREM_test_1$codering_3)
test_performance_MAP_KNN  <- cbind(test_performance_MAP_KNN,
                                  wijken_en_buurten_label= MINREM_test_1$wijken_en_buurten_label)
test_performance_MAP_KNN <- cbind(test_performance_MAP_KNN,
                                 gemeentenaam_1= MINREM_test_1$gemeentenaam_1)
test_performance_MAP_KNN <- cbind(test_performance_MAP_KNN,
                                 Jaar= MINREM_test_1$Jaar)
test_performance_MAP_KNN <- test_performance_MAP_KNN[1:11]

## Step 7 (Rest of the insights / visualizations for discussion/conclusion ) ---

## Map making ------------------------------------------------------------------

# for svm after PCA

# Load data 
test_performance <- test_performance_map_svr_fm %>% 
  # squared error ² berekenen
  mutate(afwijking = sqrt((gemiddelde_woningwaarde_35 -y_pred_SVR)^2),
         wijken_en_buurten_label = str_trim(wijken_en_buurten_label))
# Layers bekijken in CBS-gebieden-.gpkg-bestand
# cbs_layers <- sf::st_layers('data/cbsgebiedsindelingen-2021-v1/cbsgebiedsindelingen_2021_v1.gpkg')
# View(tibble(cbs_layers$name))
# Voor dit voorbeeld gekozen voor de layer cbs_buurt_2017_labelpoint. Inladen
cbs_buurten2017 <- sf::read_sf('cbsgebiedsindelingen_2021_v1.gpkg', layer = 'cbs_buurt_2017_labelpoint') %>% 
  mutate(statcode = str_trim(statcode)) 
cbs_buurten2016 <- sf::read_sf('cbsgebiedsindelingen_2021_v1.gpkg', layer = 'cbs_buurt_2016_labelpoint') %>% 
  mutate(statcode = str_trim(statcode)) 
# X en Y coordinaat toevoegen
cbs_buurten_xy2017 <- cbs_buurten2017 %>% 
  bind_cols(as.data.frame(st_coordinates(cbs_buurten2017)))
cbs_buurten_xy2016 <- cbs_buurten2016 %>% 
  bind_cols(as.data.frame(st_coordinates(cbs_buurten2016)))
# Objecten combineren
test_performance_sf2017 <- cbs_buurten_xy2017 %>% 
  inner_join((test_performance %>% filter(Jaar == 2017)), by = c('statcode' = 'codering_3'))
test_performance_sf2016 <- cbs_buurten_xy2016 %>% 
  inner_join((test_performance %>% filter(Jaar == 2016)), by = c('statcode' = 'codering_3'))
test_performance1617 <- test_performance_sf2016 %>% bind_rows(test_performance_sf2017)

# plot
test_performance1617 %>% 
  ggplot() +
  geom_sf(size = 2.5, alpha = 2.5, aes(col = afwijking)) +
  scale_color_distiller(palette = "Set3", 
                        name = 'Level of misprediction (x €100)') +
  theme_classic() 

# for RF trained after MINREM 

test_performance <- test_performance_MAP_KNN %>% 
  # squared error ² berekenen
  mutate(afwijking = sqrt((gemiddelde_woningwaarde_35 - y_pred_MINREM_rf)^2),
         wijken_en_buurten_label = str_trim(wijken_en_buurten_label))
# Layers bekijken in CBS-gebieden-.gpkg-bestand
# cbs_layers <- sf::st_layers('data/cbsgebiedsindelingen-2021-v1/cbsgebiedsindelingen_2021_v1.gpkg')
# View(tibble(cbs_layers$name))
# Voor dit voorbeeld gekozen voor de layer cbs_buurt_2017_labelpoint. Inladen
cbs_buurten2017 <- sf::read_sf('cbsgebiedsindelingen_2021_v1.gpkg', layer = 'cbs_buurt_2017_labelpoint') %>% 
  mutate(statcode = str_trim(statcode)) 
cbs_buurten2016 <- sf::read_sf('cbsgebiedsindelingen_2021_v1.gpkg', layer = 'cbs_buurt_2016_labelpoint') %>% 
  mutate(statcode = str_trim(statcode)) 
# X en Y coordinaat toevoegen
cbs_buurten_xy2017 <- cbs_buurten2017 %>% 
  bind_cols(as.data.frame(st_coordinates(cbs_buurten2017)))
cbs_buurten_xy2016 <- cbs_buurten2016 %>% 
  bind_cols(as.data.frame(st_coordinates(cbs_buurten2016)))
# Objecten combineren
test_performance_sf2017 <- cbs_buurten_xy2017 %>% 
  inner_join((test_performance %>% filter(Jaar == 2017)), by = c('statcode' = 'codering_3'))
test_performance_sf2016 <- cbs_buurten_xy2016 %>% 
  inner_join((test_performance %>% filter(Jaar == 2016)), by = c('statcode' = 'codering_3'))
test_performance1617 <- test_performance_sf2016 %>% bind_rows(test_performance_sf2017)

# plot
test_performance1617 %>% 
  ggplot() +
  geom_sf(size = 2.5, alpha = 2.5, aes(col = afwijking)) +
  scale_color_distiller(palette = "Set3", 
                        name = 'Level of misprediction (x €100)') +
  theme_classic() 
