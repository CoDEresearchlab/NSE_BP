###
# Poly-NSE composites parent NSE only
# Aga
# 28/01/21
###

##set wd
setwd("~/Desktop/NSE BP/")

# ===== Loading Packages =====
library(psych)
library(foreign)
library(dplyr)
library(dlookr)
library(lavaan)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(glmnet)
library(caret)

# ===== Load Data: TEDS data prepared as for twin analyses (age & sex regressed) =====
data <- read.csv("./data/NSEBP_agesexregressed_data_with_BP_composites_pureenv_both_twins.csv")
dim(data) #13943  2520
names(data)
labels(data)
#describe (AllData)

# ================================================================ TWIN 1 ======
# ================================================================ PRESCHOOL NSE & CHILDHOOD BP ======
##### Parent #####
## Hyperactivity
coef <- read.csv("./Elastic net res/elasticnet_preschool_NSE_parent_childhood_hyp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_preschool_NSE_childhood_hyp_parent1 <- (data$cdfac11*0.0830169608948519 + data$cerisk1*0.0574025152411451 +
                                                     data$ddfac11*0.102320257923625 + data$derisk1*0.172808635633208)

## Conduct
coef <- read.csv("./Elastic net res/elasticnet_preschool_NSE_parent_childhood_con_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_preschool_NSE_childhood_con_parent1 <- (data$cdfac11*0 + data$cdisto1*0.00827119060246403 + data$cpafel1*0.00198536949756538 + 
                                                     data$cdismak1*0.0438774975614543 + data$cerisk1*0.0490054329830372 + 
                                                     data$ddfac11*0.0605995784703746 + data$ddisto1*0.016654077965073 + 
                                                     data$dpafel1*0.034823373329411 + data$ddismak1*0.0478823734352999 + 
                                                     data$derisk1*0.173418888460208 + data$ddishou1*0.0211725570501951 + 
                                                     data$dbpangr1*0.0305592959442349 + data$dbfac11*0.033265444085687)

## EP
coef <- read.csv("./Elastic net res/elasticnet_preschool_NSE_parent_childhood_ep_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_preschool_NSE_childhood_ep_parent1 <- (data$cerisk1*0.0989497003737219 + data$derisk1*0.167516891600134)

## PP
coef <- read.csv("./Elastic net res/elasticnet_preschool_NSE_parent_childhood_pp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_preschool_NSE_childhood_pp_parent1 <- (data$cerisk1*0.0469693608369854 + data$derisk1*0.160838033397314)

# ================================================================ CHILDHOOD NSE & ADOLESCENCE BP ======
##### Parent #####
## Hyperactivity
coef <- read.csv("./Elastic net res/elasticnet_childhood_NSE_parent_adolescence_hyp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_childhood_NSE_adolescence_hyp_parent1 <- (data$gbpfrus1*0 + data$gdisp1*0.0983377723847531 + data$gbfac11*0.0305046194226972 + 
                                                       data$gpafel1*0.0460096591785917 + data$ipbpimp1*0.0206671130152069 + 
                                                       data$ipbpalo1*0.0169834068954012 + data$ipbpang1*0 + data$ipbpfru1*0.0530259240481691 + 
                                                       data$iphom031*0.0754509225030763 + data$iphom041*0.0713731854012292 + 
                                                       data$ipcla011*-0.0175676475886369 + data$ipcla221*-0.02229395672795 + 
                                                       data$ipcla261*0.0596786975694388 + data$ippafel1*0.0721714390308438 + 
                                                       data$ipcpeer1*-0.0688120782967901 + data$ipcsati1*0 + data$ipcadve1*-0.0765179045506703 + 
                                                       data$ipcacce1*-0.0653992218171917)

## Conduct
coef <- read.csv("./Elastic net res/elasticnet_childhood_NSE_parent_adolescence_con_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_childhood_NSE_adolescence_con_parent1 <- (data$gdismak1*0.0433192238942761 + data$gdisend1*0.0608360761737036 +
                                                       data$gbpfrus1*0 + data$gbpangr1*0.0072521607172997 + data$gdisp1*0.0619451239513285 + 
                                                       data$gbfac11*0 + data$gpafel1*0.0593006357029516 + data$ipbpimp1*0 + 
                                                       data$ipbpalo1*0.0296226329425655 + data$ipbpang1*0.0906104841441616 + 
                                                       data$ipbpfru1*0.0187383340099345 + data$ippafel1*0.121237808136428)

## EP
coef <- read.csv("./Elastic net res/elasticnet_childhood_NSE_parent_adolescence_ep_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_childhood_NSE_adolescence_ep_parent1 <- (data$ipcla181*0.215678682683415 + data$ipcnega1*0.106663653221727)

## PP
coef <- read.csv("./Elastic net res/elasticnet_childhood_NSE_parent_adolescence_pp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_childhood_NSE_adolescence_pp_parent1 <- (data$ipcla021*-0.0846265445644095 + data$ipcla091*-0.0533930942447675 + 
                                                      data$ipcla131*0.0232956521276963 + data$ipcla221*-0.0576475364936716 + 
                                                      data$ipcla231*-0.00741689422205455 + data$ipcla271*-0.0991107662161366 + 
                                                      data$ipcsati1*0 + data$ipcnega1*0.0992246188538995 + data$ipcacce1*-0.102459188000713)

# ================================================================ ADOLESCENCE NSE & ADULTHOOD BP ======
##### Parent #####
## Hyperactivity
coef <- read.csv("./Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_hyp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_adolescence_NSE_adulthood_hyp_parent1 <- data$lppar071

## Conduct
coef <- read.csv("./Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_con_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
data$polyNSE_adolescence_NSE_adulthood_con_parent1 <- (data$lppar051*0.125758869976562 + data$lppar071*0.147327382385784)

## EP
coef <- read.csv("./Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_ep_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
#data$polyNSE_adolescence_NSE_adulthood_ep_parent1

## PP
coef <- read.csv("./Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_pp_parent_coef_agesexregressed_twin1.csv")
coef <- coef[-1,]; coef
coef$X <- paste("data$", coef$X, sep = ""); coef
coef$X <- paste(coef$X, coef$X1, sep = "*"); coef
dput(coef$X)
#data$polyNSE_adolescence_NSE_adulthood_pp_parent1 

# ===== Write out age & sex regressed dataset with poly-NSE composites ===== 
write.csv(data, './data/NSEBP_agesexregressed_data_with_BP_composites_and_polyNSE_composites_pureenv_twin1.csv', row.names=T, quote=F)
