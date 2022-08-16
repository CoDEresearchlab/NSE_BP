###
# NSE BP data preparation for twin analyses (age & sex regress)
# Aga
# 26/07/21
###

##set wd
setwd("~/Desktop/NSE BP/")

# ================================================================ Explore variables ======
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
library(pastecs)
library(varhandle)

# ===== Load Data: TEDS data as sent by Andy =====
AllData <- read.spss("./data/504 Aga Bubel NSE version3 Mar2022.sav", to.data.frame=T, colnames = T)
dim(AllData) #27886 2369
names(AllData)
labels(AllData)
#describe (AllData)

# ===== Extract data labels from data attributes and assign them to an object called 'labels' =====
labels <- attr(AllData, "variable.labels")
labels

write.table(labels, "./labels.txt", col.names = T)

# ===== Exclusions (random == 1) =====
Sel_Data <- AllData[ AllData$random == 1, ]
dim(Sel_Data)
#N = 13943

# ===== Explore factors and convert ===== 
is.factor <- sapply(Sel_Data, is.factor)
factors <- Sel_Data[, is.factor]; factors

Sel_Data <- lapply(Sel_Data,as.numeric)
Sel_Data <- data.frame(Sel_Data)
str(Sel_Data)

# ===== Recode life events var ===== 
## Twin 1
Sel_Data$plifev11 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev01a1 == 0, 0, Sel_Data$ppbhlfev01b1))
Sel_Data$plifev21 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev02a1 == 0, 0, Sel_Data$ppbhlfev02b1))
Sel_Data$plifev31 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev03a1 == 0, 0, Sel_Data$ppbhlfev03b1))
Sel_Data$plifev41 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev04a1 == 0, 0, Sel_Data$ppbhlfev04b1))
Sel_Data$plifev51 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev05a1 == 0, 0, Sel_Data$ppbhlfev05b1))
Sel_Data$plifev61 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev06a1 == 0, 0, Sel_Data$ppbhlfev06b1))
Sel_Data$plifev71 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev07a1 == 0, 0, Sel_Data$ppbhlfev07b1))
Sel_Data$plifev81 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev08a1 == 0, 0, Sel_Data$ppbhlfev08b1))
Sel_Data$plifev91 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev09a1 == 0, 0, Sel_Data$ppbhlfev09b1))
Sel_Data$plifev101 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev10a1 == 0, 0, Sel_Data$ppbhlfev10b1))
Sel_Data$plifev111 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev11a1 == 0, 0, Sel_Data$ppbhlfev11b1))
Sel_Data$plifev121 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev12a1 == 0, 0, Sel_Data$ppbhlfev12b1))
Sel_Data$plifev131 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev13a1 == 0, 0, Sel_Data$ppbhlfev13b1))
Sel_Data$plifev141 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev14a1 == 0, 0, Sel_Data$ppbhlfev14b1))
Sel_Data$plifev151 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev15a1 == 0, 0, Sel_Data$ppbhlfev15b1))
Sel_Data$plifev161 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev16a1 == 0, 0, Sel_Data$ppbhlfev16b1))
Sel_Data$plifev171 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev17a1 == 0, 0, Sel_Data$ppbhlfev17b1))
Sel_Data$plifev181 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev18a1 == 0, 0, Sel_Data$ppbhlfev18b1))
Sel_Data$plifev191 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev19a1 == 0, 0, Sel_Data$ppbhlfev19b1))
Sel_Data$plifev201 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev20a1 == 0, 0, Sel_Data$ppbhlfev20b1))

Sel_Data$clifev11 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev01a1 == 0, 0, Sel_Data$pcbhlfev01b1))
Sel_Data$clifev21 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev02a1 == 0, 0, Sel_Data$pcbhlfev02b1))
Sel_Data$clifev31 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev03a1 == 0, 0, Sel_Data$pcbhlfev03b1))
Sel_Data$clifev41 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev04a1 == 0, 0, Sel_Data$pcbhlfev04b1))
Sel_Data$clifev51 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev05a1 == 0, 0, Sel_Data$pcbhlfev05b1))
Sel_Data$clifev61 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev06a1 == 0, 0, Sel_Data$pcbhlfev06b1))
Sel_Data$clifev71 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev07a1 == 0, 0, Sel_Data$pcbhlfev07b1))
Sel_Data$clifev81 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev08a1 == 0, 0, Sel_Data$pcbhlfev08b1))
Sel_Data$clifev91 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev09a1 == 0, 0, Sel_Data$pcbhlfev09b1))
Sel_Data$clifev101 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev10a1 == 0, 0, Sel_Data$pcbhlfev10b1))
Sel_Data$clifev111 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev11a1 == 0, 0, Sel_Data$pcbhlfev11b1))
Sel_Data$clifev121 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev12a1 == 0, 0, Sel_Data$pcbhlfev12b1))
Sel_Data$clifev131 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev13a1 == 0, 0, Sel_Data$pcbhlfev13b1))
Sel_Data$clifev141 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev14a1 == 0, 0, Sel_Data$pcbhlfev14b1))
Sel_Data$clifev151 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev15a1 == 0, 0, Sel_Data$pcbhlfev15b1))
Sel_Data$clifev161 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev16a1 == 0, 0, Sel_Data$pcbhlfev16b1))
Sel_Data$clifev171 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev17a1 == 0, 0, Sel_Data$pcbhlfev17b1))
Sel_Data$clifev181 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev18a1 == 0, 0, Sel_Data$pcbhlfev18b1))
Sel_Data$clifev191 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev19a1 == 0, 0, Sel_Data$pcbhlfev19b1))
Sel_Data$clifev201 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev20a1 == 0, 0, Sel_Data$pcbhlfev20b1))

## Twin 2
Sel_Data$plifev12 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev01a2 == 0, 0, Sel_Data$ppbhlfev01b2))
Sel_Data$plifev22 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev02a2 == 0, 0, Sel_Data$ppbhlfev02b2))
Sel_Data$plifev32 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev03a2 == 0, 0, Sel_Data$ppbhlfev03b2))
Sel_Data$plifev42 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev04a2 == 0, 0, Sel_Data$ppbhlfev04b2))
Sel_Data$plifev52 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev05a2 == 0, 0, Sel_Data$ppbhlfev05b2))
Sel_Data$plifev62 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev06a2 == 0, 0, Sel_Data$ppbhlfev06b2))
Sel_Data$plifev72 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev07a2 == 0, 0, Sel_Data$ppbhlfev07b2))
Sel_Data$plifev82 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev08a2 == 0, 0, Sel_Data$ppbhlfev08b2))
Sel_Data$plifev92 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev09a2 == 0, 0, Sel_Data$ppbhlfev09b2))
Sel_Data$plifev102 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev10a2 == 0, 0, Sel_Data$ppbhlfev10b2))
Sel_Data$plifev112 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev11a2 == 0, 0, Sel_Data$ppbhlfev11b2))
Sel_Data$plifev122 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev12a2 == 0, 0, Sel_Data$ppbhlfev12b2))
Sel_Data$plifev132 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev13a2 == 0, 0, Sel_Data$ppbhlfev13b2))
Sel_Data$plifev142 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev14a2 == 0, 0, Sel_Data$ppbhlfev14b2))
Sel_Data$plifev152 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev15a2 == 0, 0, Sel_Data$ppbhlfev15b2))
Sel_Data$plifev162 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev16a2 == 0, 0, Sel_Data$ppbhlfev16b2))
Sel_Data$plifev172 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev17a2 == 0, 0, Sel_Data$ppbhlfev17b2))
Sel_Data$plifev182 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev18a2 == 0, 0, Sel_Data$ppbhlfev18b2))
Sel_Data$plifev192 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev19a2 == 0, 0, Sel_Data$ppbhlfev19b2))
Sel_Data$plifev202 <- with(Sel_Data, ifelse(Sel_Data$ppbhlfev20a2 == 0, 0, Sel_Data$ppbhlfev20b2))

Sel_Data$clifev12 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev01a2 == 0, 0, Sel_Data$pcbhlfev01b2))
Sel_Data$clifev22 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev02a2 == 0, 0, Sel_Data$pcbhlfev02b2))
Sel_Data$clifev32 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev03a2 == 0, 0, Sel_Data$pcbhlfev03b2))
Sel_Data$clifev42 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev04a2 == 0, 0, Sel_Data$pcbhlfev04b2))
Sel_Data$clifev52 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev05a2 == 0, 0, Sel_Data$pcbhlfev05b2))
Sel_Data$clifev62 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev06a2 == 0, 0, Sel_Data$pcbhlfev06b2))
Sel_Data$clifev72 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev07a2 == 0, 0, Sel_Data$pcbhlfev07b2))
Sel_Data$clifev82 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev08a2 == 0, 0, Sel_Data$pcbhlfev08b2))
Sel_Data$clifev92 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev09a2 == 0, 0, Sel_Data$pcbhlfev09b2))
Sel_Data$clifev102 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev10a2 == 0, 0, Sel_Data$pcbhlfev10b2))
Sel_Data$clifev112 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev11a2 == 0, 0, Sel_Data$pcbhlfev11b2))
Sel_Data$clifev122 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev12a2 == 0, 0, Sel_Data$pcbhlfev12b2))
Sel_Data$clifev132 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev13a2 == 0, 0, Sel_Data$pcbhlfev13b2))
Sel_Data$clifev142 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev14a2 == 0, 0, Sel_Data$pcbhlfev14b2))
Sel_Data$clifev152 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev15a2 == 0, 0, Sel_Data$pcbhlfev15b2))
Sel_Data$clifev162 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev16a2 == 0, 0, Sel_Data$pcbhlfev16b2))
Sel_Data$clifev172 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev17a2 == 0, 0, Sel_Data$pcbhlfev17b2))
Sel_Data$clifev182 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev18a2 == 0, 0, Sel_Data$pcbhlfev18b2))
Sel_Data$clifev192 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev19a2 == 0, 0, Sel_Data$pcbhlfev19b2))
Sel_Data$clifev202 <- with(Sel_Data, ifelse(Sel_Data$pcbhlfev20a2 == 0, 0, Sel_Data$pcbhlfev20b2))


# ================================================================ Age & sex regression ======
# ===== Age & sex regress; Twin 1 ===== 
##### Year 3 #####
Sel_Data[, c("ctfac11",    "ctfac21",			
             "cgfac11",		"cgfac21",		"cdfac11",	
             "cdfac21",		"cdfac31",		"cdisto1",	
             "cbfac11",		"cbfac21",		"cpafel1",	
             "cdismak1",		"cdishou1",		"cdiexpl1",	
             "cdifirm1",		"cdijoke1",		"cdiasko1",	
             "cbpimpa1",		"cbphapp1",		"cbpamus1",	
             "cbpaway1",		"cbpangr1",		"cbpclos1",	
             "cbpfrus1",		"ctrhym1",		"ctpron1",	
             "ctsent1",		"ctword1",		"ctloca1",	
             "ctbook1",		"cttalk1",		"cteat1",	
             "cgmessy1",		"cgpuzz1",		"cgmusi1",	
             "cgtapes1",		"cgbooks1",		"cgzoo1",	
             "cgphys1",		"cgboard1",		"chsusp1",	
             "chcatar1",		"chache1",		"chpus1",	
             "chmouth1",		"chsnore1",		"chignor1",	
             "chtalk1",		"cerisk1",		"cseengp1",
             "cseengx1",		"chosp1",		  "chospx1",
             "chospdd1",		"chospca1",		"chospcx1",
             "csurgry1",		"csurage1",		"csurgga1",
             "csurggx1",		"cslprob1",		"cslwake1",
             "cslnmar1",		"cslearl1",		"cpotty1",
             "cpnapda1",		"cpnapnt1",		"cpottrp1",
             "cpottrn1",		"cpdryda1",		"cpdrynt1",
             "cpnapp1",		"cpdrink1",		"cprward1",
             "cpparbd1",		"cpwake1",		"cplift1",
             "cpreass1",		"cplaygp1",		"cprocou1",
             "cproast1",		"cprofit1",		"cproeye1",
             "cproski1",		"cprosto1",		"cprovom1",
             "cprohd1",		"cprodia1",		"cprowk1",
             "cbhanxt1", "cbhcont1", "cbhhypt1")] <- apply(
               Sel_Data[, c("ctfac11",    "ctfac21",			
                            "cgfac11",		"cgfac21",		"cdfac11",	
                            "cdfac21",		"cdfac31",		"cdisto1",	
                            "cbfac11",		"cbfac21",		"cpafel1",	
                            "cdismak1",		"cdishou1",		"cdiexpl1",	
                            "cdifirm1",		"cdijoke1",		"cdiasko1",	
                            "cbpimpa1",		"cbphapp1",		"cbpamus1",	
                            "cbpaway1",		"cbpangr1",		"cbpclos1",	
                            "cbpfrus1",		"ctrhym1",		"ctpron1",	
                            "ctsent1",		"ctword1",		"ctloca1",	
                            "ctbook1",		"cttalk1",		"cteat1",	
                            "cgmessy1",		"cgpuzz1",		"cgmusi1",	
                            "cgtapes1",		"cgbooks1",		"cgzoo1",	
                            "cgphys1",		"cgboard1",		"chsusp1",	
                            "chcatar1",		"chache1",		"chpus1",	
                            "chmouth1",		"chsnore1",		"chignor1",	
                            "chtalk1",		"cerisk1",		"cseengp1",
                            "cseengx1",		"chosp1",		  "chospx1",
                            "chospdd1",		"chospca1",		"chospcx1",
                            "csurgry1",		"csurage1",		"csurgga1",
                            "csurggx1",		"cslprob1",		"cslwake1",
                            "cslnmar1",		"cslearl1",		"cpotty1",
                            "cpnapda1",		"cpnapnt1",		"cpottrp1",
                            "cpottrn1",		"cpdryda1",		"cpdrynt1",
                            "cpnapp1",		"cpdrink1",		"cprward1",
                            "cpparbd1",		"cpwake1",		"cplift1",
                            "cpreass1",		"cplaygp1",		"cprocou1",
                            "cproast1",		"cprofit1",		"cproeye1",
                            "cproski1",		"cprosto1",		"cprovom1",
                            "cprohd1",		"cprodia1",		"cprowk1",
                            "cbhanxt1", "cbhcont1", "cbhhypt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$crepage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 4 #####
Sel_Data[, c("dhsusp1",	"dhtalk1",	
             "dhaid1",	  "dhcatar1",	"dhache1",
             "dhpus1",	  "dhmouth1",	"dhsnore1",
             "dhignor1",	"dskitch1",	"dsktrt1",
             "dskface1",	"dskknee1",	"dskwris1",
             "dskoth1",	"dskage1",	"dskstil1",
             "dasthma1",	"dallerg1",	"dproeye1",
             "dprosto1",	"dprovom1",	"dprohd1",
             "dprodia1",	"dprowk1",	"dprofit1",
             "dseengp1",	"dseengx1",	"dbacmen1",
             "dbacmm1",	"dbacmp1",	"dbacms1",
             "dbacmh1",	"dbacmo1",	"dhosp1",
             "dhospx1",	"dhospdd1",	"dhospca1",
             "dhospcx1",	"dsurgry1",	"dsurage1",
             "dsurgga1",	"dapalon1",	"dapoth1",
             "dbedsh1",	"dslprob1",	"dslwake1",
             "dslnmar1",	"dslearl1",	"dpotty1",
             "dnapda1",	"dnapnt1",	"dpottrp1",
             "dpottrn1",	"dpdryda1",	"dpdrynt1",
             "dpnapp1",	"dpdrink1",	"dprward1",
             "dpparbd1",	"dpwake1",	"dplift1",
             "dpreass1",	"dsay011",	"dsay01a1",
             "dsay021",	"dsay031",	"dtrhym1",
             "dtpron1",	"dtsent1",	"dtword1",
             "dtloca1",	"dtbook1",	"dttalk1",
             "dteat1",	  "dgmessy1",	"dgpuzz1",
             "dgmusi1",	"dgtapes1",	"dgbooks1",
             "dgzoo1",	  "dgphys1",	"dgboard1",
             "ddismak1",	"ddishou1",	"ddiexpl1",
             "ddifirm1",	"ddijoke1",	"ddiasko1",
             "dbpimpa1",	"dbphapp1",	"dbpamus1",
             "dbpaway1",	"dbpangr1",	"dbpclos1",
             "dbpfrus1",	"dtfac11",	"dtfac21",
             "dgfac11",	"dgfac21",	"ddfac11",
             "ddfac21",	"ddfac31",	"ddisto1",
             "dbfac11",	"dbfac21",	"dpafel1",
             "derisk1",	"ddrawt1",	"dtoleft1",
             "dtorigh1",	"dhand1",	  "dhands1",
             "dlltalk1",	"dllvoc1",	"dllpic1",
             "dllslow1",	"dllang1",	"dplaygp1",
             "dtvoc1",
             "dsdanxt1", "dsdcont1", "dsdhypt1", "dsdpert1")] <- apply(
               Sel_Data[, c("dhsusp1",	"dhtalk1",	
                            "dhaid1",	  "dhcatar1",	"dhache1",
                            "dhpus1",	  "dhmouth1",	"dhsnore1",
                            "dhignor1",	"dskitch1",	"dsktrt1",
                            "dskface1",	"dskknee1",	"dskwris1",
                            "dskoth1",	"dskage1",	"dskstil1",
                            "dasthma1",	"dallerg1",	"dproeye1",
                            "dprosto1",	"dprovom1",	"dprohd1",
                            "dprodia1",	"dprowk1",	"dprofit1",
                            "dseengp1",	"dseengx1",	"dbacmen1",
                            "dbacmm1",	"dbacmp1",	"dbacms1",
                            "dbacmh1",	"dbacmo1",	"dhosp1",
                            "dhospx1",	"dhospdd1",	"dhospca1",
                            "dhospcx1",	"dsurgry1",	"dsurage1",
                            "dsurgga1",	"dapalon1",	"dapoth1",
                            "dbedsh1",	"dslprob1",	"dslwake1",
                            "dslnmar1",	"dslearl1",	"dpotty1",
                            "dnapda1",	"dnapnt1",	"dpottrp1",
                            "dpottrn1",	"dpdryda1",	"dpdrynt1",
                            "dpnapp1",	"dpdrink1",	"dprward1",
                            "dpparbd1",	"dpwake1",	"dplift1",
                            "dpreass1",	"dsay011",	"dsay01a1",
                            "dsay021",	"dsay031",	"dtrhym1",
                            "dtpron1",	"dtsent1",	"dtword1",
                            "dtloca1",	"dtbook1",	"dttalk1",
                            "dteat1",	  "dgmessy1",	"dgpuzz1",
                            "dgmusi1",	"dgtapes1",	"dgbooks1",
                            "dgzoo1",	  "dgphys1",	"dgboard1",
                            "ddismak1",	"ddishou1",	"ddiexpl1",
                            "ddifirm1",	"ddijoke1",	"ddiasko1",
                            "dbpimpa1",	"dbphapp1",	"dbpamus1",
                            "dbpaway1",	"dbpangr1",	"dbpclos1",
                            "dbpfrus1",	"dtfac11",	"dtfac21",
                            "dgfac11",	"dgfac21",	"ddfac11",
                            "ddfac21",	"ddfac31",	"ddisto1",
                            "dbfac11",	"dbfac21",	"dpafel1",
                            "derisk1",	"ddrawt1",	"dtoleft1",
                            "dtorigh1",	"dhand1",	  "dhands1",
                            "dlltalk1",	"dllvoc1",	"dllpic1",
                            "dllslow1",	"dllang1",	"dplaygp1",
                            "dtvoc1",
                            "dsdanxt1", "dsdcont1", "dsdhypt1", "dsdpert1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$drepage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 7 #####
## Parent
Sel_Data[, c("gillness1",		
             "gillcp1",		"gillcf1",		"gillaut1",	
             "gillds1",		"gildysp1",		"gillhyp1",	
             "gilasth1",	"gilepil1",		"gilloth1",	
             "gscomp1",		"gscskin1",		"gscasth1",	
             "gscalle1",	"gheardif1",	"gspchdif1",	
             "gspt1",		  "gsptage1",		"gspthl1",
             "gnoise1",		"gwets1",		  "ghtics1",
             "gfits1",		"ghospad1",		"ghospl1",
             "gsickf1",	
             "gsistom1",	"gsihead1",		"gsivomi1",	
             "gaches1",		"gacches1",		"gaclimb1",	
             "gacback1",	"gactire1",		"gacdizz1",	
             "gacothe1",	"gsymprob1",	"gwtkg1",	
             "ghtcm1",		"gdiexpl1",		"gdismak1",	
             "gdisend1",	"gdishou1",		"gdiigno1",	
             "gdiprai1",	"gbpimpa1",		"gbphapp1",	
             "gbpamus1",	"gbpfrus1",		"gbpleav1",	
             "gbpangr1",	"gbpclos1",		"gschool1",	
             "gscfeel1",	"glrndif1",		"gldaut1",
             "gldds1",		"gldrest1",		"gldhyp1",
             "glddysl1",	"gldwrit1",		"gldmot1",
             "gldmath1",	"gldoth1",		"gsen1",
             "gsenass1",	"gextsup1",		"ges1to11",
             "geshelp1",	"gespsyc1",		"gesiep1",
             "gesoth1",		"gschoff1",		"gscoffl1",
             "gdisp1",		 
             "gbfac11",		"gbfac21",
             "gpafel1",
             "gpshypt1", "gpscont1", "gpspert1", 
             "gpsanxt1")] <- apply(
               Sel_Data[, c("gillness1",		
                            "gillcp1",		"gillcf1",		"gillaut1",	
                            "gillds1",		"gildysp1",		"gillhyp1",	
                            "gilasth1",	"gilepil1",		"gilloth1",	
                            "gscomp1",		"gscskin1",		"gscasth1",	
                            "gscalle1",	"gheardif1",	"gspchdif1",	
                            "gspt1",		  "gsptage1",		"gspthl1",
                            "gnoise1",		"gwets1",		  "ghtics1",
                            "gfits1",		"ghospad1",		"ghospl1",
                            "gsickf1",	
                            "gsistom1",	"gsihead1",		"gsivomi1",	
                            "gaches1",		"gacches1",		"gaclimb1",	
                            "gacback1",	"gactire1",		"gacdizz1",	
                            "gacothe1",	"gsymprob1",	"gwtkg1",	
                            "ghtcm1",		"gdiexpl1",		"gdismak1",	
                            "gdisend1",	"gdishou1",		"gdiigno1",	
                            "gdiprai1",	"gbpimpa1",		"gbphapp1",	
                            "gbpamus1",	"gbpfrus1",		"gbpleav1",	
                            "gbpangr1",	"gbpclos1",		"gschool1",	
                            "gscfeel1",	"glrndif1",		"gldaut1",
                            "gldds1",		"gldrest1",		"gldhyp1",
                            "glddysl1",	"gldwrit1",		"gldmot1",
                            "gldmath1",	"gldoth1",		"gsen1",
                            "gsenass1",	"gextsup1",		"ges1to11",
                            "geshelp1",	"gespsyc1",		"gesiep1",
                            "gesoth1",		"gschoff1",		"gscoffl1",
                            "gdisp1",		 
                            "gbfac11",		"gbfac21",
                            "gpafel1",
                            "gpshypt1", "gpscont1", "gpspert1", 
                            "gpsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$gpbage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("gttype1",
             "gtknow1",		"gtsped1",		"gteff1",
             "gtbehav1",		"gtlearn1",		"gthappy1",
             "gtdiff1",		"gtdiffl1",		"gtdiffu1",
             "gtdiffp1",		"gtdiffc1",		"gtdiffb1","gtshypt1", "gtscont1", 
             "gtspert1", "gtsanxt1")] <- apply(
               Sel_Data[, c("gttype1",
                            "gtknow1",		"gtsped1",		"gteff1",
                            "gtbehav1",		"gtlearn1",		"gthappy1",
                            "gtdiff1",		"gtdiffl1",		"gtdiffu1",
                            "gtdiffp1",		"gtdiffc1",		"gtdiffb1","gtshypt1", "gtscont1", 
                            "gtspert1", "gtsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$gtqage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("gsim011",		"gsim021",			
             "gsim031",		"gsim041",		"gsim051",
             "gsim061",		"gsim071",		"gsim081",
             "gsim091",		"gsim101",		"gsim111",
             "gsim121",		"gsim131",		"gsimilt1",
             "gcfrnd11",		"gcfrnd21",		"gcfrnd31",
             "gcfrnd41",		"gcemo11",		"gcemo21",
             "gcemo31",		"gcemo41",		"gcemo51",
             "gcemo61",		"gcemo71")] <- apply(
               Sel_Data[, c("gsim011",		"gsim021",			
                            "gsim031",		"gsim041",		"gsim051",
                            "gsim061",		"gsim071",		"gsim081",
                            "gsim091",		"gsim101",		"gsim111",
                            "gsim121",		"gsim131",		"gsimilt1",
                            "gcfrnd11",		"gcfrnd21",		"gcfrnd31",
                            "gcfrnd41",		"gcemo11",		"gcemo21",
                            "gcemo31",		"gcemo41",		"gcemo51",
                            "gcemo61",		"gcemo71")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$gciage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 9 #####
## Parent
Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
             "ipeczem1",	"ipasthm1",	"ipothal1",
             "iplndif1",	"ipldaut1",	"ipldasp1",
             "ipldrst1",	"ipldhyp1",	"iplddsl1",
             "ipldwri1",	"iplddsp1",	"ipldmth1",
             "ipldlan1",	"ipldhea1",	"ipldeye1",
             "ipldspl1",	"ipldfal1",	"ipsen1",
             "ipsenrg1",	"ipdisma1",	"ipdisho1",
             "ipdiexp1",	"ipdiexr1",	"ipdifir1",
             "ipdifrr1",	"ipdijok1",	"ipdiask1",
             "ipbpimp1",	"ipbphap1",	"ipbphar1",
             "ipbpamu1",	"ipbpamr1",	"ipbpalo1",
             "ipbpang1",	"ipbpclo1",	"ipbpclr1",
             "ipbpfru1",	"ipbpeat1",	"ipbpsle1",
             "iphom011",	"iphom021",	"iphom031",
             "iphom041",	"iphom051",	"iphom061",
             "iphom091",	"iphom101",	"iphom171",
             "iphom181",	"iphom191",	"iphom201",
             "iphom221",	"iphom231",	"ipcla011",
             "ipcla021",	"ipcla031",	"ipcla041",
             "ipcla051",	"ipcla061",	"ipcla071",
             "ipcla081",	"ipcla091",	"ipcla101",
             "ipcla111",	"ipcla121",	"ipcla131",
             "ipcla141",	"ipcla151",	"ipcla161",
             "ipcla171",	"ipcla181",	"ipcla191",
             "ipcla201",	"ipcla211",	"ipcla221",
             "ipcla231",	"ipcla241",	"ipcla251",
             "ipcla261",	"ipcla271",	"ipcla281",
             "ipcsoct1",	"ippafel1",	"ipdisto1",
             "ipcpeer1",	"ipcsati1",	"ipcteac1",	
             "ipcnega1",	"ipcadve1",
             "ipcoppo1",	"ipcacce1", "ipshypt1", "ipscont1", 
             "ipspert1", "ipsanxt1")] <- apply(
               Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
                            "ipeczem1",	"ipasthm1",	"ipothal1",
                            "iplndif1",	"ipldaut1",	"ipldasp1",
                            "ipldrst1",	"ipldhyp1",	"iplddsl1",
                            "ipldwri1",	"iplddsp1",	"ipldmth1",
                            "ipldlan1",	"ipldhea1",	"ipldeye1",
                            "ipldspl1",	"ipldfal1",	"ipsen1",
                            "ipsenrg1",	"ipdisma1",	"ipdisho1",
                            "ipdiexp1",	"ipdiexr1",	"ipdifir1",
                            "ipdifrr1",	"ipdijok1",	"ipdiask1",
                            "ipbpimp1",	"ipbphap1",	"ipbphar1",
                            "ipbpamu1",	"ipbpamr1",	"ipbpalo1",
                            "ipbpang1",	"ipbpclo1",	"ipbpclr1",
                            "ipbpfru1",	"ipbpeat1",	"ipbpsle1",
                            "iphom011",	"iphom021",	"iphom031",
                            "iphom041",	"iphom051",	"iphom061",
                            "iphom091",	"iphom101",	"iphom171",
                            "iphom181",	"iphom191",	"iphom201",
                            "iphom221",	"iphom231",	"ipcla011",
                            "ipcla021",	"ipcla031",	"ipcla041",
                            "ipcla051",	"ipcla061",	"ipcla071",
                            "ipcla081",	"ipcla091",	"ipcla101",
                            "ipcla111",	"ipcla121",	"ipcla131",
                            "ipcla141",	"ipcla151",	"ipcla161",
                            "ipcla171",	"ipcla181",	"ipcla191",
                            "ipcla201",	"ipcla211",	"ipcla221",
                            "ipcla231",	"ipcla241",	"ipcla251",
                            "ipcla261",	"ipcla271",	"ipcla281",
                            "ipcsoct1",	"ippafel1",	"ipdisto1",
                            "ipcpeer1",	"ipcsati1",	"ipcteac1",	
                            "ipcnega1",	"ipcadve1",
                            "ipcoppo1",	"ipcacce1", "ipshypt1", "ipscont1", 
                            "ipspert1", "ipsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$icpage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("itcla11",	"itcla21",	"itcla31",
             "itcla41",	"itcla51",	"itcla61",
             "itcla71",	"itcla81",	"itclasz1",
             "itsenrg1",	"itsen1",	"itcpeer1",
             "itcsati1", "itshypt1", "itscont1", 
             "itspert1", "itsanxt1")] <- apply(
               Sel_Data[, c("itcla11",	"itcla21",	"itcla31",
                            "itcla41",	"itcla51",	"itcla61",
                            "itcla71",	"itcla81",	"itclasz1",
                            "itsenrg1",	"itsen1",	"itcpeer1",
                            "itcsati1", "itshypt1", "itscont1", 
                            "itspert1", "itsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$itage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("ichom011",	"ichom021",
             "ichom031",	"ichom041",	"ichom051",
             "ichom061",	"ichom071",	"ichom081",
             "ichom091",	"ichom101",	"ichom111",
             "icho11r1",	"ichom121",	"ichom131",
             "ichom141",	"icho14r1",	"ichom151",
             "ichom161",	"icho16r1",	"ichom171",
             "ichom181",	"ichom191",	"ichom201",
             "ichom211",	"ichom221",	"ichom231",
             "icdisma1",	"icdisho1",	"icdiexp1",
             "icdiexr1",	"icdifir1",	"icdifrr1",
             "icdijok1",	"icdiask1",	"ichom251",
             "ichom261",	"icho26r1",	"ichom271",
             "icho27r1",	"ichom281",	"ichom291",
             "ichom301",	"icho30r1",	"ichom311",
             "ichom321",	"ichom331",	"iccla011",
             "iccla021",	"iccla031",	"iccla041",
             "iccla051",	"iccla061",	"iccla071",
             "iccla081",	"iccla091",	"iccla101",
             "iccla111",	"iccla121",	"iccla131",
             "iccla141",	"iccla151",	"iccla161",
             "iccla171",	"iccla181",	"iccla191",
             "iccla201",	"iccla211",	"iccla221",
             "iccla231",	"iccla241",	"iccla251",
             "iccla261",	"iccla271",	"iccla281",
             "icchato1", "icpafel1",	"icdisto1",	
             "iccpeer1",	"iccsati1",	"iccteac1",
             "iccnega1",	"iccadve1",	"iccoppo1",
             "iccacce1",
             "icshypt1", "icscont1", "icsanxt1","icspert1")] <- apply(
               Sel_Data[, c("ichom011",	"ichom021",
                            "ichom031",	"ichom041",	"ichom051",
                            "ichom061",	"ichom071",	"ichom081",
                            "ichom091",	"ichom101",	"ichom111",
                            "icho11r1",	"ichom121",	"ichom131",
                            "ichom141",	"icho14r1",	"ichom151",
                            "ichom161",	"icho16r1",	"ichom171",
                            "ichom181",	"ichom191",	"ichom201",
                            "ichom211",	"ichom221",	"ichom231",
                            "icdisma1",	"icdisho1",	"icdiexp1",
                            "icdiexr1",	"icdifir1",	"icdifrr1",
                            "icdijok1",	"icdiask1",	"ichom251",
                            "ichom261",	"icho26r1",	"ichom271",
                            "icho27r1",	"ichom281",	"ichom291",
                            "ichom301",	"icho30r1",	"ichom311",
                            "ichom321",	"ichom331",	"iccla011",
                            "iccla021",	"iccla031",	"iccla041",
                            "iccla051",	"iccla061",	"iccla071",
                            "iccla081",	"iccla091",	"iccla101",
                            "iccla111",	"iccla121",	"iccla131",
                            "iccla141",	"iccla151",	"iccla161",
                            "iccla171",	"iccla181",	"iccla191",
                            "iccla201",	"iccla211",	"iccla221",
                            "iccla231",	"iccla241",	"iccla251",
                            "iccla261",	"iccla271",	"iccla281",
                            "icchato1", "icpafel1",	"icdisto1",	
                            "iccpeer1",	"iccsati1",	"iccteac1",
                            "iccnega1",	"iccadve1",	"iccoppo1",
                            "iccacce1",
                            "icshypt1", "icscont1", "icsanxt1","icspert1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$icpage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 12 #####
## Parent
Sel_Data[, c("lpdis011",
             "lpdis021",		"lpdis031",		"lpdis03r1",
             "lpdis041",		"lpdis04r1",		
             "lpdis051",		"lpdis061",		"lppar011",	
             "lppar021",		"lppar02r1",	"lppar031",	
             "lppar03r1",	"lppar041",		
             "lppar051",		"lppar061",		"lppar06r1",	
             "lppar071",		"lpdisto1",
             "lpshypt1", "lpscont1",  
             "lpspert1", "lpsanxt1")] <- apply(
               Sel_Data[, c("lpdis011",
                            "lpdis021",		"lpdis031",		"lpdis03r1",
                            "lpdis041",		"lpdis04r1",		
                            "lpdis051",		"lpdis061",		"lppar011",	
                            "lppar021",		"lppar02r1",	"lppar031",	
                            "lppar03r1",	"lppar041",		
                            "lppar051",		"lppar061",		"lppar06r1",	
                            "lppar071",		"lpdisto1",
                            "lpshypt1", "lpscont1",  
                            "lpspert1", "lpsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$lpqage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("ltsenrg1", 
             "ltsen1", "ltshypt1", "ltscont1", 
             "ltspert1", "ltsanxt1")] <- apply(
               Sel_Data[, c("ltsenrg1", 
                            "ltsen1", "ltshypt1", "ltscont1", 
                            "ltspert1", "ltsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$ltqage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("lccha11",		"lccha1r1",			
             "lccha21",		"lccha31",		"lccha41",
             "lccha4r1",		"lccha51",		"lccha61",
             "lccha6r1",		"lcvic011",		"lcvic021",
             "lcvic031",		"lcvic041",		"lcvic051",
             "lcvic061",		"lcvic071",		"lcvic081",
             "lcvic091",		"lcvic101",		"lcvic111",
             "lcvic121",		"lcvic131",		"lcvic141",
             "lcvic151",		"lcvic161",		"lcdis11",
             "lcdis21",		"lcdis31",		"lcdis3r1",
             "lcdis41",		"lcdis4r1",		"lcdis51",
             "lcdis61",		"lcpar11",		"lcpar21",
             "lcpar2r1",		"lcpar31",		"lcpar3r1",
             "lcpar41",		"lcpar51",		"lcpar61",
             "lcpar6r1",		"lcpar71",		"lcpub11",
             "lcpub21",		"lcpub31",		"lcsex1",
             "lcpub41",		"lcpub51",		"lcpub5age1",
             "lcpub61",		"lcpub71",		"lcchato1",		
             "lcpafel1",	"lcdisto1",	
             "lcmoaca1",		"lmatenv1",		"llitenv1",	
             "llitenx1",		"llitenf1",		
             "lcvicph1",		"lcvicve1",		"lcvicso1",	
             "lcvicpr1",
             "lcshypt1", "lcscont1", "lcspert1", "lcsanxt1")] <- apply(
               Sel_Data[, c("lccha11",		"lccha1r1",			
                            "lccha21",		"lccha31",		"lccha41",
                            "lccha4r1",		"lccha51",		"lccha61",
                            "lccha6r1",		"lcvic011",		"lcvic021",
                            "lcvic031",		"lcvic041",		"lcvic051",
                            "lcvic061",		"lcvic071",		"lcvic081",
                            "lcvic091",		"lcvic101",		"lcvic111",
                            "lcvic121",		"lcvic131",		"lcvic141",
                            "lcvic151",		"lcvic161",		"lcdis11",
                            "lcdis21",		"lcdis31",		"lcdis3r1",
                            "lcdis41",		"lcdis4r1",		"lcdis51",
                            "lcdis61",		"lcpar11",		"lcpar21",
                            "lcpar2r1",		"lcpar31",		"lcpar3r1",
                            "lcpar41",		"lcpar51",		"lcpar61",
                            "lcpar6r1",		"lcpar71",		"lcpub11",
                            "lcpub21",		"lcpub31",		"lcsex1",
                            "lcpub41",		"lcpub51",		"lcpub5age1",
                            "lcpub61",		"lcpub71",		"lcchato1",		
                            "lcpafel1",	"lcdisto1",	
                            "lcmoaca1",		"lmatenv1",		"llitenv1",	
                            "llitenx1",		"llitenf1",		
                            "lcvicph1",		"lcvicve1",		"lcvicso1",	
                            "lcvicpr1",
                            "lcshypt1", "lcscont1", "lcspert1", "lcsanxt1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$lcqage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 16 #####
## Parent
Sel_Data[, c("plifev11",
             "plifev21",
             "plifev31",
             "plifev41",
             "plifev51",
             "plifev61",
             "plifev71",
             "plifev81",
             "plifev91",
             "plifev101",
             "plifev111",
             "plifev121",
             "plifev131",
             "plifev141",
             "plifev151",
             "plifev161",
             "plifev171",
             "plifev181",
             "plifev191",
             "plifev201","ppbhsdqhypt1", "ppbhsdqcont1", "ppbhsdqanxt1", "ppbhsdqpert1")] <- apply(
               Sel_Data[, c("plifev11",
                            "plifev21",
                            "plifev31",
                            "plifev41",
                            "plifev51",
                            "plifev61",
                            "plifev71",
                            "plifev81",
                            "plifev91",
                            "plifev101",
                            "plifev111",
                            "plifev121",
                            "plifev131",
                            "plifev141",
                            "plifev151",
                            "plifev161",
                            "plifev171",
                            "plifev181",
                            "plifev191",
                            "plifev201","ppbhsdqhypt1", "ppbhsdqcont1", "ppbhsdqanxt1", "ppbhsdqpert1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$ppbhage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("clifev11",
             "clifev21",
             "clifev31",
             "clifev41",
             "clifev51",
             "clifev61",
             "clifev71",
             "clifev81",
             "clifev91",
             "clifev101",
             "clifev111",
             "clifev121",
             "clifev131",
             "clifev141",
             "clifev151",
             "clifev161",
             "clifev171",
             "clifev181",
             "clifev191",
             "clifev201",
             "pcqastat1",		"pcqadata1",	
             "pcqaquan1",		"pcqasess1",		"pcqattt1",
             "pcqalifs011",		"pcqalifs021",	
             "pcqalifs02r1",		"pcqalifs031",		
             "pcqalifs041",		"pcqalifs051",		"pcqalifs061",
             "pcqalifs06r1",		"pcqalifs071",	
             "pcqalifs081",		"pcqalifs091",	
             "pcqalifs101",		"pcqalifs111",		"pcqalifs121",
             "pcqalifs131",		"pcqalifs141",	
             "pcqalifs151",		"pcqalifs161",		"pcqalifs16r1",
             "pcqalifs171",		"pcqalifs181",	
             "pcqalifs18r1",		"pcqalifs191",	
             "pcqalifs201",		"pcqalifs20r1",	
             "pcqalifs211",		"pcqaheye11",	
             "pcqaheye21",		"pcqaheye31",		"pcqaheye41",
             "pcqachaos11",		"pcqachaos1r1",	
             "pcqachaos21",		"pcqachaos31",		"pcqachaos41",
             "pcqachaos4r1",		"pcqachaos51",	
             "pcqachaos61",		"pcqachaos6r1",	
             "pcqascrtm11",		"pcqascrtm21",	
             "pcqascrtm31",		"pcqascrtm41",	
             "pcqascrtm51",		"pcqascrtm61",		"pcqascrtm71",
             "pcqascrtm81",		"pcqahopef11",	
             "pcqahopef21",		"pcqahopef31",		"pcqahopef41",
             "pcqahopef51",		"pcqahopef61",	
             "pcqagrat11",		"pcqagrat21",		"pcqagrat31",
             "pcqagrat3r1",		"pcqagrat41",	
             "pcqagrat51",		"pcqagrat61",		"pcqagrat6r1",
             "pcqacuri11",		"pcqacuri21",	
             "pcqacuri31",		"pcqacuri41",		"pcqacuri4r1",
             "pcqacuri51",		"pcqacuri61",	
             "pcqacuri71",		"pcqashs11",		"pcqashs21",
             "pcqashs31",		"pcqashs41",	
             "pcqashs4r1",		"pcqbstat1",		"pcqbdata1",
             "pcqbquan1",		"pcqbsess1",	
             "pcqbttt1",		"pcqbscen011",		"pcqbscen021",
             "pcqbscen031",		"pcqbscen041",	
             "pcqbscen051",		"pcqbscen061",		"pcqbscen071",
             "pcqbscen081",		"pcqbscen091",	
             "pcqbscen101",		"pcqbscen111",		"pcqbscen121",
             "pcqbscen131",		"pcqbscen141",	
             "pcqbscen151",		"pcqbscen161",		"pcqbscen171",
             "pcqbscen181",		"pcqbscen191",	
             "pcqbscen201",		"pcqbhmwk11",		"pcqbhmwk21",
             "pcqbhmwk2r1",		"pcqbhmwk31",	
             "pcqbhmwk41",		"pcqbhmwk51",		"pcqbhmwk61",
             "pcqbscrs11",		"pcqbscrs21",	
             "pcqbscrs31",		"pcqbscrs41",		"pcqbscrs51",
             "pcqbatsu11",		"pcqbatsu21",	
             "pcqbatsu31",		"pcqbstsc11",		"pcqbstsc1r1",
             "pcqbstsc21",		"pcqbstsc2r1",	
             "pcqbstsc31",		"pcqbstsc41",		"pcqbstsc51",
             "pcqbpevi11",		"pcqbpevi21",	
             "pcqbpevi31",		"pcqbpevi41",		"pcqbpevi51",
             "pcqbpevi61",		"pcqbpevi71",	
             "pcqbmase11",		"pcqbmase21",		"pcqbmase31",
             "pcqbmase41",		"pcqbmase51",	
             "pcqbmase61",		"pcqbmase71",		"pcqbmase81",
             "pcqbmain11",		"pcqbmain21",	
             "pcqbmain31",		"pcqbmatm11",		"pcqbmatm21",
             "pcqbmatm31",		"pcqbclen011",	
             "pcqbclen021",		"pcqbclen031",		"pcqbclen041",
             "pcqbclen051",		"pcqbclen061",	
             "pcqbclen071",		"pcqbclen081",		"pcqbclen091",
             "pcqbclen101",		"pcqbclen111",	
             "pcqbclen121",		"pcqbclen131",		"pcqbclen141",
             "pcqbclen151",		"pcqbclen161",	
             "pcqbclen171",		"pcqbclen181",		"pcqbclen191",
             "pcqbclen201",		"pcqcstat1",	
             "pcqcdata1",		"pcqcquan1",		"pcqcsess1",
             "pcqcttt1",		"pcqcpaco11",	
             "pcqcpaco21",		"pcqcpaco31",		"pcqcpaco41",
             "pcqcpaco51",		"pcqcpaco61",	
             "pcqcpaco71",		"pcqcpaco81",		"pcqcpaco91",
             "pcqcpamo11",		"pcqcpamo21",	
             "pcqcpamo31",		"pcqcpamo41",		"pcqcpamo51",
             "pcqcpamo61",		"pcqcpamo71",	
             "pcqcpadi11",		"pcqcpadi21",		"pcqcpadi31",
             "pcqcpadi41",		"pcqcpadi51",	
             "pcqcatta011",		"pcqcatta021",		"pcqcatta031",
             "pcqcatta041",		"pcqcatta04r1",	
             "pcqcatta051",		"pcqcatta05r1",	
             "pcqcatta061",		"pcqcatta071",	
             "pcqcatta081",		"pcqcatta091",	
             "pcqcatta09r1",		"pcqcatta101",		
             "pcqcatta10r1",		"pcqcatta111",		
             "pcqcatta11r1",		"pcqcatta121",		
             "pcqcatta131",		"pcqcatta141",		"pcqcatta151",
             "pcqcatta161",		"pcqcatta171",	
             "pcqcatta181",		"pcqcatta18r1",		
             "pcqcatta191",		"pcqcatta201",		"pcqcatta211",
             "pcqcatta221",		"pcqcatta22r1",	
             "pcqcatta231",		"pcqcatta23r1",	
             "pcqcatta241",		"pcqcatta251",	
             "pcqcatta261",		"pcqdstat1",	
             "pcqddata1",		"pcqdquan1",		"pcqdsess1",
             "pcqdttt1",		"pcqdseco011",	
             "pcqdseco021",		"pcqdseco031",		"pcqdseco041",
             "pcqdseco04r1",		"pcqdseco051",	
             "pcqdseco061",		"pcqdseco071",	
             "pcqdseco07r1",		"pcqdseco081",		
             "pcqdseco091",		"pcqdseco09r1",		
             "pcqdseco101",		"pcqdsees11",		"pcqdhtcm1",
             "pcqdwtkg1",		"pcqdpub011",	
             "pcqdpub021",		"pcqdpub031",		"pcqdpub041",
             "pcqdpub051",		"pcqdpub061",	
             "pcqdpub071",		"pcqdpub081",		"pcqdpub091",
             "pcqdpub101",		"pcqdpub111",	
             "pcpubm1",		"pcchatot1",		"pcattatrt1",
             "pcattacot1",		"pcattaalt1",	
             "pcattat1",		"pcpacom1",		"pcpamom1",
             "pcbhshsm1",		"pcsecom1",		"pcscentsrm1",
             "pcscencrsm1",		"pcscenpslm1",	
             "pcscenfagm1",		"pcscenm1",		"pcscenfslm1",
             "pchmwkbeht1",		"pchmwkfdbt1",	
             "pchmwkt1",		"pcatscm1",		"pcmaset1",
             "pcmainm1",		"pcmatmt1",		"pcpevit1",
             "pchopeagm1",		"pchopepam1",		"pchopem1",
             "pcgratm1",		"pccuriexm1",	
             "pccuriflm1",		"pccurim1",		"pcshsm1",
             "pcgritcoim1",		"pcgritperm1",	
             "pcgritm1",		"pcambim1", "pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1", "pcbhsdqcont1")] <- apply(
               Sel_Data[, c("clifev11",
                            "clifev21",
                            "clifev31",
                            "clifev41",
                            "clifev51",
                            "clifev61",
                            "clifev71",
                            "clifev81",
                            "clifev91",
                            "clifev101",
                            "clifev111",
                            "clifev121",
                            "clifev131",
                            "clifev141",
                            "clifev151",
                            "clifev161",
                            "clifev171",
                            "clifev181",
                            "clifev191",
                            "clifev201",
                            "pcqastat1",		"pcqadata1",	
                            "pcqaquan1",		"pcqasess1",		"pcqattt1",
                            "pcqalifs011",		"pcqalifs021",	
                            "pcqalifs02r1",		"pcqalifs031",		
                            "pcqalifs041",		"pcqalifs051",		"pcqalifs061",
                            "pcqalifs06r1",		"pcqalifs071",	
                            "pcqalifs081",		"pcqalifs091",	
                            "pcqalifs101",		"pcqalifs111",		"pcqalifs121",
                            "pcqalifs131",		"pcqalifs141",	
                            "pcqalifs151",		"pcqalifs161",		"pcqalifs16r1",
                            "pcqalifs171",		"pcqalifs181",	
                            "pcqalifs18r1",		"pcqalifs191",	
                            "pcqalifs201",		"pcqalifs20r1",	
                            "pcqalifs211",		"pcqaheye11",	
                            "pcqaheye21",		"pcqaheye31",		"pcqaheye41",
                            "pcqachaos11",		"pcqachaos1r1",	
                            "pcqachaos21",		"pcqachaos31",		"pcqachaos41",
                            "pcqachaos4r1",		"pcqachaos51",	
                            "pcqachaos61",		"pcqachaos6r1",	
                            "pcqascrtm11",		"pcqascrtm21",	
                            "pcqascrtm31",		"pcqascrtm41",	
                            "pcqascrtm51",		"pcqascrtm61",		"pcqascrtm71",
                            "pcqascrtm81",		"pcqahopef11",	
                            "pcqahopef21",		"pcqahopef31",		"pcqahopef41",
                            "pcqahopef51",		"pcqahopef61",	
                            "pcqagrat11",		"pcqagrat21",		"pcqagrat31",
                            "pcqagrat3r1",		"pcqagrat41",	
                            "pcqagrat51",		"pcqagrat61",		"pcqagrat6r1",
                            "pcqacuri11",		"pcqacuri21",	
                            "pcqacuri31",		"pcqacuri41",		"pcqacuri4r1",
                            "pcqacuri51",		"pcqacuri61",	
                            "pcqacuri71",		"pcqashs11",		"pcqashs21",
                            "pcqashs31",		"pcqashs41",	
                            "pcqashs4r1",		"pcqbstat1",		"pcqbdata1",
                            "pcqbquan1",		"pcqbsess1",	
                            "pcqbttt1",		"pcqbscen011",		"pcqbscen021",
                            "pcqbscen031",		"pcqbscen041",	
                            "pcqbscen051",		"pcqbscen061",		"pcqbscen071",
                            "pcqbscen081",		"pcqbscen091",	
                            "pcqbscen101",		"pcqbscen111",		"pcqbscen121",
                            "pcqbscen131",		"pcqbscen141",	
                            "pcqbscen151",		"pcqbscen161",		"pcqbscen171",
                            "pcqbscen181",		"pcqbscen191",	
                            "pcqbscen201",		"pcqbhmwk11",		"pcqbhmwk21",
                            "pcqbhmwk2r1",		"pcqbhmwk31",	
                            "pcqbhmwk41",		"pcqbhmwk51",		"pcqbhmwk61",
                            "pcqbscrs11",		"pcqbscrs21",	
                            "pcqbscrs31",		"pcqbscrs41",		"pcqbscrs51",
                            "pcqbatsu11",		"pcqbatsu21",	
                            "pcqbatsu31",		"pcqbstsc11",		"pcqbstsc1r1",
                            "pcqbstsc21",		"pcqbstsc2r1",	
                            "pcqbstsc31",		"pcqbstsc41",		"pcqbstsc51",
                            "pcqbpevi11",		"pcqbpevi21",	
                            "pcqbpevi31",		"pcqbpevi41",		"pcqbpevi51",
                            "pcqbpevi61",		"pcqbpevi71",	
                            "pcqbmase11",		"pcqbmase21",		"pcqbmase31",
                            "pcqbmase41",		"pcqbmase51",	
                            "pcqbmase61",		"pcqbmase71",		"pcqbmase81",
                            "pcqbmain11",		"pcqbmain21",	
                            "pcqbmain31",		"pcqbmatm11",		"pcqbmatm21",
                            "pcqbmatm31",		"pcqbclen011",	
                            "pcqbclen021",		"pcqbclen031",		"pcqbclen041",
                            "pcqbclen051",		"pcqbclen061",	
                            "pcqbclen071",		"pcqbclen081",		"pcqbclen091",
                            "pcqbclen101",		"pcqbclen111",	
                            "pcqbclen121",		"pcqbclen131",		"pcqbclen141",
                            "pcqbclen151",		"pcqbclen161",	
                            "pcqbclen171",		"pcqbclen181",		"pcqbclen191",
                            "pcqbclen201",		"pcqcstat1",	
                            "pcqcdata1",		"pcqcquan1",		"pcqcsess1",
                            "pcqcttt1",		"pcqcpaco11",	
                            "pcqcpaco21",		"pcqcpaco31",		"pcqcpaco41",
                            "pcqcpaco51",		"pcqcpaco61",	
                            "pcqcpaco71",		"pcqcpaco81",		"pcqcpaco91",
                            "pcqcpamo11",		"pcqcpamo21",	
                            "pcqcpamo31",		"pcqcpamo41",		"pcqcpamo51",
                            "pcqcpamo61",		"pcqcpamo71",	
                            "pcqcpadi11",		"pcqcpadi21",		"pcqcpadi31",
                            "pcqcpadi41",		"pcqcpadi51",	
                            "pcqcatta011",		"pcqcatta021",		"pcqcatta031",
                            "pcqcatta041",		"pcqcatta04r1",	
                            "pcqcatta051",		"pcqcatta05r1",	
                            "pcqcatta061",		"pcqcatta071",	
                            "pcqcatta081",		"pcqcatta091",	
                            "pcqcatta09r1",		"pcqcatta101",		
                            "pcqcatta10r1",		"pcqcatta111",		
                            "pcqcatta11r1",		"pcqcatta121",		
                            "pcqcatta131",		"pcqcatta141",		"pcqcatta151",
                            "pcqcatta161",		"pcqcatta171",	
                            "pcqcatta181",		"pcqcatta18r1",		
                            "pcqcatta191",		"pcqcatta201",		"pcqcatta211",
                            "pcqcatta221",		"pcqcatta22r1",	
                            "pcqcatta231",		"pcqcatta23r1",	
                            "pcqcatta241",		"pcqcatta251",	
                            "pcqcatta261",		"pcqdstat1",	
                            "pcqddata1",		"pcqdquan1",		"pcqdsess1",
                            "pcqdttt1",		"pcqdseco011",	
                            "pcqdseco021",		"pcqdseco031",		"pcqdseco041",
                            "pcqdseco04r1",		"pcqdseco051",	
                            "pcqdseco061",		"pcqdseco071",	
                            "pcqdseco07r1",		"pcqdseco081",		
                            "pcqdseco091",		"pcqdseco09r1",		
                            "pcqdseco101",		"pcqdsees11",		"pcqdhtcm1",
                            "pcqdwtkg1",		"pcqdpub011",	
                            "pcqdpub021",		"pcqdpub031",		"pcqdpub041",
                            "pcqdpub051",		"pcqdpub061",	
                            "pcqdpub071",		"pcqdpub081",		"pcqdpub091",
                            "pcqdpub101",		"pcqdpub111",	
                            "pcpubm1",		"pcchatot1",		"pcattatrt1",
                            "pcattacot1",		"pcattaalt1",	
                            "pcattat1",		"pcpacom1",		"pcpamom1",
                            "pcbhshsm1",		"pcsecom1",		"pcscentsrm1",
                            "pcscencrsm1",		"pcscenpslm1",	
                            "pcscenfagm1",		"pcscenm1",		"pcscenfslm1",
                            "pchmwkbeht1",		"pchmwkfdbt1",	
                            "pchmwkt1",		"pcatscm1",		"pcmaset1",
                            "pcmainm1",		"pcmatmt1",		"pcpevit1",
                            "pchopeagm1",		"pchopepam1",		"pchopem1",
                            "pcgratm1",		"pccuriexm1",	
                            "pccuriflm1",		"pccurim1",		"pcshsm1",
                            "pcgritcoim1",		"pcgritperm1",	
                            "pcgritm1",		"pcambim1", "pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1", "pcbhsdqcont1")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex1 + Sel_Data$pcbhage1,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 21 #####
## Parent
Sel_Data[, c("u1psdqanxt1", "u1psdqcont1", "u1psdqhypt1", "u1psdqpert1")] <- apply(
  Sel_Data[, c("u1psdqanxt1", "u1psdqcont1", "u1psdqhypt1", "u1psdqpert1")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ Sel_Data$sex1 + Sel_Data$u1page,
        na.action=na.exclude
      )
    )
  }	
)

## Child
Sel_Data[, c("u1csdqanxt1", "u1csdqcont1", "u1csdqhypt1", "u1csdqpert1")] <- apply(
  Sel_Data[, c("u1csdqanxt1", "u1csdqcont1", "u1csdqhypt1", "u1csdqpert1")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ Sel_Data$sex1 + Sel_Data$u1cage1,
        na.action=na.exclude
      )
    )
  }	
)


# ===== Age & sex regress; Twin 2 ===== 
##### Year 3 #####
Sel_Data[, c("ctfac12",    "ctfac22",			
             "cgfac12",		"cgfac22",		"cdfac12",	
             "cdfac22",		"cdfac32",		"cdisto2",	
             "cbfac12",		"cbfac22",		"cpafel2",	
             "cdismak2",		"cdishou2",		"cdiexpl2",	
             "cdifirm2",		"cdijoke2",		"cdiasko2",	
             "cbpimpa2",		"cbphapp2",		"cbpamus2",	
             "cbpaway2",		"cbpangr2",		"cbpclos2",	
             "cbpfrus2",		"ctrhym2",		"ctpron2",	
             "ctsent2",		"ctword2",		"ctloca2",	
             "ctbook2",		"cttalk2",		"cteat2",	
             "cgmessy2",		"cgpuzz2",		"cgmusi2",	
             "cgtapes2",		"cgbooks2",		"cgzoo2",	
             "cgphys2",		"cgboard2",		"chsusp2",	
             "chcatar2",		"chache2",		"chpus2",	
             "chmouth2",		"chsnore2",		"chignor2",	
             "chtalk2",		"cerisk2",		"cseengp2",
             "cseengx2",		"chosp2",		  "chospx2",
             "chospdd2",		"chospca2",		"chospcx2",
             "csurgry2",		"csurage2",		"csurgga2",
             "csurggx2",		"cslprob2",		"cslwake2",
             "cslnmar2",		"cslearl2",		"cpotty2",
             "cpnapda2",		"cpnapnt2",		"cpottrp2",
             "cpottrn2",		"cpdryda2",		"cpdrynt2",
             "cpnapp2",		"cpdrink2",		"cprward2",
             "cpparbd2",		"cpwake2",		"cplift2",
             "cpreass2",		"cplaygp2",		"cprocou2",
             "cproast2",		"cprofit2",		"cproeye2",
             "cproski2",		"cprosto2",		"cprovom2",
             "cprohd2",		"cprodia2",		"cprowk2",
             "cbhanxt2", "cbhcont2", "cbhhypt2")] <- apply(
               Sel_Data[, c("ctfac12",    "ctfac22",			
                            "cgfac12",		"cgfac22",		"cdfac12",	
                            "cdfac22",		"cdfac32",		"cdisto2",	
                            "cbfac12",		"cbfac22",		"cpafel2",	
                            "cdismak2",		"cdishou2",		"cdiexpl2",	
                            "cdifirm2",		"cdijoke2",		"cdiasko2",	
                            "cbpimpa2",		"cbphapp2",		"cbpamus2",	
                            "cbpaway2",		"cbpangr2",		"cbpclos2",	
                            "cbpfrus2",		"ctrhym2",		"ctpron2",	
                            "ctsent2",		"ctword2",		"ctloca2",	
                            "ctbook2",		"cttalk2",		"cteat2",	
                            "cgmessy2",		"cgpuzz2",		"cgmusi2",	
                            "cgtapes2",		"cgbooks2",		"cgzoo2",	
                            "cgphys2",		"cgboard2",		"chsusp2",	
                            "chcatar2",		"chache2",		"chpus2",	
                            "chmouth2",		"chsnore2",		"chignor2",	
                            "chtalk2",		"cerisk2",		"cseengp2",
                            "cseengx2",		"chosp2",		  "chospx2",
                            "chospdd2",		"chospca2",		"chospcx2",
                            "csurgry2",		"csurage2",		"csurgga2",
                            "csurggx2",		"cslprob2",		"cslwake2",
                            "cslnmar2",		"cslearl2",		"cpotty2",
                            "cpnapda2",		"cpnapnt2",		"cpottrp2",
                            "cpottrn2",		"cpdryda2",		"cpdrynt2",
                            "cpnapp2",		"cpdrink2",		"cprward2",
                            "cpparbd2",		"cpwake2",		"cplift2",
                            "cpreass2",		"cplaygp2",		"cprocou2",
                            "cproast2",		"cprofit2",		"cproeye2",
                            "cproski2",		"cprosto2",		"cprovom2",
                            "cprohd2",		"cprodia2",		"cprowk2",
                            "cbhanxt2", "cbhcont2", "cbhhypt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$crepage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 4 #####
Sel_Data[, c("dhsusp2",	"dhtalk2",	
             "dhaid2",	  "dhcatar2",	"dhache2",
             "dhpus2",	  "dhmouth2",	"dhsnore2",
             "dhignor2",	"dskitch2",	"dsktrt2",
             "dskface2",	"dskknee2",	"dskwris2",
             "dskoth2",	"dskage2",	"dskstil2",
             "dasthma2",	"dallerg2",	"dproeye2",
             "dprosto2",	"dprovom2",	"dprohd2",
             "dprodia2",	"dprowk2",	"dprofit2",
             "dseengp2",	"dseengx2",	"dbacmen2",
             "dbacmm2",	"dbacmp2",	"dbacms2",
             "dbacmh2",	"dbacmo2",	"dhosp2",
             "dhospx2",	"dhospdd2",	"dhospca2",
             "dhospcx2",	"dsurgry2",	"dsurage2",
             "dsurgga2",	"dapalon2",	"dapoth2",
             "dbedsh2",	"dslprob2",	"dslwake2",
             "dslnmar2",	"dslearl2",	"dpotty2",
             "dnapda2",	"dnapnt2",	"dpottrp2",
             "dpottrn2",	"dpdryda2",	"dpdrynt2",
             "dpnapp2",	"dpdrink2",	"dprward2",
             "dpparbd2",	"dpwake2",	"dplift2",
             "dpreass2",	"dsay012",	"dsay01a2",
             "dsay022",	"dsay032",	"dtrhym2",
             "dtpron2",	"dtsent2",	"dtword2",
             "dtloca2",	"dtbook2",	"dttalk2",
             "dteat2",	  "dgmessy2",	"dgpuzz2",
             "dgmusi2",	"dgtapes2",	"dgbooks2",
             "dgzoo2",	  "dgphys2",	"dgboard2",
             "ddismak2",	"ddishou2",	"ddiexpl2",
             "ddifirm2",	"ddijoke2",	"ddiasko2",
             "dbpimpa2",	"dbphapp2",	"dbpamus2",
             "dbpaway2",	"dbpangr2",	"dbpclos2",
             "dbpfrus2",	"dtfac12",	"dtfac22",
             "dgfac12",	"dgfac22",	"ddfac12",
             "ddfac22",	"ddfac32",	"ddisto2",
             "dbfac12",	"dbfac22",	"dpafel2",
             "derisk2",	"ddrawt2",	"dtoleft2",
             "dtorigh2",	"dhand2",	  "dhands2",
             "dlltalk2",	"dllvoc2",	"dllpic2",
             "dllslow2",	"dllang2",	"dplaygp2",
             "dtvoc2",
             "dsdanxt2", "dsdcont2", "dsdhypt2", "dsdpert2")] <- apply(
               Sel_Data[, c("dhsusp2",	"dhtalk2",	
                            "dhaid2",	  "dhcatar2",	"dhache2",
                            "dhpus2",	  "dhmouth2",	"dhsnore2",
                            "dhignor2",	"dskitch2",	"dsktrt2",
                            "dskface2",	"dskknee2",	"dskwris2",
                            "dskoth2",	"dskage2",	"dskstil2",
                            "dasthma2",	"dallerg2",	"dproeye2",
                            "dprosto2",	"dprovom2",	"dprohd2",
                            "dprodia2",	"dprowk2",	"dprofit2",
                            "dseengp2",	"dseengx2",	"dbacmen2",
                            "dbacmm2",	"dbacmp2",	"dbacms2",
                            "dbacmh2",	"dbacmo2",	"dhosp2",
                            "dhospx2",	"dhospdd2",	"dhospca2",
                            "dhospcx2",	"dsurgry2",	"dsurage2",
                            "dsurgga2",	"dapalon2",	"dapoth2",
                            "dbedsh2",	"dslprob2",	"dslwake2",
                            "dslnmar2",	"dslearl2",	"dpotty2",
                            "dnapda2",	"dnapnt2",	"dpottrp2",
                            "dpottrn2",	"dpdryda2",	"dpdrynt2",
                            "dpnapp2",	"dpdrink2",	"dprward2",
                            "dpparbd2",	"dpwake2",	"dplift2",
                            "dpreass2",	"dsay012",	"dsay01a2",
                            "dsay022",	"dsay032",	"dtrhym2",
                            "dtpron2",	"dtsent2",	"dtword2",
                            "dtloca2",	"dtbook2",	"dttalk2",
                            "dteat2",	  "dgmessy2",	"dgpuzz2",
                            "dgmusi2",	"dgtapes2",	"dgbooks2",
                            "dgzoo2",	  "dgphys2",	"dgboard2",
                            "ddismak2",	"ddishou2",	"ddiexpl2",
                            "ddifirm2",	"ddijoke2",	"ddiasko2",
                            "dbpimpa2",	"dbphapp2",	"dbpamus2",
                            "dbpaway2",	"dbpangr2",	"dbpclos2",
                            "dbpfrus2",	"dtfac12",	"dtfac22",
                            "dgfac12",	"dgfac22",	"ddfac12",
                            "ddfac22",	"ddfac32",	"ddisto2",
                            "dbfac12",	"dbfac22",	"dpafel2",
                            "derisk2",	"ddrawt2",	"dtoleft2",
                            "dtorigh2",	"dhand2",	  "dhands2",
                            "dlltalk2",	"dllvoc2",	"dllpic2",
                            "dllslow2",	"dllang2",	"dplaygp2",
                            "dtvoc2",
                            "dsdanxt2", "dsdcont2", "dsdhypt2", "dsdpert2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$drepage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 7 #####
## Parent
Sel_Data[, c("gillness2",		
             "gillcp2",		"gillcf2",		"gillaut2",	
             "gillds2",		"gildysp2",		"gillhyp2",	
             "gilasth2",	"gilepil2",		"gilloth2",	
             "gscomp2",		"gscskin2",		"gscasth2",	
             "gscalle2",	"gheardif2",	"gspchdif2",	
             "gspt2",		  "gsptage2",		"gspthl2",
             "gnoise2",		"gwets2",		  "ghtics2",
             "gfits2",		"ghospad2",		"ghospl2",
             "gsickf2",	
             "gsistom2",	"gsihead2",		"gsivomi2",	
             "gaches2",		"gacches2",		"gaclimb2",	
             "gacback2",	"gactire2",		"gacdizz2",	
             "gacothe2",	"gsymprob2",	"gwtkg2",	
             "ghtcm2",		"gdiexpl2",		"gdismak2",	
             "gdisend2",	"gdishou2",		"gdiigno2",	
             "gdiprai2",	"gbpimpa2",		"gbphapp2",	
             "gbpamus2",	"gbpfrus2",		"gbpleav2",	
             "gbpangr2",	"gbpclos2",		"gschool2",	
             "gscfeel2",	"glrndif2",		"gldaut2",
             "gldds2",		"gldrest2",		"gldhyp2",
             "glddysl2",	"gldwrit2",		"gldmot2",
             "gldmath2",	"gldoth2",		"gsen2",
             "gsenass2",	"gextsup2",		"ges1to12",
             "geshelp2",	"gespsyc2",		"gesiep2",
             "gesoth2",		"gschoff2",		"gscoffl2",
             "gdisp2",		 
             "gbfac12",		"gbfac22",
             "gpafel2",
             "gpshypt2", "gpscont2", "gpspert2", 
             "gpsanxt2")] <- apply(
               Sel_Data[, c("gillness2",		
                            "gillcp2",		"gillcf2",		"gillaut2",	
                            "gillds2",		"gildysp2",		"gillhyp2",	
                            "gilasth2",	"gilepil2",		"gilloth2",	
                            "gscomp2",		"gscskin2",		"gscasth2",	
                            "gscalle2",	"gheardif2",	"gspchdif2",	
                            "gspt2",		  "gsptage2",		"gspthl2",
                            "gnoise2",		"gwets2",		  "ghtics2",
                            "gfits2",		"ghospad2",		"ghospl2",
                            "gsickf2",	
                            "gsistom2",	"gsihead2",		"gsivomi2",	
                            "gaches2",		"gacches2",		"gaclimb2",	
                            "gacback2",	"gactire2",		"gacdizz2",	
                            "gacothe2",	"gsymprob2",	"gwtkg2",	
                            "ghtcm2",		"gdiexpl2",		"gdismak2",	
                            "gdisend2",	"gdishou2",		"gdiigno2",	
                            "gdiprai2",	"gbpimpa2",		"gbphapp2",	
                            "gbpamus2",	"gbpfrus2",		"gbpleav2",	
                            "gbpangr2",	"gbpclos2",		"gschool2",	
                            "gscfeel2",	"glrndif2",		"gldaut2",
                            "gldds2",		"gldrest2",		"gldhyp2",
                            "glddysl2",	"gldwrit2",		"gldmot2",
                            "gldmath2",	"gldoth2",		"gsen2",
                            "gsenass2",	"gextsup2",		"ges1to12",
                            "geshelp2",	"gespsyc2",		"gesiep2",
                            "gesoth2",		"gschoff2",		"gscoffl2",
                            "gdisp2",		 
                            "gbfac12",		"gbfac22",
                            "gpafel2",
                            "gpshypt2", "gpscont2", "gpspert2", 
                            "gpsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$gpbage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("gttype2",
             "gtknow2",		"gtsped2",		"gteff2",
             "gtbehav2",		"gtlearn2",		"gthappy2",
             "gtdiff2",		"gtdiffl2",		"gtdiffu2",
             "gtdiffp2",		"gtdiffc2",		"gtdiffb2","gtshypt2", "gtscont2", 
             "gtspert2", "gtsanxt2")] <- apply(
               Sel_Data[, c("gttype2",
                            "gtknow2",		"gtsped2",		"gteff2",
                            "gtbehav2",		"gtlearn2",		"gthappy2",
                            "gtdiff2",		"gtdiffl2",		"gtdiffu2",
                            "gtdiffp2",		"gtdiffc2",		"gtdiffb2","gtshypt2", "gtscont2", 
                            "gtspert2", "gtsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$gtqage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("gsim012",		"gsim022",			
             "gsim032",		"gsim042",		"gsim052",
             "gsim062",		"gsim072",		"gsim082",
             "gsim092",		"gsim102",		"gsim112",
             "gsim122",		"gsim132",		"gsimilt2",
             "gcfrnd12",		"gcfrnd22",		"gcfrnd32",
             "gcfrnd42",		"gcemo12",		"gcemo22",
             "gcemo32",		"gcemo42",		"gcemo52",
             "gcemo62",		"gcemo72")] <- apply(
               Sel_Data[, c("gsim012",		"gsim022",			
                            "gsim032",		"gsim042",		"gsim052",
                            "gsim062",		"gsim072",		"gsim082",
                            "gsim092",		"gsim102",		"gsim112",
                            "gsim122",		"gsim132",		"gsimilt2",
                            "gcfrnd12",		"gcfrnd22",		"gcfrnd32",
                            "gcfrnd42",		"gcemo12",		"gcemo22",
                            "gcemo32",		"gcemo42",		"gcemo52",
                            "gcemo62",		"gcemo72")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$gciage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 9 #####
## Parent
Sel_Data[, c("iphayfv2",	"ipnutal2",	"ipfooal2",
             "ipeczem2",	"ipasthm2",	"ipothal2",
             "iplndif2",	"ipldaut2",	"ipldasp2",
             "ipldrst2",	"ipldhyp2",	"iplddsl2",
             "ipldwri2",	"iplddsp2",	"ipldmth2",
             "ipldlan2",	"ipldhea2",	"ipldeye2",
             "ipldspl2",	"ipldfal2",	"ipsen2",
             "ipsenrg2",	"ipdisma2",	"ipdisho2",
             "ipdiexp2",	"ipdiexr2",	"ipdifir2",
             "ipdifrr2",	"ipdijok2",	"ipdiask2",
             "ipbpimp2",	"ipbphap2",	"ipbphar2",
             "ipbpamu2",	"ipbpamr2",	"ipbpalo2",
             "ipbpang2",	"ipbpclo2",	"ipbpclr2",
             "ipbpfru2",	"ipbpeat2",	"ipbpsle2",
             "iphom012",	"iphom022",	"iphom032",
             "iphom042",	"iphom052",	"iphom062",
             "iphom092",	"iphom102",	"iphom172",
             "iphom182",	"iphom192",	"iphom202",
             "iphom222",	"iphom232",	"ipcla012",
             "ipcla022",	"ipcla032",	"ipcla042",
             "ipcla052",	"ipcla062",	"ipcla072",
             "ipcla082",	"ipcla092",	"ipcla102",
             "ipcla112",	"ipcla122",	"ipcla132",
             "ipcla142",	"ipcla152",	"ipcla162",
             "ipcla172",	"ipcla182",	"ipcla192",
             "ipcla202",	"ipcla212",	"ipcla222",
             "ipcla232",	"ipcla242",	"ipcla252",
             "ipcla262",	"ipcla272",	"ipcla282",
             "ipcsoct2",	"ippafel2",	"ipdisto2",
             "ipcpeer2",	"ipcsati2",	"ipcteac2",	
             "ipcnega2",	"ipcadve2",
             "ipcoppo2",	"ipcacce2", "ipshypt2", "ipscont2", 
             "ipspert2", "ipsanxt2")] <- apply(
               Sel_Data[, c("iphayfv2",	"ipnutal2",	"ipfooal2",
                            "ipeczem2",	"ipasthm2",	"ipothal2",
                            "iplndif2",	"ipldaut2",	"ipldasp2",
                            "ipldrst2",	"ipldhyp2",	"iplddsl2",
                            "ipldwri2",	"iplddsp2",	"ipldmth2",
                            "ipldlan2",	"ipldhea2",	"ipldeye2",
                            "ipldspl2",	"ipldfal2",	"ipsen2",
                            "ipsenrg2",	"ipdisma2",	"ipdisho2",
                            "ipdiexp2",	"ipdiexr2",	"ipdifir2",
                            "ipdifrr2",	"ipdijok2",	"ipdiask2",
                            "ipbpimp2",	"ipbphap2",	"ipbphar2",
                            "ipbpamu2",	"ipbpamr2",	"ipbpalo2",
                            "ipbpang2",	"ipbpclo2",	"ipbpclr2",
                            "ipbpfru2",	"ipbpeat2",	"ipbpsle2",
                            "iphom012",	"iphom022",	"iphom032",
                            "iphom042",	"iphom052",	"iphom062",
                            "iphom092",	"iphom102",	"iphom172",
                            "iphom182",	"iphom192",	"iphom202",
                            "iphom222",	"iphom232",	"ipcla012",
                            "ipcla022",	"ipcla032",	"ipcla042",
                            "ipcla052",	"ipcla062",	"ipcla072",
                            "ipcla082",	"ipcla092",	"ipcla102",
                            "ipcla112",	"ipcla122",	"ipcla132",
                            "ipcla142",	"ipcla152",	"ipcla162",
                            "ipcla172",	"ipcla182",	"ipcla192",
                            "ipcla202",	"ipcla212",	"ipcla222",
                            "ipcla232",	"ipcla242",	"ipcla252",
                            "ipcla262",	"ipcla272",	"ipcla282",
                            "ipcsoct2",	"ippafel2",	"ipdisto2",
                            "ipcpeer2",	"ipcsati2",	"ipcteac2",	
                            "ipcnega2",	"ipcadve2",
                            "ipcoppo2",	"ipcacce2", "ipshypt2", "ipscont2", 
                            "ipspert2", "ipsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$icpage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("itcla12",	"itcla22",	"itcla32",
             "itcla42",	"itcla52",	"itcla62",
             "itcla72",	"itcla82",	"itclasz2",
             "itsenrg2",	"itsen2",	"itcpeer2",
             "itcsati2", "itshypt2", "itscont2", 
             "itspert2", "itsanxt2")] <- apply(
               Sel_Data[, c("itcla12",	"itcla22",	"itcla32",
                            "itcla42",	"itcla52",	"itcla62",
                            "itcla72",	"itcla82",	"itclasz2",
                            "itsenrg2",	"itsen2",	"itcpeer2",
                            "itcsati2", "itshypt2", "itscont2", 
                            "itspert2", "itsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$itage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("ichom012",	"ichom022",
             "ichom032",	"ichom042",	"ichom052",
             "ichom062",	"ichom072",	"ichom082",
             "ichom092",	"ichom102",	"ichom112",
             "icho11r2",	"ichom122",	"ichom132",
             "ichom142",	"icho14r2",	"ichom152",
             "ichom162",	"icho16r2",	"ichom172",
             "ichom182",	"ichom192",	"ichom202",
             "ichom212",	"ichom222",	"ichom232",
             "icdisma2",	"icdisho2",	"icdiexp2",
             "icdiexr2",	"icdifir2",	"icdifrr2",
             "icdijok2",	"icdiask2",	"ichom252",
             "ichom262",	"icho26r2",	"ichom272",
             "icho27r2",	"ichom282",	"ichom292",
             "ichom302",	"icho30r2",	"ichom312",
             "ichom322",	"ichom332",	"iccla012",
             "iccla022",	"iccla032",	"iccla042",
             "iccla052",	"iccla062",	"iccla072",
             "iccla082",	"iccla092",	"iccla102",
             "iccla112",	"iccla122",	"iccla132",
             "iccla142",	"iccla152",	"iccla162",
             "iccla172",	"iccla182",	"iccla192",
             "iccla202",	"iccla212",	"iccla222",
             "iccla232",	"iccla242",	"iccla252",
             "iccla262",	"iccla272",	"iccla282",
             "icchato2", "icpafel2",	"icdisto2",	
             "iccpeer2",	"iccsati2",	"iccteac2",
             "iccnega2",	"iccadve2",	"iccoppo2",
             "iccacce2",
             "icshypt2", "icscont2", "icsanxt2","icspert2")] <- apply(
               Sel_Data[, c("ichom012",	"ichom022",
                            "ichom032",	"ichom042",	"ichom052",
                            "ichom062",	"ichom072",	"ichom082",
                            "ichom092",	"ichom102",	"ichom112",
                            "icho11r2",	"ichom122",	"ichom132",
                            "ichom142",	"icho14r2",	"ichom152",
                            "ichom162",	"icho16r2",	"ichom172",
                            "ichom182",	"ichom192",	"ichom202",
                            "ichom212",	"ichom222",	"ichom232",
                            "icdisma2",	"icdisho2",	"icdiexp2",
                            "icdiexr2",	"icdifir2",	"icdifrr2",
                            "icdijok2",	"icdiask2",	"ichom252",
                            "ichom262",	"icho26r2",	"ichom272",
                            "icho27r2",	"ichom282",	"ichom292",
                            "ichom302",	"icho30r2",	"ichom312",
                            "ichom322",	"ichom332",	"iccla012",
                            "iccla022",	"iccla032",	"iccla042",
                            "iccla052",	"iccla062",	"iccla072",
                            "iccla082",	"iccla092",	"iccla102",
                            "iccla112",	"iccla122",	"iccla132",
                            "iccla142",	"iccla152",	"iccla162",
                            "iccla172",	"iccla182",	"iccla192",
                            "iccla202",	"iccla212",	"iccla222",
                            "iccla232",	"iccla242",	"iccla252",
                            "iccla262",	"iccla272",	"iccla282",
                            "icchato2", "icpafel2",	"icdisto2",	
                            "iccpeer2",	"iccsati2",	"iccteac2",
                            "iccnega2",	"iccadve2",	"iccoppo2",
                            "iccacce2",
                            "icshypt2", "icscont2", "icsanxt2","icspert2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$icpage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 12 #####
## Parent
Sel_Data[, c("lpdis012",
             "lpdis022",		"lpdis032",		"lpdis03r2",
             "lpdis042",		"lpdis04r2",		
             "lpdis052",		"lpdis062",		"lppar012",	
             "lppar022",		"lppar02r2",	"lppar032",	
             "lppar03r2",	"lppar042",		
             "lppar052",		"lppar062",		"lppar06r2",	
             "lppar072",		"lpdisto2",
             "lpshypt2", "lpscont2",  
             "lpspert2", "lpsanxt2")] <- apply(
               Sel_Data[, c("lpdis012",
                            "lpdis022",		"lpdis032",		"lpdis03r2",
                            "lpdis042",		"lpdis04r2",		
                            "lpdis052",		"lpdis062",		"lppar012",	
                            "lppar022",		"lppar02r2",	"lppar032",	
                            "lppar03r2",	"lppar042",		
                            "lppar052",		"lppar062",		"lppar06r2",	
                            "lppar072",		"lpdisto2",
                            "lpshypt2", "lpscont2",  
                            "lpspert2", "lpsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$lpqage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Teacher
Sel_Data[, c("ltsenrg2", 
             "ltsen2", "ltshypt2", "ltscont2", 
             "ltspert2", "ltsanxt2")] <- apply(
               Sel_Data[, c("ltsenrg2", 
                            "ltsen2", "ltshypt2", "ltscont2", 
                            "ltspert2", "ltsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$ltqage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("lccha12",		"lccha1r2",			
             "lccha22",		"lccha32",		"lccha42",
             "lccha4r2",		"lccha52",		"lccha62",
             "lccha6r2",		"lcvic012",		"lcvic022",
             "lcvic032",		"lcvic042",		"lcvic052",
             "lcvic062",		"lcvic072",		"lcvic082",
             "lcvic092",		"lcvic102",		"lcvic112",
             "lcvic122",		"lcvic132",		"lcvic142",
             "lcvic152",		"lcvic162",		"lcdis12",
             "lcdis22",		"lcdis32",		"lcdis3r2",
             "lcdis42",		"lcdis4r2",		"lcdis52",
             "lcdis62",		"lcpar12",		"lcpar22",
             "lcpar2r2",		"lcpar32",		"lcpar3r2",
             "lcpar42",		"lcpar52",		"lcpar62",
             "lcpar6r2",		"lcpar72",		"lcpub12",
             "lcpub22",		"lcpub32",		"lcsex2",
             "lcpub42",		"lcpub52",		"lcpub5age2",
             "lcpub62",		"lcpub72",		"lcchato2",		
             "lcpafel2",	"lcdisto2",	
             "lcmoaca2",		"lmatenv2",		"llitenv2",	
             "llitenx2",		"llitenf2",		
             "lcvicph2",		"lcvicve2",		"lcvicso2",	
             "lcvicpr2",
             "lcshypt2", "lcscont2", "lcspert2", "lcsanxt2")] <- apply(
               Sel_Data[, c("lccha12",		"lccha1r2",			
                            "lccha22",		"lccha32",		"lccha42",
                            "lccha4r2",		"lccha52",		"lccha62",
                            "lccha6r2",		"lcvic012",		"lcvic022",
                            "lcvic032",		"lcvic042",		"lcvic052",
                            "lcvic062",		"lcvic072",		"lcvic082",
                            "lcvic092",		"lcvic102",		"lcvic112",
                            "lcvic122",		"lcvic132",		"lcvic142",
                            "lcvic152",		"lcvic162",		"lcdis12",
                            "lcdis22",		"lcdis32",		"lcdis3r2",
                            "lcdis42",		"lcdis4r2",		"lcdis52",
                            "lcdis62",		"lcpar12",		"lcpar22",
                            "lcpar2r2",		"lcpar32",		"lcpar3r2",
                            "lcpar42",		"lcpar52",		"lcpar62",
                            "lcpar6r2",		"lcpar72",		"lcpub12",
                            "lcpub22",		"lcpub32",		"lcsex2",
                            "lcpub42",		"lcpub52",		"lcpub5age2",
                            "lcpub62",		"lcpub72",		"lcchato2",		
                            "lcpafel2",	"lcdisto2",	
                            "lcmoaca2",		"lmatenv2",		"llitenv2",	
                            "llitenx2",		"llitenf2",		
                            "lcvicph2",		"lcvicve2",		"lcvicso2",	
                            "lcvicpr2",
                            "lcshypt2", "lcscont2", "lcspert2", "lcsanxt2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$lcqage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )

##### Year 16 #####
## Parent
Sel_Data[, c("plifev12",
             "plifev22",
             "plifev32",
             "plifev42",
             "plifev52",
             "plifev62",
             "plifev72",
             "plifev82",
             "plifev92",
             "plifev102",
             "plifev112",
             "plifev122",
             "plifev132",
             "plifev142",
             "plifev152",
             "plifev162",
             "plifev172",
             "plifev182",
             "plifev192",
             "plifev202","ppbhsdqhypt2", "ppbhsdqcont2", "ppbhsdqanxt2", "ppbhsdqpert2")] <- apply(
               Sel_Data[, c("plifev12",
                            "plifev22",
                            "plifev32",
                            "plifev42",
                            "plifev52",
                            "plifev62",
                            "plifev72",
                            "plifev82",
                            "plifev92",
                            "plifev102",
                            "plifev112",
                            "plifev122",
                            "plifev132",
                            "plifev142",
                            "plifev152",
                            "plifev162",
                            "plifev172",
                            "plifev182",
                            "plifev192",
                            "plifev202","ppbhsdqhypt2", "ppbhsdqcont2", "ppbhsdqanxt2", "ppbhsdqpert2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$ppbhage,
                     na.action=na.exclude
                   )
                 )
               }	
             )

## Child
Sel_Data[, c("clifev12",
             "clifev22",
             "clifev32",
             "clifev42",
             "clifev52",
             "clifev62",
             "clifev72",
             "clifev82",
             "clifev92",
             "clifev102",
             "clifev112",
             "clifev122",
             "clifev132",
             "clifev142",
             "clifev152",
             "clifev162",
             "clifev172",
             "clifev182",
             "clifev192",
             "clifev202",
             "pcqastat2",		"pcqadata2",	
             "pcqaquan2",		"pcqasess2",		"pcqattt2",
             "pcqalifs012",		"pcqalifs022",	
             "pcqalifs02r2",		"pcqalifs032",		
             "pcqalifs042",		"pcqalifs052",		"pcqalifs062",
             "pcqalifs06r2",		"pcqalifs072",	
             "pcqalifs082",		"pcqalifs092",	
             "pcqalifs102",		"pcqalifs112",		"pcqalifs122",
             "pcqalifs132",		"pcqalifs142",	
             "pcqalifs152",		"pcqalifs162",		"pcqalifs16r2",
             "pcqalifs172",		"pcqalifs182",	
             "pcqalifs18r2",		"pcqalifs192",	
             "pcqalifs202",		"pcqalifs20r2",	
             "pcqalifs212",		"pcqaheye12",	
             "pcqaheye22",		"pcqaheye32",		"pcqaheye42",
             "pcqachaos12",		"pcqachaos1r2",	
             "pcqachaos22",		"pcqachaos32",		"pcqachaos42",
             "pcqachaos4r2",		"pcqachaos52",	
             "pcqachaos62",		"pcqachaos6r2",	
             "pcqascrtm12",		"pcqascrtm22",	
             "pcqascrtm32",		"pcqascrtm42",	
             "pcqascrtm52",		"pcqascrtm62",		"pcqascrtm72",
             "pcqascrtm82",		"pcqahopef12",	
             "pcqahopef22",		"pcqahopef32",		"pcqahopef42",
             "pcqahopef52",		"pcqahopef62",	
             "pcqagrat12",		"pcqagrat22",		"pcqagrat32",
             "pcqagrat3r2",		"pcqagrat42",	
             "pcqagrat52",		"pcqagrat62",		"pcqagrat6r2",
             "pcqacuri12",		"pcqacuri22",	
             "pcqacuri32",		"pcqacuri42",		"pcqacuri4r2",
             "pcqacuri52",		"pcqacuri62",	
             "pcqacuri72",		"pcqashs12",		"pcqashs22",
             "pcqashs32",		"pcqashs42",	
             "pcqashs4r2",		"pcqbstat2",		"pcqbdata2",
             "pcqbquan2",		"pcqbsess2",	
             "pcqbttt2",		"pcqbscen012",		"pcqbscen022",
             "pcqbscen032",		"pcqbscen042",	
             "pcqbscen052",		"pcqbscen062",		"pcqbscen072",
             "pcqbscen082",		"pcqbscen092",	
             "pcqbscen102",		"pcqbscen112",		"pcqbscen122",
             "pcqbscen132",		"pcqbscen142",	
             "pcqbscen152",		"pcqbscen162",		"pcqbscen172",
             "pcqbscen182",		"pcqbscen192",	
             "pcqbscen202",		"pcqbhmwk12",		"pcqbhmwk22",
             "pcqbhmwk2r2",		"pcqbhmwk32",	
             "pcqbhmwk42",		"pcqbhmwk52",		"pcqbhmwk62",
             "pcqbscrs12",		"pcqbscrs22",	
             "pcqbscrs32",		"pcqbscrs42",		"pcqbscrs52",
             "pcqbatsu12",		"pcqbatsu22",	
             "pcqbatsu32",		"pcqbstsc12",		"pcqbstsc1r2",
             "pcqbstsc22",		"pcqbstsc2r2",	
             "pcqbstsc32",		"pcqbstsc42",		"pcqbstsc52",
             "pcqbpevi12",		"pcqbpevi22",	
             "pcqbpevi32",		"pcqbpevi42",		"pcqbpevi52",
             "pcqbpevi62",		"pcqbpevi72",	
             "pcqbmase12",		"pcqbmase22",		"pcqbmase32",
             "pcqbmase42",		"pcqbmase52",	
             "pcqbmase62",		"pcqbmase72",		"pcqbmase82",
             "pcqbmain12",		"pcqbmain22",	
             "pcqbmain32",		"pcqbmatm12",		"pcqbmatm22",
             "pcqbmatm32",		"pcqbclen012",	
             "pcqbclen022",		"pcqbclen032",		"pcqbclen042",
             "pcqbclen052",		"pcqbclen062",	
             "pcqbclen072",		"pcqbclen082",		"pcqbclen092",
             "pcqbclen102",		"pcqbclen112",	
             "pcqbclen122",		"pcqbclen132",		"pcqbclen142",
             "pcqbclen152",		"pcqbclen162",	
             "pcqbclen172",		"pcqbclen182",		"pcqbclen192",
             "pcqbclen202",		"pcqcstat2",	
             "pcqcdata2",		"pcqcquan2",		"pcqcsess2",
             "pcqcttt2",		"pcqcpaco12",	
             "pcqcpaco22",		"pcqcpaco32",		"pcqcpaco42",
             "pcqcpaco52",		"pcqcpaco62",	
             "pcqcpaco72",		"pcqcpaco82",		"pcqcpaco92",
             "pcqcpamo12",		"pcqcpamo22",	
             "pcqcpamo32",		"pcqcpamo42",		"pcqcpamo52",
             "pcqcpamo62",		"pcqcpamo72",	
             "pcqcpadi12",		"pcqcpadi22",		"pcqcpadi32",
             "pcqcpadi42",		"pcqcpadi52",	
             "pcqcatta012",		"pcqcatta022",		"pcqcatta032",
             "pcqcatta042",		"pcqcatta04r2",	
             "pcqcatta052",		"pcqcatta05r2",	
             "pcqcatta062",		"pcqcatta072",	
             "pcqcatta082",		"pcqcatta092",	
             "pcqcatta09r2",		"pcqcatta102",		
             "pcqcatta10r2",		"pcqcatta112",		
             "pcqcatta11r2",		"pcqcatta122",		
             "pcqcatta132",		"pcqcatta142",		"pcqcatta152",
             "pcqcatta162",		"pcqcatta172",	
             "pcqcatta182",		"pcqcatta18r2",		
             "pcqcatta192",		"pcqcatta202",		"pcqcatta212",
             "pcqcatta222",		"pcqcatta22r2",	
             "pcqcatta232",		"pcqcatta23r2",	
             "pcqcatta242",		"pcqcatta252",	
             "pcqcatta262",		"pcqdstat2",	
             "pcqddata2",		"pcqdquan2",		"pcqdsess2",
             "pcqdttt2",		"pcqdseco012",	
             "pcqdseco022",		"pcqdseco032",		"pcqdseco042",
             "pcqdseco04r2",		"pcqdseco052",	
             "pcqdseco062",		"pcqdseco072",	
             "pcqdseco07r2",		"pcqdseco082",		
             "pcqdseco092",		"pcqdseco09r2",		
             "pcqdseco102",		"pcqdsees12",		"pcqdhtcm2",
             "pcqdwtkg2",		"pcqdpub012",	
             "pcqdpub022",		"pcqdpub032",		"pcqdpub042",
             "pcqdpub052",		"pcqdpub062",	
             "pcqdpub072",		"pcqdpub082",		"pcqdpub092",
             "pcqdpub102",		"pcqdpub112",	
             "pcpubm2",		"pcchatot2",		"pcattatrt2",
             "pcattacot2",		"pcattaalt2",	
             "pcattat2",		"pcpacom2",		"pcpamom2",
             "pcbhshsm2",		"pcsecom2",		"pcscentsrm2",
             "pcscencrsm2",		"pcscenpslm2",	
             "pcscenfagm2",		"pcscenm2",		"pcscenfslm2",
             "pchmwkbeht2",		"pchmwkfdbt2",	
             "pchmwkt2",		"pcatscm2",		"pcmaset2",
             "pcmainm2",		"pcmatmt2",		"pcpevit2",
             "pchopeagm2",		"pchopepam2",		"pchopem2",
             "pcgratm2",		"pccuriexm2",	
             "pccuriflm2",		"pccurim2",		"pcshsm2",
             "pcgritcoim2",		"pcgritperm2",	
             "pcgritm2",		"pcambim2", "pcbhsdqanxt2", "pcbhsdqpert2", "pcbhsdqhypt2", "pcbhsdqcont2")] <- apply(
               Sel_Data[, c("clifev12",
                            "clifev22",
                            "clifev32",
                            "clifev42",
                            "clifev52",
                            "clifev62",
                            "clifev72",
                            "clifev82",
                            "clifev92",
                            "clifev102",
                            "clifev112",
                            "clifev122",
                            "clifev132",
                            "clifev142",
                            "clifev152",
                            "clifev162",
                            "clifev172",
                            "clifev182",
                            "clifev192",
                            "clifev202",
                            "pcqastat2",		"pcqadata2",	
                            "pcqaquan2",		"pcqasess2",		"pcqattt2",
                            "pcqalifs012",		"pcqalifs022",	
                            "pcqalifs02r2",		"pcqalifs032",		
                            "pcqalifs042",		"pcqalifs052",		"pcqalifs062",
                            "pcqalifs06r2",		"pcqalifs072",	
                            "pcqalifs082",		"pcqalifs092",	
                            "pcqalifs102",		"pcqalifs112",		"pcqalifs122",
                            "pcqalifs132",		"pcqalifs142",	
                            "pcqalifs152",		"pcqalifs162",		"pcqalifs16r2",
                            "pcqalifs172",		"pcqalifs182",	
                            "pcqalifs18r2",		"pcqalifs192",	
                            "pcqalifs202",		"pcqalifs20r2",	
                            "pcqalifs212",		"pcqaheye12",	
                            "pcqaheye22",		"pcqaheye32",		"pcqaheye42",
                            "pcqachaos12",		"pcqachaos1r2",	
                            "pcqachaos22",		"pcqachaos32",		"pcqachaos42",
                            "pcqachaos4r2",		"pcqachaos52",	
                            "pcqachaos62",		"pcqachaos6r2",	
                            "pcqascrtm12",		"pcqascrtm22",	
                            "pcqascrtm32",		"pcqascrtm42",	
                            "pcqascrtm52",		"pcqascrtm62",		"pcqascrtm72",
                            "pcqascrtm82",		"pcqahopef12",	
                            "pcqahopef22",		"pcqahopef32",		"pcqahopef42",
                            "pcqahopef52",		"pcqahopef62",	
                            "pcqagrat12",		"pcqagrat22",		"pcqagrat32",
                            "pcqagrat3r2",		"pcqagrat42",	
                            "pcqagrat52",		"pcqagrat62",		"pcqagrat6r2",
                            "pcqacuri12",		"pcqacuri22",	
                            "pcqacuri32",		"pcqacuri42",		"pcqacuri4r2",
                            "pcqacuri52",		"pcqacuri62",	
                            "pcqacuri72",		"pcqashs12",		"pcqashs22",
                            "pcqashs32",		"pcqashs42",	
                            "pcqashs4r2",		"pcqbstat2",		"pcqbdata2",
                            "pcqbquan2",		"pcqbsess2",	
                            "pcqbttt2",		"pcqbscen012",		"pcqbscen022",
                            "pcqbscen032",		"pcqbscen042",	
                            "pcqbscen052",		"pcqbscen062",		"pcqbscen072",
                            "pcqbscen082",		"pcqbscen092",	
                            "pcqbscen102",		"pcqbscen112",		"pcqbscen122",
                            "pcqbscen132",		"pcqbscen142",	
                            "pcqbscen152",		"pcqbscen162",		"pcqbscen172",
                            "pcqbscen182",		"pcqbscen192",	
                            "pcqbscen202",		"pcqbhmwk12",		"pcqbhmwk22",
                            "pcqbhmwk2r2",		"pcqbhmwk32",	
                            "pcqbhmwk42",		"pcqbhmwk52",		"pcqbhmwk62",
                            "pcqbscrs12",		"pcqbscrs22",	
                            "pcqbscrs32",		"pcqbscrs42",		"pcqbscrs52",
                            "pcqbatsu12",		"pcqbatsu22",	
                            "pcqbatsu32",		"pcqbstsc12",		"pcqbstsc1r2",
                            "pcqbstsc22",		"pcqbstsc2r2",	
                            "pcqbstsc32",		"pcqbstsc42",		"pcqbstsc52",
                            "pcqbpevi12",		"pcqbpevi22",	
                            "pcqbpevi32",		"pcqbpevi42",		"pcqbpevi52",
                            "pcqbpevi62",		"pcqbpevi72",	
                            "pcqbmase12",		"pcqbmase22",		"pcqbmase32",
                            "pcqbmase42",		"pcqbmase52",	
                            "pcqbmase62",		"pcqbmase72",		"pcqbmase82",
                            "pcqbmain12",		"pcqbmain22",	
                            "pcqbmain32",		"pcqbmatm12",		"pcqbmatm22",
                            "pcqbmatm32",		"pcqbclen012",	
                            "pcqbclen022",		"pcqbclen032",		"pcqbclen042",
                            "pcqbclen052",		"pcqbclen062",	
                            "pcqbclen072",		"pcqbclen082",		"pcqbclen092",
                            "pcqbclen102",		"pcqbclen112",	
                            "pcqbclen122",		"pcqbclen132",		"pcqbclen142",
                            "pcqbclen152",		"pcqbclen162",	
                            "pcqbclen172",		"pcqbclen182",		"pcqbclen192",
                            "pcqbclen202",		"pcqcstat2",	
                            "pcqcdata2",		"pcqcquan2",		"pcqcsess2",
                            "pcqcttt2",		"pcqcpaco12",	
                            "pcqcpaco22",		"pcqcpaco32",		"pcqcpaco42",
                            "pcqcpaco52",		"pcqcpaco62",	
                            "pcqcpaco72",		"pcqcpaco82",		"pcqcpaco92",
                            "pcqcpamo12",		"pcqcpamo22",	
                            "pcqcpamo32",		"pcqcpamo42",		"pcqcpamo52",
                            "pcqcpamo62",		"pcqcpamo72",	
                            "pcqcpadi12",		"pcqcpadi22",		"pcqcpadi32",
                            "pcqcpadi42",		"pcqcpadi52",	
                            "pcqcatta012",		"pcqcatta022",		"pcqcatta032",
                            "pcqcatta042",		"pcqcatta04r2",	
                            "pcqcatta052",		"pcqcatta05r2",	
                            "pcqcatta062",		"pcqcatta072",	
                            "pcqcatta082",		"pcqcatta092",	
                            "pcqcatta09r2",		"pcqcatta102",		
                            "pcqcatta10r2",		"pcqcatta112",		
                            "pcqcatta11r2",		"pcqcatta122",		
                            "pcqcatta132",		"pcqcatta142",		"pcqcatta152",
                            "pcqcatta162",		"pcqcatta172",	
                            "pcqcatta182",		"pcqcatta18r2",		
                            "pcqcatta192",		"pcqcatta202",		"pcqcatta212",
                            "pcqcatta222",		"pcqcatta22r2",	
                            "pcqcatta232",		"pcqcatta23r2",	
                            "pcqcatta242",		"pcqcatta252",	
                            "pcqcatta262",		"pcqdstat2",	
                            "pcqddata2",		"pcqdquan2",		"pcqdsess2",
                            "pcqdttt2",		"pcqdseco012",	
                            "pcqdseco022",		"pcqdseco032",		"pcqdseco042",
                            "pcqdseco04r2",		"pcqdseco052",	
                            "pcqdseco062",		"pcqdseco072",	
                            "pcqdseco07r2",		"pcqdseco082",		
                            "pcqdseco092",		"pcqdseco09r2",		
                            "pcqdseco102",		"pcqdsees12",		"pcqdhtcm2",
                            "pcqdwtkg2",		"pcqdpub012",	
                            "pcqdpub022",		"pcqdpub032",		"pcqdpub042",
                            "pcqdpub052",		"pcqdpub062",	
                            "pcqdpub072",		"pcqdpub082",		"pcqdpub092",
                            "pcqdpub102",		"pcqdpub112",	
                            "pcpubm2",		"pcchatot2",		"pcattatrt2",
                            "pcattacot2",		"pcattaalt2",	
                            "pcattat2",		"pcpacom2",		"pcpamom2",
                            "pcbhshsm2",		"pcsecom2",		"pcscentsrm2",
                            "pcscencrsm2",		"pcscenpslm2",	
                            "pcscenfagm2",		"pcscenm2",		"pcscenfslm2",
                            "pchmwkbeht2",		"pchmwkfdbt2",	
                            "pchmwkt2",		"pcatscm2",		"pcmaset2",
                            "pcmainm2",		"pcmatmt2",		"pcpevit2",
                            "pchopeagm2",		"pchopepam2",		"pchopem2",
                            "pcgratm2",		"pccuriexm2",	
                            "pccuriflm2",		"pccurim2",		"pcshsm2",
                            "pcgritcoim2",		"pcgritperm2",	
                            "pcgritm2",		"pcambim2", "pcbhsdqanxt2", "pcbhsdqpert2", "pcbhsdqhypt2", "pcbhsdqcont2")],
               2,
               function(x){
                 rstandard(
                   lm(
                     x ~ Sel_Data$sex2 + Sel_Data$pcbhage2,
                     na.action=na.exclude
                   )
                 )
               }	
             )


##### Year 21 #####
## Parent
Sel_Data[, c("u1psdqanxt2", "u1psdqcont2", "u1psdqhypt2", "u1psdqpert2")] <- apply(
  Sel_Data[, c("u1psdqanxt2", "u1psdqcont2", "u1psdqhypt2", "u1psdqpert2")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ Sel_Data$sex2 + Sel_Data$u1page,
        na.action=na.exclude
      )
    )
  }	
)

## Child
Sel_Data[, c("u1csdqanxt2", "u1csdqcont2", "u1csdqhypt2", "u1csdqpert2")] <- apply(
  Sel_Data[, c("u1csdqanxt2", "u1csdqcont2", "u1csdqhypt2", "u1csdqpert2")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ Sel_Data$sex2 + Sel_Data$u1cage2,
        na.action=na.exclude
      )
    )
  }	
)



# ===== Write out age & sex regressed dataset ===== 
write.csv(Sel_Data, './data/NSEBP_agesexregressed_data.csv', row.names=T, quote=F)

