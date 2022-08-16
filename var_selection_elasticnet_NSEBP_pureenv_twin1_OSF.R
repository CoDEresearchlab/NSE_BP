###
# NSE BP variable selection parent NSE only (correlations & elastic net)
# Twin 1
# Aga
# 28/01/22
###

##set wd
setwd("~/Desktop/NSE BP/")

# ================================================================ TWIN 1: Analyses on the prepared data ======
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
Sel_Data <- read.csv("./data/NSEBP_agesexregressed_data.csv")
dim(Sel_Data) #13943  2370
names(Sel_Data)
labels(Sel_Data)
#describe (AllData)

# ================================================================ PRESCHOOL NSE & CHILDHOOD BP ======
# ===== NSE & BP vars in preschool =====
Variables_1contact <- Sel_Data[, c("aaloneh1",	"aralonh1",
                                   "ablen1",	  "arblen1",
                                   "arkidgr1",	"abrstfd1",
                                   "abotfed1",	"abotfdd1",
                                   "aspcare1",	"aspcard1",
                                   "ahospd1",	  "arhosp1",
                                   "aover1",	  "amprob1",
                                   "agenpro1",	"avispro1",
                                   "ahand1",	  "ahprob1",
                                   "ahcatar1",	"ahscrat1",
                                   "ahpus1",	  "ahmouth1",
                                   "atwmed1",	  "afussy1")]

Variables_3_NSE <- Sel_Data[, c("ctfac11",    "ctfac21",			
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
                                "cprohd1",		"cprodia1",		"cprowk1")]

Variables_3_BP <- Sel_Data[, c("cbhanxt1", "cbhcont1", "cbhhypt1")]

Variables_4_NSE <- Sel_Data[, c("dhsusp1",	"dhtalk1",	
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
                                "dtvoc1")]

Variables_4_BP <- Sel_Data[, c("dsdanxt1", "dsdcont1", "dsdhypt1", "dsdpert1")]

## Bind NSE and BP in preschool and scale
preschool_NSE <- cbind(Variables_1contact, Variables_3_NSE, Variables_4_NSE)
preschool_NSE <- scale(preschool_NSE)
preschool_NSE <- data.frame(preschool_NSE)
preschool_BP <- cbind(Variables_3_BP, Variables_4_BP)
preschool_BP <- scale(preschool_BP)
preschool_BP <- data.frame(preschool_BP)

## Create mean composites of BP
preschool_BP$preschool_hyp <- rowMeans(preschool_BP[, c(3,6)], na.rm = T)
preschool_BP$preschool_con <- rowMeans(preschool_BP[, c(2,5)], na.rm = T)
preschool_BP$preschool_ep <- rowMeans(preschool_BP[, c(1,4)], na.rm = T)
preschool_BP$preschool_pp <- preschool_BP$dsdpert1

## Add mean composites of BP to Sel_Data and scale
Sel_Data$preschool_hyp1 <- preschool_BP$preschool_hyp
Sel_Data$preschool_con1 <- preschool_BP$preschool_con
Sel_Data$preschool_ep1 <- preschool_BP$preschool_ep
Sel_Data$preschool_pp1 <- preschool_BP$preschool_pp

Sel_Data$preschool_hyp1 <- scale(Sel_Data$preschool_hyp1)
Sel_Data$preschool_con1 <- scale(Sel_Data$preschool_con1)
Sel_Data$preschool_ep1 <- scale(Sel_Data$preschool_ep1)
Sel_Data$preschool_pp1 <- scale(Sel_Data$preschool_ep1)

## Bind NSE and BP datasets
preschool_NSE_BP <- cbind(preschool_NSE, preschool_BP)

# ===== NSE & BP vars in childhood =====
## All raters
Variables_7_NSE <- Sel_Data[, c("gsim011",		"gsim021",			
                                "gsim031",		"gsim041",		"gsim051",
                                "gsim061",		"gsim071",		"gsim081",
                                "gsim091",		"gsim101",		"gsim111",
                                "gsim121",		"gsim131",		"gsimilt1",
                                "gillness1",		
                                "gillcp1",		"gillcf1",		"gillaut1",	
                                "gillds1",		"gildysp1",		"gillhyp1",	
                                "gilasth1",		"gilepil1",		"gilloth1",	
                                "gscomp1",		"gscskin1",		"gscasth1",	
                                "gscalle1",		"gheardif1",		"gspchdif1",	
                                "gspt1",		  "gsptage1",		"gspthl1",
                                "gnoise1",		"gwets1",		"ghtics1",
                                "gfits1",		  "ghospad1",		"ghospl1",
                                "gsickf1",	
                                "gsistom1",		"gsihead1",		"gsivomi1",	
                                "gaches1",		"gacches1",		"gaclimb1",	
                                "gacback1",		"gactire1",		"gacdizz1",	
                                "gacothe1",		"gsymprob1",		"gwtkg1",	
                                "ghtcm1",		  "gdiexpl1",		"gdismak1",	
                                "gdisend1",		"gdishou1",		"gdiigno1",	
                                "gdiprai1",		"gbpimpa1",		"gbphapp1",	
                                "gbpamus1",		"gbpfrus1",		"gbpleav1",	
                                "gbpangr1",		"gbpclos1",		"gschool1",	
                                "gscfeel1",		"glrndif1",		"gldaut1",
                                "gldds1",		  "gldrest1",		"gldhyp1",
                                "glddysl1",		"gldwrit1",		"gldmot1",
                                "gldmath1",		"gldoth1",		"gsen1",
                                "gsenass1",		"gextsup1",		"ges1to11",
                                "geshelp1",		"gespsyc1",		"gesiep1",
                                "gesoth1",		"gschoff1",		"gscoffl1",
                                "gcfrnd11",		"gcfrnd21",		"gcfrnd31",
                                "gcfrnd41",		"gcemo11",		"gcemo21",
                                "gcemo31",		"gcemo41",		"gcemo51",
                                "gcemo61",		"gcemo71",		"gttype1",
                                "gtknow1",		"gtsped1",		"gteff1",
                                "gtbehav1",		"gtlearn1",		"gthappy1",
                                "gtdiff1",		"gtdiffl1",		"gtdiffu1",
                                "gtdiffp1",		"gtdiffc1",		"gtdiffb1",
                                "gdisp1",		  "gbfac11",		"gbfac21",
                                "gpafel1")]

Variables_9_NSE <- Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
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
                                "ipcsoct1",	"ichom011",	"ichom021",
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
                                "itcla11",	"itcla21",	"itcla31",
                                "itcla41",	"itcla51",	"itcla61",
                                "itcla71",	"itcla81",	"itclasz1",
                                "itsenrg1",	"itsen1",	"icchato1",
                                "ippafel1",	"icpafel1",	"ipdisto1",
                                "icdisto1",	"ipcpeer1",	"itcpeer1",
                                "iccpeer1",	"ipcsati1",	"itcsati1",
                                "iccsati1",	"ipcteac1",	"iccteac1",
                                "ipcnega1",	"iccnega1",	"ipcadve1",
                                "iccadve1",	"ipcoppo1",	"iccoppo1",
                                "ipcacce1",	"iccacce1")]

Variables_7_BP <- Sel_Data[, c("gpshypt1", "gtshypt1", "gpscont1", "gtscont1", "gpspert1", 
                               "gtspert1", "gpsanxt1", "gtsanxt1")]

Variables_9_BP <- Sel_Data[, c("ipshypt1", "itshypt1", "icshypt1", "ipscont1", "itscont1", 
                               "icscont1", "ipspert1", "itspert1", "icspert1", "ipsanxt1", "itsanxt1", "icsanxt1")]

## Parent
Variables_7_NSE_parent <- Sel_Data[, c("gillness1",		
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
                                       "gpafel1")]

Variables_7_BP_parent <- Sel_Data[, c("gpshypt1", "gpscont1", "gpspert1", 
                                      "gpsanxt1")]

Variables_9_NSE_parent <- Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
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
                                       "ipcoppo1",	"ipcacce1")]

Variables_9_BP_parent <- Sel_Data[, c("ipshypt1", "ipscont1", 
                                      "ipspert1", "ipsanxt1")]

## Teacher
Variables_7_BP_teacher <- Sel_Data[, c("gtshypt1", "gtscont1", 
                                       "gtspert1", "gtsanxt1")]

Variables_9_BP_teacher <- Sel_Data[, c("itshypt1", "itscont1", 
                                       "itspert1", "itsanxt1")]

## Child
Variables_9_BP_child <- Sel_Data[, c("icshypt1", "icscont1", "icsanxt1", "icspert1")]

## Bind NSE and BP in childhood by rater and scale
# Parent
childhood_NSE_parent <- cbind(Variables_7_NSE_parent, Variables_9_NSE_parent)
childhood_NSE_parent <- scale(childhood_NSE_parent)
childhood_NSE_parent <- data.frame(childhood_NSE_parent)
childhood_BP_parent <- cbind(Variables_7_BP_parent, Variables_9_BP_parent)
childhood_BP_parent <- scale(childhood_BP_parent)
childhood_BP_parent <- data.frame(childhood_BP_parent)

# Teacher
childhood_BP_teacher <- cbind(Variables_7_BP_teacher, Variables_9_BP_teacher)
childhood_BP_teacher <- scale(childhood_BP_teacher)
childhood_BP_teacher <- data.frame(childhood_BP_teacher)

# Child
childhood_BP_child <- Variables_9_BP_child
childhood_BP_child <- scale(childhood_BP_child)
childhood_BP_child <- data.frame(childhood_BP_child)

## Create mean composites of BP
# Parent
childhood_BP_parent$childhood_hyp_parent <- rowMeans(childhood_BP_parent[, c(1,5)], na.rm = T)
childhood_BP_parent$childhood_con_parent <- rowMeans(childhood_BP_parent[, c(2,6)], na.rm = T)
childhood_BP_parent$childhood_ep_parent <- rowMeans(childhood_BP_parent[, c(4,8)], na.rm = T)
childhood_BP_parent$childhood_pp_parent <- rowMeans(childhood_BP_parent[, c(3,7)], na.rm = T)

# Teacher
childhood_BP_teacher$childhood_hyp_teacher <- rowMeans(childhood_BP_teacher[, c(1,5)], na.rm = T)
childhood_BP_teacher$childhood_con_teacher <- rowMeans(childhood_BP_teacher[, c(2,6)], na.rm = T)
childhood_BP_teacher$childhood_ep_teacher <- rowMeans(childhood_BP_teacher[, c(4,8)], na.rm = T)
childhood_BP_teacher$childhood_pp_teacher <- rowMeans(childhood_BP_teacher[, c(3,7)], na.rm = T)

# Child
childhood_BP_child$childhood_hyp_child <- childhood_BP_child$icshypt1
childhood_BP_child$childhood_con_child <- childhood_BP_child$icscont1
childhood_BP_child$childhood_ep_child <- childhood_BP_child$icsanxt1
childhood_BP_child$childhood_pp_child <- childhood_BP_child$icspert1

## Add mean composites of BP to Sel_Data and scale
# Parent
Sel_Data$childhood_hyp_parent1 <- childhood_BP_parent$childhood_hyp_parent
Sel_Data$childhood_con_parent1 <- childhood_BP_parent$childhood_con_parent
Sel_Data$childhood_ep_parent1 <- childhood_BP_parent$childhood_ep_parent
Sel_Data$childhood_pp_parent1 <- childhood_BP_parent$childhood_pp_parent

Sel_Data$childhood_hyp_parent1 <- scale(Sel_Data$childhood_hyp_parent1)
Sel_Data$childhood_con_parent1 <- scale(Sel_Data$childhood_con_parent1)
Sel_Data$childhood_ep_parent1 <- scale(Sel_Data$childhood_ep_parent1)
Sel_Data$childhood_pp_parent1 <- scale(Sel_Data$childhood_pp_parent1)

# Teacher
Sel_Data$childhood_hyp_teacher1 <- childhood_BP_teacher$childhood_hyp_teacher
Sel_Data$childhood_con_teacher1 <- childhood_BP_teacher$childhood_con_teacher
Sel_Data$childhood_ep_teacher1 <- childhood_BP_teacher$childhood_ep_teacher
Sel_Data$childhood_pp_teacher1 <- childhood_BP_teacher$childhood_pp_teacher

Sel_Data$childhood_hyp_teacher1 <- scale(Sel_Data$childhood_hyp_teacher1)
Sel_Data$childhood_con_teacher1 <- scale(Sel_Data$childhood_con_teacher1)
Sel_Data$childhood_ep_teacher1 <- scale(Sel_Data$childhood_ep_teacher1)
Sel_Data$childhood_pp_teacher1 <- scale(Sel_Data$childhood_pp_teacher1)

# Child
Sel_Data$childhood_hyp_child1 <- childhood_BP_child$childhood_hyp_child
Sel_Data$childhood_con_child1 <- childhood_BP_child$childhood_con_child
Sel_Data$childhood_ep_child1 <- childhood_BP_child$childhood_ep_child
Sel_Data$childhood_pp_child1 <- childhood_BP_child$childhood_pp_child

Sel_Data$childhood_hyp_child1 <- scale(Sel_Data$childhood_hyp_child1)
Sel_Data$childhood_con_child1 <- scale(Sel_Data$childhood_con_child1)
Sel_Data$childhood_ep_child1 <- scale(Sel_Data$childhood_ep_child1)
Sel_Data$childhood_pp_child1 <- scale(Sel_Data$childhood_pp_child1)

# ===== Bind NSE in preschool and BP in childhood datasets =====
## Preschool parent & childhood parent 
preschool_NSE_parent_childhood_BP_parent <- cbind(preschool_NSE, childhood_BP_parent)

## Preschool parent & childhood teacher 
preschool_NSE_parent_childhood_BP_teacher <- cbind(preschool_NSE, childhood_BP_teacher)

## Preschool parent & childhood child 
preschool_NSE_parent_childhood_BP_child <- cbind(preschool_NSE, childhood_BP_child)

# ===== Correlate NSE in preschool with BP in childhood =====
## Parent & parent
preschool_NSE_parent_childhood_BP_parent_matrix <- as.matrix(preschool_NSE_parent_childhood_BP_parent)
rcorr <- rcorr(preschool_NSE_parent_childhood_BP_parent_matrix)
preschool_NSE_parent_childhood_BP_parent_r <- rcorr$r

# ===== Plot correlations between NSE and BP in preschool ===== 
## Parent & parent
preschool_NSE_parent_childhood_BP_parent_r <- as.data.frame(preschool_NSE_parent_childhood_BP_parent_r)
preschool_NSE_parent_childhood_BP_parent_r$NSE <- rownames(preschool_NSE_parent_childhood_BP_parent_r)
rownames(preschool_NSE_parent_childhood_BP_parent_r) <- NULL
preschool_NSE_parent_childhood_BP_parent_r <- melt(preschool_NSE_parent_childhood_BP_parent_r); head(preschool_NSE_parent_childhood_BP_parent_r)

# Histogram
histogram(preschool_NSE_parent_childhood_BP_parent_r$value)

# Density plot
theme <-   theme(legend.direction = "vertical",
                 legend.position="none",
                 legend.text = element_text(size=15),
                 legend.title = element_blank(),
                 panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white",
                                                 size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "gray"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
                 axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
                 axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
                 #axis.text.y = element_blank(),
                 axis.title= element_text(size=15),
                 strip.text.x = element_text(size=15, angle = 0),
                 strip.text.y = element_text(size=15, angle = 0),
                 plot.title = element_text(size=15, angle = 0),
                 strip.background = element_rect(colour="black", fill="white"))

p <- ggplot(preschool_NSE_parent_childhood_BP_parent_r, aes(x=value))+
  geom_density(color="lightblue", fill="lightblue", alpha=1)+
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1, 1, by = 0.2))+
  #scale_y_continuous(limits = c(0,1))+
  theme+
  xlab("Correlations")+
  ylab("Density")

p

# ===== Explore NSE vars above threshold 0.2 ===== 
##### Parent & parent ##### 
preschool_NSE_parent_childhood_BP_parent_matrix <- as.matrix(preschool_NSE_parent_childhood_BP_parent)
rcorr <- rcorr(preschool_NSE_parent_childhood_BP_parent_matrix)
r <- rcorr$r
r <- round(r, digits = 2)
r <- as.data.frame(r)
r$NSE <- rownames(r)
rownames(r) <- NULL

## Hyperactivity
preschool_NSE_parent_childhood_hyp_parent <- r[r$childhood_hyp_parent > 0.20 | r$childhood_hyp_parent < -0.20,]
dput(preschool_NSE_parent_childhood_hyp_parent$NSE)
# "cdfac11", "cerisk1", "ddfac11", "derisk1"

MZ <- Sel_Data[Sel_Data$zygos == 1,]
corr.test(MZ$cdfac11, MZ$cdfac12) #0.76
corr.test(MZ$cerisk1, MZ$cerisk2) #0.95
corr.test(MZ$ddfac11, MZ$ddfac12) #0.76
corr.test(MZ$derisk1, MZ$derisk2) #0.95

## Conduct
preschool_NSE_parent_childhood_con_parent <- r[r$childhood_con_parent > 0.20 | r$childhood_con_parent < -0.20,]
dput(preschool_NSE_parent_childhood_con_parent$NSE)
# "cdfac11", "cdisto1", "cpafel1", "cdismak1", "cerisk1", 
# "ddfac11", "ddisto1", "dpafel1", "ddismak1", "derisk1", "ddishou1", "dbpangr1", "dbfac11"

## EP
preschool_NSE_parent_childhood_ep_parent <- r[r$childhood_ep_parent > 0.20 | r$childhood_ep_parent < -0.20,]
dput(preschool_NSE_parent_childhood_ep_parent$NSE)
#"cerisk1", "derisk1"

## PP
preschool_NSE_parent_childhood_pp_parent <- r[r$childhood_pp_parent > 0.20 | r$childhood_pp_parent < -0.20,]
dput(preschool_NSE_parent_childhood_pp_parent$NSE)
#"cerisk1", "derisk1"

# ===== Elastic net; Parent & parent ===== 
##### Hyperactivity #####
# Select vars and scale
dat <- Sel_Data[, c("childhood_hyp_parent1",
                    "cdfac11", "cerisk1", "ddfac11", "derisk1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #3955
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$childhood_hyp_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(childhood_hyp_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$childhood_hyp_parent1

# Build the model using the training set
model <- train(
  childhood_hyp_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_childhood_hyp_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_childhood_hyp_parent <- as.matrix(coef_childhood_hyp_parent)
write.csv(coef_childhood_hyp_parent, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_hyp_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(childhood_hyp_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_childhood_hyp_parent1 <- data.frame(FRCT = 1,
                                        var = "childhood_hyp_parent1",
                                        mean_cv_RMSE_train = mean(train_mod$RMSE),
                                        sd_cv_RMSE_train = sd(train_mod$RMSE),
                                        mean_cv_R2_train = mean(train_mod$Rsquared),
                                        sd_cv_R2_train = sd(train_mod$Rsquared),
                                        RMSE = RMSE(predictions, test.data$childhood_hyp_parent1),
                                        Rsquare = R2(predictions, test.data$childhood_hyp_parent1),
                                        alpha = model$bestTune$alpha,
                                        lambda = model$bestTune$lambda,
                                        N_train = dim(train.data)[-2],
                                        N_test = dim(test.data)[-2]
)

res_childhood_hyp_parent1
write.csv(res_childhood_hyp_parent1, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_hyp_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### Conduct #####
# Select vars and scale
dat <- Sel_Data[, c("childhood_con_parent1",
                    "cdfac11", "cdisto1", "cpafel1", "cdismak1", "cerisk1", 
                    "ddfac11", "ddisto1", "dpafel1", "ddismak1", "derisk1", "ddishou1", "dbpangr1", "dbfac11")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #3806
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$childhood_con_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(childhood_con_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$childhood_con_parent1

# Build the model using the training set
model <- train(
  childhood_con_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_childhood_con_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_childhood_con_parent <- as.matrix(coef_childhood_con_parent)
write.csv(coef_childhood_con_parent, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_con_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(childhood_con_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_childhood_con_parent1 <- data.frame(FRCT = 1,
                                        var = "childhood_con_parent1",
                                        mean_cv_RMSE_train = mean(train_mod$RMSE),
                                        sd_cv_RMSE_train = sd(train_mod$RMSE),
                                        mean_cv_R2_train = mean(train_mod$Rsquared),
                                        sd_cv_R2_train = sd(train_mod$Rsquared),
                                        RMSE = RMSE(predictions, test.data$childhood_con_parent1),
                                        Rsquare = R2(predictions, test.data$childhood_con_parent1),
                                        alpha = model$bestTune$alpha,
                                        lambda = model$bestTune$lambda,
                                        N_train = dim(train.data)[-2],
                                        N_test = dim(test.data)[-2]
)

res_childhood_con_parent1
write.csv(res_childhood_con_parent1, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_con_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### EP #####
# Select vars and scale
dat <- Sel_Data[, c("childhood_ep_parent1","cerisk1","derisk1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #3992
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$childhood_ep_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(childhood_ep_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$childhood_ep_parent1

# Build the model using the training set
model <- train(
  childhood_ep_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_childhood_ep_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_childhood_ep_parent <- as.matrix(coef_childhood_ep_parent)
write.csv(coef_childhood_ep_parent, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_ep_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(childhood_ep_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_childhood_ep_parent1 <- data.frame(FRCT = 1,
                                       var = "childhood_ep_parent1",
                                       mean_cv_RMSE_train = mean(train_mod$RMSE),
                                       sd_cv_RMSE_train = sd(train_mod$RMSE),
                                       mean_cv_R2_train = mean(train_mod$Rsquared),
                                       sd_cv_R2_train = sd(train_mod$Rsquared),
                                       RMSE = RMSE(predictions, test.data$childhood_ep_parent1),
                                       Rsquare = R2(predictions, test.data$childhood_ep_parent1),
                                       alpha = model$bestTune$alpha,
                                       lambda = model$bestTune$lambda,
                                       N_train = dim(train.data)[-2],
                                       N_test = dim(test.data)[-2]
)

res_childhood_ep_parent1
write.csv(res_childhood_ep_parent1, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_ep_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### PP #####
# Select vars and scale
dat <- Sel_Data[, c("childhood_pp_parent1",
                    "cerisk1", "derisk1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #3992
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$childhood_pp_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(childhood_pp_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$childhood_pp_parent1

# Build the model using the training set
model <- train(
  childhood_pp_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_childhood_pp_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_childhood_pp_parent <- as.matrix(coef_childhood_pp_parent)
write.csv(coef_childhood_pp_parent, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_pp_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(childhood_pp_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_childhood_pp_parent1 <- data.frame(FRCT = 1,
                                       var = "childhood_pp_parent1",
                                       mean_cv_RMSE_train = mean(train_mod$RMSE),
                                       sd_cv_RMSE_train = sd(train_mod$RMSE),
                                       mean_cv_R2_train = mean(train_mod$Rsquared),
                                       sd_cv_R2_train = sd(train_mod$Rsquared),
                                       RMSE = RMSE(predictions, test.data$childhood_pp_parent1),
                                       Rsquare = R2(predictions, test.data$childhood_pp_parent1),
                                       alpha = model$bestTune$alpha,
                                       lambda = model$bestTune$lambda,
                                       N_train = dim(train.data)[-2],
                                       N_test = dim(test.data)[-2]
)

res_childhood_pp_parent1
write.csv(res_childhood_pp_parent1, './Elastic net res/elasticnet_preschool_NSE_parent_childhood_pp_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

# ================================================================ CHILDHOOD NSE & ADOLESCENCE BP ======
# ===== NSE & BP vars in childhood =====
## All raters
Variables_7_NSE <- Sel_Data[, c("gsim011",		"gsim021",			
                                "gsim031",		"gsim041",		"gsim051",
                                "gsim061",		"gsim071",		"gsim081",
                                "gsim091",		"gsim101",		"gsim111",
                                "gsim121",		"gsim131",		"gsimilt1",
                                "gillness1",		
                                "gillcp1",		"gillcf1",		"gillaut1",	
                                "gillds1",		"gildysp1",		"gillhyp1",	
                                "gilasth1",		"gilepil1",		"gilloth1",	
                                "gscomp1",		"gscskin1",		"gscasth1",	
                                "gscalle1",		"gheardif1",		"gspchdif1",	
                                "gspt1",		  "gsptage1",		"gspthl1",
                                "gnoise1",		"gwets1",		"ghtics1",
                                "gfits1",		  "ghospad1",		"ghospl1",
                                "gsickf1",	
                                "gsistom1",		"gsihead1",		"gsivomi1",	
                                "gaches1",		"gacches1",		"gaclimb1",	
                                "gacback1",		"gactire1",		"gacdizz1",	
                                "gacothe1",		"gsymprob1",		"gwtkg1",	
                                "ghtcm1",		  "gdiexpl1",		"gdismak1",	
                                "gdisend1",		"gdishou1",		"gdiigno1",	
                                "gdiprai1",		"gbpimpa1",		"gbphapp1",	
                                "gbpamus1",		"gbpfrus1",		"gbpleav1",	
                                "gbpangr1",		"gbpclos1",		"gschool1",	
                                "gscfeel1",		"glrndif1",		"gldaut1",
                                "gldds1",		  "gldrest1",		"gldhyp1",
                                "glddysl1",		"gldwrit1",		"gldmot1",
                                "gldmath1",		"gldoth1",		"gsen1",
                                "gsenass1",		"gextsup1",		"ges1to11",
                                "geshelp1",		"gespsyc1",		"gesiep1",
                                "gesoth1",		"gschoff1",		"gscoffl1",
                                "gcfrnd11",		"gcfrnd21",		"gcfrnd31",
                                "gcfrnd41",		"gcemo11",		"gcemo21",
                                "gcemo31",		"gcemo41",		"gcemo51",
                                "gcemo61",		"gcemo71",		"gttype1",
                                "gtknow1",		"gtsped1",		"gteff1",
                                "gtbehav1",		"gtlearn1",		"gthappy1",
                                "gtdiff1",		"gtdiffl1",		"gtdiffu1",
                                "gtdiffp1",		"gtdiffc1",		"gtdiffb1",
                                "gdisp1",		  "gbfac11",		"gbfac21",
                                "gpafel1")]

Variables_9_NSE <- Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
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
                                "ipcsoct1",	"ichom011",	"ichom021",
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
                                "itcla11",	"itcla21",	"itcla31",
                                "itcla41",	"itcla51",	"itcla61",
                                "itcla71",	"itcla81",	"itclasz1",
                                "itsenrg1",	"itsen1",	"icchato1",
                                "ippafel1",	"icpafel1",	"ipdisto1",
                                "icdisto1",	"ipcpeer1",	"itcpeer1",
                                "iccpeer1",	"ipcsati1",	"itcsati1",
                                "iccsati1",	"ipcteac1",	"iccteac1",
                                "ipcnega1",	"iccnega1",	"ipcadve1",
                                "iccadve1",	"ipcoppo1",	"iccoppo1",
                                "ipcacce1",	"iccacce1")]

Variables_7_BP <- Sel_Data[, c("gpshypt1", "gtshypt1", "gpscont1", "gtscont1", "gpspert1", 
                               "gtspert1", "gpsanxt1", "gtsanxt1")]

Variables_9_BP <- Sel_Data[, c("ipshypt1", "itshypt1", "icshypt1", "ipscont1", "itscont1", 
                               "icscont1", "ipspert1", "itspert1", "ipsanxt1", "itsanxt1", "icsanxt1")]

## Parent
Variables_7_NSE_parent <- Sel_Data[, c("gillness1",		
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
                                       "gpafel1")]

Variables_7_BP_parent <- Sel_Data[, c("gpshypt1", "gpscont1", "gpspert1", 
                                      "gpsanxt1")]

Variables_9_NSE_parent <- Sel_Data[, c("iphayfv1",	"ipnutal1",	"ipfooal1",
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
                                       "ipcoppo1",	"ipcacce1")]

Variables_9_BP_parent <- Sel_Data[, c("ipshypt1", "ipscont1", 
                                      "ipspert1", "ipsanxt1")]

## Teacher
Variables_7_BP_teacher <- Sel_Data[, c("gtshypt1", "gtscont1", 
                                       "gtspert1", "gtsanxt1")]

Variables_9_BP_teacher <- Sel_Data[, c("itshypt1", "itscont1", 
                                       "itspert1", "itsanxt1")]

## Child
Variables_9_BP_child <- Sel_Data[, c("icshypt1", "icscont1", "icsanxt1")]

## Bind NSE and BP in childhood by rater and scale
# Parent
childhood_NSE_parent <- cbind(Variables_7_NSE_parent, Variables_9_NSE_parent)
childhood_NSE_parent <- scale(childhood_NSE_parent)
childhood_NSE_parent <- data.frame(childhood_NSE_parent)
childhood_BP_parent <- cbind(Variables_7_BP_parent, Variables_9_BP_parent)
childhood_BP_parent <- scale(childhood_BP_parent)
childhood_BP_parent <- data.frame(childhood_BP_parent)

# Teacher
childhood_BP_teacher <- cbind(Variables_7_BP_teacher, Variables_9_BP_teacher)
childhood_BP_teacher <- scale(childhood_BP_teacher)
childhood_BP_teacher <- data.frame(childhood_BP_teacher)

# Child
childhood_BP_child <- Variables_9_BP_child
childhood_BP_child <- scale(childhood_BP_child)
childhood_BP_child <- data.frame(childhood_BP_child)

## Create mean composites of BP
# Parent
childhood_BP_parent$childhood_hyp_parent <- rowMeans(childhood_BP_parent[, c(1,5)], na.rm = T)
childhood_BP_parent$childhood_con_parent <- rowMeans(childhood_BP_parent[, c(2,6)], na.rm = T)
childhood_BP_parent$childhood_ep_parent <- rowMeans(childhood_BP_parent[, c(4,8)], na.rm = T)
childhood_BP_parent$childhood_pp_parent <- rowMeans(childhood_BP_parent[, c(3,7)], na.rm = T)

# Teacher
childhood_BP_teacher$childhood_hyp_teacher <- rowMeans(childhood_BP_teacher[, c(1,5)], na.rm = T)
childhood_BP_teacher$childhood_con_teacher <- rowMeans(childhood_BP_teacher[, c(2,6)], na.rm = T)
childhood_BP_teacher$childhood_ep_teacher <- rowMeans(childhood_BP_teacher[, c(4,8)], na.rm = T)
childhood_BP_teacher$childhood_pp_teacher <- rowMeans(childhood_BP_teacher[, c(3,7)], na.rm = T)

# Child
childhood_BP_child$childhood_hyp_child <- childhood_BP_child$icshypt1
childhood_BP_child$childhood_con_child <- childhood_BP_child$icscont1
childhood_BP_child$childhood_ep_child <- childhood_BP_child$icsanxt1

## Add mean composites of BP to Sel_Data and scale
# Parent
Sel_Data$childhood_hyp_parent1 <- childhood_BP_parent$childhood_hyp_parent
Sel_Data$childhood_con_parent1 <- childhood_BP_parent$childhood_con_parent
Sel_Data$childhood_ep_parent1 <- childhood_BP_parent$childhood_ep_parent
Sel_Data$childhood_pp_parent1 <- childhood_BP_parent$childhood_pp_parent

Sel_Data$childhood_hyp_parent1 <- scale(Sel_Data$childhood_hyp_parent1)
Sel_Data$childhood_con_parent1 <- scale(Sel_Data$childhood_con_parent1)
Sel_Data$childhood_ep_parent1 <- scale(Sel_Data$childhood_ep_parent1)
Sel_Data$childhood_pp_parent1 <- scale(Sel_Data$childhood_pp_parent1)

# Teacher
Sel_Data$childhood_hyp_teacher1 <- childhood_BP_teacher$childhood_hyp_teacher
Sel_Data$childhood_con_teacher1 <- childhood_BP_teacher$childhood_con_teacher
Sel_Data$childhood_ep_teacher1 <- childhood_BP_teacher$childhood_ep_teacher
Sel_Data$childhood_pp_teacher1 <- childhood_BP_teacher$childhood_pp_teacher

Sel_Data$childhood_hyp_teacher1 <- scale(Sel_Data$childhood_hyp_teacher1)
Sel_Data$childhood_con_teacher1 <- scale(Sel_Data$childhood_con_teacher1)
Sel_Data$childhood_ep_teacher1 <- scale(Sel_Data$childhood_ep_teacher1)
Sel_Data$childhood_pp_teacher1 <- scale(Sel_Data$childhood_pp_teacher1)

# Child
Sel_Data$childhood_hyp_child1 <- childhood_BP_child$childhood_hyp_child
Sel_Data$childhood_con_child1 <- childhood_BP_child$childhood_con_child
Sel_Data$childhood_ep_child1 <- childhood_BP_child$childhood_ep_child
#Sel_Data$childhood_pp_child1 <- childhood_BP_child$childhood_pp_child

Sel_Data$childhood_hyp_child1 <- scale(Sel_Data$childhood_hyp_child1)
Sel_Data$childhood_con_child1 <- scale(Sel_Data$childhood_con_child1)
Sel_Data$childhood_ep_child1 <- scale(Sel_Data$childhood_ep_child1)
#Sel_Data$childhood_pp_child1 <- scale(Sel_Data$childhood_pp_child1)

# ===== NSE & BP vars in adolescence =====
## All raters
Variables_12_NSE <- Sel_Data[, c("lccha11",		"lccha1r1",			
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
                                 "lcpub61",		"lcpub71",		"lpdis011",
                                 "lpdis021",		"lpdis031",		"lpdis03r1",
                                 "lpdis041",		"lpdis04r1",		
                                 "lpdis051",		"lpdis061",		"lppar011",	
                                 "lppar021",		"lppar02r1",	"lppar031",	
                                 "lppar03r1",	"lppar041",		
                                 "lppar051",		"lppar061",		"lppar06r1",	
                                 "lppar071",		"ltsenrg1",		
                                 "ltsen1",		  "lcchato1",		"lppafel1",	
                                 "lcpafel1",		"lpdisto1",		"lcdisto1",	
                                 "lcmoaca1",		"lmatenv1",		"llitenv1",	
                                 "llitenx1",		"llitenf1",		"lpubtot1",	
                                 "lcvicph1",		"lcvicve1",		"lcvicso1",	
                                 "lcvicpr1")]

Variables_16_NSE <- Sel_Data[, c("ppbhlfev01a1",	"ppbhlfev01b1",			
                                 "ppbhlfev02a1",	"ppbhlfev02b1",		
                                 "ppbhlfev03a1",	"ppbhlfev03b1",		
                                 "ppbhlfev04a1",	"ppbhlfev04b1",		
                                 "ppbhlfev05a1",	"ppbhlfev05b1",		
                                 "ppbhlfev06a1",	"ppbhlfev06b1",		
                                 "ppbhlfev07a1",	"ppbhlfev07b1",		
                                 "ppbhlfev08a1",	"ppbhlfev08b1",		
                                 "ppbhlfev09a1",	"ppbhlfev09b1",		
                                 "ppbhlfev10a1",	"ppbhlfev10b1",		
                                 "ppbhlfev11a1",	"ppbhlfev11b1",		
                                 "ppbhlfev12a1",	"ppbhlfev12b1",		
                                 "ppbhlfev13a1",	"ppbhlfev13b1",		
                                 "ppbhlfev14a1",	"ppbhlfev14b1",		
                                 "ppbhlfev15a1",	"ppbhlfev15b1",		
                                 "ppbhlfev16a1",	"ppbhlfev16b1",		
                                 "ppbhlfev17a1",	"ppbhlfev17b1",		
                                 "ppbhlfev18a1",	"ppbhlfev18b1",		
                                 "ppbhlfev19a1",	"ppbhlfev19b1",		
                                 "ppbhlfev20a1",	"ppbhlfev20b1",		
                                 "pcbhlfev01a1",		"pcbhlfev01b1",	
                                 "pcbhlfev02a1",		"pcbhlfev02b1",	
                                 "pcbhlfev03a1",		"pcbhlfev03b1",	
                                 "pcbhlfev04a1",		"pcbhlfev04b1",	
                                 "pcbhlfev05a1",		"pcbhlfev05b1",	
                                 "pcbhlfev06a1",		"pcbhlfev06b1",	
                                 "pcbhlfev07a1",		"pcbhlfev07b1",	
                                 "pcbhlfev08a1",		"pcbhlfev08b1",	
                                 "pcbhlfev09a1",		"pcbhlfev09b1",	
                                 "pcbhlfev10a1",		"pcbhlfev10b1",	
                                 "pcbhlfev11a1",		"pcbhlfev11b1",	
                                 "pcbhlfev12a1",		"pcbhlfev12b1",	
                                 "pcbhlfev13a1",		"pcbhlfev13b1",	
                                 "pcbhlfev14a1",		"pcbhlfev14b1",	
                                 "pcbhlfev15a1",		"pcbhlfev15b1",	
                                 "pcbhlfev16a1",		"pcbhlfev16b1",	
                                 "pcbhlfev17a1",		"pcbhlfev17b1",	
                                 "pcbhlfev18a1",		"pcbhlfev18b1",	
                                 "pcbhlfev19a1",		"pcbhlfev19b1",	
                                 "pcbhlfev20a1",		"pcbhlfev20b1",	
                                 "pcl2lfev01a1",		"pcl2lfev01b1",	
                                 "pcl2lfev02a1",		"pcl2lfev02b1",	
                                 "pcl2lfev03a1",		"pcl2lfev03b1",	
                                 "pcl2lfev04a1",		"pcl2lfev04b1",	
                                 "pcl2lfev05a1",		"pcl2lfev05b1",	
                                 "pcl2lfev06a1",		"pcl2lfev06b1",	
                                 "pcl2lfev07a1",		"pcl2lfev07b1",	
                                 "pcl2lfev08a1",		"pcl2lfev08b1",	
                                 "pcl2lfev09a1",		"pcl2lfev09b1",	
                                 "pcl2lfev10a1",		"pcl2lfev10b1",	
                                 "pcl2lfev11a1",		"pcl2lfev11b1",	
                                 "pcl2lfev12a1",		"pcl2lfev12b1",	
                                 "pcl2lfev13a1",		"pcl2lfev13b1",	
                                 "pcl2lfev14a1",		"pcl2lfev14b1",	
                                 "pcl2lfev15a1",		"pcl2lfev15b1",	
                                 "pcl2lfev16a1",		"pcl2lfev16b1",	
                                 "pcl2lfev17a1",		"pcl2lfev17b1",	
                                 "pcl2lfev18a1",		"pcl2lfev18b1",	
                                 "pcl2lfev19a1",		"pcl2lfev19b1",	
                                 "pcl2lfev20a1",		"pcl2lfev20b1",	
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
                                 "pcgritm1",		"pcambim1")]

Variables_12_BP <- Sel_Data[, c("lpshypt1", "ltshypt1", "lcshypt1", "lpscont1", "ltscont1", 
                                "lcscont1", "lpspert1", "ltspert1", "lcspert1", "lpsanxt1", "ltsanxt1", 
                                "lcsanxt1")]

Variables_16_BP <- Sel_Data[, c("pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1",
                                "ppbhsdqhypt1", "pcbhsdqcont1", "ppbhsdqcont1")]

## Parent
Variables_12_NSE_parent <- Sel_Data[, c("lpdis011",
                                        "lpdis021",		"lpdis031",		"lpdis03r1",
                                        "lpdis041",		"lpdis04r1",		
                                        "lpdis051",		"lpdis061",		"lppar011",	
                                        "lppar021",		"lppar02r1",	"lppar031",	
                                        "lppar03r1",	"lppar041",		
                                        "lppar051",		"lppar061",		"lppar06r1",	
                                        "lppar071",		"lpdisto1")]

Variables_12_BP_parent <- Sel_Data[, c("lpshypt1", "lpscont1",  
                                       "lpspert1", "lpsanxt1")]

Variables_16_NSE_parent <- Sel_Data[, c("ppbhlfev01a1",	"ppbhlfev01b1",			
                                        "ppbhlfev02a1",	"ppbhlfev02b1",		
                                        "ppbhlfev03a1",	"ppbhlfev03b1",		
                                        "ppbhlfev04a1",	"ppbhlfev04b1",		
                                        "ppbhlfev05a1",	"ppbhlfev05b1",		
                                        "ppbhlfev06a1",	"ppbhlfev06b1",		
                                        "ppbhlfev07a1",	"ppbhlfev07b1",		
                                        "ppbhlfev08a1",	"ppbhlfev08b1",		
                                        "ppbhlfev09a1",	"ppbhlfev09b1",		
                                        "ppbhlfev10a1",	"ppbhlfev10b1",		
                                        "ppbhlfev11a1",	"ppbhlfev11b1",		
                                        "ppbhlfev12a1",	"ppbhlfev12b1",		
                                        "ppbhlfev13a1",	"ppbhlfev13b1",		
                                        "ppbhlfev14a1",	"ppbhlfev14b1",		
                                        "ppbhlfev15a1",	"ppbhlfev15b1",		
                                        "ppbhlfev16a1",	"ppbhlfev16b1",		
                                        "ppbhlfev17a1",	"ppbhlfev17b1",		
                                        "ppbhlfev18a1",	"ppbhlfev18b1",		
                                        "ppbhlfev19a1",	"ppbhlfev19b1",		
                                        "ppbhlfev20a1",	"ppbhlfev20b1")]

Variables_16_BP_parent <- Sel_Data[, c("ppbhsdqhypt1", "ppbhsdqcont1")]

## Teacher
Variables_12_BP_teacher <- Sel_Data[, c("ltshypt1", "ltscont1", 
                                        "ltspert1", "ltsanxt1")]

## Child
Variables_12_BP_child <- Sel_Data[, c("lcshypt1", "lcscont1", "lcspert1", "lcsanxt1")]

Variables_16_BP_child <- Sel_Data[, c("pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1", "pcbhsdqcont1")]

## Bind NSE and BP in adolescence by rater and scale
# Parent
adolescence_NSE_parent <- cbind(Variables_12_NSE_parent, Variables_16_NSE_parent)
adolescence_NSE_parent <- scale(adolescence_NSE_parent)
adolescence_NSE_parent <- data.frame(adolescence_NSE_parent)
adolescence_BP_parent <- cbind(Variables_12_BP_parent, Variables_16_BP_parent)
adolescence_BP_parent <- scale(adolescence_BP_parent)
adolescence_BP_parent <- data.frame(adolescence_BP_parent)

# Teacher
adolescence_BP_teacher <- Variables_12_BP_teacher
adolescence_BP_teacher <- scale(adolescence_BP_teacher)
adolescence_BP_teacher <- data.frame(adolescence_BP_teacher)

# Child
adolescence_BP_child <- cbind(Variables_12_BP_child, Variables_16_BP_child)
adolescence_BP_child <- scale(adolescence_BP_child)
adolescence_BP_child <- data.frame(adolescence_BP_child)

## Create mean composites of BP
# Parent
adolescence_BP_parent$adolescence_hyp_parent <- rowMeans(adolescence_BP_parent[, c(1,5)], na.rm = T)
adolescence_BP_parent$adolescence_con_parent <- rowMeans(adolescence_BP_parent[, c(2,6)], na.rm = T)
adolescence_BP_parent$adolescence_ep_parent <- adolescence_BP_parent$lpsanxt1
adolescence_BP_parent$adolescence_pp_parent <- adolescence_BP_parent$lpspert1

# Teacher
adolescence_BP_teacher$adolescence_hyp_teacher <- adolescence_BP_teacher$ltshypt1
adolescence_BP_teacher$adolescence_con_teacher <- adolescence_BP_teacher$ltscont1
adolescence_BP_teacher$adolescence_ep_teacher <- adolescence_BP_teacher$ltsanxt1
adolescence_BP_teacher$adolescence_pp_teacher <- adolescence_BP_teacher$ltspert1

# Child
adolescence_BP_child$adolescence_hyp_child <- rowMeans(adolescence_BP_child[, c(1,7)], na.rm = T)
adolescence_BP_child$adolescence_con_child <- rowMeans(adolescence_BP_child[, c(2,8)], na.rm = T)
adolescence_BP_child$adolescence_ep_child <- rowMeans(adolescence_BP_child[, c(4,5)], na.rm = T)
adolescence_BP_child$adolescence_pp_child <- rowMeans(adolescence_BP_child[, c(3,6)], na.rm = T)

## Add mean composites of BP to Sel_Data and scale
# Parent
Sel_Data$adolescence_hyp_parent1 <- adolescence_BP_parent$adolescence_hyp_parent
Sel_Data$adolescence_con_parent1 <- adolescence_BP_parent$adolescence_con_parent
Sel_Data$adolescence_ep_parent1 <- adolescence_BP_parent$adolescence_ep_parent
Sel_Data$adolescence_pp_parent1 <- adolescence_BP_parent$adolescence_pp_parent

Sel_Data$adolescence_hyp_parent1 <- scale(Sel_Data$adolescence_hyp_parent1)
Sel_Data$adolescence_con_parent1 <- scale(Sel_Data$adolescence_con_parent1)
Sel_Data$adolescence_ep_parent1 <- scale(Sel_Data$adolescence_ep_parent1)
Sel_Data$adolescence_pp_parent1 <- scale(Sel_Data$adolescence_pp_parent1)

# Teacher
Sel_Data$adolescence_hyp_teacher1 <- adolescence_BP_teacher$adolescence_hyp_teacher
Sel_Data$adolescence_con_teacher1 <- adolescence_BP_teacher$adolescence_con_teacher
Sel_Data$adolescence_ep_teacher1 <- adolescence_BP_teacher$adolescence_ep_teacher
Sel_Data$adolescence_pp_teacher1 <- adolescence_BP_teacher$adolescence_pp_teacher

Sel_Data$adolescence_hyp_teacher1 <- scale(Sel_Data$adolescence_hyp_teacher1)
Sel_Data$adolescence_con_teacher1 <- scale(Sel_Data$adolescence_con_teacher1)
Sel_Data$adolescence_ep_teacher1 <- scale(Sel_Data$adolescence_ep_teacher1)
Sel_Data$adolescence_pp_teacher1 <- scale(Sel_Data$adolescence_pp_teacher1)

# Child
Sel_Data$adolescence_hyp_child1 <- adolescence_BP_child$adolescence_hyp_child
Sel_Data$adolescence_con_child1 <- adolescence_BP_child$adolescence_con_child
Sel_Data$adolescence_ep_child1 <- adolescence_BP_child$adolescence_ep_child
Sel_Data$adolescence_pp_child1 <- adolescence_BP_child$adolescence_pp_child

Sel_Data$adolescence_hyp_child1 <- scale(Sel_Data$adolescence_hyp_child1)
Sel_Data$adolescence_con_child1 <- scale(Sel_Data$adolescence_con_child1)
Sel_Data$adolescence_ep_child1 <- scale(Sel_Data$adolescence_ep_child1)
Sel_Data$adolescence_pp_child1 <- scale(Sel_Data$adolescence_pp_child1)

# ===== Bind NSE in childhood and BP in adolescence datasets =====
## Childhood parent & adolescence parent 
childhood_NSE_parent_adolescence_BP_parent <- cbind(childhood_NSE_parent, adolescence_BP_parent)

# ===== Correlate NSE in childhood with BP in adolescence =====
## Parent & parent
childhood_NSE_parent_adolescence_BP_parent_matrix <- as.matrix(childhood_NSE_parent_adolescence_BP_parent)
rcorr <- rcorr(childhood_NSE_parent_adolescence_BP_parent_matrix)
childhood_NSE_parent_adolescence_BP_parent_r <- rcorr$r

# ===== Plot correlations between NSE in childhood and BP in adolescence ===== 
## Parent & parent
childhood_NSE_parent_adolescence_BP_parent_r <- as.data.frame(childhood_NSE_parent_adolescence_BP_parent_r)
childhood_NSE_parent_adolescence_BP_parent_r$NSE <- rownames(childhood_NSE_parent_adolescence_BP_parent_r)
rownames(childhood_NSE_parent_adolescence_BP_parent_r) <- NULL
childhood_NSE_parent_adolescence_BP_parent_r <- melt(childhood_NSE_parent_adolescence_BP_parent_r); head(childhood_NSE_parent_adolescence_BP_parent_r)

# Histogram
histogram(childhood_NSE_parent_adolescence_BP_parent_r$value)

# Density plot
theme <-   theme(legend.direction = "vertical",
                 legend.position="none",
                 legend.text = element_text(size=15),
                 legend.title = element_blank(),
                 panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white",
                                                 size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "gray"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
                 axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
                 axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
                 #axis.text.y = element_blank(),
                 axis.title= element_text(size=15),
                 strip.text.x = element_text(size=15, angle = 0),
                 strip.text.y = element_text(size=15, angle = 0),
                 plot.title = element_text(size=15, angle = 0),
                 strip.background = element_rect(colour="black", fill="white"))

p <- ggplot(childhood_NSE_parent_adolescence_BP_parent_r, aes(x=value))+
  geom_density(color="lightblue", fill="lightblue", alpha=1)+
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1, 1, by = 0.2))+
  #scale_y_continuous(limits = c(0,0.75))+
  theme+
  xlab("Correlations")+
  ylab("Density")

p

# ===== Explore NSE vars above threshold 0.2 ===== 
##### Parent & parent ##### 
childhood_NSE_parent_adolescence_BP_parent_matrix <- as.matrix(childhood_NSE_parent_adolescence_BP_parent)
rcorr <- rcorr(childhood_NSE_parent_adolescence_BP_parent_matrix)
r <- rcorr$r
r <- round(r, digits = 2)
r <- as.data.frame(r)
r$NSE <- rownames(r)
rownames(r) <- NULL

## Hyperactivity
childhood_NSE_parent_adolescence_hyp_parent <- r[r$adolescence_hyp_parent > 0.20 | r$adolescence_hyp_parent < -0.20,]
dput(childhood_NSE_parent_adolescence_hyp_parent$NSE)
# "gbpfrus1", "gdisp1", "gbfac11", "gpafel1", 
# "ipbpimp1", "ipbpalo1", "ipbpang1", "ipbpfru1", "iphom031", "iphom041", 
# "ipcla011", "ipcla221", "ipcla261", "ippafel1", "ipcpeer1", "ipcsati1", "ipcadve1", "ipcacce1"

## Conduct
childhood_NSE_parent_adolescence_con_parent <- r[r$adolescence_con_parent > 0.20 | r$adolescence_con_parent < -0.20,]
dput(childhood_NSE_parent_adolescence_con_parent$NSE)
# "gdismak1", "gdisend1", "gbpfrus1", "gbpangr1", 
# "gdisp1", "gbfac11", "gpafel1", "ipbpimp1", "ipbpalo1", 
# "ipbpang1", "ipbpfru1", "ippafel1"

## EP
childhood_NSE_parent_adolescence_ep_parent <- r[r$adolescence_ep_parent > 0.20 | r$adolescence_ep_parent < -0.20,]
dput(childhood_NSE_parent_adolescence_ep_parent$NSE)
# "ipcla181", "ipcnega1"

## PP
childhood_NSE_parent_adolescence_pp_parent <- r[r$adolescence_pp_parent > 0.20 | r$adolescence_pp_parent < -0.20,]
dput(childhood_NSE_parent_adolescence_pp_parent$NSE)
# "ipcla021", "ipcla091", 
# "ipcla131", "ipcla221", "ipcla231", "ipcla271", "ipcsati1", 
# "ipcnega1", "ipcacce1"

# ===== Elastic net; Parent & parent ===== 
##### Hyperactivity #####
# Select vars and scale
dat <- Sel_Data[, c("adolescence_hyp_parent1",
                    "gbpfrus1", "gdisp1", "gbfac11", "gpafel1", 
                    "ipbpimp1", "ipbpalo1", "ipbpang1", "ipbpfru1", "iphom031", "iphom041", 
                    "ipcla011", "ipcla221", "ipcla261", "ippafel1", "ipcpeer1", "ipcsati1", "ipcadve1", "ipcacce1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #2504
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$adolescence_hyp_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(adolescence_hyp_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$adolescence_hyp_parent1

# Build the model using the training set
model <- train(
  adolescence_hyp_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_adolescence_hyp_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_adolescence_hyp_parent <- as.matrix(coef_adolescence_hyp_parent)
write.csv(coef_adolescence_hyp_parent, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_hyp_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(adolescence_hyp_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_adolescence_hyp_parent1 <- data.frame(FRCT = 1,
                                          var = "adolescence_hyp_parent1",
                                          mean_cv_RMSE_train = mean(train_mod$RMSE),
                                          sd_cv_RMSE_train = sd(train_mod$RMSE),
                                          mean_cv_R2_train = mean(train_mod$Rsquared),
                                          sd_cv_R2_train = sd(train_mod$Rsquared),
                                          RMSE = RMSE(predictions, test.data$adolescence_hyp_parent1),
                                          Rsquare = R2(predictions, test.data$adolescence_hyp_parent1),
                                          alpha = model$bestTune$alpha,
                                          lambda = model$bestTune$lambda,
                                          N_train = dim(train.data)[-2],
                                          N_test = dim(test.data)[-2]
)

res_adolescence_hyp_parent1
write.csv(res_adolescence_hyp_parent1, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_hyp_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### Conduct #####
# Select vars and scale
dat <- Sel_Data[, c("adolescence_con_parent1", 
                    "gdismak1", "gdisend1", "gbpfrus1", "gbpangr1", 
                    "gdisp1", "gbfac11", "gpafel1", "ipbpimp1", "ipbpalo1", 
                    "ipbpang1", "ipbpfru1", "ippafel1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #2578
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$adolescence_con_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(adolescence_con_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$adolescence_con_parent1

# Build the model using the training set
model <- train(
  adolescence_con_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_adolescence_con_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_adolescence_con_parent <- as.matrix(coef_adolescence_con_parent)
write.csv(coef_adolescence_con_parent, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_con_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(adolescence_con_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_adolescence_con_parent1 <- data.frame(FRCT = 1,
                                          var = "adolescence_con_parent1",
                                          mean_cv_RMSE_train = mean(train_mod$RMSE),
                                          sd_cv_RMSE_train = sd(train_mod$RMSE),
                                          mean_cv_R2_train = mean(train_mod$Rsquared),
                                          sd_cv_R2_train = sd(train_mod$Rsquared),
                                          RMSE = RMSE(predictions, test.data$adolescence_con_parent1),
                                          Rsquare = R2(predictions, test.data$adolescence_con_parent1),
                                          alpha = model$bestTune$alpha,
                                          lambda = model$bestTune$lambda,
                                          N_train = dim(train.data)[-2],
                                          N_test = dim(test.data)[-2]
)

res_adolescence_con_parent1
write.csv(res_adolescence_con_parent1, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_con_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### EP #####
# Select vars and scale
dat <- Sel_Data[, c("adolescence_ep_parent1", 
                    "ipcla181", "ipcnega1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #2671
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$adolescence_ep_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(adolescence_ep_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$adolescence_ep_parent1

# Build the model using the training set
model <- train(
  adolescence_ep_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_adolescence_ep_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_adolescence_ep_parent <- as.matrix(coef_adolescence_ep_parent)
write.csv(coef_adolescence_ep_parent, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_ep_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(adolescence_ep_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_adolescence_ep_parent1 <- data.frame(FRCT = 1,
                                         var = "adolescence_ep_parent1",
                                         mean_cv_RMSE_train = mean(train_mod$RMSE),
                                         sd_cv_RMSE_train = sd(train_mod$RMSE),
                                         mean_cv_R2_train = mean(train_mod$Rsquared),
                                         sd_cv_R2_train = sd(train_mod$Rsquared),
                                         RMSE = RMSE(predictions, test.data$adolescence_ep_parent1),
                                         Rsquare = R2(predictions, test.data$adolescence_ep_parent1),
                                         alpha = model$bestTune$alpha,
                                         lambda = model$bestTune$lambda,
                                         N_train = dim(train.data)[-2],
                                         N_test = dim(test.data)[-2]
)

res_adolescence_ep_parent1
write.csv(res_adolescence_ep_parent1, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_ep_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

##### PP #####
# Select vars and scale
dat <- Sel_Data[, c("adolescence_pp_parent1",
                    "ipcla021", "ipcla091", 
                    "ipcla131", "ipcla221", "ipcla231", "ipcla271", "ipcsati1", 
                    "ipcnega1", "ipcacce1")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] # 2644
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$adolescence_pp_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(adolescence_pp_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$adolescence_pp_parent1

# Build the model using the training set
model <- train(
  adolescence_pp_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_adolescence_pp_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_adolescence_pp_parent <- as.matrix(coef_adolescence_pp_parent)
write.csv(coef_adolescence_pp_parent, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_pp_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(adolescence_pp_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_adolescence_pp_parent1 <- data.frame(FRCT = 1,
                                         var = "adolescence_pp_parent1",
                                         mean_cv_RMSE_train = mean(train_mod$RMSE),
                                         sd_cv_RMSE_train = sd(train_mod$RMSE),
                                         mean_cv_R2_train = mean(train_mod$Rsquared),
                                         sd_cv_R2_train = sd(train_mod$Rsquared),
                                         RMSE = RMSE(predictions, test.data$adolescence_pp_parent1),
                                         Rsquare = R2(predictions, test.data$adolescence_pp_parent1),
                                         alpha = model$bestTune$alpha,
                                         lambda = model$bestTune$lambda,
                                         N_train = dim(train.data)[-2],
                                         N_test = dim(test.data)[-2]
)

res_adolescence_pp_parent1
write.csv(res_adolescence_pp_parent1, './Elastic net res/elasticnet_childhood_NSE_parent_adolescence_pp_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

# ================================================================ ADOLESCENCE NSE & ADULTHOOD BP ======
# ===== NSE & BP vars in adolescence =====
## All raters
Variables_12_NSE <- Sel_Data[, c("lccha11",		"lccha1r1",			
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
                                 "lcpub61",		"lcpub71",		"lpdis011",
                                 "lpdis021",		"lpdis031",		"lpdis03r1",
                                 "lpdis041",		"lpdis04r1",		
                                 "lpdis051",		"lpdis061",		"lppar011",	
                                 "lppar021",		"lppar02r1",	"lppar031",	
                                 "lppar03r1",	"lppar041",		
                                 "lppar051",		"lppar061",		"lppar06r1",	
                                 "lppar071",		"ltsenrg1",		
                                 "ltsen1",		  "lcchato1",		"lppafel1",	
                                 "lcpafel1",		"lpdisto1",		"lcdisto1",	
                                 "lcmoaca1",		"lmatenv1",		"llitenv1",	
                                 "llitenx1",		"llitenf1",		"lpubtot1",	
                                 "lcvicph1",		"lcvicve1",		"lcvicso1",	
                                 "lcvicpr1")]

Variables_16_NSE <- Sel_Data[, c("plifev11",
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
                                 "plifev201",
                                 "clifev11",
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
                                 "pcgritm1",		"pcambim1")]

Variables_12_BP <- Sel_Data[, c("lpshypt1", "ltshypt1", "lcshypt1", "lpscont1", "ltscont1", 
                                "lcscont1", "lpspert1", "ltspert1", "lcspert1", "lpsanxt1", "ltsanxt1", 
                                "lcsanxt1")]

Variables_16_BP <- Sel_Data[, c("pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1",
                                "ppbhsdqhypt1", "pcbhsdqcont1", "ppbhsdqcont1")]

## Parent
Variables_12_NSE_parent <- Sel_Data[, c("lpdis011",
                                        "lpdis021",		"lpdis031",		"lpdis03r1",
                                        "lpdis041",		"lpdis04r1",		
                                        "lpdis051",		"lpdis061",		"lppar011",	
                                        "lppar021",		"lppar02r1",	"lppar031",	
                                        "lppar03r1",	"lppar041",		
                                        "lppar051",		"lppar061",		"lppar06r1",	
                                        "lppar071",		"lpdisto1")]

Variables_12_BP_parent <- Sel_Data[, c("lpshypt1", "lpscont1",  
                                       "lpspert1", "lpsanxt1")]

Variables_16_NSE_parent <- Sel_Data[, c("plifev11",
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
                                        "plifev201")]

Variables_16_BP_parent <- Sel_Data[, c("ppbhsdqhypt1", "ppbhsdqcont1")]

## Child
Variables_12_BP_child <- Sel_Data[, c("lcshypt1", "lcscont1", "lcspert1", "lcsanxt1")]

Variables_16_BP_child <- Sel_Data[, c("pcbhsdqanxt1", "pcbhsdqpert1", "pcbhsdqhypt1", "pcbhsdqcont1")]

## Bind NSE and BP in adolescence by rater and scale
# Parent
adolescence_NSE_parent <- cbind(Variables_12_NSE_parent, Variables_16_NSE_parent)
adolescence_NSE_parent <- scale(adolescence_NSE_parent)
adolescence_NSE_parent <- data.frame(adolescence_NSE_parent)
adolescence_BP_parent <- cbind(Variables_12_BP_parent, Variables_16_BP_parent)
adolescence_BP_parent <- scale(adolescence_BP_parent)
adolescence_BP_parent <- data.frame(adolescence_BP_parent)

# Child
adolescence_BP_child <- cbind(Variables_12_BP_child, Variables_16_BP_child)
adolescence_BP_child <- scale(adolescence_BP_child)
adolescence_BP_child <- data.frame(adolescence_BP_child)

## Create mean composites of BP
# Parent
adolescence_BP_parent$adolescence_hyp_parent <- rowMeans(adolescence_BP_parent[, c(1,5)], na.rm = T)
adolescence_BP_parent$adolescence_con_parent <- rowMeans(adolescence_BP_parent[, c(2,6)], na.rm = T)
adolescence_BP_parent$adolescence_ep_parent <- adolescence_BP_parent$lpsanxt1
adolescence_BP_parent$adolescence_pp_parent <- adolescence_BP_parent$lpspert1

# Child
adolescence_BP_child$adolescence_hyp_child <- rowMeans(adolescence_BP_child[, c(1,7)], na.rm = T)
adolescence_BP_child$adolescence_con_child <- rowMeans(adolescence_BP_child[, c(2,8)], na.rm = T)
adolescence_BP_child$adolescence_ep_child <- rowMeans(adolescence_BP_child[, c(4,5)], na.rm = T)
adolescence_BP_child$adolescence_pp_child <- rowMeans(adolescence_BP_child[, c(3,6)], na.rm = T)

## Add mean composites of BP to Sel_Data and scale
# Parent
Sel_Data$adolescence_hyp_parent1 <- adolescence_BP_parent$adolescence_hyp_parent
Sel_Data$adolescence_con_parent1 <- adolescence_BP_parent$adolescence_con_parent
Sel_Data$adolescence_ep_parent1 <- adolescence_BP_parent$adolescence_ep_parent
Sel_Data$adolescence_pp_parent1 <- adolescence_BP_parent$adolescence_pp_parent

Sel_Data$adolescence_hyp_parent1 <- scale(Sel_Data$adolescence_hyp_parent1)
Sel_Data$adolescence_con_parent1 <- scale(Sel_Data$adolescence_con_parent1)
Sel_Data$adolescence_ep_parent1 <- scale(Sel_Data$adolescence_ep_parent1)
Sel_Data$adolescence_pp_parent1 <- scale(Sel_Data$adolescence_pp_parent1)

# Child
Sel_Data$adolescence_hyp_child1 <- adolescence_BP_child$adolescence_hyp_child
Sel_Data$adolescence_con_child1 <- adolescence_BP_child$adolescence_con_child
Sel_Data$adolescence_ep_child1 <- adolescence_BP_child$adolescence_ep_child
Sel_Data$adolescence_pp_child1 <- adolescence_BP_child$adolescence_pp_child

Sel_Data$adolescence_hyp_child1 <- scale(Sel_Data$adolescence_hyp_child1)
Sel_Data$adolescence_con_child1 <- scale(Sel_Data$adolescence_con_child1)
Sel_Data$adolescence_ep_child1 <- scale(Sel_Data$adolescence_ep_child1)
Sel_Data$adolescence_pp_child1 <- scale(Sel_Data$adolescence_pp_child1)

# ===== BP vars in adulthood =====
## All raters
Variables_21_BP <- Sel_Data[, c("u1psdqanxt1", "u1csdqanxt1",
                                "u1psdqcont1", "u1csdqcont1", 
                                "u1psdqhypt1", "u1csdqhypt1", 
                                "u1psdqpert1", "u1csdqpert1")]

## Parent
Variables_21_BP_parent <- Sel_Data[, c("u1psdqanxt1", "u1psdqcont1",
                                       "u1psdqhypt1", "u1psdqpert1")]

## Child
Variables_21_BP_child <- Sel_Data[, c("u1csdqanxt1", "u1csdqcont1", 
                                      "u1csdqhypt1", "u1csdqpert1")]

## BP in adulthood by rater
# Parent
adulthood_BP_parent <- Variables_21_BP_parent

# Child
adulthood_BP_child <- Variables_21_BP_child

## Create mean composites of BP
# Parent
adulthood_BP_parent$adulthood_hyp_parent <- adulthood_BP_parent$u1psdqhypt1
adulthood_BP_parent$adulthood_con_parent <- adulthood_BP_parent$u1psdqcont1
adulthood_BP_parent$adulthood_ep_parent <- adulthood_BP_parent$u1psdqanxt1
adulthood_BP_parent$adulthood_pp_parent <- adulthood_BP_parent$u1psdqpert1

# Child
adulthood_BP_child$adulthood_hyp_child <- adulthood_BP_child$u1csdqhypt1
adulthood_BP_child$adulthood_con_child <- adulthood_BP_child$u1csdqcont1
adulthood_BP_child$adulthood_ep_child <- adulthood_BP_child$u1csdqanxt1
adulthood_BP_child$adulthood_pp_child <- adulthood_BP_child$u1csdqpert1

## Add mean composites of BP to Sel_Data
# Parent
Sel_Data$adulthood_hyp_parent1 <- adulthood_BP_parent$adulthood_hyp_parent
Sel_Data$adulthood_con_parent1 <- adulthood_BP_parent$adulthood_con_parent
Sel_Data$adulthood_ep_parent1 <- adulthood_BP_parent$adulthood_ep_parent
Sel_Data$adulthood_pp_parent1 <- adulthood_BP_parent$adulthood_pp_parent

# Child
Sel_Data$adulthood_hyp_child1 <- adulthood_BP_child$adulthood_hyp_child
Sel_Data$adulthood_con_child1 <- adulthood_BP_child$adulthood_con_child
Sel_Data$adulthood_ep_child1 <- adulthood_BP_child$adulthood_ep_child
Sel_Data$adulthood_pp_child1 <- adulthood_BP_child$adulthood_pp_child

# ===== Bind NSE in adolescence and BP in adulthood datasets =====
## Adolescence parent & adulthood parent 
adolescence_NSE_parent_adulthood_BP_parent <- cbind(adolescence_NSE_parent, adulthood_BP_parent)

# ===== Correlate NSE in adolescence with BP in adulthood =====
## Parent & parent
adolescence_NSE_parent_adulthood_BP_parent_matrix <- as.matrix(adolescence_NSE_parent_adulthood_BP_parent)
rcorr <- rcorr(adolescence_NSE_parent_adulthood_BP_parent_matrix)
adolescence_NSE_parent_adulthood_BP_parent_r <- rcorr$r

# ===== Plot correlations between NSE and BP in adolescence ===== 
## Parent & parent
adolescence_NSE_parent_adulthood_BP_parent_r <- as.data.frame(adolescence_NSE_parent_adulthood_BP_parent_r)
adolescence_NSE_parent_adulthood_BP_parent_r$NSE <- rownames(adolescence_NSE_parent_adulthood_BP_parent_r)
rownames(adolescence_NSE_parent_adulthood_BP_parent_r) <- NULL
adolescence_NSE_parent_adulthood_BP_parent_r <- melt(adolescence_NSE_parent_adulthood_BP_parent_r); head(adolescence_NSE_parent_adulthood_BP_parent_r)

# Histogram
histogram(adolescence_NSE_parent_adulthood_BP_parent_r$value)

# Density plot
theme <-   theme(legend.direction = "vertical",
                 legend.position="none",
                 legend.text = element_text(size=15),
                 legend.title = element_blank(),
                 panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white",
                                                 size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "gray"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
                 axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
                 axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
                 axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
                 #axis.text.y = element_blank(),
                 axis.title= element_text(size=15),
                 strip.text.x = element_text(size=15, angle = 0),
                 strip.text.y = element_text(size=15, angle = 0),
                 plot.title = element_text(size=15, angle = 0),
                 strip.background = element_rect(colour="black", fill="white"))

p <- ggplot(adolescence_NSE_parent_adulthood_BP_parent_r, aes(x=value))+
  geom_density(color="lightblue", fill="lightblue", alpha=1)+
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1, 1, by = 0.2))+
  #scale_y_continuous(limits = c(0,0.75))+
  theme+
  xlab("Correlations")+
  ylab("Density")

p

# ===== Explore NSE vars above threshold 0.2 ===== 
##### Parent & parent ##### 
adolescence_NSE_parent_adulthood_BP_parent_matrix <- as.matrix(adolescence_NSE_parent_adulthood_BP_parent)
rcorr <- rcorr(adolescence_NSE_parent_adulthood_BP_parent_matrix)
r <- rcorr$r
r <- round(r, digits = 2)
r <- as.data.frame(r)
r$NSE <- rownames(r)
rownames(r) <- NULL

## Hyperactivity
adolescence_NSE_parent_adulthood_hyp_parent <- r[r$adulthood_hyp_parent > 0.20 | r$adulthood_hyp_parent < -0.20,]
dput(adolescence_NSE_parent_adulthood_hyp_parent$NSE)
# "lppar071"

## Conduct
adolescence_NSE_parent_adulthood_con_parent <- r[r$adulthood_con_parent > 0.20 | r$adulthood_con_parent < -0.20,]
dput(adolescence_NSE_parent_adulthood_con_parent$NSE)
# "lppar051", "lppar071"

## EP
adolescence_NSE_parent_adulthood_ep_parent <- r[r$adulthood_ep_parent > 0.20 | r$adulthood_ep_parent < -0.20,]
dput(adolescence_NSE_parent_adulthood_ep_parent$NSE)

## PP
adolescence_NSE_parent_adulthood_pp_parent <- r[r$adulthood_pp_parent > 0.20 | r$adulthood_pp_parent < -0.20,]
dput(adolescence_NSE_parent_adulthood_pp_parent$NSE)

# ===== Elastic net; Parent & parent ===== 
##### Conduct #####
# Select vars and scale
dat <- Sel_Data[, c("adulthood_con_parent1",
                    "lppar051", "lppar071")]

dat <- scale(dat); dat <- data.frame(dat)

# Exclude missing data
dat <- dat[complete.cases(dat),] #3181
dim(dat) 

# Split the data into training and test set
set.seed(12345); training.samples <- dat$adulthood_con_parent1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Predictor variables
x <- model.matrix(adulthood_con_parent1~., train.data)[,-1]

# Outcome variable
y <- train.data$adulthood_con_parent1

# Build the model using the training set
model <- train(
  adulthood_con_parent1 ~., data = train.data, method = "glmnet",
  trControl = trainControl("repeatedcv", number = 10, repeats = 100),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

# Coefficient of the final model; need to specify the best lambda
coef_adulthood_con_parent <- coef(model$finalModel, model$bestTune$lambda)
coef_adulthood_con_parent <- as.matrix(coef_adulthood_con_parent)
write.csv(coef_adulthood_con_parent, './Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_con_parent_coef_agesexregressed_twin1.csv', row.names=T, quote=F)

train_mod <- model$results

# Make predictions on the test data
x.test <- model.matrix(adulthood_con_parent1 ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
res_adulthood_con_parent1 <- data.frame(FRCT = 1,
                                        var = "adulthood_con_parent1",
                                        mean_cv_RMSE_train = mean(train_mod$RMSE),
                                        sd_cv_RMSE_train = sd(train_mod$RMSE),
                                        mean_cv_R2_train = mean(train_mod$Rsquared),
                                        sd_cv_R2_train = sd(train_mod$Rsquared),
                                        RMSE = RMSE(predictions, test.data$adulthood_con_parent1),
                                        Rsquare = R2(predictions, test.data$adulthood_con_parent1),
                                        alpha = model$bestTune$alpha,
                                        lambda = model$bestTune$lambda,
                                        N_train = dim(train.data)[-2],
                                        N_test = dim(test.data)[-2]
)

res_adulthood_con_parent1
write.csv(res_adulthood_con_parent1, './Elastic net res/elasticnet_adolescence_NSE_parent_adulthood_con_parent_model_res_agesexregressed_twin1.csv', row.names=F, quote=F)

# ================================================================ WRITE OUT A DATASET WITH BP COMPOSITES======
write.csv(Sel_Data, './data/NSEBP_agesexregressed_data_with_BP_composites_pureenv_twin1.csv', row.names=F, quote=F)





