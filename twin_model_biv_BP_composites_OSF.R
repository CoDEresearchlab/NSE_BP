
#### Model: bivariate saturated and Cholesky ACE decomposition
#### Script based on BivACE.R script from 2015 SGDP summer school.

###Aga 30/07/2021

###NSE BP



### ---------------------------------------------------------------------------
### Setup / retrieve data

## clean workspace
rm(list=ls())

## import dependencies
Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) 
library(OpenMx)
library(psych)
library(foreign)

mxOption(NULL,"Default optimizer","SLSQP")
mxOption(key='Number of Threads', value=parallel::detectCores())
## set working directory
setwd('~/Desktop/NSE BP/')

## input/output configuration
inFile <- './data/NSE_BP_cleandata.csv'				# prepared data to analyse
SPSSFile <- './data/504 Aga Bubel NSE version3 Mar2022.sav'		
outFileStem <- './twin_res/twin_biv/'		# stem of output filenames

## open raw data and extract variable labels
rawDat <- read.spss(SPSSFile, use.value.labels=F, to.data.frame=T)
labels <- attr(rawDat, 'variable.labels')

## open cleaned data
dat <- read.csv(inFile)

## select variables
nv	<- 2		# number of variables for a twin 
ntv	<- 2*nv		# number of variables for a pair 

# define pairs of variables. For Cholesky analysis, the first variable in each pair
# is the predictor variable, and the second is the predicted variable. Variables are
# assumed to be double-entered, but are listed here WITHOUT a trailing '1' or '2'.
VarPairs <- rbind(
  c("polyNSE_preschool_NSE_childhood_hyp_parent", "childhood_hyp_parent"),
  c("polyNSE_preschool_NSE_childhood_hyp_parent", "childhood_hyp_teacher"),
  c("polyNSE_preschool_NSE_childhood_hyp_parent", "childhood_hyp_child"),
  c("polyNSE_childhood_NSE_adolescence_hyp_parent", "adolescence_hyp_parent"),
  c("polyNSE_childhood_NSE_adolescence_hyp_parent", "adolescence_hyp_teacher"),
  c("polyNSE_childhood_NSE_adolescence_hyp_parent", "adolescence_hyp_child"),
  c("polyNSE_adolescence_NSE_adulthood_hyp_parent", "adulthood_hyp_parent"),
  c("polyNSE_adolescence_NSE_adulthood_hyp_parent", "adulthood_hyp_child"),
  c("polyNSE_preschool_NSE_childhood_con_parent", "childhood_con_parent"),
  c("polyNSE_preschool_NSE_childhood_con_parent", "childhood_con_teacher"),
  c("polyNSE_preschool_NSE_childhood_con_parent", "childhood_con_child"),
  c("polyNSE_childhood_NSE_adolescence_con_parent", "adolescence_con_parent"),
  c("polyNSE_childhood_NSE_adolescence_con_parent", "adolescence_con_teacher"),
  c("polyNSE_childhood_NSE_adolescence_con_parent", "adolescence_con_child"),
  c("polyNSE_adolescence_NSE_adulthood_con_parent", "adulthood_con_parent"),
  c("polyNSE_adolescence_NSE_adulthood_con_parent", "adulthood_con_child"),
  c("polyNSE_preschool_NSE_childhood_ep_parent", "childhood_ep_parent"),
  c("polyNSE_preschool_NSE_childhood_ep_parent", "childhood_ep_teacher"),
  c("polyNSE_preschool_NSE_childhood_ep_parent", "childhood_ep_child"),
  c("polyNSE_childhood_NSE_adolescence_ep_parent", "adolescence_ep_parent"),
  c("polyNSE_childhood_NSE_adolescence_ep_parent", "adolescence_ep_teacher"),
  c("polyNSE_childhood_NSE_adolescence_ep_parent", "adolescence_ep_child"),
  c("polyNSE_preschool_NSE_childhood_pp_parent", "childhood_pp_parent"),
  c("polyNSE_preschool_NSE_childhood_pp_parent", "childhood_pp_teacher"),
  c("polyNSE_childhood_NSE_adolescence_pp_parent", "adolescence_pp_parent"),
  c("polyNSE_childhood_NSE_adolescence_pp_parent", "adolescence_pp_teacher"),
  c("polyNSE_childhood_NSE_adolescence_pp_parent", "adolescence_pp_child"))

VarPairs <- rbind(
  c("polyNSE_preschool_NSE_childhood_pp_parent", "childhood_pp_child"))
  
#VarPairs <- rbind( c('first_var', 'second_var') )  #single pair to test script

### ---------------------------------------------------------------------------
### Start loop (repeat for each pair of variables in VarPairs)

for (i in 1:nrow(VarPairs)) {
  
  ## select variables (all variables for twin 1, then all for twin 2)
  Vars <- c(VarPairs[i,1], VarPairs[i,2])
  selVars <- c(paste(Vars, "1", sep=""), paste(Vars, "2", sep=""))
  
  ## report progress
  cat("\n", paste("--- Starting run for", Vars[1], "x", Vars[2], "- pair", i, "of", nrow(VarPairs)), "---\n")
  
  ## select data
  mzData <- subset(dat, zygos==1, selVars)
  dzData <- subset(dat, zygos==2, selVars)
  
  
  ## -----------------------------------------------------------------------
  ## Print descriptive statistics by zygosity group
  #
  #describe(mzData)
  #describe(dzData)
  #colMeans(mzData,na.rm=TRUE)
  #cov(mzData,use="complete")
  #cor(mzData,use="complete")
  #colMeans(dzData,na.rm=TRUE)
  #cov(dzData,use="complete")
  #cor(dzData,use="complete")
  
  
  
  ## -----------------------------------------------------------------------
  ## 1. Specify and run a bivariate saturated model (Cholesky decomposition)
  ## (Estimate MZ and DZ covariances and means by fitting a 'model' to the data)
  
  ## matrix and algebra for expected means and covariances
  MZlow		<-mxMatrix( type="Lower", nrow=ntv, ncol=ntv, free=T, values=5, name="LowMZ" )
  CovMZ		<-mxAlgebra( expression=LowMZ %*% t(LowMZ), name="ExpCovMZ" )
  MeanMZ		<-mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=c(4,90,4,90), name="ExpMeanMZ" )
  
  DZlow		<-mxMatrix( type="Lower", nrow=ntv, ncol=ntv, free=T, values=5, name="LowDZ" )
  CovDZ		<-mxAlgebra( expression=LowDZ %*% t(LowDZ), name="ExpCovDZ" )
  MeanDZ		<-mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=c(4,90,4,90), name="ExpMeanDZ" )
  
  ## data objects for multiple groups
  dataMZ		<- mxData( observed=mzData, type="raw" )
  dataDZ		<- mxData( observed=dzData, type="raw" )
  
  ## objective objects for multiple groups
  objMZ		<- mxExpectationNormal( covariance="ExpCovMZ", means="ExpMeanMZ", dimnames=selVars)
  objDZ		<- mxExpectationNormal( covariance="ExpCovDZ", means="ExpMeanDZ", dimnames=selVars)
  
  fitFunction	<- mxFitFunctionML()
  
  
  ## combine groups
  modelMZ		<- mxModel( MZlow, CovMZ, MeanMZ, dataMZ, objMZ, fitFunction, name="MZ")
  modelDZ		<- mxModel( DZlow, CovDZ, MeanDZ, dataDZ, objDZ, fitFunction, name="DZ")
  minus2ll	<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
  obj			<- mxFitFunctionAlgebra( "m2LL" )
  SatModel	<- mxModel( "Sat", modelMZ, modelDZ, minus2ll, obj)
  
  ## run saturated 'model' (Cholesky decomposition)
  SatFit		<- mxRun(SatModel)
  (SatSum		<- summary(SatFit))
  
  ## generate saturated output
  #round(SatFit@output$estimate,4)
  #
  #mxEval(MZ.ExpMeanMZ, SatFit)
  #mxEval(MZ.ExpCovMZ, SatFit)
  #mxEval(DZ.ExpMeanDZ, SatFit)
  #mxEval(DZ.ExpCovDZ, SatFit)
  
  
  ## -----------------------------------------------------------------------
  ## 1b. Specify and run a bivariate saturated model (Gaussian decomposition)
  ## (Used to fit a constrained model to test assumptions)
  ##
  ## TODO: currently unused.
  
  ## starting values
  svM			<-c(4,90,4,90)
  svSD		<-c(1.5,15,1.5,15)
  svMZ		<-c(-.3,.7,-.3,-.3,.7,-.3)
  svDZ		<-c(-.3,.2,-.1,-.1,.5,-.3)
  
  ## matrix and algebra for expected means and covariances
  MeanMZ		<-mxMatrix( "Full", 1, ntv, free=T, values=svM, labels=c("MZm11", "MZm21", "MZm12", "MZm22"), name="ExpMeanMZ" )
  MZsd		<-mxMatrix( "Diag", ntv, ntv, free=T, values=svSD,labels=c("MZs11", "MZs21", "MZs12", "MZs22"), name="sdMZ" )
  Cormz		<-mxMatrix( "Stand", ntv, ntv, free=T, values=svMZ, labels=c("rPhMZ1","rMZ1","MZxtxt1","MZxtxt2","rMZ2","rPhMZ2"),name="MZCor") 
  CovMZ		<-mxAlgebra( expression=sdMZ %*% MZCor %*% t(sdMZ), name="ExpCovMZ" )
  
  MeanDZ		<-mxMatrix( "Full", 1, ntv, free=T, values=svM, labels=c("DZm11", "DZm21", "DZm12", "DZm22"), name="ExpMeanDZ" )
  DZsd		<-mxMatrix( "Diag", ntv, ntv, free=T, values=svSD, labels=c("DZs11", "DZs21", "DZs12", "DZs22"), name="sdDZ" )
  Cordz		<-mxMatrix( "Stand", ntv, ntv, free=T, values=svDZ, labels=c("rPhDZ1","rDZ1","DZxtxt1","DZxtxt2","rDZ2","rPhDZ2"),name="DZCor") 
  CovDZ		<-mxAlgebra( expression=sdDZ %*% DZCor %*% t(sdDZ), name="ExpCovDZ" )
  
  ## data objects for multiple groups
  dataMZ		<- mxData( observed=mzData, type="raw" )
  dataDZ		<- mxData( observed=dzData, type="raw" )
  
  ## objective objects for multiple groups
  objMZ		<- mxExpectationNormal( covariance="ExpCovMZ", means="ExpMeanMZ", dimnames=selVars)
  objDZ		<- mxExpectationNormal( covariance="ExpCovDZ", means="ExpMeanDZ", dimnames=selVars)
  
  fitFunction	<- mxFitFunctionML()
  
  ## combine groups
  modelMZ		<- mxModel( MeanMZ, MZsd, Cormz, CovMZ, dataMZ, objMZ, fitFunction, name="MZ")
  modelDZ		<- mxModel( MeanDZ, DZsd, Cordz, CovDZ, dataDZ, objDZ, fitFunction, name="DZ")
  minus2ll	<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
  obj			<- mxFitFunctionAlgebra( "m2LL" )
  Conf1		<- mxCI (c ('MZ.MZCor[2,1]', 'MZ.MZCor[3,1]', 'MZ.MZCor[4,1]', 'MZ.MZCor[4,2]') )
  Conf2		<- mxCI (c ('DZ.DZCor[3,1]', 'DZ.DZCor[4,1]', 'DZ.DZCor[4,2]') )
  SatGModel	<- mxModel( "SatG", modelMZ, modelDZ, minus2ll, obj, Conf1, Conf2)
  
  ## run saturated model (Gaussian decomposition)
  #SatGFit		<- mxRun(SatGModel, intervals=F)
  #(SatGSum	<- summary(SatGFit))
  
  ## generate SatGModel output
  #round(SatGFit@output$estimate,4)
  #
  #mxEval(MZ.ExpMeanMZ, SatGFit)
  #mxEval(MZ.ExpCovMZ, SatGFit)
  #mxEval(DZ.ExpMeanDZ, SatGFit)
  #mxEval(DZ.ExpCovDZ, SatGFit)
  
  
  ## -----------------------------------------------------------------------
  ## 2. Specify and run Sub1: constrained bivariate model (Gaussian decomposition)
  ## (Equal Means & Variances across Twin Order & zyg group;
  ## One overall set of Within-person cross-trait correlations;
  ## Symmetric xtwin-xtrait cor matrices in MZ and DZ group)
  ##
  ## To manipulate the parameters in matrices we wish to change in the full model,
  ## we use the 'omxSetParameters' function with the original 'labels' and 'newlabels'
  ## to indicate the changes; i.e., specifying the same label effectively constraints
  ## the parameters to be the same.
  ##
  
  
  Sub1Model	<- mxModel(SatGModel, name="Sub1" )
  
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svM, labels=c("MZm11", "MZm21", "MZm12", "MZm22"),
                                 newlabels=c("m11", "m21", "m11", "m21"))
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svM, labels=c("DZm11", "DZm21", "DZm12", "DZm22"),
                                 newlabels=c("m11", "m21", "m11", "m21"))
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svSD, labels=c("MZs11", "MZs21", "MZs12", "MZs22"),
                                 newlabels=c("s11", "s21", "s11", "s21"))
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svSD, labels=c("DZs11", "DZs21", "DZs12", "DZs22"),
                                 newlabels=c("s11", "s21", "s11", "s21"))
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svMZ, labels=c("rPhMZ1","rMZ1","MZxtxt1","MZxtxt2","rMZ2","rPhMZ2"), 
                                 newlabels=c("rPh1","rMZ1","MZxtxt1","MZxtxt1","rMZ2","rPh1"))
  Sub1Model	<- omxSetParameters( Sub1Model, free=T, values=svDZ, labels=c("rPhDZ1","rDZ1","DZxtxt1","DZxtxt2","rDZ2","rPhDZ2"), 
                                 newlabels=c("rPh1","rDZ1","DZxtxt1","DZxtxt1","rDZ2","rPh1"))
  
  ## run sub-model
  #Sub1Fit      <- mxRun( Sub1Model, intervals=T )
  #(Sub1Sum     <- summary( Sub1Fit ))
  
  ## generate Sub1Model output
  #mxEval(MZ.ExpMeanMZ, Sub1Fit)
  #mxEval(MZ.ExpCovMZ, Sub1Fit)
  #mxEval(DZ.ExpMeanDZ, Sub1Fit)
  #mxEval(DZ.ExpCovDZ, Sub1Fit)
  #mxEval(MZ.MZCor, Sub1Fit)
  #mxEval(DZ.DZCor, Sub1Fit)
  #
  #mxCompare(SatGFit, Sub1Fit)
  
  
  ## -----------------------------------------------------------------------
  ## 3. Specify and run ACE Model, with ONE overall set of means
  
  ## matrices to store a, c, and e path coefficients
  pathA		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=c(1,-4,1), labels=c("a11", "a21", "a22"), name="a" ) 
  pathC		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=c(1,-2,4), labels=c("c11", "c21", "c22"), name="c" )
  pathE		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=c(1,-1,8), labels=c("e11", "e21", "e22"), name="e" )
  
  ## matrix for expected means
  MeanG		<- mxMatrix( "Full", 1, ntv, free=T, values=svM, labels=c("m11", "m21", "m11", "m21"), name="ExpMean" )
  
  ## matrices generated to hold A, C, and E computed variance components
  covA		<- mxAlgebra( expression=a %*% t(a), name="A" )
  covC		<- mxAlgebra( expression=c %*% t(c), name="C" ) 
  covE		<- mxAlgebra( expression=e %*% t(e), name="E" )
  
  ## algebra to compute total variances and standard deviations (diagonal only)
  covP	<- mxAlgebra( expression=A+C+E, name="V" )
  matI	<- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
  invSD	<- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")
  
  ## algebra to compute standardized variance components
  StA			<- mxAlgebra( expression=A/V, name="h2")
  StC			<- mxAlgebra( expression=C/V, name="c2")
  StE			<- mxAlgebra( expression=E/V, name="e2")
  
  ## algebra to compute phenotypic, A, C and E correlations
  rph			<- mxAlgebra( expression= solve(sqrt(I*V)) %*% V %*% solve(sqrt(I*V)), name="Rph")
  rA			<- mxAlgebra( expression= solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), name="Ra" )
  rC			<- mxAlgebra( expression= solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), name="Rc" )
  rE			<- mxAlgebra( expression= solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), name="Re" )
  
  # algebra to compute standardised paths
  sta			<- mxAlgebra( expression=iSD %&% a, name="sta")
  stc			<- mxAlgebra( expression=iSD %&% c, name='stc')
  ste			<- mxAlgebra( expression=iSD %&% e, name='ste')
  
  # algebra to compute squared standardised paths
  sta2		<- mxAlgebra( expression=sta * sta, name='sta2')
  stc2		<- mxAlgebra( expression=stc * stc, name='stc2')
  ste2		<- mxAlgebra( expression=ste * ste, name='ste2')
  
  ## algebra to compute Rph-A, Rph-C and Rph-E between variables
  rphace		<- mxAlgebra( expression= cbind ( 	(sqrt(h2[1,1])*Ra[2,1]*sqrt(h2[2,2])),  
                                             (sqrt(c2[1,1])*Rc[2,1]*sqrt(c2[2,2])), 
                                             (sqrt(e2[1,1])*Re[2,1]*sqrt(e2[2,2])) ), name="RphACE" )
  
  ## algebra for expected variance/covariance matrices in MZ and DZ twins
  covMZ		<- mxAlgebra( expression= rbind( cbind(A+C+E , A+C),
                                          cbind(A+C  , A+C+E)), name="expCovMZ" )
  covDZ		<- mxAlgebra( expression= rbind( cbind(A+C+E      , 0.5%x%A+C),
                                          cbind(0.5%x%A+C , A+C+E)), name="expCovDZ" )
  
  ## data objects for multiple groups
  dataMZ		<- mxData( observed=mzData, type="raw" )
  dataDZ		<- mxData( observed=dzData, type="raw" )
  
  ## objective objects for multiple groups
  objMZ		<- mxExpectationNormal( covariance="expCovMZ", means="ExpMean", dimnames=selVars)
  objDZ		<- mxExpectationNormal( covariance="expCovDZ", means="ExpMean", dimnames=selVars)
  
  fitFunction	<- mxFitFunctionML()
  
  ## combine groups
  params		<- list( pathA, pathC, pathE, covA, covC, covE, covP, matI, invSD, StA, StC, StE, rph, rA, rC, rE, sta, stc, ste, sta2, stc2, ste2,  rphace, MeanG )
  modelMZ		<- mxModel( params, covMZ, dataMZ, objMZ, fitFunction, name="MZ" )
  modelDZ		<- mxModel( params, covDZ, dataDZ, objDZ, fitFunction, name="DZ" )
  minus2ll	<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
  obj			<- mxFitFunctionAlgebra( "m2LL" )
  
  ## define required confidence intervals
  
  # univariate ACE estimates
  Conf1		<- mxCI( c('h2[1,1]', 'h2[2,2]', 'c2[1,1]', 'c2[2,2]', 'e2[1,1]', 'e2[2,2]') )
  
  # correlations (phenotypic, genetic, shared and non-shared environmental)
  Conf2		<- mxCI( c('Rph[2,1]', 'Ra[2,1]', 'Rc[2,1]', 'Re[2,1]') )
  
  # proportions of rPh due to A, C and E
  Conf3		<- mxCI( c('RphACE[1,1]', 'RphACE[1,2]', 'RphACE[1,3]') )
  
  # shared and unique (standardised, squared) genetic paths
  Conf4		<- mxCI( c('ACE.sta2[1,1]', 'ACE.sta2[2,1]', 'ACE.sta2[2,2]') )
  
  # shared and unique (standardised, squared) c paths
  Conf5		<- mxCI( c('ACE.stc2[1,1]', 'ACE.stc2[2,1]', 'ACE.stc2[2,2]') )
  
  # shared and unique (standardised, squared) e paths
  Conf6		<- mxCI( c('ACE.ste2[1,1]', 'ACE.ste2[2,1]', 'ACE.ste2[2,2]') )
  
  ## define model
  AceModel	<- mxModel( "ACE", params, modelMZ, modelDZ, minus2ll, obj, Conf1, Conf2, Conf3, Conf4, Conf5, Conf6)
  
  ## run full ACE model
  AceFit		<- mxRun(AceModel, intervals=T)
  (AceSum		<- summary(AceFit))
  
  ## generate ACE output
  #round(AceFit@output$estimate,4)
  #
  #AceFit$ACE.h2
  #AceFit$ACE.c2
  #AceFit$ACE.e2
  #
  #AceFit$ACE.Ra
  #AceFit$ACE.Rc
  #AceFit$ACE.Re
  #
  #AceFit$ACE.Rph
  #AceFit$ACE.RphACE
  
  
  ## -----------------------------------------------------------------------
  ## Compare models: fit statistics
  #
  #mxCompare(SatFit, AceFit)
  
  
  
  ## -----------------------------------------------------------------------
  ## Output results to file
  
  ## open file
  fileName <- paste(outFileStem, paste(Vars[1], Vars[2], sep="-"), sep="")
  sink( fileName, append=T)
  
  ## title (variables)
  varStr <- ""
  for (i in 1:(length(selVars)/2)) {
    varStr = paste(varStr, "  ", selVars[i], ": ", labels[selVars[i]], "\n", sep="")
  }
  
  cat( paste("Bivariate modelling for:\n", varStr, "\n", sep="") )
  
  ## test model assumptions. TODO: currently unused
  #cat("\n--Fit statistics: compare saturated Gaussian decomposition to constrained submodel--\n")
  #cat("(Equal means and variances between twin1/2 and zygosity group; equal within-individual\ncross-trait correlations; symmetric cross-twin cross-trait correlations in MZ and DZ groups.)\n")
  #print( mxCompare(SatGFit, Sub1Fit) )
  
  ## test ACE model fit
  cat("\n--Fit statistics: compare saturated Cholesky decomposition to ACE model--\n")
  print( mxCompare(SatFit, AceFit) )
  
  ## path estimates
  cat("\n--Path estimates (unstandardised)--\n")
  print(round(AceFit@output$estimate,4))
  
  ## standardised paths
  cat("\n--Path estimates (standardised, not squared)--\n")
  stPaths <- cbind(AceFit$ACE.sta@result, AceFit$ACE.stc@result, AceFit$ACE.ste@result)
  colnames(stPaths) <- paste(rep( c("a", "c", "e"), each=length(selVars)/2), c(1,2), sep="")
  rownames(stPaths) <- paste("v", 1:nrow(stPaths), sep="")
  print(round(stPaths,4))
  
  ## standardised, squared paths
  cat("\n--Path estimates (standardised, squared)--\n")
  stSqPaths <- cbind(AceFit$ACE.sta2@result, AceFit$ACE.stc2@result, AceFit$ACE.ste2@result)
  colnames(stSqPaths) <- paste(rep( c("a", "c", "e"), each=length(selVars)/2), c(1,2), sep="")
  rownames(stSqPaths) <- paste("v", 1:nrow(stSqPaths), sep="")
  print(round(stSqPaths,4))
  
  ## components of rPh
  cat("\n--Component contributions to rPh--\n")
  rACE <- AceFit$ACE.RphACE@result
  colnames(rACE) <- c("A", "C", "E")
  rownames(rACE) <- "contrib"
  print(round(rACE,4))
  
  ## bivariate ACE estimates
  cat("\n--Bivariate ACE estimates--\n")
  cat("(Diagonals: univariate results. Off-diagonals: proportions of rPh.)\n")
  aceEsts <- cbind(AceFit$ACE.h2@result, AceFit$ACE.c2@result, AceFit$ACE.e2@result)
  colnames(aceEsts) <- rep( c("h2", "c2", "e2"), each=length(selVars)/2 )
  rownames(aceEsts) <- paste("v", 1:nrow(aceEsts), sep="")
  print(round(aceEsts,4))
  
  ## correlations
  cat("\n--Correlations--\n")
  corrs <- cbind(AceFit$ACE.Rph@result[2,1], AceFit$ACE.Ra@result[2,1], AceFit$ACE.Rc@result[2,1], AceFit$ACE.Re@result[2,1])
  colnames(corrs) <- c("rPh", "rA", "rC", "rE")
  print(round(corrs,4))
  
  ## full ACE model summary (including CIs)
  cat("\n--ACE model summary--\n")
  print(AceSum)
  
  sink()
  
  
  ### ---------------------------------------------------------------------------
  ### End loop
}
