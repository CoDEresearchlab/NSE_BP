#####
#Univariate twin model fitting NSE BP
#29/07/2021
#####

## import dependencies
library(OpenMx)
library(psych)
library(foreign)

## set working directory
setwd('~/Desktop/NSE BP/')

## input/output configuration
inFile <- './data/NSE_BP_cleandata.csv'				# prepared data to analyse
SPSSFile <- './data/504 Aga Bubel NSE version3 Mar2022.sav'				# only for variable labels
outFileStem <- './twin_res/twin_univ_sexes_combined/'
# stem of output filenames

## open raw data and extract variable labels
rawDat <- read.spss('data/504 Aga Bubel NSE version3 Mar2022.sav', use.value.labels=F, to.data.frame=T)
labels <- attr(rawDat, 'variable.labels')

## open cleaned data
dat <- read.csv(
  inFile
)

## variables to test (assumed to be double-entered, but listed here WITHOUT the trailing '1' or '2
Vars <- c("preschool_hyp", "preschool_con", "preschool_ep", "preschool_pp", 
          "childhood_hyp_parent", "childhood_con_parent", "childhood_ep_parent", "childhood_pp_parent", 
          "childhood_hyp_teacher", "childhood_con_teacher", "childhood_ep_teacher", "childhood_pp_teacher", 
          "childhood_hyp_child", "childhood_con_child", "childhood_ep_child", 
          "adolescence_hyp_parent", "adolescence_con_parent", "adolescence_ep_parent", "adolescence_pp_parent", 
          "adolescence_hyp_teacher", "adolescence_con_teacher", 
          "adolescence_ep_teacher", "adolescence_pp_teacher", 
          "adolescence_hyp_child", "adolescence_con_child", "adolescence_ep_child", "adolescence_pp_child",
          "adulthood_hyp_parent", "adulthood_con_parent", "adulthood_ep_parent", "adulthood_pp_parent", 
          "adulthood_hyp_child", "adulthood_con_child", "adulthood_ep_child", "adulthood_pp_child")

Vars <- c("childhood_pp_child")
## explore dataset
#names(dat)
#summary(dat)
#describe(dat)
#str(dat)

### ---------------------------------------------------------------------------
### Start loop (repeat for each variable in Vars)

for (currentVar in Vars) {
  
  ## report progress
  cat("\n", paste("--- Starting run for", currentVar, "- variable", which(Vars==currentVar), "of", length(Vars)), "---\n")
  
  ## select variables and data
  nv			<- 1  # number of variables for a twin (i.e., 1 for a univariate model)
  ntv			<- nv*2  # number of variables for a twin pair (2 * nv)
  
  selVars		<- paste(currentVar, c(1,2), sep="")
  
  mzData		<- subset( dat, zygos == 1, selVars )
  dzData		<- subset( dat, zygos == 2, selVars )
  
  
  ## -----------------------------------------------------------------------
  ## Print descriptive statistics by zygosity group
  #
  describe(mzData)
  colMeans(mzData,na.rm=TRUE)
  cov(mzData,use="complete")
  cor(mzData,use="complete")
  describe(dzData)
  colMeans(dzData,na.rm=TRUE)
  cov(dzData,use="complete")
  cor(dzData,use="complete")
  
  
  ## -----------------------------------------------------------------------
  ## Graph descriptive statistics by zygosity group
  #
  #par(mfcol=c(1,2))
  #hist(dat[,selVars[1]][dat$rzygos==1])
  #hist(dat[,selVars[2]][dat$rzygos==1])
  #hist(dat[,selVars[1]][dat$rzygos==2])
  #hist(dat[,selVars[2]][dat$rzygos==2])
  #plot(dat[,selVars[1]][dat$rzygos==1],dat[,selVars[2]][dat$rzygos==1])
  #plot(dat[,selVars[1]][dat$rzygos==2],dat[,selVars[2]][dat$rzygos==2])
  
  
  ## -----------------------------------------------------------------------
  ## Create starting values
  
  ## means
  mzMeanStart	<- mean(mzData[, selVars[1]], na.rm=T)
  dzMeanStart	<- mean(dzData[, selVars[1]], na.rm=T)
  
  ## covariances
  mzCovStart	<- cov(mzData[, selVars], use="complete")[1,2]
  dzCovStart	<- cov(dzData[, selVars], use="complete")[1,2]
  
  ## variance components (for the ACE model)
  
  # (MZ/DZ correlations)
  rMZ			<- cor(mzData,use="complete")
  rDZ			<- cor(dzData,use="complete")
  
  # (Falconer's formulae)
  fA			<- 2*(rMZ[2,1] - rDZ[2,1])
  fC			<- rMZ[2,1] - fA
  fE			<- 1 - rMZ[2,1]
  
  # (ensure no impossible/problematic values)
  if (fA < 0) { fA = 0 } else if (fA >= 0.95) { fA = 0.9 }
  if (fC < 0) { fC = 0 } else if (fC >= 0.95) { fC = 0.9 }
  if (fE < 0) { fE = 0 } else if (fE >= 0.95) { fE = 0.9 }
  
  # (Variance component path values: sqrt(component value * variance). For the
  # ACE model there are assumed to be no variance differences between twin 1
  # and twin 2, so use either)
  V			<- var(dat[, selVars[1]], na.rm=T)
  
  aStart		<- sqrt(fA * V)
  cStart		<- sqrt(fC * V)
  eStart		<- sqrt(fE * V)
  
  ## offset values (so the model doesn't converge too quickly)
  mzMeanStart = mzMeanStart + (mzMeanStart / 10)
  dzMeanStart = dzMeanStart + (dzMeanStart / 10)
  mzCovStart = mzCovStart + (mzCovStart / 10)
  dzCovStart = dzCovStart + (dzCovStart / 10)
  aStart = aStart + 0.05
  cStart = cStart + 0.05
  eStart = eStart + 0.05
  
  
  ## -----------------------------------------------------------------------
  ## 1. Specify and run a fully saturated model (Cholesky decomposition)
  
  ## matrix and algebra for expected means and covariances
  CholMZ		<-mxMatrix( type="Lower", nrow=ntv, ncol=ntv, free=T, values=mzCovStart, name="lowMZ" )
  CholDZ		<-mxMatrix( type="Lower", nrow=ntv, ncol=ntv, free=T, values=dzCovStart, name="lowDZ" )
  MZCov		<-mxAlgebra( expression=lowMZ %*% t(lowMZ), name="expCovMZ" )
  DZCov		<-mxAlgebra( expression=lowDZ %*% t(lowDZ), name="expCovDZ" )
  
  MZMeans		<-mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=mzMeanStart, labels=c("Mmz1","Mmz2"), name="expMeanMZ" )
  DZMeans		<-mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=dzMeanStart, labels=c("Mdz1","Mdz2"), name="expMeanDZ" )
  
  ## algebras needed for standardizing the covariances
  matI		<-mxMatrix( type="Iden", nrow=ntv, ncol=ntv, name="I")
  MZcor		<-mxAlgebra( expression= solve(sqrt(I*expCovMZ)) %*% expCovMZ %*% solve(sqrt(I*expCovMZ)), name="rMZ" )
  DZcor		<-mxAlgebra( expression= solve(sqrt(I*expCovDZ)) %*% expCovDZ %*% solve(sqrt(I*expCovDZ)), name="rDZ" )
  
  ## data objects for multiple groups
  dataMZ		<- mxData( observed=mzData, type="raw" )
  dataDZ		<- mxData( observed=dzData, type="raw" )
  
  ## objective objects for multiple groups
  ## mxExpectationNormal: objective functions which uses Full Information Maximum
  ## Likelihood, the preferred method for raw data.
  ## Objective functions are functions for which free parameter values are chosen
  ## such that the value of the objective function is minimized.
  objMZ		<- mxExpectationNormal( covariance="expCovMZ", means="expMeanMZ", dimnames=selVars)
  objDZ		<- mxExpectationNormal( covariance="expCovDZ", means="expMeanDZ", dimnames=selVars)
  
  fitFunction	<- mxFitFunctionML()
  
  ## combine groups
  modelMZ		<- mxModel( CholMZ, MZCov, MZMeans, matI, MZcor, dataMZ, objMZ, fitFunction, name="MZ")
  modelDZ		<- mxModel( CholDZ, DZCov, DZMeans, matI, DZcor, dataDZ, objDZ, fitFunction, name="DZ")
  minus2ll	<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
  obj			<- mxFitFunctionAlgebra( "m2LL" )
  Conf		<- mxCI (c ('MZ.rMZ[2,1]', 'DZ.rDZ[2,1]') )
  SatModel	<- mxModel( "Sat", modelMZ, modelDZ, minus2ll, obj, Conf)
  
  ## run saturated model
  SatFit		<- mxRun(SatModel, intervals=T)
  (SatSumm	<- summary(SatFit))
  
  ## generate saturated output
  #SatFit$MZ$expCovMZ
  #SatFit$DZ$expCovDZ
  #SatFit$MZ$rMZ
  #SatFit$DZ$rDZ
  #
  #mxEval(MZ.expMeanMZ, SatFit)
  #mxEval(MZ.expCovMZ, SatFit)
  #mxEval(DZ.expMeanDZ, SatFit)
  #mxEval(DZ.expCovDZ, SatFit)
  
  
  ## -----------------------------------------------------------------------
  ## 2. Specify and run full ACE Model
  
  ## matrix and algebra for expected means vector
  meanG		<- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=mzMeanStart, label="M", name="expMean" )
  
  ## matrices to store a, c, and e path coefficients
  pathA		<-mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=aStart, label="a11", name="a" )
  pathC		<-mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=cStart, label="c11", name="c" )
  pathE		<-mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=eStart, label="e11", name="e" )
  
  ## algebra to generate matrices to hold A, C, and E computed variance components
  covA		<-mxAlgebra( expression=a %*% t(a), name="A" )
  covC		<-mxAlgebra( expression=c %*% t(c), name="C" )
  covE		<-mxAlgebra( expression=e %*% t(e), name="E" )
  
  ## algebra to compute total variance
  covP		<-mxAlgebra( expression=A+C+E, name="V" )
  
  ## algebra to get '1/sd'
  Id			<-mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
  SD			<-mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")
  
  ## algebra to compute standardized path estimates
  stA			<-mxAlgebra( expression=a%*%iSD, name="sta")
  stC			<-mxAlgebra( expression=c%*%iSD, name="stc")
  stE			<-mxAlgebra( expression=e%*%iSD, name="ste")
  
  ## algebra to compute standardized variance components
  h2			<-mxAlgebra( expression=A/V, name="h2")
  c2			<-mxAlgebra( expression=C/V, name="c2")
  e2			<-mxAlgebra( expression=E/V, name="e2")
  
  ## algebra to generate a matrix to hold (standardized) estimates
  rowVars		<-Vars
  colVars		<-rep(c('A','C','E','h2','c2','e2'),each=nv)
  estVars		<-mxAlgebra( expression=cbind(A,C,E,A/V,C/V,E/V), name="Est", dimnames=list(rowVars,colVars))
  
  ## algebra for expected variance/covariance matrices in MZ & DZ twins
  covMZ		<-mxAlgebra( expression= rbind( cbind(A+C+E,A+C), cbind(A+C,A+C+E)  ), 		name="expCovMZ" )
  covDZ		<-mxAlgebra( expression= rbind( cbind(A+C+E,0.5%x%A+C),cbind(0.5%x%A+C,A+C+E)),	name="expCovDZ" )
  
  ## data objects for multiple groups
  dataMZ		<-mxData( observed=mzData, type="raw" )
  dataDZ		<-mxData( observed=dzData, type="raw" )
  
  ## objective objects for multiple groups
  objMZ		<- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars)
  objDZ		<- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars)
  
  fitFunction	<- mxFitFunctionML()
  
  ## combine groups
  params		<-	list(meanG, pathA, pathC, pathE, covA, covC, covE, covP, Id, SD, stA, stC, stE, h2, c2, e2, estVars, fitFunction )
  modelMZ		<-mxModel( params, covMZ, dataMZ, objMZ, name="MZ" )
  modelDZ		<-mxModel( params, covDZ, dataDZ, objDZ, name="DZ" )
  minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
  obj			<-mxFitFunctionAlgebra( "m2LL" )
  Conf		<-mxCI (c ('MZ.h2[1,1]', 'MZ.c2[1,1]', 'MZ.e2[1,1]') )
  ACEModel   	<-mxModel( "ACE", params, modelMZ, modelDZ, minus2ll, obj, Conf) 
  
  ## run full ACE model
  ACEFit		<-mxRun(ACEModel, intervals=T)
  (ACESum		<-summary(ACEFit))
  
  ## generate ACE output
  #ACEFit$ACE.h2
  #ACEFit$ACE.c2
  #ACEFit$ACE.e2
  #round(ACEFit@output$estimate,4)
  #round(ACEFit$Est@result,4)
  
  
  ## -----------------------------------------------------------------------
  ## 3. Specify and run sub-model: AE
  
  AEModel		<-mxModel( ACEFit, name="AE")
  AEModel		<-omxSetParameters( AEModel, labels="c11", free=F, values=0 )
  AEFit		<-mxRun(AEModel, intervals=F)
  (AESum		<-summary (AEFit))
  
  round(AEFit@output$estimate,4)
  round(AEFit$Est@result,4)
  
  
  ## -----------------------------------------------------------------------
  # 4. Specify and run sub-model: CE
  
  CEModel		<-mxModel( ACEFit, name="CE")
  CEModel		<-omxSetParameters( CEModel, labels="a11", free=F, values=0 )
  CEFit		<-mxRun(CEModel, intervals=F)
  (CESum		<-summary (CEFit))
  
  round(CEFit@output$estimate,4)
  round(CEFit$Est@result,4)
  
  
  ## -----------------------------------------------------------------------
  # 5. Specify and run sub-model: E
  
  EModel		<- mxModel( ACEFit, name="E")
  EModel		<- omxSetParameters( EModel, labels=c("a11","c11"), free=F, values=0 )
  EFit		<- mxRun(EModel)
  (ESum		<- summary(EFit))
  
  round(EFit@output$estimate,4)
  round(EFit$Est@result,4)
  
  
  ## -----------------------------------------------------------------------
  ## Compare models: fit statistics
  ##
  ## Note: the [2,1] within the compare command  will suppress the fit of the first model
  ## (ACE) to be printed for that comparison (to avoid repeats in the output table)
  #
  Nested.fit <- 	rbind(
    mxCompare(SatFit, ACEFit),
    mxCompare(ACEFit, AEFit)[2,],
    mxCompare(ACEFit, CEFit)[2,],
    mxCompare(ACEFit, EFit)[2,]    ) 
  
  
  ## -----------------------------------------------------------------------
  ## Output results to file
  
  sink( paste(outFileStem, currentVar, sep="") , append=T)
  cat( paste(currentVar, ": ", labels[paste(currentVar, "1", sep="")], sep="") )
  cat("\n\nMODEL FITTING: Univariate ACE\n\n")
  print(ACESum)
  
  cat("\nMODEL FITTING: Univariate comparative fit statistics\n(saturated/ACE, ACE/AE, ACE/CE, ACE/E)\n\n")
  print(
    rbind(
      mxCompare(SatFit, ACEFit),
      mxCompare(ACEFit, AEFit)[2,],
      mxCompare(ACEFit, CEFit)[2,],
      mxCompare(ACEFit, EFit)[2,]
    ))
  
  sink()
  
  write.csv(Nested.fit, paste(outFileStem, "nested_fit_.csv", sep=""))
  
  
  # (Some p values from the submodel comparison are very small. Consider making them
  # human-readable with something like: lapply(valuesList, round, digits=3).)
  
  
  ### ---------------------------------------------------------------------------
  ### End loop
}

