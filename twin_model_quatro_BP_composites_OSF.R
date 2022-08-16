## clean workspace
rm(list=ls())

setwd("~/Desktop/NSE BP/")

## import dependencies
Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) 
library(OpenMx)
library(psych)
library(foreign)

mxOption(NULL,"Default optimizer","SLSQP")
mxOption(key='Number of Threads', value=parallel::detectCores())

# ------------------------------------------------------------------------------------------------------------
# Read in data file 
data <- read.csv ('data/NSE_BP_cleandata.csv', header=T)	#missing default = "NA"

# ------------------------------------------------------------------------------------------------------------
nv	<- 4    		# number of variables
ntv	<- 2*nv			# number of variables*max family size

# define pairs of variables. For Cholesky analysis, the first variable in each pair
# is the predictor variable, and the second is the predicted variable. Variables are
# assumed to be double-entered, but are listed here WITHOUT a trailing '1' or '2'.
selVars <- c("polyNSE_preschool_NSE_childhood_hyp_parent1",
             "polyNSE_childhood_NSE_adolescence_hyp_parent1",
             "polyNSE_adolescence_NSE_adulthood_hyp_parent1",
             "adulthood_hyp_parent1",
             
             "polyNSE_preschool_NSE_childhood_hyp_parent2",
             "polyNSE_childhood_NSE_adolescence_hyp_parent2",
             "polyNSE_adolescence_NSE_adulthood_hyp_parent2",
             "adulthood_hyp_parent2")

selVars <- c("polyNSE_preschool_NSE_childhood_hyp_parent1",
             "polyNSE_childhood_NSE_adolescence_hyp_parent1",
             "polyNSE_adolescence_NSE_adulthood_hyp_parent1",
             "adulthood_hyp_child1",
             
             "polyNSE_preschool_NSE_childhood_hyp_parent2",
             "polyNSE_childhood_NSE_adolescence_hyp_parent2",
             "polyNSE_adolescence_NSE_adulthood_hyp_parent2",
             "adulthood_hyp_child2")

selVars <- c("polyNSE_preschool_NSE_childhood_con_parent1",
             "polyNSE_childhood_NSE_adolescence_con_parent1",
             "polyNSE_adolescence_NSE_adulthood_con_parent1",
             "adulthood_con_parent1",
             
             "polyNSE_preschool_NSE_childhood_con_parent2",
             "polyNSE_childhood_NSE_adolescence_con_parent2",
             "polyNSE_adolescence_NSE_adulthood_con_parent2",
             "adulthood_con_parent2")

selVars <- c("polyNSE_preschool_NSE_childhood_con_parent1",
             "polyNSE_childhood_NSE_adolescence_con_parent1",
             "polyNSE_adolescence_NSE_adulthood_con_parent1",
             "adulthood_con_child1",
             
             "polyNSE_preschool_NSE_childhood_con_parent2",
             "polyNSE_childhood_NSE_adolescence_con_parent2",
             "polyNSE_adolescence_NSE_adulthood_con_parent2",
             "adulthood_con_child2")

mzData	<- subset(data, zygos==1, selVars)
dzData	<- subset(data, zygos==2, selVars)

# Descriptive Statistics of data by zygosity group
describe(mzData)
describe(dzData)
colMeans(mzData,na.rm=TRUE)
cov(mzData,use="complete")
cor(mzData,use="complete")
colMeans(dzData,na.rm=TRUE)
cov(dzData,use="complete")
cor(dzData,use="complete")

# Create start values 
Stmean <-colMeans(mzData[,1:nv],na.rm=TRUE)

# Create Labels for Lower Triangular Matrices
aLabs <- paste("a", do.call(c, sapply(seq(1, nv), function(x){ paste(x:nv, x,sep="") })), sep="")
cLabs <- paste("c", do.call(c, sapply(seq(1, nv), function(x){ paste(x:nv, x,sep="") })), sep="")
eLabs <- paste("e", do.call(c, sapply(seq(1, nv), function(x){ paste(x:nv, x,sep="") })), sep="")
# Create Labels for Column and Diagonal Matrices
mLabs		<- paste("m",1:nv,sep="")

# (model 2) Specify Cholesky ACE Decomposition 
# -------------------------------------------------------------------------------------------------

# Matrices declared to store a, c, and e Path Coefficients
pathA	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=.4, labels=aLabs, name="a" )
pathC	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=.1, labels=cLabs, name="c" )
pathE	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=.2, labels=eLabs, name="e" )

# Matrices generated to hold A, C, and E computed Variance Components
covA	<- mxAlgebra( expression=a %*% t(a), name="A" )
covC	<- mxAlgebra( expression=c %*% t(c), name="C" ) 
covE	<- mxAlgebra( expression=e %*% t(e), name="E" )

# Algebra to compute total variances and standard deviations (diagonal only)
covP	<- mxAlgebra( expression=A+C+E, name="V" )
matI	<- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
invSD	<- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")

# Algebra to comopute Correlations
invSDa<- mxAlgebra( expression=solve(sqrt(I*A)), name="iSDa")
invSDc<- mxAlgebra( expression=solve(sqrt(I*C)), name="iSDc")
invSDe<- mxAlgebra( expression=solve(sqrt(I*E)), name="iSDe")
Rph	<- mxAlgebra( expression=iSD  %&% V , name="Phcor")
Rg	<- mxAlgebra( expression=iSDa %&% A , name="Acor")
Rc	<- mxAlgebra( expression=iSDc %&% C , name="Ccor")
Re	<- mxAlgebra( expression=iSDe %&% E , name="Ecor")

# standardised paths
sta <- mxAlgebra( expression=iSD %&% a, name="sta")
stc <- mxAlgebra( expression=iSD %&% c, name='stc')
ste <- mxAlgebra( expression=iSD %&% e, name='ste')

# squared standardised paths
sta2 <- mxAlgebra( expression=sta * sta, name='sta2')
stc2 <- mxAlgebra( expression=stc * stc, name='stc2')
ste2 <- mxAlgebra( expression=ste * ste, name='ste2')

# Algebras to compute Standardized Variance Components
rowVars	<- rep('v',nv)
colVars	<- rep(c('A','C','E','h2','c2','e2'),each=nv)
estVars	<- mxAlgebra( expression=cbind(A,C,E,A/V,C/V,E/V), name="est", dimnames=list(rowVars,colVars))

# Algebra to compute standardized variance components separately ENABLING EASE OF CIs COMPUTATION 
StA	<- mxAlgebra( expression=A/V, name='h2')
StC	<- mxAlgebra( expression=C/V, name='c2')
StE	<- mxAlgebra( expression=E/V, name='e2')

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
MeanG	<- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=Stmean, labels=c(mLabs,mLabs), name="expMean" )

covMZ	<- mxAlgebra( expression= rbind( cbind(A+C+E , A+C),
                                       cbind(A+C , A+C+E)), name="expCovMZ" )
covDZ	<- mxAlgebra( expression= rbind( cbind(A+C+E       , 0.5%x%A+C),
                                       cbind(0.5%x%A+C , A+C+E)), name="expCovDZ" )

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ	<- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars )
objDZ	<- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars )

fitFunction	<- mxFitFunctionML()

# Combine Groups
pars		<- list( pathA, pathC, pathE, covA, covC, covE, covP, matI, invSD, invSDa,  invSDc, invSDe, Rph, Rg, Rc, Re, sta, stc, ste, sta2, stc2, ste2, estVars, StA, StC, StE) 
modelMZ	<- mxModel( pars, covMZ, MeanG, dataMZ, objMZ, fitFunction, name="MZ" )
modelDZ	<- mxModel( pars, covDZ, MeanG, dataDZ, objDZ, fitFunction, name="DZ" )
minus2ll	<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<- mxFitFunctionAlgebra( "m2LL" )

# ----------------------------------------------------------------------------------------------------------------------------------
CholAceModel<- mxModel( "CholACE", pars, modelMZ, modelDZ, minus2ll, obj)

#Confidence Intervals DIFFERNET VERSIONS SELECT DEPENDING ON MODEL

#MULTIVARIATE with 4 vars
#SPLIT UP!!!! eg like below
CholAceModel <- mxModel(CholAceModel, mxCI(c('CholACE.sta2[1,1]',
                                             'CholACE.sta2[2,1]',
                                             'CholACE.sta2[3,1]',
                                             'CholACE.sta2[4,1]',
                                             'CholACE.sta2[2,2]',
                                             'CholACE.sta2[3,2]',
                                             'CholACE.sta2[4,2]',
                                             'CholACE.sta2[3,3]',
                                             'CholACE.sta2[4,3]',
                                             'CholACE.sta2[4,4]')))

CholAceModel <- mxModel(CholAceModel, mxCI(c('CholACE.stc2[1,1]',
                                             'CholACE.stc2[2,1]',
                                             'CholACE.stc2[3,1]',
                                             'CholACE.stc2[4,1]',
                                             'CholACE.stc2[2,2]',
                                             'CholACE.stc2[3,2]',
                                             'CholACE.stc2[4,2]',
                                             'CholACE.stc2[3,3]',
                                             'CholACE.stc2[4,3]',
                                             'CholACE.stc2[4,4]')))

CholAceModel <- mxModel(CholAceModel, mxCI(c('CholACE.ste2[1,1]',
                                             'CholACE.ste2[2,1]',
                                             'CholACE.ste2[3,1]',
                                             'CholACE.ste2[4,1]',
                                             'CholACE.ste2[2,2]',
                                             'CholACE.ste2[3,2]',
                                             'CholACE.ste2[4,2]',
                                             'CholACE.ste2[3,3]',
                                             'CholACE.ste2[4,3]',
                                             'CholACE.ste2[4,4]')))

# ----------------------------------------------------------------------------------------------------------------------------------
# (model 2) RUN Cholesky Decomposition ACE Model

CholAceFit <- mxRun(CholAceModel, intervals=T)
ChoCholAceSumm	<- summary(CholAceFit, verbose=T)
ChoCholAceSumm
round(CholAceFit@output$estimate,4)
round(CholAceFit$est@result,4)

# Print the rG, rC, rE matrices (above, in the summary output you will find them with 95% CI)
CholAceFit$Acor
CholAceFit$Ccor
CholAceFit$Ecor
CholAceFit$Phcor

# Print unstandardised and standardised paths matrices
CholAceFit$sta
CholAceFit$stc
CholAceFit$ste
CholAceFit$sta2
CholAceFit$stc2
CholAceFit$ste2
