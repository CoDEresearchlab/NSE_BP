###
# MZ differences
# Aga
# 08/12/21
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
library(ggplot2)
library(cowplot)
library(reshape2)
library(likert)
library(pastecs)
library(ggsci)
library(gridExtra)
library(tidyverse)

# ===== Load Data: TEDS data prepared as for twin analyses (age & sex regressed) =====
data <- read.csv("./data/NSE_BP_cleandata.csv")
dim(data) #13943  2490
names(data)
labels(data)
#describe (AllData)

# ===== Select MZ twins only =====
data <- data[data$zygos == 1,]

# ===== Approach 1: based on difference scores: preschool NSE & childhood BP =====
## Preschool NSE difference scores
data$diff_polyNSE_preschool_NSE_childhood_hyp_parent <- data$polyNSE_preschool_NSE_childhood_hyp_parent1 - data$polyNSE_preschool_NSE_childhood_hyp_parent2
data$diff_polyNSE_preschool_NSE_childhood_con_parent <- data$polyNSE_preschool_NSE_childhood_con_parent1 - data$polyNSE_preschool_NSE_childhood_con_parent2
data$diff_polyNSE_preschool_NSE_childhood_ep_parent <- data$polyNSE_preschool_NSE_childhood_ep_parent1 - data$polyNSE_preschool_NSE_childhood_ep_parent2
data$diff_polyNSE_preschool_NSE_childhood_pp_parent <- data$polyNSE_preschool_NSE_childhood_pp_parent1 - data$polyNSE_preschool_NSE_childhood_pp_parent2

## Childhood BP difference scores
data$diff_childhood_hyp_parent <- data$childhood_hyp_parent1 - data$childhood_hyp_parent2
data$diff_childhood_con_parent <- data$childhood_con_parent1 - data$childhood_con_parent2
data$diff_childhood_ep_parent <- data$childhood_ep_parent1 - data$childhood_ep_parent2
data$diff_childhood_pp_parent <- data$childhood_pp_parent1 - data$childhood_pp_parent2

data$diff_childhood_hyp_teacher <- data$childhood_hyp_teacher1 - data$childhood_hyp_teacher2
data$diff_childhood_con_teacher <- data$childhood_con_teacher1 - data$childhood_con_teacher2
data$diff_childhood_ep_teacher <- data$childhood_ep_teacher1 - data$childhood_ep_teacher2
data$diff_childhood_pp_teacher <- data$childhood_pp_teacher1 - data$childhood_pp_teacher2

data$diff_childhood_hyp_child <- data$childhood_hyp_child1 - data$childhood_hyp_child2
data$diff_childhood_con_child <- data$childhood_con_child1 - data$childhood_con_child2
data$diff_childhood_ep_child <- data$childhood_ep_child1 - data$childhood_ep_child2

## Correlate
hyp <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_hyp_parent, data$diff_childhood_hyp_parent)
hyp$r*hyp$r #2.7%

con <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_con_parent, data$diff_childhood_con_parent)
con$r*con$r #2.1%

ep <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_ep_parent, data$diff_childhood_ep_parent)
ep$r*ep$r #0.4%

pp <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_pp_parent, data$diff_childhood_pp_parent)
pp$r*pp$r #0.9%

hyp <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_hyp_parent, data$diff_childhood_hyp_teacher)
hyp$r*hyp$r #0.1%

con <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_con_parent, data$diff_childhood_con_teacher)
con$r*con$r #0.01%

ep <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_ep_parent, data$diff_childhood_ep_teacher)
ep$r*ep$r #0.4%

pp <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_pp_parent, data$diff_childhood_pp_teacher)
pp$r*pp$r #0.2%

hyp <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_hyp_parent, data$diff_childhood_hyp_child)
hyp$r*hyp$r #0.5%

con <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_con_parent, data$diff_childhood_con_child)
con$r*con$r #0.5%

ep <- rcorr(data$diff_polyNSE_preschool_NSE_childhood_ep_parent, data$diff_childhood_ep_child)
ep$r*ep$r #0.1%

# ===== Approach 1: based on difference scores: childhood NSE & adolescence BP =====
## Childhood NSE difference scores
# Parent
data$diff_polyNSE_childhood_NSE_adolescence_hyp_parent <- data$polyNSE_childhood_NSE_adolescence_hyp_parent1 - data$polyNSE_childhood_NSE_adolescence_hyp_parent2
data$diff_polyNSE_childhood_NSE_adolescence_con_parent <- data$polyNSE_childhood_NSE_adolescence_con_parent1 - data$polyNSE_childhood_NSE_adolescence_con_parent2
data$diff_polyNSE_childhood_NSE_adolescence_ep_parent <- data$polyNSE_childhood_NSE_adolescence_ep_parent1 - data$polyNSE_childhood_NSE_adolescence_ep_parent2
data$diff_polyNSE_childhood_NSE_adolescence_pp_parent <- data$polyNSE_childhood_NSE_adolescence_pp_parent1 - data$polyNSE_childhood_NSE_adolescence_pp_parent2

## Adolescence BP difference scores
# Parent
data$diff_adolescence_hyp_parent <- data$adolescence_hyp_parent1 - data$adolescence_hyp_parent2
data$diff_adolescence_con_parent <- data$adolescence_con_parent1 - data$adolescence_con_parent2
data$diff_adolescence_ep_parent <- data$adolescence_ep_parent1 - data$adolescence_ep_parent2
data$diff_adolescence_pp_parent <- data$adolescence_pp_parent1 - data$adolescence_pp_parent2

# Teacher
data$diff_adolescence_hyp_teacher <- data$adolescence_hyp_teacher1 - data$adolescence_hyp_teacher2
data$diff_adolescence_con_teacher <- data$adolescence_con_teacher1 - data$adolescence_con_teacher2
data$diff_adolescence_ep_teacher <- data$adolescence_ep_teacher1 - data$adolescence_ep_teacher2
data$diff_adolescence_pp_teacher <- data$adolescence_pp_teacher1 - data$adolescence_pp_teacher2

# Child
data$diff_adolescence_hyp_child <- data$adolescence_hyp_child1 - data$adolescence_hyp_child2
data$diff_adolescence_con_child <- data$adolescence_con_child1 - data$adolescence_con_child2
data$diff_adolescence_ep_child <- data$adolescence_ep_child1 - data$adolescence_ep_child2
data$diff_adolescence_pp_child <- data$adolescence_pp_child1 - data$adolescence_pp_child2

## Correlate
# Parent
hyp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_hyp_parent, data$diff_adolescence_hyp_parent)
hyp$r*hyp$r #8.1%

con <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_con_parent, data$diff_adolescence_con_parent)
con$r*con$r #6.2%

ep <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_ep_parent, data$diff_adolescence_ep_parent)
ep$r*ep$r #4.4%

pp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_pp_parent, data$diff_adolescence_pp_parent)
pp$r*pp$r #5.7%

# Teacher
hyp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_hyp_parent, data$diff_adolescence_hyp_teacher)
hyp$r*hyp$r #2.0%

con <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_con_parent, data$diff_adolescence_con_teacher)
con$r*con$r #0.6%

ep <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_ep_parent, data$diff_adolescence_ep_teacher)
ep$r*ep$r #0.002%

pp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_pp_parent, data$diff_adolescence_pp_teacher)
pp$r*pp$r #0.1%

# Child
hyp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_hyp_parent, data$diff_adolescence_hyp_child)
hyp$r*hyp$r #2.2%

con <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_con_parent, data$diff_adolescence_con_child)
con$r*con$r #3.7%

ep <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_ep_parent, data$diff_adolescence_ep_child)
ep$r*ep$r #1.6%

pp <- rcorr(data$diff_polyNSE_childhood_NSE_adolescence_pp_parent, data$diff_adolescence_pp_child)
pp$r*pp$r #0.4%

# ===== Approach 1: based on difference scores: adolescence NSE & adulthood BP =====
## Adolescence NSE difference scores
# Parent
data$diff_polyNSE_adolescence_NSE_adulthood_hyp_parent <- data$polyNSE_adolescence_NSE_adulthood_hyp_parent1 - data$polyNSE_adolescence_NSE_adulthood_hyp_parent2
data$diff_polyNSE_adolescence_NSE_adulthood_con_parent <- data$polyNSE_adolescence_NSE_adulthood_con_parent1 - data$polyNSE_adolescence_NSE_adulthood_con_parent2

## Adulthood BP difference scores
# Parent
data$diff_adulthood_hyp_parent <- data$adulthood_hyp_parent1 - data$adulthood_hyp_parent2
data$diff_adulthood_con_parent <- data$adulthood_con_parent1 - data$adulthood_con_parent2

# Child
data$diff_adulthood_hyp_child <- data$adulthood_hyp_child1 - data$adulthood_hyp_child2
data$diff_adulthood_con_child <- data$adulthood_con_child1 - data$adulthood_con_child2

## Correlate
# Parent
hyp <- rcorr(data$diff_polyNSE_adolescence_NSE_adulthood_hyp_parent, data$diff_adulthood_hyp_parent)
hyp$r*hyp$r #0.8%

con <- rcorr(data$diff_polyNSE_adolescence_NSE_adulthood_con_parent, data$diff_adulthood_con_parent)
con$r*con$r #0.3%

# Child
hyp <- rcorr(data$diff_polyNSE_adolescence_NSE_adulthood_hyp_parent, data$diff_adulthood_hyp_child)
hyp$r*hyp$r #0.05%

con <- rcorr(data$diff_polyNSE_adolescence_NSE_adulthood_con_parent, data$diff_adulthood_con_child)
con$r*con$r #0.2%

# ===== Approach 2: based on residuals: preschool NSE & childhood BP =====
## Add dummy for easy regression
data$dummy <- 1

## Preschool NSE residuals
data[, c("resi_polyNSE_preschool_NSE_childhood_hyp_parent","dummy")] <- apply(
  data[, c("polyNSE_preschool_NSE_childhood_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_preschool_NSE_childhood_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_preschool_NSE_childhood_con_parent","dummy")] <- apply(
  data[, c("polyNSE_preschool_NSE_childhood_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_preschool_NSE_childhood_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_preschool_NSE_childhood_ep_parent","dummy")] <- apply(
  data[, c("polyNSE_preschool_NSE_childhood_ep_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_preschool_NSE_childhood_ep_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_preschool_NSE_childhood_pp_parent","dummy")] <- apply(
  data[, c("polyNSE_preschool_NSE_childhood_pp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_preschool_NSE_childhood_pp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

## Childhood BP residuals
data[, c("resi_childhood_hyp_parent","dummy")] <- apply(
  data[, c("childhood_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_con_parent","dummy")] <- apply(
  data[, c("childhood_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_ep_parent","dummy")] <- apply(
  data[, c("childhood_ep_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_ep_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_pp_parent","dummy")] <- apply(
  data[, c("childhood_pp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_pp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_hyp_teacher","dummy")] <- apply(
  data[, c("childhood_hyp_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_hyp_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_con_teacher","dummy")] <- apply(
  data[, c("childhood_con_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_con_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_ep_teacher","dummy")] <- apply(
  data[, c("childhood_ep_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_ep_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_pp_teacher","dummy")] <- apply(
  data[, c("childhood_pp_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_pp_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_hyp_child","dummy")] <- apply(
  data[, c("childhood_hyp_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_hyp_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_con_child","dummy")] <- apply(
  data[, c("childhood_con_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_con_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_childhood_ep_child","dummy")] <- apply(
  data[, c("childhood_ep_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$childhood_ep_child2,
        na.action=na.exclude
      )
    )
  }	
)

## Correlate
hyp <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_hyp_parent, data$resi_childhood_hyp_parent)
hyp$r*hyp$r # 3.1%

con <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_con_parent, data$resi_childhood_con_parent)
con$r*con$r # 2.9%

ep <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_ep_parent, data$resi_childhood_ep_parent)
ep$r*ep$r # 0.7%

pp <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_pp_parent, data$resi_childhood_pp_parent)
pp$r*pp$r # 1.3%

hyp <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_hyp_parent, data$resi_childhood_hyp_teacher)
hyp$r*hyp$r # 0.3%

con <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_con_parent, data$resi_childhood_con_teacher)
con$r*con$r # 0.07%

ep <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_ep_parent, data$resi_childhood_ep_teacher)
ep$r*ep$r # 0.4%

pp <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_pp_parent, data$resi_childhood_pp_teacher)
pp$r*pp$r # 0.07%

hyp <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_hyp_parent, data$resi_childhood_hyp_child)
hyp$r*hyp$r # 0.5%

con <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_con_parent, data$resi_childhood_con_teacher)
con$r*con$r # 0.07%

ep <- rcorr(data$resi_polyNSE_preschool_NSE_childhood_ep_parent, data$resi_childhood_ep_child)
ep$r*ep$r # 0.2%

# ===== Approach 2: based on residuals: childhood NSE & adolescence BP =====
## Add dummy for easy regression
data$dummy <- 1

## Childhood NSE residuals
# Parent ratings
data[, c("resi_polyNSE_childhood_NSE_adolescence_hyp_parent","dummy")] <- apply(
  data[, c("polyNSE_childhood_NSE_adolescence_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_childhood_NSE_adolescence_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_childhood_NSE_adolescence_con_parent","dummy")] <- apply(
  data[, c("polyNSE_childhood_NSE_adolescence_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_childhood_NSE_adolescence_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_childhood_NSE_adolescence_ep_parent","dummy")] <- apply(
  data[, c("polyNSE_childhood_NSE_adolescence_ep_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_childhood_NSE_adolescence_ep_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_childhood_NSE_adolescence_pp_parent","dummy")] <- apply(
  data[, c("polyNSE_childhood_NSE_adolescence_pp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_childhood_NSE_adolescence_pp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

## Adolescence BP residuals
# Parent ratings
data[, c("resi_adolescence_hyp_parent","dummy")] <- apply(
  data[, c("adolescence_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_con_parent","dummy")] <- apply(
  data[, c("adolescence_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_ep_parent","dummy")] <- apply(
  data[, c("adolescence_ep_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_ep_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_pp_parent","dummy")] <- apply(
  data[, c("adolescence_pp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_pp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

# Teacher ratings
data[, c("resi_adolescence_hyp_teacher","dummy")] <- apply(
  data[, c("adolescence_hyp_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_hyp_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_con_teacher","dummy")] <- apply(
  data[, c("adolescence_con_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_con_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_ep_teacher","dummy")] <- apply(
  data[, c("adolescence_ep_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_ep_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_pp_teacher","dummy")] <- apply(
  data[, c("adolescence_pp_teacher1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_pp_teacher2,
        na.action=na.exclude
      )
    )
  }	
)

# Child ratings
data[, c("resi_adolescence_hyp_child","dummy")] <- apply(
  data[, c("adolescence_hyp_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_hyp_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_con_child","dummy")] <- apply(
  data[, c("adolescence_con_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_con_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_ep_child","dummy")] <- apply(
  data[, c("adolescence_ep_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_ep_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adolescence_pp_child","dummy")] <- apply(
  data[, c("adolescence_pp_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adolescence_pp_child2,
        na.action=na.exclude
      )
    )
  }	
)

## Correlate
# Parent
hyp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_hyp_parent, data$resi_adolescence_hyp_parent)
hyp$r*hyp$r #9.5%

con <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_con_parent, data$resi_adolescence_con_parent)
con$r*con$r #6.6%

ep <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_ep_parent, data$resi_adolescence_ep_parent)
ep$r*ep$r #5.3%

pp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_pp_parent, data$resi_adolescence_pp_parent)
pp$r*pp$r #7.5%

# Teacher
hyp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_hyp_parent, data$resi_adolescence_hyp_teacher)
hyp$r*hyp$r #3.6%

con <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_con_parent, data$resi_adolescence_con_teacher)
con$r*con$r #1.0%

ep <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_ep_parent, data$resi_adolescence_ep_teacher)
ep$r*ep$r #0.2%

pp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_pp_parent, data$resi_adolescence_pp_teacher)
pp$r*pp$r #1.4%

# Child
hyp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_hyp_parent, data$resi_adolescence_hyp_child)
hyp$r*hyp$r #2.3%

con <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_con_parent, data$resi_adolescence_con_child)
con$r*con$r #3.8%

ep <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_ep_parent, data$resi_adolescence_ep_child)
ep$r*ep$r #1.5%

pp <- rcorr(data$resi_polyNSE_childhood_NSE_adolescence_pp_parent, data$resi_adolescence_pp_child)
pp$r*pp$r #1.0%

# ===== Approach 2: based on residuals: adolescence NSE & adulthood BP =====
## Add dummy for easy regression
data$dummy <- 1

## Adolescence NSE residuals
# Parent ratings
data[, c("resi_polyNSE_adolescence_NSE_adulthood_hyp_parent","dummy")] <- apply(
  data[, c("polyNSE_adolescence_NSE_adulthood_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_adolescence_NSE_adulthood_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_polyNSE_adolescence_NSE_adulthood_con_parent","dummy")] <- apply(
  data[, c("polyNSE_adolescence_NSE_adulthood_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$polyNSE_adolescence_NSE_adulthood_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

## Adulthood BP residuals
# Parent ratings
data[, c("resi_adulthood_hyp_parent","dummy")] <- apply(
  data[, c("adulthood_hyp_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adulthood_hyp_parent2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adulthood_con_parent","dummy")] <- apply(
  data[, c("adulthood_con_parent1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adulthood_con_parent2,
        na.action=na.exclude
      )
    )
  }	
)

# Child ratings
data[, c("resi_adulthood_hyp_child","dummy")] <- apply(
  data[, c("adulthood_hyp_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adulthood_hyp_child2,
        na.action=na.exclude
      )
    )
  }	
)

data[, c("resi_adulthood_con_child","dummy")] <- apply(
  data[, c("adulthood_con_child1","dummy")],
  2,
  function(x){
    rstandard(
      lm(
        x ~ data$adulthood_con_child2,
        na.action=na.exclude
      )
    )
  }	
)

## Correlate
# Parent
hyp <- rcorr(data$resi_polyNSE_adolescence_NSE_adulthood_hyp_parent, data$resi_adulthood_hyp_parent)
hyp$r*hyp$r #1.4%

con <- rcorr(data$resi_polyNSE_adolescence_NSE_adulthood_con_parent, data$resi_adulthood_con_parent)
con$r*con$r #0.8%

# Child
hyp <- rcorr(data$resi_polyNSE_adolescence_NSE_adulthood_hyp_parent, data$resi_adulthood_hyp_child)
hyp$r*hyp$r #0.2%

con <- rcorr(data$resi_polyNSE_adolescence_NSE_adulthood_con_parent, data$resi_adulthood_con_child)
con$r*con$r #0.5%