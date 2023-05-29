#Conditional Permutation ImportanceによるTI値の算出

library(randomForest)
library(permimp)

data =read.delim(file.choose(), header=TRUE)

data$zero <- as.factor(data$zero)
model = randomForest(zero ~ ., data = data, ntree=10000)

permimp(model)

#ベイズモデルによる分析

library(brms)
library(bayesplot)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

data=read.delim(file.choose(), row.name=NULL)

LSJ=brm(oleh~A_animacy*corpus+A_number_sd+A_definite*corpus+TI_sd+(A_animacy||X)
                  ,family=bernoulli(),data=data,seed=1, prior=c(set_prior("",class="Intercept")), control = list(adapt_delta = 0.95, max_treedepth=15),iter = 10000,
                  warmup = 8000, thin=4)

stanplot(LSJ,type='intervals',pars='^b_',prob=0.8,prob_outer=0.95)

brms::conditional_effects(LSJ, categorical=FALSE)
