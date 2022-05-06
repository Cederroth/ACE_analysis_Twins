###########################################################
# Analysis tinnitus twin concordance registry data Sweden #
# Liability threshold / ACDE model                        #
###########################################################

# Tinnitus study by Cederroth et al 
# Data by Jan Bulla, 
# October 2016

library(mets)

# wd, packages                                                  
setwd("D:/Dropbox/Analysis/Consulting/Tinnet/2016 Twin registry")
setwd("C:/Users/Jan/Box Sync/STR")



require(xlsx)
require(foreign)
require(lmtest)



# data preparation
# ----------------
                  
# read data
# salt and stage merged
dat <- read.csv2("Data/STAGE_SALT_merged_new2.csv", header = TRUE)
sum(is.na(dat$alt_twinnr.2))

# data preparation
# flag for age exclusion: by.diff.age or by.age.group
flag.age <- "by.diff.age" # by.age.group by.diff.age
if (flag.age == "by.diff.age") {
  # exclude pairs with age differences greater than 1
  table(dat$Age.1 - dat$Age.2)
  sum(abs(dat$Age.1 - dat$Age.2) >= 2)
  sum(dat$Same)
  dat <- dat[abs(dat$Age.1 - dat$Age.2) < 2, ]
} else if (flag.age == "by.age.group") {
  # exclude pairs according to the "Same" variable identifying age groups
  dat <- dat[dat$Same == 0, ]
}
# calculate average age, age^2,...
dat$age <- (dat$Age.1 + dat$Age.2) / 2
dat$age0.5 <- dat$age ^ 0.5
dat$age1.5 <- dat$age ^ 1.5
dat$age2 <- dat$age ^ 2
dat$age2.5 <- dat$age ^ 2.5
dat$age3 <- dat$age ^ 3
dat$age3.5 <- dat$age ^ 3.5
dat$age4 <- dat$age ^ 4
dat$age4.5 <- dat$age ^ 4.5
dat$age5 <- dat$age ^ 5
dat$age5.5 <- dat$age ^ 5.5
dat$age6 <- dat$age ^ 6
dat$age6.5 <- dat$age ^ 6.5
dat$lage <- log(dat$age)
dat$laget10 <- log(dat$age - 10)
dat$age.old30 <- as.factor(dat$age > 29)
dat$age.old35 <- as.factor(dat$age > 34)
dat$age.old40 <- as.factor(dat$age > 39)
dat$age.dumy <- as.numeric(dat$age.old35 == FALSE)
dat$age.dumo <- as.numeric(dat$age.old35 == TRUE)
# keep pairs without any tinnitus 
# jacob: now I keep the "non-tinnitus pairs", i.e., pairs where neither has tinnitus. to keep track of these pairs, I generate a dummy variable
dat$pairs.no.tinn <- (dat$BUZZ.1 + dat$BUZZ.2 == 4) 
# exclude unknown zygozity (3)
table(dat$bestzyg)
sum(is.na(dat$bestzyg))
dat <- dat[dat$bestzyg != 3, ]
dat$bestzyg <- as.factor(dat$bestzyg)
# exclude singletons (done for BUZZ1/2)
sum(is.na(dat$BUZZ.1))
sum(is.na(dat$BUZZ.2))
sum(is.na(dat$BUZZBOTH.1[dat$BUZZ.1 == 1])) # still people not replying to BUZZBOTH, but replied to BUZZ
sum(is.na(dat$BUZZBOTH.2[dat$BUZZ.2 == 1]))
# add simplified zygosity variable, grouping zygosity 2 and 4
dat$bestzyg.sim24 <- dat$bestzyg
levels(dat$bestzyg.sim24) <- c("1", "24", "24")
# add dummy variables
dat$zyg1 <- as.numeric(dat$bestzyg == 1)
dat$zyg2 <- as.numeric(dat$bestzyg == 2)
dat$zyg4 <- as.numeric(dat$bestzyg == 4)
# create data sets for general tinnitus
dat.all <- dat[!is.na(dat$ConcTin), ]
# data set for monoz. and dizyg. same sex
dat.mzdzs <- dat.all[is.element(dat.all$bestzyg, c(1, 2)), ]
sum(dat.mzdzs$sex.1 !=  dat.mzdzs$sex.2) 
dat.mzdzs$sex <- as.factor(dat.mzdzs$sex.1)
dat.mzdzs$bestzyg <- factor(dat.mzdzs$bestzyg)  
# data set for monoz. and dizyg. same sex by zygosity
dat.mzdzs.z1 <- dat.mzdzs[dat.mzdzs$bestzyg == 1, ] 
dat.mzdzs.z2 <- dat.mzdzs[dat.mzdzs$bestzyg == 2, ] 
# dummy variables
dat.mzdzs$zyg2f <- as.numeric(dat.mzdzs$bestzyg == 2 & dat.mzdzs$sex == 2)
dat.mzdzs$notzyg2f <- as.numeric(!dat.mzdzs$zyg2f)
dat.mzdzs.z2$sex.m <- as.numeric(dat.mzdzs.z2$sex == 1)
dat.mzdzs.z2$sex.f <- as.numeric(dat.mzdzs.z2$sex == 2)
# create data sets for bilateral and unilateral tinnitus
dat.bil <- dat.mzdzs[!is.na(dat.mzdzs$ConcBi) | (dat.mzdzs$pairs.no.tinn == TRUE), ]
dat.uni <- dat.mzdzs[!is.na(dat.mzdzs$ConcUni) | (dat.mzdzs$pairs.no.tinn == TRUE), ]
# checks
# jabob: now the numbers are much higher, due to inclusion of all pairs with no tinnitus at all
dim(dat.all) # 23829. data set with all zygosities (MZ, DZ-SS, DZ-OS), all tinnitus types
dim(dat.mzdzs)# 15762. data set with same sex twins (MZ, DZ-SS), all tinntus types  
dim(dat.bil)# 14165. data set with same sex twins (MZ, DZ-SS), only bilateral tinnitus 
dim(dat.uni)# 13632. data set with same sex twins (MZ, DZ-SS), only unilateral tinnitus 
dat.org <- dat # keep original data set for checks



# analysis of bilateral tinnitus cases
# 1. formatting to long format
# ------------------------------------

dat <- dat.bil

str(dat)

# Restrict to variables: 
# pair-number, zygosity, tinnitus status, age at interview, sex and related covariates
#dat$twin_pair <- dat$?..alt_pairid
dat$twin_pair <- dat[, 1]
dat.w <- dat[, c("twin_pair","bestzyg","BUZZ.1","BUZZ.2","Age.1","Age.2","sex.1","sex.2")]
nrow(dat.w)
with(dat.w, table(BUZZ.1,BUZZ.2)) # jacob: we now have 11830 concordant non-tinnitus pairs in the sample
with(dat.w, table(sex.1,sex.2)) 
table(dat.w$bestzyg)

dat.l <- fast.reshape(dat.w,c("BUZZ","Age","sex"),sep=".")
str(dat.l)

# jacob: from here on I do not really follow all you have been doing  . did my best to get all right, but am not fully sure, so please have a look
#dat$nr <- with(dat, ave(time,twin_pair,FUN=seq_along))
dat.l$twid <- 10*dat.l$twin_pair+dat.l$num  
head(dat.l)
nrow(dat.l)
dat.l <- droplevels(subset(dat.l,BUZZ<=2))
dat.l$crstatus <- (dat.l$BUZZ==1)*1
#levels(dat.l$crstatus) <- c("alive","tinnitus")
with(dat.l, table(crstatus))
dat.l$time <- dat.l$Age
k <- length(dat.l$time)
dat.l$time2 <- dat.l$time+runif(k)
summary(cbind(dat.l$time,dat.l$time2))
dat.l$zyg <- dat.l$bestzyg
dat.l$zyg <- factor(dat.l$zyg, labels=c("MZ","DZ")) # coding: 1. Monozygotic 2. Dizygotic same sex
dat.l$zygbin <- factor(dat.l$zyg=="MZ", labels=c("DZ","MZ")) # jacob: just switching of the level order is intended?
with(dat.l, table(zyg,zygbin))
with(dat.l, table(zyg,sex))
dat.l$tin=factor(dat.l$BUZZ==1, labels=c("no","yes"))
dat.l$tin=(dat.l$BUZZ==1)*1
with(dat.l, table(tin,sex))



# analysis of bilateral tinnitus cases
# 2. model fitting
# ------------------------------------

# dat.lmzdzss <- droplevels(subset(dat.l,zyg=="MZ" | zyg=="DZ"))
# with(dat.lmzdzss, table(tin,zyg))

## Saturated model
bp00 <- biprobit(tin~+sex+Age+cluster(twin_pair)+strata(zyg), 
                 data=dat.l,eqmarg=FALSE, pairsonly = TRUE)
score(bp00) # jacob: you said to carry out a convergence check , 10^-3 / -4 is OK. for each values, or the sum?

#(interaction of age and sex is not significant)
summary(bp00)

bp01 <- biprobit(tin~+sex+Age+cluster(twin_pair)+strata(zyg), data=dat.l,
                 pairsonly = TRUE)
score(bp01)
summary(bp01)

compare(bp00,bp01) # jacob: I suppose bp01 is better as usually for the LRT?

bp02 <- biprobit(tin~+sex+Age, id="twin_pair", rho=~1+zyg, data=dat.l,
                 cor.contrast=rbind(c(1,0),c(1,1)),
                 iid=TRUE,pairs.only=FALSE)
score(bp02)
summary(bp02)
compare(bp01, bp02) # jacob: so we stay with bp01

bp.flex <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
               type=c("flex"))
score(bp.flex) # most general model
summary(bp.flex)

bp.u <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                 type=c("u"))
score(bp.u)    # tin outcome on sex and age, type = u threshold identical, long format required, 
summary(bp.u)
compare(bp.flex,bp.u)


bp.ace <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                 type=c("ace")) # ace model
score(bp.ace)
summary(bp.ace)

bp.ade <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                 type=c("ade"))
score(bp.ade)
summary(bp.ade)
AIC(bp.ade,bp.ace)

bp.ae <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                 type=c("ae"))
score(bp.ae)
summary(bp.ae)
compare(bp.ade,bp.ae)  # force var. comp (d) to be zero, LR stats unreliable

bp.ce <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                type=c("ce"))
score(bp.ce)
summary(bp.ce)

compare(bp.ade,bp.ae, bp.ce)
AIC(bp.flex,bp.u,bp.ade,bp.ace, bp.ae, bp.ce)


dat.l$Agecat <-cut(dat.l$Age, c(0,24,34,44,54,64,74,84))
with(dat.l, table(Agecat))

bp.ace.age <- bptwin(tin ~+1+sex+Age, strata="Agecat", id="twin_pair", zyg="zyg", DZ="DZ", data=dat.l,
                 type=c("ace")) # ace model
score(bp.ace.age)
summary(bp.ace.age)









#############################################################################################
# END END END  END END END  END END END  END END END  END END END  END END END  END END END #
#############################################################################################




#  MZ and DZ+OS
#


bp.all.flex <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
                  type=c("flex"))
score(bp.all.flex)
summary(bp.all.flex)

bp.all.u <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
               type=c("u"))
score(bp.all.u)
summary(bp.all.u)
compare(bp.all.flex,bp.all.u)


bp.all.ace <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
                 type=c("ace"))
score(bp.all.ace)
summary(bp.all.ace)

bp.all.ade <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
                 type=c("ade"))
score(bp.all.ade)
summary(bp.all.ade)
AIC(bp.all.ade,bp.all.ace)

bp.all.ae <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
                type=c("ae"))
score(bp.all.ae)
summary(bp.all.ae)
compare(bp.all.ade,bp.all.ae)

bp.all.ce <- bptwin(tin ~+1+sex+Age, id="twin_pair", zyg="zygbin", DZ="DZ", data=dat.l,
                type=c("ce"))
score(bp.all.ce)
summary(bp.all.ce)

compare(bp.all.ade,bp.all.ae, bp.all.ce)
AIC(bp.all.flex,bp.all.u,bp.all.ade,bp.all.ace, bp.all.ae, bp.all.ce)

summary(bp.all.ade)
summary(bp.ade)


# Inverse probability weighting according to time to event.
# 
#

a0.time <- twinlm.time(tin~sex, id="twin_pair", zyg="zyg", DZ="DZ",
                      data=dat.lmzdzss,
                      cens.formula=Surv(time2,crstatus=="0")~1,
                      breaks=Inf)
score(a0.time) # too high!
summary(a0.time)

a.time <- twinlm.time(tin~sex, id="twin_pair", zyg="zyg", DZ="DZ",
                      data=dat.lmzdzss,
                   cens.formula=Surv(time2,crstatus=="0")~zyg+sex,
                   breaks=Inf,
                   control=list(method="NR",start=coef(a0.time))) # 
with(dat.lmzdzss, table(crstatus))
score(a.time) # Too high!
summary(a.time)


a <- biprobit.time(tin~sex, rho=~1+zyg, id="twin_pair", data=dat.lmzdzss, 
                   eqmarg=TRUE, type=c("ace"),
                   cens.formula=Surv(time,crstatus=="0")~zyg+sex,
                   breaks=seq(40,90,by=10),fix.censweights=TRUE,
                   cor.contrast=rbind(c(1,0),c(1,1)),
                   iid=TRUE,pairs.only=FALSE,
                   control=list(method="NR",start=coef(a0.time)))
score(a)
summary(a)
