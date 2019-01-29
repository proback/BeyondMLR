# ThreeLevel.R - 3 level analysis with seed germination data

# Be sure to install first if needed
library(splines)
library(gridExtra)
library(GGally)
library(mice)
library(nlme)
library(lme4)
library(lmeresampler)
library(mnormt)
library(boot)
library(HLMdiag)
library(tidyverse)

#great base theme for ggplots
theme.1 <- theme(axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  plot.title=element_text(hjust=.9,face="italic",size=12))

# seedwd <- read.csv(file=file.choose())  # seeds2.csv
seedwd <- read.csv("~/Stats 316 S18/Class/Data/seeds2.csv")
seedwd[135:146,2:11]   # illustrate wide data

# Investigate patterns of missingness
md.pattern(seedwd)

# Remove plants that did not germinate
seedwd <- seedwd %>%
  mutate(nope = is.na(hgt13)+is.na(hgt18)+is.na(hgt23)+is.na(hgt28)) %>%
  filter(nope < 4)

# Create data frame in LONG form (one obs per plant measurement time)
seedlg <- seedwd %>%
  gather(key = time, value = hgt, hgt13:hgt28) %>%
  mutate(time = as.integer(str_sub(time, -2)),
         time13 = time - 13 ) 

seedlg %>% filter(plant %in% c(236:242)) %>%
  arrange(plant, time13) %>%
  select(2:10)

# Variables in seedlg:
# pot = Pot plant was grown in (1-72)
# plant = Unique plant identification number
# species = L for leadplant and C for coneflower
# soil = STP for reconstructed prairie, REM for remnant prairie, and
#   CULT for cultivated land
# sterile = Y for yes and N for no
# germin = Y if plant germinated, N if not.  Should be Y for all obsservations
#   in seedlg (true except for plant 281 - probably an error)
# time = number of days after planting when height measured
# time13 = centered time, so that first day of measurement is Day 0
# hgt = height of plant (in mm).

# create indicator variables for later analyses
seedlg <- seedlg %>%
  mutate(cult=ifelse(soil=="CULT",1,0),
         rem=ifelse(soil=="REM",1,0),
         stp=ifelse(soil=="STP",1,0),
         lead=ifelse(species=="L",1,0),
         cone=ifelse(species=="C",1,0),
         strl=ifelse(sterile=="Y",1,0),
         nostrl=ifelse(sterile=="N",1,0) )

# create separate data sets of leadplants and coneflowers
conedata <- seedlg %>%
  filter(cone==1)
leaddata <- seedlg %>%
  filter(lead==1)

## Exploratory data analysis ##

# Rough look at Level Three covariates
seedlg %>% count(species)
seedlg %>% count(soil)
seedlg %>% count(sterile)

# From here on, do everything separately for leadplants and coneflowers #

# Add average across all time points for each plant for EDA plots
meanplant <- seedlg %>% group_by(plant) %>%
  summarise(meanplant = mean(hgt, na.rm = TRUE))
seedwd <- seedwd %>%
  left_join(meanplant, by = "plant")
conewd <- seedwd %>%
  filter(species=="C")
leadwd <- seedwd %>%
  filter(species=="L")

# One obs per plant - assume plants relatively independent within pots
cone.soil <- ggplot(conedata,aes(x=soil,y=hgt)) + 
  geom_boxplot() + 
  theme.1 + 
  labs(x="Soil type",y="Plant Height (mm)",title="Coneflowers (a)")
cone.sterile <- ggplot(conedata,aes(x=sterile,y=hgt)) + 
  geom_boxplot() + 
  theme.1 + 
  labs(x="Sterilized",y="Plant Height (mm)",title="Coneflowers (b)")
lead.soil <- ggplot(leaddata,aes(x=soil,y=hgt)) + 
  geom_boxplot() + 
  theme.1 + 
  labs(x="Soil type",y="Plant Height (mm)",title="Leadplants (a)")
lead.sterile <- ggplot(leaddata,aes(x=sterile,y=hgt)) + 
  geom_boxplot() + 
  theme.1 + 
  labs(x="Sterilized",y="Plant Height (mm)",title="Leadplants (b)")
boxplots.byspecies <- grid.arrange(cone.soil, cone.sterile, lead.soil, lead.sterile,
                                   ncol=2)

# Plot time trend for 25 randomly selected leadplants - linear fit
randplant = as.vector(sample(leadwd$plant,size=25))
seedrdl <- seedlg %>%
  filter(plant %in% randplant)
ggplot(seedrdl, aes(x=time, y=hgt)) + 
  facet_wrap(~plant,ncol=5) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme.1 +
  theme(strip.text.x=element_blank()) + 
  labs(x="Time",y="Plant height (mm)")

# spaghetti plot by pot - add loess smoothing curve for overall trend
oldw <- getOption("warn")
options(warn = -1)
ggplot(leaddata, aes(x=time, y=hgt)) + 
  geom_line(aes(group=plant), color="dark grey") + 
  theme.1 + 
  facet_wrap(~pot, ncol=7) + 
  geom_smooth(se=FALSE, color="black") + 
  labs(x="Days since seeds planted",y="Plant height (mm)")
options(warn = oldw)

# spaghetti plot by species - all plants (This plot used earlier now)
#   add loess smoothing curve for overall trend
# To get rid of warnings, run as Rmd with warning=FALSE in global options
#   or chunk options
seedlg <- seedlg %>%
  mutate(speciesname = ifelse(species=="C","Coneflowers","Leadplants") )
options(warn = -1)
ggplot(seedlg,aes(x=time,y=hgt)) + 
  geom_line(aes(group=plant),color="dark grey") + 
  facet_wrap(~speciesname,ncol=2) + 
  geom_smooth(se=FALSE,color="black") + 
  theme.1 + 
  labs(x="Days since seeds planted",y="Plant height (mm)")
options(warn = oldw)

# spaghetti plot by soil type with loess overall trend
leaddata <- leaddata %>%
  mutate(soilname = recode(soil, STP="Reconstructed", CULT="Cultivated",
                                    REM="Remnant") )
options(warn = -1)
ggplot(leaddata,aes(x=time,y=hgt)) + 
  geom_line(aes(group=plant),color="dark grey") + 
  facet_wrap(~soilname,ncol=3) + 
  geom_smooth(se=FALSE,color="black") + 
  theme.1 + 
  labs(x="Days since seeds planted",y="Plant height (mm)") 
options(warn = oldw)

# spaghetti plot by sterilization with loess overall trend
leaddata <- leaddata %>%
  mutate(sterilename = recode(sterile, Y="Sterilized", N="Not Sterilized") )
options(warn = -1)
ggplot(leaddata,aes(x=time,y=hgt)) + 
  geom_line(aes(group=plant),color="dark grey") + 
  facet_wrap(~sterilename,ncol=2) + 
  geom_smooth(se=FALSE,color="black") + 
  theme.1 + 
  labs(x="Days since seeds planted",y="Plant height (mm)") 
options(warn = oldw)

# Summary stats for linear models by plant - use centered time
hgt.list=lmList(hgt~time13 | plant, data=leaddata, na.action=na.exclude)
# coef(hgt.list)
int = as.matrix(coef(hgt.list))[,1]
summary(int)   # summary statistics for 107 intercepts
rate = as.matrix(coef(hgt.list))[,2]
summary(rate)
rsq <- by(leaddata, leaddata$plant, function(data)
  summary(lm(hgt ~ time13, data = data,na.action=na.exclude))$r.squared)
summary(rsq)
sum(rsq[!is.na(rsq)]>=.8)/length(rsq[!is.na(rsq)])

int.rate.rsq <- as.data.frame(cbind(int,rate,rsq))
int.hist <- ggplot(int.rate.rsq,aes(x=int)) + 
  geom_histogram(binwidth=0.5,color="black",fill="white") + 
  theme.1 + labs(x="Intercepts",y="Frequency",title="(a)")
rate.hist <- ggplot(int.rate.rsq,aes(x=rate)) + 
  geom_histogram(binwidth=0.05,color="black",fill="white") + 
  theme.1 + labs(x="Slopes",y="Frequency",title="(b)")
rsq.hist <- ggplot(int.rate.rsq,aes(x=rsq)) + 
  geom_histogram(binwidth=0.1,color="black",fill="white") +
  theme.1 + labs(x="R squared values",y="Frequency",title="(c)")
intrate.lpbyplant <- grid.arrange(int.hist,rate.hist,rsq.hist,ncol=2)

#    Descriptive statistics of the estimates obtained by fitting the 
#      linear model by plant.
mean(int)
sd(int)
mean(rate, na.rm=T)
sd(rate, na.rm=T)
cor(int, rate, use="complete.obs")   

# Summary stats for linear models by pot
hgt2.list=lmList(hgt~time13 | pot, data=leaddata, na.action=na.exclude)
# coef(hgt2.list)
int2 = as.matrix(coef(hgt2.list))[,1]
summary(int2)   # summary statistics for 32 intercepts
rate2 = as.matrix(coef(hgt2.list))[,2]
summary(rate2)
rsq2 <- by(leaddata, leaddata$pot, function(data)
  summary(lm(hgt ~ time13, data = data,na.action=na.exclude))$r.squared)
summary(rsq2)

int.rate.rsq2 <- as.data.frame(cbind(int2,rate2,rsq2))
int2.hist <- ggplot(int.rate.rsq2,aes(x=int2)) + 
  geom_histogram(binwidth=0.5,color="black",fill="white") + 
  theme.1 + labs(x="Intercepts",y="Frequency",title="(a)")
rate2.hist <- ggplot(int.rate.rsq2,aes(x=rate2)) + 
  geom_histogram(binwidth=0.05,color="black",fill="white") + 
  theme.1 + labs(x="Slopes",y="Frequency",title="(b)")
rsq2.hist <- ggplot(int.rate.rsq2,aes(x=rsq2)) + 
  geom_histogram(binwidth=0.2,color="black",fill="white") + 
  theme.1 + labs(x="R squared values",y="Frequency",title="(c)")
intrate.lpbypot <- grid.arrange(int2.hist,rate2.hist,rsq2.hist,ncol=2)

#    Descriptive statistics of the estimates obtained by fitting the 
#      linear model by pot.
mean(int2)
sd(int2)
mean(rate2, na.rm=T)
sd(rate2, na.rm=T)
cor(int2, rate2, use="complete.obs")   

# Try to compare variability between plants and between pots
#   Plus ANOVA-type boxplot of within pot vs within plant variability

seedwdplus <- seedwd %>%
  filter(species=="L") %>%
  mutate(int = int, rate = rate)
bypot <- seedwdplus %>%
  group_by(pot) %>%
  summarise(sd_int = unname(sd(int)),
            mean_int = unname(mean(int)),
            sd_rate = unname(sd(rate)),
            mean_rate = unname(mean(rate)) )

mean(bypot$sd_int,na.rm=T)   # intercept variability between plants = .489
mean(bypot$sd_rate,na.rm=T)   # rate variability between plants = .039
sd(bypot$mean_int,na.rm=T)   # intercept variability between pots = .478
sd(bypot$mean_rate,na.rm=T)   # rate variability between pots = .051

# Boxplots to compare ints and rates by factor levels
hgt.listc=lmList(hgt~time13 | plant, data=conedata, na.action=na.exclude)
intc = as.matrix(coef(hgt.listc))[,1]
ratec = as.matrix(coef(hgt.listc))[,2]
allfits <- tibble(int = c(int, intc),
                  rate = c(rate, ratec),
                  species = c(rep("Leadplants", 107), rep("Coneflowers", 176)) )

int.byspecies <- ggplot(allfits,aes(x=species,y=int)) + 
  geom_boxplot(aes(group=species)) + 
  theme.1 +
  labs(x="Species",y="Intercepts",title="(a)") 
rate.byspecies <- ggplot(allfits, aes(x=species,y=rate)) + 
  geom_boxplot(aes(group=species)) + 
  theme.1 + 
  labs(x="Species",y="Slopes",title="(b)") 
intrate.byspecies <- grid.arrange(int.byspecies,rate.byspecies,ncol=2)

seedwdplus <- seedwdplus %>%
  mutate(soilname = recode(soil, STP="Reconstructed", CULT="Cultivated",
                           REM="Remnant") )
int.bysoil <- ggplot(seedwdplus,aes(x=soilname,y=int)) + 
  geom_boxplot() + 
  theme.1 +
  labs(x="Soil type",y="Intercepts",title="(a)")
rate.bysoil <- ggplot(seedwdplus,aes(x=soilname,y=rate)) + 
  geom_boxplot() + 
  theme.1 +
  labs(x="Soil type",y="Slopes",title="(b)")
intrate.bysoil <- grid.arrange(int.bysoil,rate.bysoil,ncol=2)

seedwdplus <- seedwdplus %>%
  mutate(sterilename = recode(sterile, Y="Sterilized", N="Not Sterilized") )
int.bysterile <- ggplot(seedwdplus,aes(x=sterilename,y=int)) + 
  geom_boxplot() + 
  theme.1 +
  labs(x="Sterile",y="Intercepts",title="(a)")
rate.bysterile <- ggplot(seedwdplus,aes(x=sterilename,y=rate)) + 
  geom_boxplot() + 
  theme.1 + 
  labs(x="Sterile",y="Slopes",title="(b)")
intrate.bysterile <- grid.arrange(int.bysterile,rate.bysterile,ncol=2)


# Examine correlation structure
seed.nona <- leaddata %>%
  filter(!is.na(hgt))
hgt.lm = lm(hgt~time13, data=seed.nona)
seed.nona <- seed.nona %>%
  mutate(lmres = resid(hgt.lm))
hgtw <- seed.nona %>%
  select(plant, time13, lmres) %>%
  spread(key = time13, value = lmres, sep = "") 
hgtw.1 <- na.omit(hgtw)
ggpairs(hgtw.1[,2:5], lower=list(continuous="smooth"),
  upper=list(), diag=list(continuous="bar", discrete="bar"),
  axisLabels="show") 


############################################################################

### Model fitting - leadplants ###

# Model A - unconditional means
modelal = lmer(hgt ~ 1 + (1|plant) + (1|pot), REML=T, data=leaddata)
summary(modelal)

# Model B - unconditional growth
modelbl = lmer(hgt ~ time13 + (time13|plant) + (time13|pot), 
  REML=T, data=leaddata)
summary(modelbl)

# Model C - add covariates at pot level
modelcl = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + (time13|plant) + (time13|pot), REML=T, data=leaddata)
summary(modelcl)   # get corr=-1 at pot level

anova(modelbl,modelcl)    # go with Model C (although that had corr=-1)
                          # remember that anova() assumes REML=F

# Try Model C without correlation between L2 errors
modelcl.noL2corr = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + 
  time13:cult + time13:rem + (time13|plant) + (1|pot) + (0+time13|pot), 
  REML=T, data=leaddata)
summary(modelcl.noL2corr)

# Try Model C without time at pot level
modelcl0 = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + (time13|plant) + (1|pot), REML=T, data=leaddata)
summary(modelcl0)

anova(modelcl0,modelcl)   # go with Model C.0 (fewer varcomps)

#########################

# run bootstrapAnova function, then line below takes about 15 minutes
#   (much quicker with B=100 rather than B=1000)

# First run models with REML=F to try to speed up bootstrapAnova
modelcl0 = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + (time13|plant) + (1|pot), REML=F, data=leaddata)
modelcl = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + (time13|plant) + (time13|pot), REML=F, data=leaddata)

# Parametric bootstrap code for lme4-models
#   from Fabian Scheipl on stack exchange

#m0 is the lmer model under the null hypothesis (i.e. the smaller model)
#mA is the lmer model under the alternative

bootstrapAnova <- function(mA, m0, B=1000){
  oneBootstrap <- function(m0, mA){
    d <- drop(simulate(m0))
    m2 <-refit(mA, newresp=d)
    m1 <-refit(m0, newresp=d)
    return(anova(m2,m1)$Chisq[2])
  }  
  nulldist <- replicate(B, oneBootstrap(m0, mA))
  ret <- anova(mA, m0)
  ret$"Pr(>Chisq)"[2] <- mean(ret$Chisq[2] < nulldist)
  names(ret)[8] <- "Pr_boot(>Chisq)"
  attr(ret, "heading") <- c(attr(ret, "heading")[1], 
                            paste("Parametric bootstrap with", B,"samples."),
                            attr(ret, "heading")[-1])
  attr(ret, "nulldist") <- nulldist
  return(ret)
}
#use like this (and increase B if you want reviewers to believe you):
# (bRLRT <- bootstrapAnova(mA=<BIG MODEL>, m0=<SMALLER MODEL>))

bRLRT = bootstrapAnova(mA=modelcl, m0=modelcl0, B=100)
bRLRT
nullLRT = attr(bRLRT,"nulldist")
x=seq(0,max(nullLRT),length=100)
y=dchisq(x,2)
nullLRT.1 <- as.data.frame(cbind(nullLRT=nullLRT,x=x,y=y))
ggplot(nullLRT.1) + 
  geom_histogram(aes(x=nullLRT,y=..density..),binwidth=1,color="black",fill="white") + 
  geom_vline(xintercept=2.089,size=1) + 
  theme.1 + 
  geom_line(aes(x=x,y=y)) +
  labs(x="Likelihood Ratio Test Statistics from Null Distribution",y="Density")
sum(nullLRT>=2.089)/100   # parametric bootstrap p-value


# Use lmeresampler to get bootstrapped CIs for var components

## running a parametric bootstrap (30 seconds or so)
boo1 <- bootstrap(model = modelcl, fn = varcomp.mer, type = "parametric", B = 100)

## bootstrap confidence intervals are easily found using 'boot.ci'
##  - there are actually 7 variance components
##  - varcomp.mer lists 11, but 4 are actually 0
boot.ci(boo1, index = 10, type="perc")
boot.ci(boo1, index = 11, type="perc")

## you can also examine the bootstrap samples graphically
plot(boo1, index = 10)
plot(boo1, index = 11)

######################################

# Model D - add interactions to Model C.1
modeldl0 = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + strl:cult + strl:rem + time13:strl:cult + time13:strl:rem +
  (time13|plant) + (1|pot), REML=T, data=leaddata)
summary(modeldl0)

# Model F - simplify and move toward "final model"
modelfl0 = lmer(hgt ~ time13 + time13:strl + 
  time13:rem + time13:strl:rem +
  (time13|plant) + (1|pot), REML=T, data=leaddata)
summary(modelfl0)

anova(modelfl0,modeldl0)        # go with Model F
anova(modelfl0,modeldl0)[2,6]   # [1] 11.14747

# Model E - simplify and move toward "final model"
modelel0 = lmer(hgt ~ time13 + time13:strl +
  (time13|plant) + (1|pot), REML=T, data=leaddata)
summary(modelel0)

anova(modelel0,modelfl0)  # go with Model F

# First run models with REML=F to try to speed up bootstrapAnova
modeldl0 = lmer(hgt ~ time13 + strl + cult + rem + time13:strl + time13:cult +
  time13:rem + strl:cult + strl:rem + time13:strl:cult + time13:strl:rem +
  (time13|plant) + (1|pot), REML=F, data=leaddata)
modelfl0 = lmer(hgt ~ time13 + time13:strl + 
  time13:rem + time13:strl:rem +
  (time13|plant) + (1|pot), REML=F, data=leaddata)

##########################################

# Compare Models F and D with parametric bootstrap
#   Ran in about 5 minutes - not bad
#   Histogram shows chi-square has slightly thinner tails and taller peak,
#     although the fit was pretty close, as was pvalue (.1323 vs. .22)
bRLRT2 = bootstrapAnova(mA=modeldl0, m0=modelfl0, B=100)
bRLRT2
nullLRT2 = attr(bRLRT2,"nulldist")
x2=seq(0,max(nullLRT2),length=100)
y2=dchisq(x2,anova(modelfl0,modeldl0)[2,6])
nullLRT.2 <- as.data.frame(cbind(nullLRT=nullLRT2,x=x2,y=y2))
ggplot(nullLRT.2) + 
  geom_histogram(aes(x=nullLRT,y=..density..),binwidth=2,color="black",fill="white") + 
  theme.1 + 
  geom_line(aes(x=x,y=y)) + 
  geom_vline(xintercept=11.14747,size=1) +
  labs(x="Likelihood Ratio Test Statistics from Null Distribution",y="Density")
sum(nullLRT2>=11.14747)/100   # parametric bootstrap p-value


# Use lmeresampler to get bootstrapped CIs for fixed effects

## running a parametric bootstrap (15 seconds or so)
boo1 <- bootstrap(model = modelfl0, fn = fixef, type = "parametric", B = 100)

## bootstrap confidence intervals are easily found using 'boot.ci'
##  - there are actually 5 fixed effects
boot.ci(boo1, index = 1, type="perc")
boot.ci(boo1, index = 2, type="perc")
boot.ci(boo1, index = 3, type="perc")
boot.ci(boo1, index = 4, type="perc")
boot.ci(boo1, index = 5, type="perc")

## you can also examine the bootstrap samples graphically
plot(boo1, index = 1)
plot(boo1, index = 2)
plot(boo1, index = 3)
plot(boo1, index = 4)
plot(boo1, index = 5)

###########################################

# Diagram to help explain boundary constraints
b0 <- seq(-4,12,length=51)
sigma2 <- seq(-8,4,length=51)
xy <- expand.grid(b0,sigma2)

# Include all points
Sigma <- matrix(c(12,0,0,6),2,2)
Mu <- c(4,-2)
z <- dmnorm(xy, Mu, Sigma)
zframe <- data.frame(xy, z)
MLE <- xy[z==max(z),]
con.1 <- ggplot(data=zframe,aes(x=Var1,y=Var2,z=z)) + 
  theme.1 +
  geom_contour(stat="contour",lineend="butt",linejoin="round",linemitre=1,
    na.rm=FALSE,colour="black") + 
  labs(x="b0",y="sigma2",title="(a)") + 
  scale_y_continuous(limits=c(-8,4)) + 
  geom_abline(intercept=0,slope=0) + 
  geom_abline(intercept=0,slope=1000)

# Include all points where sigma2 >= 0
z <- ifelse(xy[,2]<0,0,dmnorm(xy, Mu, Sigma))
zframe.1 <- zframe[zframe$Var2>=0,]
MLE <- xy[z==max(z),]
con.2 <- ggplot(data=zframe.1,aes(x=Var1,y=Var2,z=z)) + 
  geom_contour(stat="contour",lineend="butt",linejoin="round",linemitre=1,
    na.rm=FALSE,colour="black") + 
  theme.1 + 
  scale_y_continuous(limits=c(-8,4)) + 
  labs(x="b0",y="sigma2",title="(b)") + 
  geom_abline(intercept=0,slope=0) + 
  geom_abline(intercept=0,slope=1000) 
contour.boundary <- grid.arrange(con.1,con.2, ncol=2)


# estimates of variance components (not SDs) from Model B
sig = .0822
sig0 = .299
sig1 = .00119
sig01 = .280*sqrt(sig0*sig1)
sig00 = .0442
sig11 = .00126
sig0011 = (-.610)*sqrt(sig00*sig11)

# Sigma(ij) = error cov matrix for plant j from pot i
foo1=c(0,5,10,15)
foov=(sig+sig0+sig00)+(foo1^2)*(sig1+sig11)+2*foo1*(sig01+sig0011)
foov
# [1] 0.4254000 0.4939437 0.6849874 0.9985310
sqrt(foov)
# [1] 0.6522270 0.7028113 0.8276396 0.9992653
foo2=c(0,0,0,5,5,10)
foo3=c(5,10,15,10,15,15)
fooc=(sig0+sig00)+(foo2+foo3)*(sig01+sig0011)+foo2*foo3*(sig1+sig11)
fooc
# [1] 0.3468468 0.3504937 0.3541405 0.4766405 0.5415374 0.7289342
dim1=c(1,1,1,2,2,3)
dim2=c(2,3,4,3,4,4)
foor=fooc/sqrt(foov[dim1]*foov[dim2])
foor
# [1] 0.7566591 0.6492922 0.5433705 0.8194283 0.7710968 0.8813862

# Sigma(ijk) = error cov matrix for plants j and k from pot i
foovv=sig00+(foo1^2)*sig11+2*foo1*sig0011
foovv
# [1] 0.04420000 0.03017749 0.07915497 0.19113246
foocc=sig00+(foo2+foo3)*sig0011+foo2*foo3*sig11
foocc
# [1]  0.021438743 -0.001322514 -0.024083772  0.038916228  0.047654971
# [6]  0.119393714
foorr=foocc/sqrt(foov[dim1]*foov[dim2])
foorr
# [1]  0.046769406 -0.002449968 -0.036952598  0.066903794  0.067856071
# [6]  0.144364163
foorrdiag=foovv/foov
foorrdiag
# [1] 0.10390221 0.06109499 0.11555683 0.19141364


# Repeat calculation of error cov matrices with sig11=sig0011=0 -> (1|pot)
sig = .0822
sig0 = .299
sig1 = .00119
sig01 = .280*sqrt(sig0*sig1)
sig00 = .0442
sig11 = 0
sig0011 = 0

# Sigma(ij) = error cov matrix for plant j from pot i
foo1=c(0,5,10,15)
foov=(sig+sig0+sig00)+(foo1^2)*(sig1+sig11)+2*foo1*(sig01+sig0011)
foov
# [1] 0.4254000 0.5079662 0.6500324 0.8515986
sqrt(foov)
# [1] 0.6522270 0.7127175 0.8062459 0.9228210
foo2=c(0,0,0,5,5,10)
foo3=c(5,10,15,10,15,15)
fooc=(sig0+sig00)+(foo2+foo3)*(sig01+sig0011)+foo2*foo3*(sig1+sig11)
fooc
# [1] 0.3696081 0.3960162 0.4224243 0.4819243 0.5380824 0.6537405
dim1=c(1,1,1,2,2,3)
dim2=c(2,3,4,3,4,4)
foor=fooc/sqrt(foov[dim1]*foov[dim2])
foor
# [1] 0.7951065 0.7530897 0.7018312 0.8386754 0.8181142 0.8786591

# Sigma(ijk) = error cov matrix for plants j and k from pot i
foovv=sig00+(foo1^2)*sig11+2*foo1*sig0011
foovv
# [1] 0.0442 0.0442 0.0442 0.0442
foocc=sig00+(foo2+foo3)*sig0011+foo2*foo3*sig11
foocc
# [1] 0.0442 0.0442 0.0442 0.0442 0.0442 0.0442
foorr=foocc/sqrt(foov[dim1]*foov[dim2])
foorr
# [1] 0.09508371 0.08405354 0.07343550 0.07691966 0.06720280 0.05940696
foorrdiag=foovv/foov
foorrdiag
# [1] 0.10390221 0.08701366 0.06799661 0.05190239


# Explore correlation structure in leadplants - Model B

#Standard error covariance structure for unconditional growth model
std.lme=lme(hgt ~ time13, data=leaddata, random =  ~ time13 | pot/plant, 
  na.action=na.exclude)
summary(std.lme)

corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)$'1/11'
    # must be number of subject with complete data
    # corStruct is lower triangle of pairwise correlations
    # corMatrix creates a large number of symmetric matrices with diag of 1s
  print(corm)
  covm <- getVarCov(glsob, individual='1/11')
  return(covm)}

unstruct <- gls(hgt~time13, data=leaddata, 
  correlation=corSymm(form = ~ 1 |pot/plant), method="REML", 
  weights=varIdent(form = ~ 1|time13), na.action=na.exclude)
corandcov(unstruct)

hetercom <- gls(hgt~time13, data=leaddata, na.action=na.exclude,
  correlation=corCompSymm(,form = ~ 1 |pot/plant),
  weights=varIdent(form = ~1|time13), method="REML")
corandcov(hetercom)

hauto1 <- gls(hgt~time13, data=leaddata, na.action=na.exclude,
  correlation=corAR1(,form = ~ 1 |pot/plant), 
  weights=varIdent(form = ~1|time13), method="REML")
corandcov(hauto1)

toep <- gls(hgt~time13, data=leaddata, na.action=na.exclude,
  correlation=corARMA(,form = ~ 1 |pot/plant,p=3,q=0), method="REML")
corandcov(toep)

# Heterogeneous compound symmetry error structure
hcs.lme=lme(hgt~time13, data=leaddata, random =  ~ 1 | pot/plant, 
  correlation=corCompSymm(,form = ~ 1 |pot/plant), na.action=na.exclude,
  weights=varIdent(form = ~1|time13))
summary(hcs.lme)

# Heterogeneous autoregressive error structure
har1.lme=lme(hgt~time13, data=leaddata, random =  ~ 1 | pot/plant, 
  correlation=corAR1(,form = ~ 1 |pot/plant), na.action=na.exclude,
  weights=varIdent(form = ~1|time13))
summary(har1.lme)

#Toeplitz error covariance structure
toep.lme=lme(hgt~time13, data=leaddata, random =  ~ 1 | pot/plant, 
  correlation=corARMA(,form = ~ 1 |pot/plant,p=3,q=0), na.action=na.exclude)
summary(toep.lme)

comsym <- gls(hgt~time13, data=leaddata, na.action=na.exclude,
  correlation=corCompSymm(,form = ~ 1 |pot/plant), method="REML")
corandcov(comsym)

auto1 <- gls(hgt~time13, data=leaddata, na.action=na.exclude,
  correlation=corAR1(,form = ~ 1 |pot/plant), method="REML")
corandcov(auto1)

# Compound symmetry error structure
cs.lme=lme(hgt~time13, data=leaddata, random = ~ 1|pot/plant,
  na.action=na.exclude, correlation=corCompSymm(,form = ~ 1 |pot/plant))
summary(cs.lme)

# Autoregressive error structure
ar1.lme=lme(hgt~time13, data=leaddata, random =  ~ 1 | pot/plant, 
  correlation=corAR1(,form = ~ 1 |pot/plant), na.action=na.exclude)
summary(ar1.lme)

anova(std.lme,cs.lme,hcs.lme,ar1.lme,har1.lme,toep.lme,test=F)

#Unstructured error covariance structure - no convergence
# unstr.lme=lme(hgt~time13, data=leaddata, random =  ~ 1 | pot/plant, 
#   correlation = corSymm(, form =  ~ 1 | pot/plant), na.action=na.exclude,
#   weights=varIdent(form = ~ 1|time13))
# summary(unstr.lme)

