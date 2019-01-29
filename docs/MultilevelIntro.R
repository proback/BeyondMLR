# MultilevelIntro.R

# Variables in original data set
# id			      unique musician identification number
# diary		      cumulative total of diaries filled out by musician
# previous      number of previous dairy entries filled out
# perform_type	type of performance (solo, large or small ensemble)
# memory		    performed from Memory, using Score, or Unspecified
# audience		  who attended (Instructor, Public, Students, or Juried)
# pa			      positive affect from PANAS
# na			      negative affect from PANAS
# age			      musician age
# gender		    musician gender
# instrument	  Voice, Orchestral, or Piano
# years_study	  number of years studied the instrument
# mpqsr		      stress reaction subscale from MPQ 
# mpqab		      absorption subscale from MPQ 
# mpqpem		    positive emotionality composite scale from MPQ
# mpqnem		    negative emotionality composite scale from MPQ
# mpqcon		    constraint composite scale from MPQ

# Be sure to install these packages if necessary
library(MASS)
library(mnormt)
library(lme4)
# library(HLMdiag)
library(gridExtra) 
library(tidyverse)

music <- read_csv("~/Stats 316 S18/Class/Data/musicdata.csv")
music    

music %>% count(id)    # number of diary entries for each subject
music %>% count(diary) # number of subjects with diary entry of a given number

select <- dplyr::select
keydata <- music %>% 
  dplyr::select(id, diary, perform_type, memory, audience, na, gender, instrument,
         mpqab, mpqpem, mpqnem)
keydata

# Create Level2 data set by picking off one observation per subject
#  - should be 37 rows and 6 columns (one per L2 variable)
music.lev2 <-  keydata %>%
  group_by(id) %>%
  filter(row_number() == 1) %>%
  select(id, gender:mpqnem)

# Add average across all performances for each subject for EDA plots
meanbysubj <- music %>% group_by(id) %>%
  summarise(meanbysubj = mean(na, na.rm = TRUE))
music.lev2 <- music.lev2 %>%
  left_join(meanbysubj, by = "id")


# Exploratory data analysis

# Summarize Level 1 covariates (and responses) by ignoring within subject 
#   correlation and pretending all observations are independent
music %>% count(perform_type) 
music %>% count(memory) 
music %>% count(audience) 

# create ggplot theme for plots
# theme with grid, grey background 
theme.1 <- theme(axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  plot.title=element_text(hjust=.9,face="italic",size=12))
theme1 <- theme(axis.line = element_line(size = .5, color = "black"), 
  panel.background = element_rect(fill="white"), 
  panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
  plot.title=element_text(hjust=.9,face="italic",size=12))

## Histogram of negative affect frequencies
na.all <- ggplot(data=music,aes(x=na)) + 
  geom_histogram(binwidth = 2, fill = "white",color = "black") + 
  theme.1 + xlim(10,35) +
  xlab("Negative Affect") + ylab("Frequency") + labs(title="(a)") 
na.mean <- ggplot(data=music.lev2,aes(x=meanbysubj)) + 
  geom_histogram(binwidth = 2, fill = "white", color = "black") + 
  theme.1 + xlim(10,35) +
  xlab("Mean Negative Affect") + ylab("Frequency") + labs(title="(b)") 
mli.hist1 <- grid.arrange(na.all,na.mean,ncol=1)

# Summarize Level 2 covariates using data set with one observation per subject,
music.lev2 %>% ungroup(id) %>% count(gender)
music.lev2 %>% ungroup(id) %>% count(instrument)

nem1 <- ggplot(data=music.lev2,aes(x=mpqnem)) + 
  geom_histogram(binwidth = 5, fill = "white",color = "black") + theme.1 + 
  xlab("NEM") + ylab("Frequency") + labs(title="(a)")
pem1 <- ggplot(data=music.lev2,aes(x=mpqpem)) + 
  geom_histogram(binwidth = 5, fill = "white", color = "black") + theme.1 + 
  xlab("PEM") + ylab("") + labs(title="(b)")
abs <- ggplot(data=music.lev2,aes(x=mpqab)) + 
  geom_histogram(binwidth = 5, fill = "white", color = "black ") + theme.1 + 
  xlab("Absorption") + ylab("") + labs(title="(c)")
mli.histmat1 <- grid.arrange(nem1,pem1,abs,ncol=3)

# Look at relationships among Level 1 covariates and primary response
#   (again ignoring correlation).  Boxplots for categorical covariates and
#   scatterplots and lattice plot for continuous covariates.
# boxplot of negative affect by performance type
box.perform <- ggplot(data=music,aes(factor(perform_type),na)) + geom_boxplot() + 
  theme.1 + coord_flip() + ylab("Negative affect") + xlab("") + labs(title="(a)")
# boxplot of negative affect by audience
box.audience <- ggplot(data=music,aes(factor(audience),na)) + geom_boxplot() + 
  theme.1 + coord_flip() +  ylab("Negative affect") + xlab("") + labs(title="(b)")
# scatterplot of negative affect versus number of previous performances
## flip coordinates
scatter.previous <- ggplot(data=music, aes(x=previous,y=na)) + geom_point() + 
  theme.1 + geom_smooth(method="lm",color="black") + ylab("Negative affect") + 
  xlab("Previous Performances") + labs(title="(c)")
# all three together
mli.boxscatmat1 <- grid.arrange(box.perform,box.audience,scatter.previous,ncol=2)

# Lattice plot for NA vs. Performance Type
ggplot(music,aes(x=factor(perform_type),y=na)) + theme.1 + 
  geom_dotplot(binaxis="y",stackdir="center",binwidth=25/30) + facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + coord_flip() +
  labs(x="Performance Type",y="Negative Affect")

# Lattice plot for NA vs. Audience
ggplot(music,aes(x=factor(audience),y=na)) + theme.1 + 
  geom_dotplot(binaxis="y",stackdir="center",binwidth=25/30) + facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + coord_flip() +
  labs(x="Audience",y="Negative Affect")

# Lattice plot for NA vs. Previous Performances
ggplot(music,aes(x=previous,y=na)) + theme.1 + 
  geom_point() + geom_smooth(method="lm",color="black") + facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + ylim(10,35) +
  labs(x="Previous Performances",y="Negative Affect")

# Look at relationships among Level 2 covariates and negative affect
#   (again ignoring correlation)
instr.all <- ggplot(data=music,aes(factor(instrument),na)) + geom_boxplot() + 
  coord_flip() + theme.1 + ylab("Negative Affect") + xlab("") + labs(title="(a)") +
  ylim(10,35)
instr.mean <- ggplot(data=music.lev2,aes(factor(instrument),meanbysubj)) + 
  geom_boxplot() + coord_flip() + theme.1 + ylab("Mean Negative Affect") + xlab("") + 
  labs(title="(b)") + ylim(10,35)
mli.boxmat1 <- grid.arrange(instr.all,instr.mean,ncol=1)

pem2.all <- ggplot(data=music,aes(x=mpqpem,y=na)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("Negative Affect") + 
  xlab("PEM") + labs(title="(a1)")
nem2.all <- ggplot(data=music,aes(x=mpqnem,y=na)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("") + xlab("NEM") + 
  labs(title="(b1)")
abs2.all <- ggplot(data=music,aes(x=mpqab,y=na)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("") + 
  xlab("Absorption") + labs(title="(c1)")
pem2.mean <- ggplot(data=music.lev2,aes(x=mpqpem,y=meanbysubj)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("Mean Negative Affect") + 
  xlab("PEM") + labs(title="(a2)")
nem2.mean <- ggplot(data=music.lev2,aes(x=mpqnem,y=meanbysubj)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("") + xlab("NEM") + 
  labs(title="(b2)")
abs2.mean <- ggplot(data=music.lev2,aes(x=mpqab,y=meanbysubj)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("") + 
  xlab("Absorption") + labs(title="(c2)")
mli.scatmat1 <- grid.arrange(pem2.all,nem2.all,abs2.all,
  pem2.mean,nem2.mean,abs2.mean,ncol=3)


# Try using multiple linear regression to analyze data even though 
#   many assumptions are violated.
# First generate indicator variables for two covariates used in Section 3
music <- music %>%
  mutate(orch = ifelse(instrument=="orchestral instrument",1,0),
         large = ifelse(perform_type=="Large Ensemble",1,0) )
# Run the regression model, assuming linear, indep, normal, equal var
modelc0 = lm(na ~ orch + large + orch:large, data=music)
summary(modelc0)
# residual diagnostic plots
# plot(modelc0)

# Add new indicators to music data set
music <- music %>%
  mutate(students = ifelse(audience=="Student(s)",1,0),
         juried = ifelse(audience=="Juried Recital",1,0),
         public = ifelse(audience=="Public Performance",1,0),
         solo = ifelse(perform_type=="Solo",1,0),
         memory1 = ifelse(memory=="Memory",1,0),
         female = ifelse(gender=="Female",1,0),
         vocal = ifelse(instrument=="voice",1,0) )

#Model id22 (for Musician #22 only)
music %>% filter(id==22) %>% 
  select(id, diary, perform_type, audience, na, instrument)
id22 <- music %>% filter(id==22)   # pick off data for Musician 22
regr.id22 = lm(na ~ large, data=id22)
summary(regr.id22)

# LVCF analysis (can't analyze large with only 1 obs per subject)
lvcf.na <- music %>% 
  group_by(id) %>%
  filter(row_number() == n() ) %>%
  select(id, na) %>%
  rename (lvcf.na = na)
music.lev2 <- music.lev2 %>%
  left_join(lvcf.na, by = "id") %>%
  mutate(orch = ifelse(instrument=="orchestral instrument",1,0) )
lvcf.model=lm(lvcf.na ~ orch, data = music.lev2)
summary(lvcf.model)

# histogram for the intercepts by subject
#  "by" line runs "coefficients" function for each subject
#  "coefficients" puts int and slope from regression of na on large into list
#  "[[1]]" picks off the first value from the list (the intercept)
int <- by(music, music$id, function(data)
                  coefficients(lm(na ~ large, data = data))[[1]])
summary(int)   # summary statistics for 37 intercepts
music.lev2 <- music.lev2 %>%
  ungroup(id) %>%
  mutate(int = int[1:37])
inthist <- ggplot(data=music.lev2,aes(x=int)) + 
  geom_histogram(color="black",fill="white",binwidth=2) + theme.1 + 
  xlab("Intercepts from 37 subjects") + ylab("Frequency") + labs(title="(a)")

# histogram for fitted rate of change
#   same as above, except second coefficient (slope) picked off
rate <- by(music, music$id, function(data)
              coefficients(lm(na ~ large, data = data))[[2]])
summary(rate)
music.lev2 <- music.lev2 %>%
  mutate(rate = rate[1:37])
slopehist <- ggplot(data=music.lev2,aes(x=rate)) + 
  geom_histogram(color="black",fill="white",binwidth=2) + theme.1 + 
  xlab("Slopes from 37 subjects") + ylab("Frequency") + labs(title="(b)")
mli.histmat2 <- grid.arrange(inthist,slopehist,ncol=1)

#    Descriptive statistics of the estimates obtained by fitting the 
#      linear model by id.
mean(int)
sd(int)
mean(rate,na.rm=T)    # issues here since 7 NAs among rates
sd(rate,na.rm=T)
# correlation between slopes and intercepts
cor(int, rate,use="pairwise.complete.obs")   

# Fit Level Two models to fitted intercepts and slopes from Level One
model.int = lm(int ~ orch, data = music.lev2)
summary(model.int)
model.rate = lm(rate ~ orch, data = music.lev2)
summary(model.rate)

# Plot the relationships between instrument and intercept and slope
int.box <- ggplot(data=music.lev2,aes(factor(orch),int)) + geom_boxplot() + 
  theme.1 + coord_flip() + ylab("Fitted Intercepts") + xlab("Orchestral") + 
  labs(title="(a)")
slope.box <- ggplot(data=music.lev2,aes(factor(orch),rate)) + geom_boxplot() + 
  theme.1 + coord_flip() + ylab("Fitted Slopes") + xlab("Orchestral") + 
  labs(title="(b)")
mli.boxmat2 <- grid.arrange(int.box,slope.box,ncol=1)

# Plot the relationship between intercepts and slopes
ggplot(data=music.lev2,aes(x=int,y=rate)) + geom_point() + 
  geom_smooth(method="lm",color="black") + theme.1 + ylab("Fitted slopes") + 
  xlab("Fitted intercepts")


#Model 0 
model0 <- lmer(na ~ orch + large + orch:large +
  (large|id), REML=T, data=music)
summary(model0)

#Model A (Unconditional means model)
model.a <- lmer(na~ 1 + (1|id), REML=T, data=music)
summary(model.a)

# Plot random intercepts model
ints.a = fixef(model.a)[1] + ranef(model.a)[[1]][1]
n = length(music.lev2$id)
modela.plot = data.frame(id=music.lev2$id,ints.a=ints.a[[1]],slopes.a=rep(0,n))
ggplot() + theme.1 + 
  scale_x_continuous(name="Large Ensemble indicator", limits=c(0,1), breaks=c(0,1)) +
  scale_y_continuous(name="Negative Affect", limits=c(10,25)) +  
  geom_abline(data=modela.plot, aes(intercept=ints.a, slope=slopes.a), 
    color="dark gray") +
  geom_abline(aes(intercept=fixef(model.a)[1], slope=0), size=1) 


# Picture variance components from unconditional means model
ggplot(data=music) + geom_boxplot(aes(factor(id),na)) + theme.1 + 
  xlab("Subject ID") + ylab("Negative Affect")


#Model B (Add large as Level 1 covariate)
model.b <- lmer(na ~ large +  (large |id), 
  REML=T, data=music)
summary(model.b)

# Plot random intercepts and slopes model
ints.b = fixef(model.b)[1] + ranef(model.b)[[1]][1]
slopes.b = fixef(model.b)[2] + ranef(model.b)[[1]][2]
modelb.plot = data.frame(id=music.lev2$id,ints.b=ints.b[[1]],slopes.b=slopes.b[[1]])
ggplot() + theme.1 + 
  scale_x_continuous(name="Large Ensemble indicator", limits=c(0,1), breaks=c(0,1)) +
  scale_y_continuous(name="Negative Affect", limits=c(10,25)) +  
  geom_abline(data=modelb.plot, aes(intercept=ints.b, slope=slopes.b), 
    color="dark gray") +
  geom_abline(aes(intercept=fixef(model.b)[1], slope=fixef(model.b)[2]), size=1) 


#Model C (Add orch as Level 2 covariate.  Same as Model 0.)
model.c <- lmer(na ~ orch + large + orch:large +
  (large|id), REML=T, data=music)
summary(model.c)

#Model C2 (Run as random intercepts model.)
model.c2 <- lmer(na ~ orch + large + orch:large +
  (1|id), REML=T, data=music)
summary(model.c2)

# Model D (Add negative emotionality as second Level 2 covariate)
model.d <- lmer(na ~ orch + mpqnem + large + orch:large + 
  mpqnem:large + (large|id), REML=T, data=music)
summary(model.d)

# Compare Model D to Model C (they differ by 2 df)
#   Don't use REML if comparing nested models using LRT
model.c.ml <- lmer(na ~ orch + large + orch:large +
  (large|id), REML=F, data=music)
model.d.ml <- lmer(na ~ orch + mpqnem + large + orch:large + 
  mpqnem:large + (large|id), REML=F, data=music)
anova(model.d.ml,model.c.ml)
# anova() automatically uses ML for LRT tests
anova(model.d,model.c)

# Use LRT to determine if mpqnem:large is significant in Model D
model.d1.ml <- lmer(na ~ orch + mpqnem + large + orch:large + 
  (large|id), REML=F, data=music)
anova(model.d.ml,model.d1.ml)
# Again can just use REML=F models for nested comparisons
model.d1 <- lmer(na ~ orch + mpqnem + large + orch:large + 
  (large|id), data=music)
anova(model.d,model.d1)

# Model E (Center baseline NEM in Model D)
mean(music$mpqnem)
music <- music %>%
  mutate(cmpqnem = mpqnem - mean(mpqnem))
model.e <- lmer(na ~ orch + cmpqnem + large + orch:large + 
  cmpqnem:large + (large|id), REML=T, data=music)
summary(model.e)

# Model F (One - of many - reasonable final models)
model.f <- lmer(na ~ previous + students + juried + public + solo +
  mpqpem + mpqab + orch + mpqnem + mpqnem:solo + 
 (previous + students + juried + public + solo|id), 
  REML=T, data=music)
summary(model.f)


## Use lmeresampler to get bootstrapped CIs for fixed effects and var components

library(lmeresampler)
library(HLMdiag)

## running a parametric bootstrap (10 minutes or so)
oldw <- getOption("warn")
options(warn = -1)
boo1 <- bootstrap(model = model.f, fn = varcomp.mer, type = "parametric", B = 100)
# boo1
options(warn = oldw)

## you can extract the boostrapped values as a data frame
# as.data.frame(boo1)

## bootstrap confidence intervals are easily found using 'boot.ci'
##  - there are actually 22 variance components
library(boot)
boot.ci(boo1, index = 1, type=c("norm", "basic", "perc"))
boot.ci(boo1, index = 2, type=c("norm", "basic", "perc"))
# normal and basic CIs for D11 (index 2) include negatives

## you can also examine the bootstrap samples graphically
plot(boo1, index = 1)
plot(boo1, index = 2)


## repeat for fixed effects (8 minutes or so)
options(warn = -1)
boo2 <- bootstrap(model = model.f, fn = fixef, type = "parametric", B = 100)
# boo2
options(warn = oldw)

## you can extract the boostrapped values as a data frame
# as.data.frame(boo2)

## bootstrap confidence intervals are easily found using 'boot.ci'
##  - there are actually 11 fixed effects
boot.ci(boo2, index = 1, type=c("norm", "basic", "perc"))
boot.ci(boo2, index = 2, type=c("norm", "basic", "perc"))
boot.ci(boo2, index = 3, type = "perc")
boot.ci(boo2, index = 4, type = "perc")
boot.ci(boo2, index = 5, type = "perc")
boot.ci(boo2, index = 6, type = "perc")
boot.ci(boo2, index = 7, type = "perc")
boot.ci(boo2, index = 8, type = "perc")
boot.ci(boo2, index = 9, type = "perc")
boot.ci(boo2, index = 10, type = "perc")
boot.ci(boo2, index = 11, type = "perc")

## you can also examine the bootstrap samples graphically
plot(boo2, index = 1)
plot(boo2, index = 2)


## repeat for fixed effects with case residuals (7 minutes or so)
options(warn = -1)
boo3 <- bootstrap(model = model.f, fn = fixef, type = "case", B = 100, 
                  resample = c(TRUE, TRUE))
# boo3
options(warn = oldw)

## you can extract the boostrapped values as a data frame
# as.data.frame(boo3)

## bootstrap confidence intervals are easily found using 'boot.ci'
##  - there are actually 11 fixed effects
boot.ci(boo3, index = 1, type=c("norm", "basic", "perc"))
boot.ci(boo3, index = 2, type=c("norm", "basic", "perc"))

## you can also examine the bootstrap samples graphically
plot(boo3, index = 1)
plot(boo3, index = 2)

## end of bootstrapped CIs


# Create data where multilevel model gives different result than OLS regression
#   Hopefully see differences in both coefficients and SEs

# Simulation 1 - OLS coefficient is wrong direction
subject = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10))
lambda0 = c(rep(10,10),rep(20,10),rep(30,10),rep(40,10))
lambda1 = rep(-0.5,40)
previj = c(1:10,4:13,7:16,10:19)
eij = rnorm(40,0,1)
yij = lambda0 + lambda1*previj + eij
simdata = data.frame(subject=subject,lambda0=lambda0,lambda1=lambda1,
  previj=previj,eij=eij,yij=yij)

plot(yij~previj)
olsreg.sim = lm(yij~previj)
summary(olsreg.sim)
AIC(olsreg.sim); BIC(olsreg.sim)
mlm.sim = lmer(yij~previj + (1|subject), data=simdata)
summary(mlm.sim)

# ggplot for first simulation
ints.sim = fixef(mlm.sim)[1] + ranef(mlm.sim)[[1]][1]
slopes.sim = rep(fixef(mlm.sim)[2],4)
subj.sim = c("Subject 1", "Subject 2", "Subject 3", "Subject 4")
sim1.plot = data.frame(id=subj.sim,ints.sim=ints.sim[[1]],slopes.sim=slopes.sim)
sim1.plot2 = data.frame(model=c("MLM","OLS"), int2=c(fixef(mlm.sim)[1],
  summary(olsreg.sim)$coefficients[1,1]), slp2=c(fixef(mlm.sim)[2],
  summary(olsreg.sim)$coefficients[2,1]))
ggplot() + theme.1 + 
  scale_x_continuous(name="Previous Performances", limits=c(0,20)) +
  scale_y_continuous(name="Negative Affect", limits=c(0,40)) + 
  geom_point(data=simdata, aes(x=previj,y=yij)) + 
  geom_abline(data=sim1.plot, aes(intercept=ints.sim, slope=slopes.sim, 
    linetype=id, group=id), color="dark gray", show_guide=TRUE) +
  geom_abline(data=sim1.plot2, aes(intercept=int2, slope=slp2, 
    linetype=model, group=model), size=1, show_guide=TRUE)


# Simulation 2 - use estimated parameters from Model C
#   - SEs usually higher with OLS
orch.sim = music$orch
large.sim = music$large
subj.sim = music$id
reps.sim = as.numeric(table(music$id))
b00 = 15.9297
b01 = 1.6926
b10 = -0.9106
b11 = -1.4239
sig01 = (-0.635)*2.3781*0.67236
Sigma = matrix(c(2.3781^2,sig01,sig01,0.67236^2),2,2)

OLSest = matrix(NA,nrow=1000,ncol=4)
OLSse = matrix(NA,nrow=1000,ncol=4)
OLStval = matrix(NA,nrow=1000,ncol=4)
OLSaic = rep(NA,1000); OLSbic = rep(NA,1000)
MLMest = matrix(NA,nrow=1000,ncol=4)
MLMse = matrix(NA,nrow=1000,ncol=4)
MLMtval = matrix(NA,nrow=1000,ncol=4)
MLMaic = rep(NA,1000); MLMbic = rep(NA,1000)

for(k in 1:1000)  {
  e0e1 = mvrnorm(37,c(0,0),Sigma)
  e0i = rep(e0e1[,1],times=reps.sim)
  e1i = rep(e0e1[,2],times=reps.sim)
  lam0i = b00 + b01*orch.sim + e0i
  lam1i = b10 + b11*orch.sim + e1i
  Eij = rnorm(497,0,4.66984)
  Yij = lam0i + lam1i*large.sim + Eij
  data.sim = data.frame(subj.sim=subj.sim,orch.sim=orch.sim,large.sim=large.sim,
    e0i=e0i,e1i=e1i,lam0i=lam0i,lam1i=lam1i,Eij=Eij,Yij=Yij)

  olsreg.sim = lm(Yij~orch.sim+large.sim+orch.sim:large.sim, data=data.sim)
  OLSest[k,]=summary(olsreg.sim)$coefficients[,1]
  OLSse[k,]=summary(olsreg.sim)$coefficients[,2]
  OLStval[k,]=summary(olsreg.sim)$coefficients[,3]
  OLSaic[k]=AIC(olsreg.sim)
  OLSbic[k]=BIC(olsreg.sim)

  mlm.sim = lmer(Yij ~ orch.sim + large.sim + orch.sim:large.sim +
    (large.sim|subj.sim), REML=T, data=data.sim)
  MLMest[k,]=fixef(mlm.sim)
  MLMse[k,]=sqrt(diag(vcov(mlm.sim)))
  MLMtval[k,]=MLMest[k,]/MLMse[k,]   }

apply(OLSest,2,summary)
apply(MLMest,2,summary)
apply(OLSse,2,summary)
apply(MLMse,2,summary)
apply(OLStval,2,summary)
apply(MLMtval,2,summary)
#summary(OLSaic)
#summary(OLSbic)
#summary(as.numeric(MLMaic))
#summary(as.numeric(MLMbic))

par(mfrow=c(2,2))
plot(density(MLMtval[,1])); lines(density(OLStval[,1]),lty=2)
plot(density(MLMtval[,2])); lines(density(OLStval[,2]),lty=2)
plot(density(MLMtval[,3])); lines(density(OLStval[,3]),lty=2)
plot(density(MLMtval[,4])); lines(density(OLStval[,4]),lty=2)
#par(mfrow=c(2,1))
#plot(density(as.numeric(MLMaic))); lines(density(OLSaic),lty=2)
#plot(density(as.numeric(MLMbic))); lines(density(OLSbic),lty=2)

# ggplots for second simulation
est.data = data.frame(b00=c(MLMest[,1],OLSest[,1]),b01=c(MLMest[,2],OLSest[,2]),
  b10=c(MLMest[,3],OLSest[,3]),b11=c(MLMest[,4],OLSest[,4]),
  model=c(rep("MLM",1000),rep("OLS",1000)))
b00dens <- ggplot(data=est.data, aes(x=b00,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(hat(Beta)["00"])) + 
  ylab("Density") + labs(title="(a)")
b01dens <- ggplot(data=est.data, aes(x=b01,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(hat(Beta)["01"])) + 
  ylab("Density") + labs(title="(b)")
b10dens <- ggplot(data=est.data, aes(x=b10,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(hat(Beta)[10])) + 
  ylab("Density") + labs(title="(c)")
b11dens <- ggplot(data=est.data, aes(x=b11,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(hat(Beta)[11])) + 
  ylab("Density") + labs(title="(d)")
mli.density1 <- grid.arrange(b00dens,b01dens,b10dens,b11dens,ncol=2)

se.data = data.frame(b00=c(MLMse[,1],OLSse[,1]),b01=c(MLMse[,2],OLSse[,2]),
  b10=c(MLMse[,3],OLSse[,3]),b11=c(MLMse[,4],OLSse[,4]),
  model=c(rep("MLM",1000),rep("OLS",1000)))
b00dens <- ggplot(data=se.data, aes(x=b00,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(SE(hat(Beta)["00"]))) + 
  ylab("Density") + labs(title="(a)")
b01dens <- ggplot(data=se.data, aes(x=b01,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(SE(hat(Beta)["01"]))) + 
  ylab("Density") + labs(title="(b)")
b10dens <- ggplot(data=se.data, aes(x=b10,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(SE(hat(Beta)[10]))) + 
  ylab("Density") + labs(title="(c)")
b11dens <- ggplot(data=se.data, aes(x=b11,linetype=model,group=model)) + 
  geom_density(fill=NA) + theme.1 + xlab(expression(SE(hat(Beta)[11]))) + 
  ylab("Density") + labs(title="(d)")
mli.density2 <- grid.arrange(b00dens,b01dens,b10dens,b11dens,ncol=2)

