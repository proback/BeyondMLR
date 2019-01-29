# TwoLevel Longitudinal.R

chart.wide <- read.csv("~/Stats 316 S18/Class/Data/chart_wide_condense.csv")
library(mice)
md.pattern(chart.wide)

# Create data frame in LONG form (one obs per school-year)
#   chart.long is 1854x10 with 121 NAs for MathAvgScore
library(tidyverse)
library(reshape2)
chart.long <- chart.wide %>%
  gather(key = "key", value = "MathAvgScore", MathAvgScore.0:MathAvgScore.2) %>%
  separate(key, into = c("name", "year08"), sep = "\\.") %>%
  select(-c(X, name)) %>%
  arrange(schoolid, year08) %>%
  mutate(year08 = as.numeric(year08))
head(chart.long)

# print first 6 rows after sorting by school and year within school
smallchart.long <- filter(chart.long, row_number() <= 72)
head(smallchart.long)

# Add average across all years for each school for EDA plots
chart.means <- chart.long %>%
  group_by(schoolid) %>%
  summarise(mean3yr = mean(MathAvgScore, na.rm=T))
chart.wide <- chart.wide %>%
  mutate(urban0 = ifelse(urban==1, "urban", "rural"),
         charter0 = ifelse(charter==1, "charter", "public non-charter")) %>%
  left_join(chart.means, by="schoolid")
  
wide.charter <- chart.wide %>% filter(charter == 1)
set.seed(27)  #pulls same random sample every time
samp = sample(1:length(chart.wide$charter==0), size=dim(wide.charter)[1])
samp   # getting equal number of charters and non-charters

wide.public <- chart.wide %>%
  filter(charter == 0) %>%
  sample_n( dim(wide.charter)[1] )
sampdata <- bind_rows(wide.charter, wide.public) %>%
  select(-X) %>%
  mutate(vars = row_number())   # Just use numbers 1-146 as school ids
head(sampdata)

sampdata.l <- sampdata %>%
  gather(key = "key", value = "MathAvgScore", MathAvgScore.0:MathAvgScore.2) %>%
  separate(key, into = c("name", "year08"), sep = "\\.") %>%
  select(-name) %>%
  arrange(charter, vars, year08) %>%
  mutate(year08 = as.numeric(year08))
head(sampdata.l)

##########################################################################

##Packages
library(lattice)
library(nlme)
library(lme4)
library(ggplot2)
library(gridExtra)
# library(HH)   
theme.1 <- theme(axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  plot.title=element_text(hjust=.9,face="italic",size=12))


###EDA###
# table(chart.wide$charter0)
# table(chart.wide$urban0)
chart.wide %>% count(charter0)
chart.wide %>% count(urban0)

###histogram of mean math scores (1 obs per school)
ggplot(data=chart.wide,aes(x=mean3yr)) + 
  geom_histogram(binwidth=5,color="black",fill="white") + theme.1 + 
  xlab("Mean Math Scores by School") + ylab("Frequency")

#summaries (charter vs. non-charter) - 1 obs per school
# tapply(chart.wide$mean3yr, chart.wide$charter, summary)
chart.wide %>% group_by(charter0) %>%
  summarise(means = mean(mean3yr), sds = sd(mean3yr), meds = median(mean3yr),
            q1s = quantile(mean3yr, 0.25), q3s = quantile(mean3yr, 0.75),
            mins = min(mean3yr), maxs = max(mean3yr), ns = n())

#summaries (urban vs. rural)
chart.wide %>% group_by(urban0) %>%
  summarise(means = mean(mean3yr), sds = sd(mean3yr), meds = median(mean3yr),
            q1s = quantile(mean3yr, 0.25), q3s = quantile(mean3yr, 0.75),
            mins = min(mean3yr), maxs = max(mean3yr), ns = n())

###boxplots of math scores by charter v. public and rural v. urban
charter.school <- ggplot(data=chart.wide, aes(x=factor(charter0),y=mean3yr)) + 
  geom_boxplot() + coord_flip() + theme.1 + ylab("Mean Math Scores by School") + 
  xlab("") + labs(title="(a)")
urban.school <- ggplot(data=chart.wide, aes(x=factor(urban0),y=mean3yr)) + 
  geom_boxplot() + coord_flip() + theme.1 + ylab("Mean Math Scores by School") + 
  xlab("") + labs(title="(b)")
lon.box1 <- grid.arrange(charter.school,urban.school,ncol=1,nrow=2)

#Changing percentage scale to 0 to 100
chart.long <- chart.long %>%
  mutate(SchPctFree = schPctfree*100, SchPctSped = schPctsped*100,
         SchPctNonw = schPctnonw*100)

chart.wide <- chart.wide %>%
  mutate(SchPctFree.wide = schPctfree*100, SchPctSped.wide = schPctsped*100, 
         SchPctNonw.wide = schPctnonw*100)

# math score vs. continuous level two covariates
PctFree.school <- ggplot(data=chart.wide,aes(x=schPctfree,y=mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Percent Free/Reduced Lunch") + 
  ylab("Mean Math Scores by School") + labs(title="(a)") +
  geom_smooth(se=FALSE,method="lm",color="black")
PctSped.school <- ggplot(data=chart.wide,aes(x=schPctsped,y=mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Percent Special Ed") + 
  ylab("Mean Math Scores by School") + labs(title="(b)") + 
  geom_smooth(se=FALSE,method="lm",color="black")
PctNonw.school <- ggplot(data=chart.wide,aes(x=schPctnonw,y=mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Percent Non-white") + 
  ylab("Mean Math Scores by School") + labs(title="(c)") + 
  geom_smooth(se=FALSE,method="lm",color="black")
lon.scat1 <- grid.arrange(PctFree.school,PctSped.school,PctNonw.school,ncol=2)

#Lattice plots
#  First change names of Central and Chaska
df <- mtcars %>% mutate(cyl = factor(cyl, levels = c(4, 6, 8)))

smallchart.long <- smallchart.long %>%
  mutate(schoolName = factor(schoolName, levels = 
    c(levels(schoolName), "CENTRAL108", "CENTRAL13", "CHASKAEAST", "CHASKAWEST") ),
    schname10 = strtrim(schoolName, 10))
smallchart.long$schoolName[7:9]="CENTRAL108"
smallchart.long$schoolName[37:39]="CHASKAEAST"
smallchart.long$schoolName[40:42]="CHASKAWEST"
smallchart.long$schoolName[61:63]="CENTRAL13"

#lon-lat1.eps
ggplot(smallchart.long,aes(x=year08,y=MathAvgScore)) + theme.1 + 
  geom_point() + geom_line() + facet_wrap(~schoolName,ncol=6) + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  theme(strip.text.x=element_blank()) + labs(x="Years since 2008",y="Math Score")

#lon-spag1.eps
ggplot(sampdata.l,aes(x=year08,y=MathAvgScore)) + theme.1 + 
  geom_line(aes(group=schoolid),color="dark grey") + 
  geom_smooth(aes(group=1),color="black",size=1) + 
  labs(x="Years since 2008",y="Math Score") 

#lon-lat2.eps
ggplot(smallchart.long,aes(x=year08,y=MathAvgScore)) + theme.1 + 
  geom_point() + stat_smooth(method=lm) + facet_wrap(~schoolName,ncol=6) + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  scale_y_continuous(limits=c(640,665), breaks=c(640, 650, 660)) +
  theme(strip.text.x=element_blank()) + labs(x="Years since 2008",y="Math Scores")

##Spaghetti Plots

#get rid of NA data
newsampdata.l <- sampdata.l %>% na.omit()

#lon-spag3.eps
ggplot(newsampdata.l,aes(x=year08, y=MathAvgScore)) + 
  geom_line(aes(group=schoolid),color="dark grey") + 
  facet_grid(~charter0) + 
  geom_smooth(aes(group=1),color="black",size=1) + 
  labs(x="Years since 2008",y="Math Scores") 

library(data.table)
library(Hmisc)

newsampdata.l <- newsampdata.l %>%
  mutate(splitup = paste("Quartile", as.numeric(cut2(schPctfree, g=4))))
ggplot(newsampdata.l,aes(x=year08,y=MathAvgScore)) + 
  geom_line(aes(group=schoolid),color="dark grey") + 
  facet_grid(~splitup) +
  labs(x="Years since 2008",y="Math Scores") + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  theme.1 + geom_smooth(method="loess",color="black",se=FALSE,size=.75)


#95% CI's for slope and intercept of 24 schools	(2 are filtered out since 1 obs)		
library(tidyr)
library(broom)
regressions <- smallchart.long %>% 
  group_by(schoolid) %>% 
  do(fit = lm(MathAvgScore ~ year08, data=.))

sd_filter <- smallchart.long %>%
  group_by(schoolid) %>%
  summarise(sds = sd(MathAvgScore)) 

regressions <- regressions %>%
  right_join(sd_filter, by="schoolid") %>%
  filter(!is.na(sds))

lm_info1 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = year08, int = `(Intercept)`)

lm_info2 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, std.error) %>%
  spread(key = term, value = std.error) %>%
  rename(se_rate = year08, se_int = `(Intercept)`)

lm_info <- regressions %>%
  glance(fit) %>%
  ungroup() %>%
  select(schoolid, r.squared, df.residual) %>%
  inner_join(lm_info1, by = "schoolid") %>%
  inner_join(lm_info2, by = "schoolid") %>%
  mutate(tstar = qt(.975, df.residual), 
         intlb = int - tstar * se_int, intub = int + tstar * se_int,
         ratelb = rate - tstar * se_rate, rateub = rate + tstar * se_rate)
head(data.frame(lm_info))

#lon-cis1.eps
slope.ci <- ggplot(lm_info, aes(y=int, x=1:22)) + geom_point() + theme.1 + 
  geom_errorbar(aes(ymin=intlb, ymax=intub)) + 
  coord_flip() + labs(y="Intercepts",x="Schools",title="(a)")
int.ci <- ggplot(lm_info, aes(y=rate, x=1:22)) + geom_point() + theme.1 + 
  geom_errorbar(aes(ymin=ratelb, ymax=rateub)) + 
  coord_flip() + labs(y="Slopes",x="Schools",title="(b)")
lon.cis1 <- grid.arrange(slope.ci, int.ci, ncol=2)


# Find slope and intercept of all 618 schools	(540 after filter those with 1 obs)		
regressions <- chart.long %>% 
  group_by(schoolid) %>% 
  do(fit = lm(MathAvgScore ~ year08, data=.))

sd_filter <- chart.long %>%
  group_by(schoolid) %>%
  summarise(sds = sd(MathAvgScore)) 

regressions <- regressions %>%
  right_join(sd_filter, by="schoolid") %>%
  filter(!is.na(sds))

lm_info1 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = year08, int = `(Intercept)`)

lm_info2 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, std.error) %>%
  spread(key = term, value = std.error) %>%
  rename(se_rate = year08, se_int = `(Intercept)`)

lm_info <- regressions %>%
  glance(fit) %>%
  ungroup() %>%
  select(schoolid, r.squared, df.residual) %>%
  inner_join(lm_info1, by = "schoolid") %>%
  inner_join(lm_info2, by = "schoolid") %>%
  mutate(tstar = qt(.975, df.residual), 
         intlb = int - tstar * se_int, intub = int + tstar * se_int,
         ratelb = rate - tstar * se_rate, rateub = rate + tstar * se_rate)
head(data.frame(lm_info))

# summary stats for intercepts  					
summary(lm_info$int)
sd(lm_info$int)

# summary stats for fitted rate of change
summary(lm_info$rate)
sd(lm_info$rate,na.rm=T)

# summary stats for R sq
summary(lm_info$r.squared)

# histograms for ints, rates of change, and Rsq values - lon-histmat1.eps
int.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=int), binwidth=4, color="black", fill="white") + theme.1 + 
  labs(x="Intercepts", y="Frequency", title="(a)")
rate.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=rate), binwidth=2, color="black", fill="white") + theme.1 + 
  labs(x="Slopes", y="Frequency", title="(b)")
rsq.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=r.squared), binwidth=0.2, color="black", fill="white") + theme.1 +
  labs(x="Rsquared values", y="Frequency", title="(c)")
lon.histmat1 <- grid.arrange(int.hist1, rate.hist1, rsq.hist1, ncol=2)

# correlation between slopes and intercepts for subjects with slope
with(lm_info, cor(int, rate, use="complete.obs")) 
ggplot(data = lm_info, aes(x = int, y = rate)) + 
  geom_point(color = "dark grey") + theme.1 + xlab("Fitted Intercepts") + 
  ylab("Fitted Slopes") + geom_smooth(se=FALSE, method="lm", color="black")

# Boxplots to compare school types
chart.wide <- lm_info %>%
  select(schoolid, int, rate , r.squared) %>%
  right_join(chart.wide, by = "schoolid")

#lon-box2.eps
int.box1 <- ggplot(chart.wide) + geom_boxplot(aes(x = factor(charter0), y = int)) + 
  theme.1 + coord_flip() + labs(x="School Type", y="Fitted Intercepts", title="(a)")
rate.box1 <- ggplot(chart.wide) + geom_boxplot(aes(x = factor(charter0), y = rate)) + 
  theme.1 + coord_flip() + labs(x="School Type", y="Fitted Slopes", title="(b)")
lon.box2 <- grid.arrange(int.box1, rate.box1, nrow=2)

#lon-box3.eps
year08.box <- chart.long %>% filter(year08 == 0) %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(charter), y = MathAvgScore)) + 
  theme.1 + coord_flip() + labs(x="School Type", y="Math Score in 2008", title="(a)")
year10.box <- chart.long %>% filter(year08 == 2) %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(charter), y = MathAvgScore)) + 
  theme.1 + coord_flip() + labs(x="School Type", y="Math Score in 2010", title="(b)")
lon.box3 <- grid.arrange(year08.box, year10.box, nrow=2)

# OLS estimates plotted against the predictor Pct Free and Reduced Lunch
box1 <- ggplot(chart.wide) + geom_boxplot(aes(x = factor(charter0), y = SchPctFree.wide)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type", y="Percent Free and Reduced Lunch", title="(a)")
int.scat1 <- ggplot(chart.wide, aes(x = SchPctFree.wide, y = int)) + geom_point() + 
  theme.1 + labs(x="Percent Free and Reduced Lunch", y="Fitted Intercepts", title="(b)") + 
  geom_smooth(se=FALSE, method="lm", color="black", size=.75)
rate.scat1 <- ggplot(chart.wide, aes(x = SchPctFree.wide, y = rate)) + geom_point() + 
  theme.1 + labs(x="Percent Free and Reduced Lunch", y="Fitted Slopes", title="(c)") + 
  geom_smooth(se=FALSE, method="lm", color="black", size=.75)
lon.boxscatmat1 <- grid.arrange(box1, int.scat1, rate.scat1, ncol=2)

# Divide subjects into low and high percent FRL at median in order to 
#   illustrate charter school effect by percent FRL
with(chart.wide, cor( cbind( schPctnonw, int, rate), use="pairwise.complete.obs"))
medpctfree <- median(chart.wide$schPctfree)
chart.wide <- chart.wide %>%
  mutate(highpctfree = ifelse(schPctfree > medpctfree,"High Pct Free/Reduced Lunch",
                              "Low Pct Free/Reduced Lunch"))

#lon-boxmat1.eps
int.box <- ggplot(chart.wide) + theme.1 +
  geom_boxplot(aes(x=factor(charter0),y=int)) + coord_flip() +
  labs(x="School Type",y="Fitted Intercepts",title="(a)") +
  facet_grid(highpctfree~.)
rate.box <- ggplot(chart.wide) + theme.1 +
  geom_boxplot(aes(x=factor(charter0),y=rate)) + coord_flip() +
  labs(x="School Type",y="Fitted Slopes",title="(b)") +
  facet_grid(highpctfree~.)
lon.boxmat1 <- grid.arrange(int.box,rate.box,ncol=2)


##Correlation structure	
library(GGally)
score.nonm <- chart.long %>%
  filter(is.na(MathAvgScore) == FALSE)
hgtm.lm <- lm(MathAvgScore~year08, data=score.nonm)
score.nonm <- score.nonm %>%
  mutate(lmres = resid(hgtm.lm))

hgtwm <- score.nonm %>%
  select(schoolid, lmres, year08) %>%
  mutate(name = rep("lmres", n())) %>%
  unite(newcol, name, year08, sep = ".") %>%
  spread(key = newcol, value = lmres)

#lon-chart-cor1.eps
ggpairs(hgtwm[,c(2:4)], upper = list(), lower = list(continuous = "smooth"),
  diag = list(continuous = "bar", discrete = "bar"),
  axisLabels = "show") 

#lon-scat3.eps
Norwood <- chart.long %>% slice(7:9)
model0 <- lm(MathAvgScore ~ year08, data = Norwood)
ggplot(Norwood, aes(x = year08, y = MathAvgScore)) + 
  theme.1 + geom_point() + scale_y_continuous(limits=c(654,661)) +
  geom_smooth(se=FALSE, method="lm", color="black", size=.75) +
  labs(x="Years since 2008", y="Math Score", title="Norwood Central") + 
  geom_segment(aes(x = year08[1], y = MathAvgScore[1], 
    xend = year08[1], yend = model0$fitted.values[1]), linetype=2) +
  geom_segment(aes(x = Norwood$year08[2], y = MathAvgScore[2], 
    xend = year08[2], yend = model0$fitted.values[2]), linetype=2) +
  geom_segment(aes(x = Norwood$year08[3], y = MathAvgScore[3], 
    xend = year08[3], yend = model0$fitted.values[3]), linetype=2)


############################################################################

#Model A (Unconditional means model)
model.a <- lmer(MathAvgScore~ 1 + (1|schoolid), REML=T, data=chart.long)
summary(model.a)

#Model B (Unconditional growth)
model.b <- lmer(MathAvgScore~ year08 + (year08|schoolid), 
  REML=T, data=chart.long)
summary(model.b)

# Modeling quadratic time trend
chart.long <- chart.long %>%
  mutate(yearc = year08 - 1, yearc2 = yearc ^ 2)
model.b2 <- lmer(MathAvgScore~ yearc + yearc2 + (1|schoolid), 
  REML=T, data=chart.long)
summary(model.b2)   # better than model.b - signif pos sq term
                    # would indicate faster growth 09-10 than 08-09
# model.b3 <- lmer(MathAvgScore~ yearc + yearc2 + (yearc+yearc2|schoolid), 
#   REML=T, data=chart.long)
# summary(model.b3)   # won't run - too many random effects
model.b4 <- lmer(MathAvgScore~ yearc + (yearc|schoolid), 
  REML=T, data=chart.long)
summary(model.b4)   # same as model.b
model.b5 <- lmer(MathAvgScore~ year08 + (1|schoolid), 
  REML=T, data=chart.long)
summary(model.b5)   # very similar to model.b - aic favors b, bic favors b5

# Modeling piecewise linear time trend
chart.long <- chart.long %>%
  mutate(year0809 = ifelse(year08==2, 0, year08),
         year0910 = ifelse(year08==0, 0, year08 - 1))
model.b6 <- lmer(MathAvgScore~ year0809 + year0910 + (1|schoolid), 
  REML=T, data=chart.long)
summary(model.b6)   # tiny bit better than model.b2 but same story
                    # slope of 0.2 in 0809 but 2.5 in 0910
# model.b7 <- lmer(MathAvgScore~ year0809 + year0910 + (year0809+year0910|schoolid), 
#   REML=T, data=chart.long)
# summary(model.b7)   # again, too many random effects


#Model C (uncontrolled effects of school type on intercept and slope)
model.c <- lmer(MathAvgScore~ charter + year08 + charter:year08 +
  (year08|schoolid), REML=T, data=chart.long)
summary(model.c)


#    Model B
fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + c(0,1,2)*fixef.b[[2]]
fit.frame1 <- data.frame(fit.b=fit.b,num=c(0,1,2))
fit.plot1 <- ggplot(fit.frame1,aes(x=num,y=fit.b)) + 
  geom_point(shape=1,fill="black",size=3) + theme.1 + geom_line() +
  scale_y_continuous(limits=c(640,660)) + 
  labs(x="Years since 2008",y="Predicted Math Score",
    title="Model B \n Unconditional growth")

#    Model C.
fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + c(0,1,2)*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + 
          c(0,1,2)*fixef.c[[3]] +
          c(0,1,2)*fixef.c[[4]]
fit.frame2 <- data.frame(fit=c(fit.c0,fit.c1),num=c(0,1,2,0,1,2),
  type0=c(rep("Public Non-charter",3),rep("Charter",3)))
fit.plot2 <- ggplot(fit.frame2,aes(x=num,y=fit)) + theme.1 +
  geom_point(aes(shape=type0)) + 
  theme(legend.position=c(.2,.9)) + theme(legend.title=element_blank()) +
  geom_line(aes(linetype=type0)) + scale_y_continuous(limits=c(640,660)) + 
  labs(x="Years since 2008",y="Predicted Math Score",
    title="Model C \n Uncontrolled charter effect")

#lon-scat4.eps
lon.scat4 <- grid.arrange(fit.plot1, fit.plot2, ncol=2)


#Model D (Introduce urban at level 2)
model.d <- lmer(MathAvgScore~ charter + urban + year08 + 
  charter:year08 + urban:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d)

#Model D2 (Introduce SchPctFree at level 2)
model.d2 <- lmer(MathAvgScore~ charter + SchPctFree + year08 + 
  charter:year08 + SchPctFree:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d2)
anova(model.d2,model.c)

#Model D3 (Introduce SchPctNonw at level 2)
model.d3 <- lmer(MathAvgScore~ charter + SchPctNonw + year08 + 
  charter:year08 + SchPctNonw:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d3)

#Model D4 (Introduce SchPctSped at level 2)
model.d4 <- lmer(MathAvgScore~ charter + SchPctSped + year08 + 
  charter:year08 + SchPctSped:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d4)

########## Potential Final Models #############

#Model F (add more level 2 covariates)
model.f <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + SchPctSped + year08 + 
  charter:urban + charter:SchPctFree +
  charter:SchPctNonw + charter:SchPctSped +
  charter:year08 + urban:year08 + 
  SchPctFree:year08 + SchPctNonw:year08 + SchPctSped:year08 +
  charter:urban:year08 + charter:SchPctSped:year08 +
  charter:SchPctFree:year08 + charter:SchPctNonw:year08 + 
  (year08|schoolid), REML=T, data=chart.long)
summary(model.f)

model.f1 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + year08 + charter:year08 + SchPctNonw:year08 +
  (year08|schoolid), REML=T, data=chart.long)
summary(model.f1)

model.f1a <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + year08 + charter:year08 + SchPctNonw:year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)
summary(model.f1a)

model.f2 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=T, data=chart.long)
summary(model.f2)

model.f2a <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)
summary(model.f2a)

model.f2ml <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=F, data=chart.long)
summary(model.f2ml)
model.f2aml <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=F, data=chart.long)
summary(model.f2aml)
anova(model.f2ml,model.f2aml)

# estimates of variance terms (not SDs) from Model f2
sig = 8.8231
sig0 = 16.9468
sig1 = .003569
sig01 = .981*sqrt(sig0*sig1)

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 25.76990 26.25599 26.74922
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 17.18806 17.42932 17.67772
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.6607791 0.6638479 0.6670471

# Repeat calculation of error cov matrices with sig01=0
sig = 8.8231
sig0 = 16.9468
sig1 = .003559
sig01 = 0

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 25.76990 25.77346 25.78414
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 16.94680 16.94680 16.95392
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.6575745 0.6574384 0.6576691



# Standard error covariance structure

std.lme <- lme(MathAvgScore~year08*charter, data= chart.long, 
  random=~year08|schoolid, control=lmeControl(opt="optim"),
  na.action=na.exclude)
summary(std.lme)

# Compare to standard error cov structure in lmer - matches well
std.lmer=lmer(MathAvgScore~year08*charter+(year08|schoolid),
  data=chart.long)
summary(std.lmer)

# estimates of variance terms (not SDs) from Model C (std) in lme
sig = 2.9481^2                # 8.6913
sig0 = 5.9972^2               # 35.9664
sig1 = .4756^2                # 0.2262
sig01 = .634*sqrt(sig0*sig1)  # 1.8083

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 44.65770 48.50057 52.79584
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 37.77475 39.58308 41.84381
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.8116708 0.8151952 0.8269095


# Unstructured error covariance structure
corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[2]]
    # must be number of subject with complete data
    # corStruct is lower triangle of pairwise correlations
    # corMatrix creates a large number of symmetric matrices with diag of 1s
  print(corm)
  covm <- getVarCov(glsob, individual=2)
  return(covm)}

unstruct <- gls(MathAvgScore~year08*charter, chart.long, 
  correlation=corSymm(form = ~ 1 |schoolid),
  weights=varIdent(form = ~ 1|year08),method="REML", na.action=na.exclude)
# corandcov(unstruct)   # give error for some reason

# Do VarCov matrix by hand since getVarCov() doesn't always work with gls
#   Thinks schools are identified by number (e.g. 2) instead of distschNum
S = corMatrix(unstruct$modelStruct$corStruct)[[2]]   # 2=ind. with full data
vw = 1/varWeights(unstruct$modelStruct$varStruct)[4:6] # 3 weights for ind. 2
vars = (unstruct$sigma^2)*vw
result = t(S * sqrt(vars))*sqrt(vars)
print(result)

unstr.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random =  ~ 1 | schoolid,
  correlation = corSymm(, form =  ~ 1 | schoolid), na.action=na.exclude,
  weights=varIdent(form = ~ 1|year08))
summary(unstr.lme)

# Compound symmetry error structure
comsym <- gls(MathAvgScore~year08*charter,chart.long, 
  na.action=na.exclude,
  correlation=corCompSymm(,form = ~ 1 |schoolid), method="REML")
corandcov(comsym)

cs.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random = ~ 1|schoolid,na.action=na.exclude,
  correlation=corCompSymm(,form = ~ 1 |schoolid))
summary(cs.lme)

# Compare to comp symm error cov structure in lmer - matches well
#   However, this doesn't include same restrictions on corr matrix and
#   hence has one fewer df.
cs.lmer=lmer(MathAvgScore~year08*charter+(1|schoolid),data=chart.long)
summary(cs.lmer)
anova(cs.lme,std.lme)

# Heterogeneous compound symmetry error structure
# lmeControl(msMaxIter=200)  # didn't help
hcs.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random =  ~ 1 | schoolid, na.action=na.exclude,
  correlation=corCompSymm(,form = ~ 1 |schoolid), 
  weights=varIdent(form = ~1|year08))
summary(hcs.lme)
hcs.lme$modelStruct
anova(hcs.lme,cs.lme)   # hcs not converging here

# Autoregressive error structure
auto1 <- gls(MathAvgScore~year08*charter,chart.long, 
  na.action=na.exclude,
  correlation=corAR1(,form = ~ 1 |schoolid), method="REML")
corandcov(auto1)

ar1.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random =  ~ 1 | schoolid,
  correlation=corAR1(,form = ~ 1 |schoolid), na.action=na.exclude)
summary(ar1.lme)

# Heterogeneous autoregressive error structure
har1.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random =  ~ 1 | schoolid, 
  correlation=corAR1(,form = ~ 1 |schoolid), na.action=na.exclude,
  weights=varIdent(form = ~1|year08))
summary(har1.lme)
har1.lme$modelStruct
anova(har1.lme,ar1.lme)

#Toeplitz error covariance structure
toep <- gls(MathAvgScore~year08*charter,chart.long,
  na.action=na.exclude,
  correlation=corARMA(,form = ~ 1 |schoolid,p=2,q=0), method="REML")
corandcov(toep)

toep.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
  random =  ~ 1 | schoolid,
  correlation=corARMA(,form = ~ 1 |schoolid,p=2,q=0),
  na.action=na.exclude)
summary(toep.lme)
