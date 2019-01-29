# GLMM.R

# be sure to insteall first if needed
library(gplots)
library(ggplot2)
library(gridExtra)
library(tidyverse)

theme.1 <- theme(axis.title.x = element_text(size = 11),
  axis.title.y = element_text(size = 11),
  plot.title=element_text(hjust=.9,face="italic",size=12))

refdata <- read.csv("~/Stats 316 S18/Class/Data/basketball0910.csv")
dim(refdata)        # should be 4972 x 19
head(refdata)       # examine first 6 rows

# Illustrate long data
refdata %>% slice(1:10) %>%
  select(., 2, 4, 5, 6, 7, 9, 10, 12, 15, 19)

# Variables in refdata
# game = unique game identification number
# date = date game was played (YYYYMMDD)
# visitor = visiting team abbreviation
# hometeam = home team abbreviation
# foul.num = cumulative foul number within game
# foul.home = indicator if foul was called on the home team
# foul.vis = indicator if foul was called on the visiting team
# foul.diff = the difference in fouls before the current foul was called (home - visitor)
# score.diff = the score differential before the current foul was called (home - visitor)
# lead.vis = indicator if visiting team has the lead
# lead.home = indicator if home team has the lead
# previous.foul.home = indicator if previous foul was called on the home team
# previous.foul.vis = indicator if previous foul was called on the visiting team
# foul.type = categorical variable if current foul was offensive, personal, or shooting
# shooting = indicator if foul was a shooting foul
# personal = indicator if foul was a personal foul
# offensive = indicator if foul was an offensive foul
# time = number of minutes left in the first half when foul called

# EDA

# Summarize Level 1 covariates (and responses) by ignoring within subject 
#   correlation and pretending all observations are independent
time.hist <- ggplot(data = refdata, aes(x = time)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "white") + theme.1 + 
  xlab("Time left in first half") + ylab("Frequency") + labs(title = "(a)")
score.hist <- ggplot(data = refdata, aes(x = score.diff)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "white") + theme.1 + 
  xlab("Score difference (home-visitor)") + ylab("Frequency") + labs(title = "(b)")
foul.hist <- ggplot(data = refdata, aes(x = foul.diff)) + 
  geom_histogram(binwidth = 1.5, color = "black", fill = "white") + theme.1 + 
  xlab("Foul difference (home-visitor)") + ylab("Frequency") + labs(title = "(c)")
gmu.histmat1 <- grid.arrange(time.hist, score.hist, foul.hist, ncol=2, nrow=2)

with(refdata, summary(time))
with(refdata, summary(score.diff))
with(refdata, summary(foul.diff))
with(refdata, sd(time))
with(refdata, sd(score.diff))
with(refdata, sd(foul.diff))

refdata %>% count(foul.home) %>% mutate(prop = n/sum(n))
refdata %>% count(lead.home) %>% mutate(prop = n/sum(n)) 
refdata %>% count(previous.foul.home) %>% mutate(prop = n/sum(n)) 
refdata %>% count(foul.type) %>% mutate(prop = n/sum(n)) 

# There are no Level 2 covariates other than Home and Visiting Team.
#   Tables below give overall summary of which teams called for more fouls.
f1 <- refdata %>% count(hometeam) %>% 
  mutate(prop = n/sum(n)) %>%
  rename(n_fouls = n)
print(f1, n = Inf)
f2 <- refdata %>% count(visitor) %>% 
  mutate(prop = n/sum(n)) %>%
  rename(n_fouls = n)
print(f2, n = Inf)
table(refdata$hometeam)
table(refdata$visitor)

#   These tables tell how many games each team appears in.
onepergame <- refdata %>%
  group_by(game) %>%
  filter(row_number() == 1) %>%
  ungroup
p1 <- onepergame %>% count(hometeam) %>% 
  mutate(prop = n/sum(n)) %>%
  rename(n_games = n)
print(p1, n = Inf)
p2 <- onepergame %>% count(visitor) %>% 
  mutate(prop = n/sum(n)) %>%
  rename(n_games = n)
print(p2, n = Inf)
table(onepergame$hometeam)
table(onepergame$visitor)

#   These tables tell average total fouls for both teams in the first half
#   of games where a certain team is home or visitor
allhome <- p1 %>%
  inner_join(f1, by = "hometeam") %>%
  mutate(fouls_per_game = n_fouls / n_games) %>%
  arrange(desc(fouls_per_game))
print(allhome, n = Inf)

allvis <- p2 %>%
  inner_join(f2, by = "visitor") %>%
  mutate(fouls_per_game = n_fouls / n_games) %>%
  arrange(desc(fouls_per_game))
print(allvis, n = Inf)

allboth <- allhome %>%
  rename(team = hometeam, home_fouls = fouls_per_game) %>%
  select(team, home_fouls) %>%
  inner_join(allvis, by = c("team" = "visitor")) %>%
  select(team, home_fouls, fouls_per_game) %>%
  rename(visitor_fouls = fouls_per_game) %>%
  mutate(diff = home_fouls - visitor_fouls) %>%
  arrange(desc(diff))
print(allboth, n = Inf)

allhome0 = table(refdata$hometeam)/table(onepergame$hometeam)
allvis0 = table(refdata$visitor)/table(onepergame$visitor)
sort(allhome0)
sort(allvis0)
sort(allhome0-allvis0)

# Look at relationships among Level 1 covariates and primary response
#   (again ignoring correlation).  Cdplots and elogits for continuous covariates and
#   segmented barcharts for categorical covariates.

with(refdata, by(time, foul.home, summary))
with(refdata, by(score.diff, foul.home, summary))
with(refdata, by(foul.diff, foul.home, summary))

# Find empirical logits and proportions by grouping fouls by foul.diff
#   First remove foul.diff=-8 or 6, since not enough data
foo1 <- refdata %>%
  filter(foul.diff >= -7 & foul.diff <= 5)

foo1 %>% count(foul.diff, foul.home)
table(foo1$foul.home, foo1$foul.diff)

foul.df <- foo1 %>%
  group_by(foul.diff) %>%
  summarise(foul.phats = mean(foul.home)) %>%
  mutate(foul.elogits = log(foul.phats/(1 - foul.phats)) )
print(foul.df)

ggplot(data = foul.df, aes(x = foul.diff, y = foul.elogits)) + geom_point() + 
  geom_smooth(method = "lm", color = "black") + theme.1 + 
  ylab("Empirical logits") + xlab("Foul differential")

model0 = lm(foul.elogits ~ foul.diff, data = foul.df)
summary(model0)

# Repeat by grouping fouls by score.diff
#   First remove score.diff<=-12 or >=19, since not enough data
foo1 <- refdata %>%
  filter(score.diff >= -11 & score.diff <= 18)

foo1 %>% count(score.diff, foul.home)
table(foo1$foul.home, foo1$score.diff)

score.df <- foo1 %>%
  group_by(score.diff) %>%
  summarise(score.phats = mean(foul.home)) %>%
  mutate(score.elogits = log(score.phats/(1 - score.phats)) )
print(score.df)

ggplot(data = score.df, aes(x = score.diff, y = score.elogits)) + geom_point() + 
  geom_smooth(method = "lm", color = "black") + theme.1 + 
  ylab("Empirical logits") + xlab("Score differential")

model0 = lm(score.elogits ~ score.diff, data = score.df)
summary(model0)

# Repeat by grouping fouls by time remaining
#   Must group in intervals since time truly continuous
foo1 <- refdata %>% 
  mutate(group = cut(time, breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, Inf),
                        labels = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19))) %>%
  mutate(times = as.numeric(levels(group))[group]) %>%
  select(-group)

foo1 %>% count(times, foul.home)
table(foo1$foul.home, foo1$times)

time.df <- foo1 %>%
  group_by(times) %>%
  summarise(time.phats = mean(foul.home)) %>%
  mutate(time.elogits = log(time.phats/(1 - time.phats)) )
print(time.df)

ggplot(data = time.df, aes(x = times, y = time.elogits)) + geom_point() + 
  geom_smooth(method = "lm", color = "black") + theme.1 + 
  ylab("Empirical logits") + xlab("Time remaining")

model0 = lm(time.elogits ~ times, data = time.df)
summary(model0)


refdata <- refdata %>%
  mutate(foul.factor = as.factor(ifelse(foul.home == 1, "Home", "Visitor")) )

foul.cd = ggplot(data = refdata, aes(x = foul.diff)) + 
  theme(legend.title = element_blank()) + theme.1 +
  geom_density(aes(fill = foul.factor), position = "fill", adjust = 2) + 
  xlab("Foul difference (H-V)") + ylab("Probability of Home Foul") + 
  labs(title="(a)") + scale_fill_manual(values = c("grey20", "grey80"))
score.cd = ggplot(data = refdata, aes(x = score.diff)) + 
  theme(legend.title = element_blank()) + theme.1 +
  geom_density(aes(fill = foul.factor), position = "fill", adjust = 2) + 
  xlab("Score difference (H-V)") + ylab("Probability of Home Foul") + 
  labs(title="(b)") + scale_fill_manual(values = c("grey20", "grey80"))
time.cd = ggplot(data = refdata, aes(x = time)) + 
  theme(legend.title = element_blank()) + theme.1 +
  geom_density(aes(fill = foul.factor), position = "fill", adjust = 2) + 
  xlab("Time left in half") + ylab("Probability of Home Foul") + 
  labs(title="(c)") + scale_fill_manual(values = c("grey20", "grey80"))
foul.el <- ggplot(data = foul.df, aes(x = foul.diff, y = foul.elogits)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Foul difference (H-V)") + 
  ylab("Empirical Log-odds of Home Foul") + labs(title = "(d)") +
  geom_smooth(se = FALSE, method = "lm", color = "black")
score.el <- ggplot(data = score.df, aes(x = score.diff, y = score.elogits)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Score difference (H-V)") + 
  ylab("Empirical Log-odds of Home Foul") + labs(title = "(e)") +
  geom_smooth(se = FALSE, method = "lm", color = "black")
time.el <- ggplot(data = time.df, aes(x = times, y = time.elogits)) + 
  geom_point(color="dark grey") + theme.1 + xlab("Time left in half") + 
  ylab("Empirical Log-odds of Home Foul") + labs(title = "(f)") +
  geom_smooth(se = FALSE, method = "lm", color = "black")
gmu.cdelogitmat1 <- grid.arrange(foul.cd, score.cd, time.cd,
  foul.el, score.el, time.el, ncol = 3, nrow = 2)


refdata <- refdata %>%
  mutate(leadyes = ifelse(lead.home == 0, "No", "Yes"),
         prevyes = ifelse(previous.foul.home == 0, "No", "Yes")) %>%
  rename(whofoul = foul.factor)

table1.df <- refdata %>% 
  count(foul.type, whofoul) %>% 
  mutate(prop = n/sum(n))

table2.df <- refdata %>% 
  count(leadyes, whofoul) %>% 
  mutate(prop = n/sum(n))

table3.df <- refdata %>% 
  count(prevyes, whofoul) %>% 
  mutate(prop = n/sum(n))

barplot1 = ggplot(table1.df, aes(foul.type, fill = whofoul, weight = prop)) + 
  geom_bar() + xlab("Foul Type") + ylab("Proportion within Foul Type") + 
  scale_fill_manual( values = c("grey20","grey80")) + labs(title = "(a)") +
  theme(legend.title = element_blank()) + theme.1
barplot2 = ggplot(table2.df, aes(leadyes, fill = whofoul, weight = prop)) + 
  geom_bar() + xlab("Home Team in Lead") + 
  ylab("Proportion within Team with Lead") + 
  scale_fill_manual( values = c("grey20","grey80")) + labs(title = "(b)") +
  theme(legend.title = element_blank()) + theme.1
barplot3 = ggplot(table3.df, aes(prevyes, fill = whofoul, weight = prop)) + 
  geom_bar() + xlab("Previous Foul on Home Team") + 
  ylab("Proportion within Previous Foul") + 
  scale_fill_manual( values = c("grey20","grey80")) + labs(title = "(c)") + 
  theme.1 + theme(legend.title = element_blank())
gmu.barmat1 <- grid.arrange(barplot1, barplot2, barplot3, ncol = 2, nrow = 2)


# Logistic regression model (not multilevel)
mod0 = glm(foul.home ~ foul.diff + score.diff + lead.home + time + 
  foul.diff:time + lead.home:time, family = binomial, data = refdata)
summary(mod0)


# Model for Game 110 only (foul.diff as sole predictor)
game110 <- refdata %>% filter(game == 110) %>%   # pick off data for Game 110
  select(., 2, 4, 5, 6, 7, 9, 10, 12, 15, 19)
dim(game110)
lreg.game110 = glm(foul.home ~ foul.diff, family = binomial, data = game110)
summary(lreg.game110)

# Examine intercepts and slopes by game for model 
#   with foul.diff as only predictor
library(broom)
regressions <- refdata %>% 
  group_by(game) %>% 
  do(fit = glm(foul.home ~ foul.diff, family = binomial, data = .))

glm_info <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(game, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = foul.diff, int = `(Intercept)`)

# Descriptive statistics of the estimates obtained by fitting the 
#   logistic model by game.
summary(glm_info$int)
summary(glm_info$rate)
sd(glm_info$int)
sd(glm_info$rate)
cor(glm_info$int, glm_info$rate)   

# histograms for ints, rates of change - gmu-histmat2.eps
int.hist2 <- ggplot(glm_info) + 
  geom_histogram(aes(x = int), color = "black", fill = "white") + theme.1 + 
  labs(x = "Intercepts", y = "Frequency", title = "(a)") +
  scale_x_continuous(limits = c(-10, 10))
rate.hist2 <- ggplot(glm_info) + 
  geom_histogram(aes(x = rate), color = "black", fill = "white") + theme.1 + 
  labs(x = "Slopes", y = "Frequency", title = "(b)") +
  scale_x_continuous(limits = c(-5, 5))
gmu.histmat2 <- grid.arrange(int.hist2, rate.hist2, ncol = 1)


# Model for Boston College at home only (foul.diff as sole predictor)
bc.home = refdata %>% filter(hometeam == "BC") # pick off data when BC at home
dim(bc.home)
lreg.bchm = glm(foul.home ~ foul.diff, family = binomial, data = bc.home)
summary(lreg.bchm)


# Examine intercepts by hometeam for model with foul.diff as only predictor
int <- by(refdata, refdata$hometeam, function(data)
            coefficients(glm(foul.home~foul.diff,family=binomial,data=data))[[1]])
summary(int)   # summary statistics for 39 intercepts

# Examine slopes by hometeam for model with foul.diff as only predictor
rate <- by(refdata, refdata$hometeam, function(data)
            coefficients(glm(foul.home~foul.diff,family=binomial,data=data))[[2]])
summary(rate)

regressions <- refdata %>% 
  group_by(hometeam) %>% 
  do(fit = glm(foul.home ~ foul.diff, family = binomial, data = .))

glm_info <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(hometeam, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = foul.diff, int = `(Intercept)`)

# Descriptive statistics of the estimates obtained by fitting the 
#   logistic model by hometeam.
summary(glm_info$int)
summary(glm_info$rate)
sd(glm_info$int)
sd(glm_info$rate)
cor(glm_info$int, glm_info$rate)   

# histograms for ints, rates of change - gmu-histmat3.eps
int.hist3 <- ggplot(glm_info) + 
  geom_histogram(aes(x = int), binwidth = .1, color = "black", fill = "white") +
  theme.1 + labs(x = "Intercepts", y = "Frequency", title = "(a)")
rate.hist3 <- ggplot(glm_info) + 
  geom_histogram(aes(x = rate), binwidth = .04, color = "black", fill = "white") +
  theme.1 + labs(x = "Slopes", y = "Frequency", title = "(b)")
gmu.histmat3 <- grid.arrange(int.hist3, rate.hist3, ncol = 1)


# Multilevel model with only foul.diff and 1 random effect
library(lme4)
model.0a <- glmer(foul.home ~ foul.diff + (1|game), family = binomial,
                  data = refdata)
summary(model.0a)

# Multilevel model with only foul.diff and errors on slope and int and 1 RE
model.0b <- glmer(foul.home ~ foul.diff + (foul.diff|game), family = binomial,
  data = refdata)
summary(model.0b)
anova(model.0b, model.0a)

# Logistic regression model (not multilevel) with only foul.diff
mod0a <- glm(foul.home ~ foul.diff, family = binomial, data = refdata)
summary(mod0a)


# Model A (Multilevel model with only foul.diff and all 3 random effects)
model.a <- glmer(foul.home ~ foul.diff + (1|game) + (1|hometeam) + (1|visitor),
  family = binomial, data = refdata)
summary(model.a)
anova(model.a, model.0a)  # model A is better, but p-value can be off (REML=F)

# Since glmer() automatically uses full ML (there's no REML option), so we
#   can't derive the LRT from REML
actualLRT <- anova(model.a, model.0a)[2,6]                     
LRTdf <- anova(model.a, model.0a)[2,7]
#actualLRT=as.numeric(2*(logLik(model.a)-logLik(model.0a)))   # same as above
#actualLRT
pvalREML <- 1 - pchisq(actualLRT, df = LRTdf)
pvalREML

################################################################################

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

# Run bootstrapAnova function - took over an hour to run with B=1000
#   Must first run bootstrapAnova function.R
bRLRT <- bootstrapAnova(mA = model.a, m0 = model.0a, B = 10)
bRLRT
nullLRT <- attr(bRLRT, "nulldist")
x <- seq(0, max(nullLRT), length = 1000)
y <- dchisq(x, 2)
nullLRT.df <- data.frame(nullLRT)
chisq.df <- data.frame(x,y)
ggplot(data = nullLRT.df, aes(x = nullLRT)) + theme.1 +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", 
                 fill = "white") +  
  xlab("Likelihood Ratio Test Statistics from Null Distribution") + 
  ylab("Density") + geom_line(data = chisq.df, aes(x = x, y = y))
sum(nullLRT>=16.074)/10   # parametric bootstrap p-value

###############################################################################

# Model A2 (Multilevel model with only foul.diff and all 3 random effects
#             applied to both intercepts and slopes)
model.a2 <- glmer(foul.home ~ foul.diff + (foul.diff|game) +
  (foul.diff|hometeam) + (foul.diff|visitor), family = binomial, data = refdata)
summary(model.a2)
anova(model.a2, model.a)  # model A is better by LRT; assumes REML=F
### Correlations between error terms for int and slope always -1 ###

# Model A3 (Multilevel model with only foul.diff and all 3 random effects
#             applied to both intercepts and slopes but no correlations)
model.a3 <- glmer(foul.home ~ foul.diff + (1|game) +
  (1|hometeam) + (1|visitor) + (0+foul.diff|game) + (0+foul.diff|hometeam) +
  (0+foul.diff|visitor), family = binomial, data = refdata)
summary(model.a3)
anova(model.a3, model.a)  # model A is better by LRT; assumes REML=F

# Run bootstrapAnova function - took multiple hours to run with B=1000
# bRLRT3 = bootstrapAnova(mA=model.a3, m0=model.a, B=10)
# bRLRT3
# nullLRT3 = attr(bRLRT3,"nulldist")
# x3=seq(0,max(nullLRT3),length=1000)
# y3=dchisq(x3,LRT3df)

# nullLRT3.df = data.frame(nullLRT3)
# chisq.df = data.frame(x3,y3)
# xint.df = data.frame(actualLRT3)
# ggplot(data=nullLRT3.df,aes(x=nullLRT3)) + theme.1 +
#   geom_histogram(aes(y=..density..),binwidth=1,color="black",fill="white") +  
#   xlab("Likelihood Ratio Test Statistics from Null Distribution") + ylab("Density") +
#   geom_line(data=chisq.df, aes(x=x3,y=y3)) +
#   geom_vline(data=xint.df, aes(xintercept=actualLRT3), colour="red")


# Model B - final model?
model.b <- glmer(foul.home ~ foul.diff + score.diff + lead.home + time + 
  offensive + personal + foul.diff:offensive + foul.diff:personal + 
  foul.diff:time + lead.home:time + (1|game) + (1|hometeam) + (1|visitor),
  family = binomial, data = refdata)
summary(model.b)   
exp(fixef(model.b))

# Get estimated random effects based on Model B
#   Actually conditional modes given the response, evaluated at 
#   parameter estimates.
re.int <- ranef(model.b)$`game`[["(Intercept)"]]
# hist(re.int,xlab="Random Effect",main="Random Effects for Game")
Home.re <- ranef(model.b)$`hometeam`[["(Intercept)"]]
# hist(Home.re,xlab="Random Effect",main="Random Effects for Home Team")
Visiting.re <- ranef(model.b)$`visitor`[["(Intercept)"]]
# hist(Visiting.re,xlab="Random Effect",
#   main="Random Effects for the Visiting Team",xlim=c(-0.5,0.5))
cbind(Home.re,Visiting.re)   # 39x2 matrix of REs by team

home.df <- data.frame(Home.re)
ggplot(data = home.df, aes(x = Home.re)) + 
  geom_histogram(binwidth = .05, color = "black", fill = "white") + theme.1 + 
  xlab("Random Home Team Effects") + ylab("Frequency")

# Prediction intervals for random effects based on Model B
# ranef1=dotplot(ranef(model.b, postVar = TRUE), strip = FALSE)
# print(ranef1[[3]], more = TRUE) ##HOME
# print(ranef1[[2]], more = TRUE) ##VIS
# print(ranef1[[1]], more = TRUE)
randoms <- ranef(model.b, postVar = TRUE)
qq <- attr(ranef(model.b, postVar = TRUE)[[3]], "postVar")
rand.interc <- randoms$hometeam
df <- data.frame(Intercepts = randoms$hometeam[,1],
              sd.interc = 2*sqrt(qq[,,1:length(qq)]),
              lev.names = rownames(rand.interc))
df$lev.names <- factor(df$lev.names, levels = df$lev.names[order(df$Intercepts)])
p <- ggplot(df, aes(lev.names, Intercepts))
p <- p + geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=Intercepts-sd.interc, ymax=Intercepts+sd.interc), 
    width=0,color="black") + geom_point(aes(size=2)) +
  guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(1,1,1,16,16,16)) 
p <- p + theme(axis.text.x=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.3)),
               axis.text.y=element_text(size=rel(1.2)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())
p <- p+ coord_flip() + labs(y="Estimated Random Effects",x="Home Teams")
print(p)
