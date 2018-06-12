##############################################################
#Ian's Thesis Project
##############################################################

##############################################################
#Install packages
if(!require(nlme)){install.packages("nlme")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(TukeyC)){install.packages("TukeyC")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(FSA)){install.packages("FSA")}
if(!require(car)){install.packages("car")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(pwr)){install.packages("pwr")}


# ##############################################################
#Import *.csv data

rd <- read.csv("C:/Users/seileri/Desktop/R data/RD calc.csv")

View(rd)
str(rd)

# ##############################################################
# #Change nominal var to factors, if R returns error regarding
# #*.csv str'chr'not stored as factor when creating model object
# #(because R sucks). Change needed variable
# ##############################################################
# #rd$Ecotype <-as.factor(rd$Ecotype)
# #rd$Treatment <- as.factor(rd$Treatment)
# #rd$Chemical <- as.factor(rd$Chemical)
# #rd$Conc <- as.factor(rd$Conc)

#CPA -- change replicate to a factor for the block
rd$Replicate<-as.factor(rd$Replicate)

# ##############################################################
# #Summary Stats
# #############################################################
# library(FSA)
#
# Summarize(Root_calc~Conc*Chemical, data=rd,digits=3)
#
# ##############################################################
# #Nested anova, mixed effects (nlme)
# #Response variable = $Root
# #Nested factors (nominal variables) = Treatment, Chemical, Conc
# #Random factors (nominal var.) = Ecotype
# ##############################################################
#
# library(nlme)
#
# model=lme(Root_calc~Chemical*Conc+Replicate, random=~1|Ecotype,data=rd,method="REML")
#
# anova.lme(model,type="sequential",adjustSigma = FALSE)
#
#
# ##############################################################
# #Post-hoc comparasin of means
# ##############################################################
# library(multcomp)
#
# posthoc = glht(model,linfct = mcp(Chemical ="Tukey"))
#
# mcs = summary(posthoc,test=adjusted("single-step"))
#
# mcs
#
# # Adjustment options: "none", "single-step", "Shaffer",
# #                     "Westfall", "free", "holm", "hochberg",
# #                    "hommel", "bonferroni", "BH", "BY", "fdr"
# # "single-step" implements adjusted p values based on the joint
# #  normal or t distribution of the linear function
#
# cld(mcs,level=0.05,decreasing=TRUE)
#
# # cld = Compact Letter Display
# # Means sharing a letter are not significantly different
#
# #############################################################
# #Post-hoc comparison of least-square means
# #############################################################
#
# library(multcompView)
# library(lsmeans)
#
# leastsquare = lsmeans(model,pairwise ~ Chemical, adjust="tukey")
# ###  Tukey-adjusted comparisons
#
# leastsquare
#
# cld(leastsquare,alpha=0.05,Letters=letters,adjust="tukey")
#     ### Use lower-case letters for .group
#     ### Tukey-adjusted comparisons
#
# #############################################################
# #Test the significance of the random effect in the mixed effects model
# #############################################################
#
# model.fixed = gls(Root_calc ~ Chemical*Conc+Replicate,
#                   data=rd,
#                   method="REML")
#
# #REML = 'Restricted Maximum Likelihood Estimation explanation found at
# #http://users.stat.umn.edu/~gary/classes/5303/handouts/REML.pdf
#
# anova(model,
#       model.fixed)
#   #CPA--the lower AIC for model versus model.fixed and the significant p-value
#   #shows that the random effect is a good addition
#
#   #CPA--use method = "ML" to compare models with different fixed effects
#
# #############################################################
# #Check assumptions
# #############################################################
#
# hist(residuals(model),col="darkgray")
# #Should be approximately normal
#
#
#
# plot(fitted(model),residuals(model))
# #Should be unbiased and homoscedastic.
#
#   #CPA-see you your residuals get a bit wider with increased fit?
#   #we'll probably want to play with alternate variance structures if
#   #we continue to see this pattern when you alter your model
#
#
# ##############################################################
# # Boxplot
#
# boxplot(Root_calc~Ecotype, data=rd)
#
# ##############################################################
# # 2-way ANOVA using lm modeling
# # Using interactions, ecotype, conc
#
# #Interaction Plot
#
#   interaction.plot(x.factor     = rd$Conc,        ### Forms x-axis
#                    trace.factor = rd$Chemical,
#                    response     = rd$Root_calc,
#                    fun = mean,
#                    type="b",
#                    col=c("black","red","green"),  ### Colors for levels of trace var.
#                    pch=c(19, 15, 17),             ### Symbols for levels of trace var.
#                    fixed=TRUE,                    ### Order by factor order in data
#                    leg.bty = "o")
#
# ##linear model using lm
#
# library(car)
#
# model.lm <- lm(Root_calc~Chemical*Conc*Ecotype*Conc:Ecotype,data=rd)
#
# Anova(model.lm,type="II")
#
# ##Post-hoc testing
# library(lsmeans)
#
# lsmeansLT(model.lm,pairwise~Chemical,adjust="tukey")

###################################################################
##ANOVA

##Summary - Means and sum stats by grouping
library(Rmisc)

sum = summarySE(rd,
                measurevar="Root_calc",
                groupvars=c("Chemical","Conc","Ecotype"))
sum

##Interaction plots using sum function
#
library(ggplot2)

pd = position_dodge(.2)

ggplot(sum, aes(x=Chemical,
                y=Root_calc,
                color=Ecotype)) +
  geom_errorbar(aes(ymin=Root_calc-se,
                    ymax=Root_calc+se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue","red","green"))

####################################################################
##lm model

model <- lm(Root_calc~Chemical*Conc*Ecotype,data=rd) #Interactions model

library(car)

Anova(model, type="III")     # Part of 'car' package, Can use type="II" or "III"

anova(model)                 # Part of r base function, calculates Type I (sequential)

summary(model)

##Effects only

model.eff <- lm(Root_calc~Chemical+Conc+Ecotype,data=rd) #Only effects, no interactions

library(car)

Anova(model.eff, type="III")     # Part of 'car' package, Can use type="II" or "III"

anova(model.eff)                 # Part of r base function, calculates Type I (sequential)

summary(model.eff)

##Assumption checking

hist(residuals(model),
     col="darkgray")

plot(fitted(model),
     residuals(model))

#CPA -- let's incorporate a block and random effects to see which results in the best model
#    -- also, let's start with the full model and strip out unnecessary variables as
#    -- as needed

library(nlme)
M1 <- gls(Root_calc ~ Chemical*Conc+Replicate, data=rd)
#gls with no weights is equivalent to lm
#this incorporates replicate as a block

plot(fitted(M1),
     residuals(M1))

M2 <- lme(Root_calc ~ Chemical*Conc+Replicate, random=~1|Ecotype, data=rd)

plot(fitted(M2),
     residuals(M2))

#not much difference with residuals

anova(M1,M2)

#not much difference with models, but ecotype isn't incorporated into M1
#conceptually ecotype should be a random effect, but let's try to mess with
#replicate as a block or a random effect and incorporate Ecotype as a main effect

M3 <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, method="ML", data=rd)
M4 <- lme(Root_calc ~ Chemical*Conc*Ecotype, random=~1|Replicate, method="ML", data=rd)
#need to use method=ML for these since main effects are different

anova(M3,M4)
#this is more convincing evidence that the random effects model isn't required.

hist(residuals(M3),
     col="darkgray")

plot(fitted(M3),
     residuals(M3))
#maybe not quite as good as other models, also need to plot residuals by categories

plot(rd$Chemical, residuals(M3), xlab="Chemical", ylab="Residuals")
plot(rd$Conc, residuals(M3), xlab="Conc", ylab="Residuals")
plot(rd$Ecotype, residuals(M3), xlab="Ecotype", ylab="Residuals")
plot(rd$Replicate, residuals(M3), xlab="Replicate", ylab="Residuals")
#nothing sticks out as bad, but chemical or conc could be better

#before we optimize the model, let's try some alternate variance structures to
#see if we can't help the residuals for chemical and concentration
#we go back to REML for this since alternate variance structures
#are nested within the main model

M3.full <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, method="REML", data=rd)

vf1 <- varIdent(form = ~1|Chemical)#allows variance to differ by strata
vf2 <- varIdent(form = ~1|Conc)
vf3 <- varComb(vf1, vf2)#combines both of the vf above

M4 <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf1, method="REML", data=rd)
M5 <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf2, method="REML", data=rd)
M6 <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf3, method="REML", data=rd)

anova(M3.full, M4, M5, M6)
#M5 has the lowest AIC, but the p-value comparison is to M4
anova(M3.full, M5)
#M5 is better, but does it handle residuals better?

hist(residuals(M5),
     col="darkgray")

plot(fitted(M5),
     residuals(M5))

plot(rd$Chemical, residuals(M5), xlab="Chemical", ylab="Residuals")
plot(rd$Conc, residuals(M5), xlab="Conc", ylab="Residuals")
plot(rd$Ecotype, residuals(M5), xlab="Ecotype", ylab="Residuals")
plot(rd$Replicate, residuals(M5), xlab="Replicate", ylab="Residuals")
#when you run these, run the plots from M3 first to see how they change
#they aren't much different but the seem to produce fewer outliers in Conc

#let's compare M5 to your original

model <- gls(Root_calc~Chemical*Conc*Ecotype, method="ML", data=rd)
M5 <- gls(Root_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf2, method="ML", data=rd)

anova(model,M5)
#incorporating the alternate variance structure and the replicate produces a better model

anova(M5)
#from here, you can try backwards selection, but you have to start with the
#three-way interaction, which is significant.  I'll photocopy something for you
#when you're ready.  next up is same approach with shoot?