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

rd <- read.csv("data.csv")

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

rd$Replicate<-as.factor(rd$Replicate)

###################################################################
##ANOVA

##Summary - Means and sum stats by grouping

library(Rmisc)

sum = summarySE(rd,
                measurevar="Shoot_calc",
                groupvars=c("Chemical","Conc","Ecotype"))
sum

##Interaction plots using sum function

library(ggplot2)

pd = position_dodge(.2)

ggplot(sum, aes(x=Chemical,
                y=Shoot_calc,
                color=Ecotype)) +
  geom_errorbar(aes(ymin=Shoot_calc-se,
                    ymax=Shoot_calc+se),
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

model <- lm(Shoot_calc~Chemical*Conc*Ecotype,data=rd) #Interactions model

library(car)

Anova(model, type="III")     # Part of 'car' package, Can use type="II" or "III"

anova(model)                 # Part of r base function, calculates Type I (sequential)

summary(model)

##Effects only

model.eff <- lm(Shoot_calc~Chemical+Conc+Ecotype,data=rd) #Only effects, no interactions

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
M1 <- gls(Shoot_calc ~ Chemical*Conc+Replicate, data=rd)
#gls with no weights is equivalent to lm
#this incorporates replicate as a block

plot(fitted(M1),
     residuals(M1))

M2 <- lme(Shoot_calc ~ Chemical*Conc+Replicate, random=~1|Ecotype, data=rd)

plot(fitted(M2),
     residuals(M2))

#not much difference with residuals

anova(M1,M2)



M3 <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, method="ML", data=rd)
M4 <- lme(Shoot_calc ~ Chemical*Conc*Ecotype, random=~1|Replicate, method="ML", data=rd)
#need to use method=ML for these since main effects are different

anova(M3,M4)
#this is more convincing evidence that the random effects model isn't required.

hist(residuals(M3),
     col="darkgray")

plot(fitted(M3),
     residuals(M3))
#maybe not quite as good as other models, also need to plot residuals by categories

plot(rd$Chemical, residuals(M3), xlab="Chemical", ylab="Residuals, Shoot")
plot(rd$Conc, residuals(M3), xlab="Conc", ylab="Residuals, Shoot")
plot(rd$Ecotype, residuals(M3), xlab="Ecotype", ylab="Residuals, Shoot")
plot(rd$Replicate, residuals(M3), xlab="Replicate", ylab="Residuals, Shoot")
#nothing sticks out as bad, but chemical or conc could be better

M3.full <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, method="REML", data=rd)

vf1 <- varIdent(form = ~1|Chemical)#allows variance to differ by strata
vf2 <- varIdent(form = ~1|Conc)
vf3 <- varComb(vf1, vf2)#combines both of the vf above

M4 <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf1, method="REML", data=rd)
M5 <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf2, method="REML", data=rd)
M6 <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf3, method="REML", data=rd)

anova(M3.full, M4, M5, M6)

anova(M3.full, M5)


hist(residuals(M5),
     col="darkgray")

plot(fitted(M5),
     residuals(M5))

plot(rd$Chemical, residuals(M5), xlab="Chemical", ylab="Residuals, Shoot")
plot(rd$Conc, residuals(M5), xlab="Conc", ylab="Residuals, Shoot")
plot(rd$Ecotype, residuals(M5), xlab="Ecotype", ylab="Residuals, Shoot")
plot(rd$Replicate, residuals(M5), xlab="Replicate", ylab="Residuals, Shoot")


model <- gls(Shoot_calc~Chemical*Conc*Ecotype, method="ML", data=rd)
M5 <- gls(Shoot_calc ~ Chemical*Conc*Ecotype+Replicate, weights=vf2, method="ML", data=rd)

anova(model,M5)
#incorporating the alternate variance structure and the replicate produces a better model

anova(M5)
