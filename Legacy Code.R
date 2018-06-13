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
