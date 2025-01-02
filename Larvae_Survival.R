{library(AER)
  library(ecotox)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(Rmisc)
  library(RColorBrewer)
  library(emmeans)
  library(gam)
  library(mgcv)
  library(multcomp)
  library(emmeans)
  library(car)
  library(carData)
  library(lmtest)
  library(zoo)
  library(lattice)
  library(grid)
  library(gridExtra)
  library(patchwork)
}

attach(Larvae_Survival)
df<-Larvae_Survival
str(df)

# Convert categorical variables to factors
df$Line <- as.factor(df$Line)
df$Hour <- as.factor(df$Hour)
df$Recovery <- as.factor(df$Recovery)

# Fit ANOVA model: Effect of Temperature on fraction
model_temp <- aov(fraction ~ Temperature, data = df)
summary(model_temp)

# Fit ANOVA model: Effect of Recovery on fraction
model_recovery <- aov(fraction ~ Recovery, data = df)
summary(model_recovery)

# Fit ANOVA model: Combined effect of Temperature and Recovery
model_combined <- aov(fraction ~ Temperature + Recovery, data = df)
summary(model_combined)


df2<-dlply(df,.(Hour))
df2
dfacute<-df2$'2'
dfchronic<-df2$'6'

dfacute2<-dlply(dfacute,. (Recovery))
dfacute_Early<-dfacute2$'early'
dfacute_Late<-dfacute2$'late'

dfchronic2<-dlply(dfchronic,. (Recovery))
dfchronic_Early<-dfchronic2$'early'
dfchronic_Late<-dfchronic2$'late'


#2hour_stress_statistics
#Early
#25C
modelacute25_early<-lm(Alive~Line, data = dfacute_Early[dfacute_Early$Temperature == "25",])
summary(aov(modelacute25_early))
summary(modelacute25_early)
emm1<-emmeans(modelacute25_early, pairwise~Line)
emm1
emmeans_model1 <- emmeans(modelacute25_early, ~ Line)
cld(emmeans_model1, Letters=letters)
plot(cld(emmeans_model1, Letters=letters))

#29C  
modelacute29_early<-lm(Alive~Line, data = dfacute_Early[dfacute_Early$Temperature == "29",])
summary(aov(modelacute29_early))
summary(modelacute29_early)
emm2<-emmeans(modelacute29_early, pairwise~Line)
emm2
#write.csv(emm2$contrasts, file = "2hour_stress_29C_statistics.csv")
emmeans_model2 <- emmeans(modelacute29_early, ~ Line)
cld(emmeans_model2, Letters=letters)
plot(cld(emmeans_model2, Letters=letters))

#33C
modelacute33_early<-lm(Alive~Line, data = dfacute_Early[dfacute_Early$Temperature == "33",])
summary(aov(modelacute33_early))
summary(modelacute33_early)
emm3<-emmeans(modelacute33_early, pairwise~Line)
emm3
#write.csv(emm3$contrasts, file = "2hour_stress_33C_statistics.csv")
emmeans_model3 <- emmeans(modelacute33_early, ~ Line)
cld(emmeans_model3, Letters=letters)
plot(cld(emmeans_model3, Letters=letters))

#37C
dfacute
modelacute37_early<-lm(Alive~Line, data = dfacute_Early[dfacute_Early$Temperature == "37",])
summary(aov(modelacute37_early))
summary(modelacute37_early)
emm4<-emmeans(modelacute37_early, pairwise~Line)
emm4
#write.csv(emm4$contrasts, file = "2hour_stress_37C_statistics.csv")
emmeans_model4 <- emmeans(modelacute37_early, ~ Line)
cld(emmeans_model4, Letters=letters)
plot(cld(emmeans_model4, Letters=letters))

#41
modelacute41_early<-lm(Alive~Line, data = dfacute_Early[dfacute_Early$Temperature == "41",])
summary(aov(modelacute41_early))
summary(modelacute41_early)
emm5<-emmeans(modelacute41_early, pairwise~Line)
emm5
#write.csv(emm5$contrasts, file = "2hour_stress_41C_statistics.csv")
emmeans_model5 <- emmeans(modelacute41_early, ~ Line)
cld(emmeans_model5, Letters=letters)
plot(cld(emmeans_model5, Letters=letters))

##25 __. Late

#25C
modelacute25_late<-lm(Alive~Line, data = dfacute_Late[dfacute_Late$Temperature == "25",])
summary(aov(modelacute25_late))
summary(modelacute25_late)
emm1<-emmeans(modelacute25_late, pairwise~Line)
emm1
emmeans_model1 <- emmeans(modelacute25_late, ~ Line)
cld(emmeans_model1, Letters=letters)
plot(cld(emmeans_model1, Letters=letters))

#29C  
modelacute29_late<-lm(Alive~Line, data = dfacute_Late[dfacute_Late$Temperature == "29",])
summary(aov(modelacute29_late))
summary(modelacute29_late)
emm2<-emmeans(modelacute29_late, pairwise~Line)
emm2
#write.csv(emm2$contrasts, file = "2hour_stress_29C_statistics.csv")
emmeans_model2 <- emmeans(modelacute29_late, ~ Line)
cld(emmeans_model2, Letters=letters)
plot(cld(emmeans_model2, Letters=letters))

#33C
modelacute33_late<-lm(Alive~Line, data = dfacute_Late[dfacute_Late$Temperature == "33",])
summary(aov(modelacute33_late))
summary(modelacute33_late)
emm3<-emmeans(modelacute33_late, pairwise~Line)
emm3
#write.csv(emm3$contrasts, file = "2hour_stress_33C_statistics.csv")
emmeans_model3 <- emmeans(modelacute33_late, ~ Line)
cld(emmeans_model3, Letters=letters)
plot(cld(emmeans_model3, Letters=letters))

#37C
modelacute37_late<-lm(Alive~Line, data = dfacute_Late[dfacute_Late$Temperature == "37",])
summary(aov(modelacute37_late))
summary(modelacute37_late)
emm4<-emmeans(modelacute37_late, pairwise~Line)
emm4
#write.csv(emm4$contrasts, file = "2hour_stress_37C_statistics.csv")
emmeans_model4 <- emmeans(modelacute37_late, ~ Line)
cld(emmeans_model4, Letters=letters)
plot(cld(emmeans_model4, Letters=letters))

#41
modelacute41_late<-lm(Alive~Line, data = dfacute_Late[dfacute_Late$Temperature == "41",])
summary(aov(modelacute41_late))
summary(modelacute41_late)
emm5<-emmeans(modelacute41_late, pairwise~Line)
emm5
#write.csv(emm5$contrasts, file = "2hour_stress_41C_statistics.csv")
emmeans_model5 <- emmeans(modelacute41_late, ~ Line)
cld(emmeans_model5, Letters=letters)
plot(cld(emmeans_model5, Letters=letters))


##Chronic

#Early
#25C
modelchronic25_early<-lm(Alive~Line, data = dfchronic_Early[dfchronic_Early$Temperature == "25",])
summary(aov(modelchronic25_early))
summary(modelchronic25_early)
emm1<-emmeans(modelchronic25_early, pairwise~Line)
emm1
emmeans_model1 <- emmeans(modelchronic25_early, ~ Line)
cld(emmeans_model1, Letters=letters)
plot(cld(emmeans_model1, Letters=letters))

#29C  
modelchronic29_early<-lm(Alive~Line, data = dfchronic_Early[dfchronic_Early$Temperature == "29",])
summary(aov(modelchronic29_early))
summary(modelchronic29_early)
emm2<-emmeans(modelchronic29_early, pairwise~Line)
emm2
#write.csv(emm2$contrasts, file = "2hour_stress_29C_statistics.csv")
emmeans_model2 <- emmeans(modelchronic29_early, ~ Line)
cld(emmeans_model2, Letters=letters)
plot(cld(emmeans_model2, Letters=letters))

#33C
modelchronic33_early<-lm(Alive~Line, data = dfchronic_Early[dfchronic_Early$Temperature == "33",])
summary(aov(modelchronic33_early))
summary(modelchronic33_early)
emm3<-emmeans(modelchronic33_early, pairwise~Line)
emm3
#write.csv(emm3$contrasts, file = "2hour_stress_33C_statistics.csv")
emmeans_model3 <- emmeans(modelchronic33_early, ~ Line)
cld(emmeans_model3, Letters=letters)
plot(cld(emmeans_model3, Letters=letters))

#37C
dfchronic
modelchronic37_early<-lm(Alive~Line, data = dfchronic_Early[dfchronic_Early$Temperature == "37",])
summary(aov(modelchronic37_early))
summary(modelchronic37_early)
emm4<-emmeans(modelchronic37_early, pairwise~Line)
emm4
#write.csv(emm4$contrasts, file = "2hour_stress_37C_statistics.csv")
emmeans_model4 <- emmeans(modelchronic37_early, ~ Line)
cld(emmeans_model4, Letters=letters)
plot(cld(emmeans_model4, Letters=letters))

#41
modelchronic41_early<-lm(Alive~Line, data = dfchronic_Early[dfchronic_Early$Temperature == "41",])
summary(aov(modelchronic41_early))
summary(modelchronic41_early)
emm5<-emmeans(modelchronic41_early, pairwise~Line)
emm5
#write.csv(emm5$contrasts, file = "2hour_stress_41C_statistics.csv")
emmeans_model5 <- emmeans(modelchronic41_early, ~ Line)
cld(emmeans_model5, Letters=letters)
plot(cld(emmeans_model5, Letters=letters))

##25 __. Late

#25C
modelchronic25_late<-lm(Alive~Line, data = dfchronic_Late[dfchronic_Late$Temperature == "25",])
summary(aov(modelchronic25_late))
summary(modelchronic25_late)
emm1<-emmeans(modelchronic25_late, pairwise~Line)
emm1
emmeans_model1 <- emmeans(modelchronic25_late, ~ Line)
cld(emmeans_model1, Letters=letters)
plot(cld(emmeans_model1, Letters=letters))

#29C  
modelchronic29_late<-lm(Alive~Line, data = dfchronic_Late[dfchronic_Late$Temperature == "29",])
summary(aov(modelchronic29_late))
summary(modelchronic29_late)
emm2<-emmeans(modelchronic29_late, pairwise~Line)
emm2
#write.csv(emm2$contrasts, file = "2hour_stress_29C_statistics.csv")
emmeans_model2 <- emmeans(modelchronic29_late, ~ Line)
cld(emmeans_model2, Letters=letters)
plot(cld(emmeans_model2, Letters=letters))

#33C
modelchronic33_late<-lm(Alive~Line, data = dfchronic_Late[dfchronic_Late$Temperature == "33",])
summary(aov(modelchronic33_late))
summary(modelchronic33_late)
emm3<-emmeans(modelchronic33_late, pairwise~Line)
emm3
#write.csv(emm3$contrasts, file = "2hour_stress_33C_statistics.csv")
emmeans_model3 <- emmeans(modelchronic33_late, ~ Line)
cld(emmeans_model3, Letters=letters)
plot(cld(emmeans_model3, Letters=letters))

#37C
modelchronic37_late<-lm(Alive~Line, data = dfchronic_Late[dfchronic_Late$Temperature == "37",])
summary(aov(modelchronic37_late))
summary(modelchronic37_late)
emm4<-emmeans(modelchronic37_late, pairwise~Line)
emm4
#write.csv(emm4$contrasts, file = "2hour_stress_37C_statistics.csv")
emmeans_model4 <- emmeans(modelchronic37_late, ~ Line)
cld(emmeans_model4, Letters=letters)
plot(cld(emmeans_model4, Letters=letters))

#41
modelchronic41_late<-lm(Alive~Line, data = dfchronic_Late[dfchronic_Late$Temperature == "41",])
summary(aov(modelchronic41_late))
summary(modelchronic41_late)
emm5<-emmeans(modelchronic41_late, pairwise~Line)
emm5
#write.csv(emm5$contrasts, file = "2hour_stress_41C_statistics.csv")
emmeans_model5 <- emmeans(modelchronic41_late, ~ Line)
cld(emmeans_model5, Letters=letters)
plot(cld(emmeans_model5, Letters=letters))

