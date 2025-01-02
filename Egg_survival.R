
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

attach(Egg_survival)
df<-Egg_survival

df2<-dlply(df,.(Stress))
df2
dfacute<-df2$'2'
dfchronic<-df2$'6'
getwd()
#write.csv(dfacute, file = '2hour_stress.csv')
#write.csv(dfchronic, file = '6hour_stress.csv')


#Split the ACUTE dataset into appropriate groups (Based on Line)
#df3<-dlply(dfacute,.(Line))

#AdfPK10<-df3$PK10
#AdfMindin<-df3$Mindin
#AdfKintampo<-df3$Kintampo
#AdfKedougou<-df3$Kedougou
#AdfNgoye<-df3$Ngoye
#AdfThies<-df3$Thies
#AdfKumasi<-df3$Kumasi

#write.csv(")

#Q1. how do the egg dfching of populations differ across the various temperature treatments?
#Split the data set into appropriate groups (Based on stress levels)

#####Short/Acute(2hr)Stress#####

#2hour_stress_statistics
dfacute
#25C
modelacute25<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "25",])
summary(modelacute25)
emm1<-emmeans(modelacute25, pairwise~Line)
emm1
#write.csv(emm1$contrasts, file = "2hour_stress_25C_statistics.csv")
emmeans_model1 <- emmeans(modelacute25, ~ Line)
cld(emmeans_model1, Letters=letters)
plot(cld(emmeans_model1, Letters=letters))


dfacute
modelacute29<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "29",])
summary(modelacute29)
emm2<-emmeans(modelacute29, pairwise~Line)
emm2
#write.csv(emm2$contrasts, file = "2hour_stress_29C_statistics.csv")
emmeans_model2 <- emmeans(modelacute29, ~ Line)
cld(emmeans_model2, Letters=letters)
plot(cld(emmeans_model2, Letters=letters))

#33C
dfacute
modelacute33<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "33",])
summary(modelacute33)
emm3<-emmeans(modelacute33, pairwise~Line)
emm3
write.csv(emm3$contrasts, file = "2hour_stress_33C_statistics.csv")
emmeans_model3 <- emmeans(modelacute33, ~ Line)
cld(emmeans_model3, Letters=letters)
plot(cld(emmeans_model3, Letters=letters))

#37C
dfacute
modelacute37<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "37",])
summary(modelacute37)
emm4<-emmeans(modelacute37, pairwise~Line)
emm4
write.csv(emm4$contrasts, file = "2hour_stress_37C_statistics.csv")
emmeans_model4 <- emmeans(modelacute37, ~ Line)
cld(emmeans_model4, Letters=letters)
plot(cld(emmeans_model4, Letters=letters))

#41
dfacute
modelacute41<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "41",])
summary(modelacute41)
emm5<-emmeans(modelacute41, pairwise~Line)
emm5
write.csv(emm5$contrasts, file = "2hour_stress_41C_statistics.csv")
emmeans_model5 <- emmeans(modelacute41, ~ Line)
cld(emmeans_model5, Letters=letters)
plot(cld(emmeans_model5, Letters=letters))

#45
dfacute
modelacute45<-lm(E_ArcSign~Line, data = dfacute[dfacute$Temperature == "45",])
summary(modelacute45)
emm6<-emmeans(modelacute45, pairwise~Line)
emm6
write.csv(emm6$contrasts, file = "2hour_stress_45C_statistics.csv")
emmeans_model6 <- emmeans(modelacute45, ~ Line)
cld(emmeans_model6, Letters=letters)
plot(cld(emmeans_model6, Letters=letters))


#6hour_stress_statistics

dfchronic

#25C
modelchronic25<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "25",])
summary(modelchronic25)
emm1a<-emmeans(modelchronic25, pairwise~Line)
emm1a
write.csv(emm1a$contrasts, file = "6hour_stress_25C_statistics.csv")
emmeans_model7 <- emmeans(modelchronic25, ~ Line)
cld(emmeans_model7, Letters=letters)
plot(cld(emmeans_model7, Letters=letters))

#29C
dfchronic
modelchronic29<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "29",])
summary(modelchronic29)
emm2a<-emmeans(modelchronic29, pairwise~Line)
emm2a
write.csv(emm2a$contrasts, file = "6hour_stress_29C_statistics.csv")
emmeans_model8 <- emmeans(modelchronic29, ~ Line)
cld(emmeans_model8, Letters=letters)
plot(cld(emmeans_model8, Letters=letters))

#33C
dfchronic
modelchronic33<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "33",])
summary(modelchronic33)
emm3a<-emmeans(modelchronic33, pairwise~Line)
emm3a
write.csv(emm3a$contrasts, file = "6hour_stress_33C_statistics.csv")
emmeans_model9 <- emmeans(modelchronic33, ~ Line)
cld(emmeans_model9, Letters=letters)
plot(cld(emmeans_model9, Letters=letters))

#37C
dfchronic
modelchronic37<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "37",])
summary(modelchronic37)
emm4a<-emmeans(modelchronic37, pairwise~Line)
emm4a
write.csv(emm4a$contrasts, file = "6hour_stress_37C_statistics.csv")
emmeans_model10 <- emmeans(modelchronic37, ~ Line)
cld(emmeans_model10, Letters=letters)
plot(cld(emmeans_model10, Letters=letters))

#41
dfchronic
modelchronic41<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "41",])
summary(modelchronic41)
emm5a<-emmeans(modelchronic41, pairwise~Line)
emm5a
write.csv(emm5a$contrasts, file = "6hour_stress_41C_statistics.csv")
emmeans_model11 <- emmeans(modelchronic41, ~ Line)
cld(emmeans_model11, Letters=letters)
plot(cld(emmeans_model11, Letters=letters))

#45
dfchronic
modelchronic45<-lm(E_ArcSign~Line, data = dfchronic[dfchronic$Temperature == "45",])
summary(modelchronic45)
emm6a<-emmeans(modelchronic45, pairwise~Line)
emm6a
write.csv(emm6a$contrasts, file = "6hour_stress_45C_statistics.csv")
emmeans_model12 <- emmeans(modelchronic45, ~ Line)
cld(emmeans_model12, Letters=letters)
plot(cld(emmeans_model12, Letters=letters))
