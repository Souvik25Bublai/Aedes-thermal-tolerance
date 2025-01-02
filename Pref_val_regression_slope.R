


#Calculate "slope"
#Load Packages and Datasheets
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
  library(multcompView)
  library(emmeans)
  library(car)
  library(carData)
  library(lmtest)
  library(zoo)
  library(lattice)
  library(patchwork)
  library(lme4)
  library(patchwork)
  library(scales)
  library(ggpubr)
  library(tidyverse)
}

attach(Egg_survival)
df<-Egg_survival


df3<-dlply(df,.(Line))

######grouping into appropriate categories (based on Line)######
dfPK10<-df3$"PK10"
dfMindin<-df3$"Mindin"
dfKintampo<-df3$"Kintampo"
dfKedougou<-df3$"Kedougou"
dfNgoye<-df3$"Ngoye"
dfKumasi<-df3$"Kumasi"
dfThies<-df3$"Thies"

#View(dfPK10)
#View(dfMindin)
#View(dfKintampo)
#View(dfKedougou)
#View(dfNgone)
#View(dfKumasi)
#View(dfThies)

#BASED ON REEGRESSION VALUES of each line

#Line == PK10
AmodelPK10<-glm(Egg~Temperature, data = dfPK10[dfPK10$Stress == "acute",])
AmodelPK10
summary(AmodelPK10)
CmodelPK10<-glm(Egg~Temperature, data = dfPK10[dfPK10$Stress == "chronic",])
CmodelPK10
summary(CmodelPK10)

#Line == Mindin
AmodelMindin<-glm(Egg~Temperature, data = dfMindin[dfMindin$Stress == "acute",])
AmodelMindin
summary(AmodelMindin)
CmodelMindin<-glm(Egg~Temperature, data = dfMindin[dfMindin$Stress == "chronic",])
CmodelMindin
summary(CmodelMindin)

#Line == Kintampo
AmodelKintampo<-glm(Egg~Temperature, data = dfKintampo[dfKintampo$Stress == "acute",])
AmodelKintampo
summary(AmodelKintampo)
CmodelKintampo<-glm(Egg~Temperature, data = dfKintampo[dfKintampo$Stress == "chronic",])
CmodelKintampo
summary(CmodelKintampo)

#Line == Kedougou
AmodelKedougou<-glm(Egg~Temperature, data = dfKedougou[dfKedougou$Stress == "acute",])
AmodelKedougou
summary(AmodelKedougou)
CmodelKedougou<-glm(Egg~Temperature, data = dfKedougou[dfKedougou$Stress == "chronic",])
CmodelKedougou
summary(CmodelKedougou)

#Line == Ngoye
AmodelNgoye<-glm(Egg~Temperature, data = dfNgoye[dfNgoye$Stress == "acute",])
AmodelNgoye
summary(AmodelNgoye)
CmodelNgoye<-glm(Egg~Temperature, data = dfNgoye[dfNgoye$Stress == "chronic",])
CmodelNgoye
summary(CmodelNgoye)

#Line == Thies
AmodelThies<-glm(Egg~Temperature, data = dfThies[dfThies$Stress == "acute",])
AmodelThies
summary(AmodelThies)
CmodelThies<-glm(Egg~Temperature, data = dfThies[dfThies$Stress == "chronic",])
CmodelThies
summary(CmodelThies)

#Line == Kumasi
AmodelKumasi<-glm(Egg~Temperature, data = dfKumasi[dfKumasi$Stress == "acute",])
AmodelKumasi
summary(AmodelKumasi)
CmodelKumasi<-glm(Egg~Temperature, data = dfKumasi[dfKumasi$Stress == "chronic",])
CmodelKumasi
summary(CmodelKumasi)

###All the slope values has been recorded manually into another xlxs file, 
#"Pref_reg_val_Stress".


