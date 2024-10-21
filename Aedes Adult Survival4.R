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
  library(emmeans)
  library(car)
  library(carData)
  library(lmtest)
  library(zoo)
  library(lattice)
}


attach(Aedes_Adult_survival)
df<-Aedes_Adult_survival
View(df)
str(df)
df$Line<-as.factor(df$Line)
df$Sex<-as.factor(df$Sex)
df$Hour<-as.factor(df$Hour)
df$Recovery<-as.factor(df$Recovery)


model25<- aov(Probability ~ Sex, data = df)
summary(model25)
model255<- aov(Probability ~ Temperature, data = df)
summary(model255)




###Average
library(dplyr)


averages <- df %>%
  group_by(Line, Temperature, Hour, Recovery, Sex) %>%
  summarize(
    Avg_Probability = mean(Probability),
    SE_Probability = sd(Probability) / sqrt(n())
  ) %>%
  ungroup()

# Print the calculated averages with standard error
print(averages)
write.csv(averages, file = "Aedes_Adult_Average.csv")
#####


#Split the dataset into appropriate groups (Based on stress levels--"Hour")

df2<-dlply(df,.(Hour)) 
df2
df2h<-df2$'2'
df6h<-df2$'6'

write.csv(df2h, file = '2hour_stress.csv')
write.csv(df6h, file = '6hour_stress.csv')

str(df2h)

df2h_early <- df2h %>% filter(Recovery == "early")
df2h_late <- df2h %>% filter(Recovery == "late")
# Plot the graph using the filtered data
ggplot(df2h_early, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 1),
    legend.key.size = unit(1, "cm")
  ) +
  labs(x = "Temperature (°C)", y = "Probability %")

df6h_early <- df6h %>% filter(Recovery == "early")
df6h_late <- df6h %>% filter(Recovery == "late")
# Plot the graph using the filtered data
ggplot(df6h_late, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 1),
    legend.key.size = unit(1, "cm")
  ) +
  labs(x = "Temperature (°C)", y = "Probability %")

#Split the 2hr dataset into appropriate groups (Based on Sex)
df3<-dlply(df2h,.(Sex))
df2hMALE<-df3$'m'
df2hFEMALE<-df3$'f'
write.csv(df2hMALE, file = "2hour_stress_MALE.csv")
write.csv(df2hFEMALE, file = "2hour_stress_FEMALE.csv")

#Split the chronic dataset into appropriate groups (Based on Sex)
df4<-dlply(df6h,.(Sex))
df6hMALE<-df4$'m'
df6hFEMALE<-df4$'f'
write.csv(df6hMALE, file = "6hour_stress_MALE.csv")
write.csv(df6hFEMALE, file = "6hour_stress_FEMALE.csv")

df2hFEMALE
df2hMALE
df6hFEMALE
df6hMALE

#Stat

#####2hour Stress#####

#2hour_stress_female_EARLY-RECOVERY_statistics
df2hFEMALE
model1<-lm(ArcSign~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "early",])
summary(model1)
emm1 <- emmeans(model1, pairwise ~ Line)
emm1
write.csv(emm1$contrasts, file = "2hour_stress_female_Early-Recovery_statistics.csv")
getwd()
#2hour_stress_male_EARLY-RECOVERY_statistics
df2hMALE
model2<-lm(ArcSign~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "early",])
summary(model2)
emm2 <- emmeans(model2, pairwise ~ Line)
emm2
write.csv(emm2$contrasts, file = "2hour_stress_male_Early-Recovery_statistics.csv")


#2hour_stress_female_LATE-RECOVERY_statistics
df2hFEMALE
model3<-lm(ArcSign~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "late",])
summary(model3)
emm3 <- emmeans(model3, pairwise ~ Line)
emm3
write.csv(emm3$contrasts, file = "2hour_stress_female_Late-Recovery_statistics.csv")

#2hour_stress_male_LATE-RECOVERY_statistics
df2hMALE
model4<-lm(ArcSign~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "late",])
summary(model4)
emm4 <- emmeans(model4, pairwise ~ Line)
emm4
write.csv(emm4$contrasts, file = "2hour_stress_male_Late-Recovery_statistics.csv")


#####Long/Chronic(6hr)Stress#####

#6hour_stress_female_EARLY-RECOVERY_statistics
df6hFEMALE
model5<-lm(ArcSign~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "early",])
summary(model5)
emm5 <- emmeans(model5, pairwise ~ Line)
emm5
write.csv(emm5$contrasts, file = "6hour_stress_female_Early-Recovery_statistics.csv")

#6hour_stress_male_EARLY-RECOVERY_statistics
df6hMALE
model6<-lm(ArcSign~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "early",])
summary(model6)
emm6 <- emmeans(model6, pairwise ~ Line)
emm6
write.csv(emm6$contrasts, file = "6hour_stress_male_Early-Recovery_statistics.csv")


#6hour_stress_female_LATE-RECOVERY_statistics
df6hFEMALE
model7<-lm(ArcSign~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "late",])
summary(model7)
emm7 <- emmeans(model7, pairwise ~ Line)
emm7
write.csv(emm7$contrasts, file = "6hour_stress_female_Late-Recovery_statistics.csv")

#6hour_stress_male_LATE-RECOVERY_statistics
df6hMALE
model8<-lm(ArcSign~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "late",])
summary(model8)
emm8 <- emmeans(model8, pairwise ~ Line)
emm8
write.csv(emm8$contrasts, file = "6hour_stress_male_Late-Recovery_statistics.csv")



#Split the acute Male and Female dataset into appropriate groups (Based on Recovery)

df5<-dlply(df2hMALE,. (Recovery))
df5
df2hMALE_Early<-df5$early
df2hMALE_Late<-df5$late

df6<-dlply(df2hFEMALE,. (Recovery))
df6  
df2hFEMALE_Early<-df6$early 
df2hFEMALE_Late<-df6$late  

#Split the chronic Male and Female dataset into appropriate groups (Based on Recovery)

df7<-dlply(df6hMALE,. (Recovery))
df7
df6hMALE_Early<-df7$early
df6hMALE_Late<-df7$late

df8<-dlply(df6hFEMALE,. (Recovery))
df8  
df6hFEMALE_Early<-df8$early 
df6hFEMALE_Late<-df8$late  

#Write down the csv files
write.csv(df2hMALE_Early, file = "2hour_stress_MALE_Early_Recovery.csv")
write.csv(df2hMALE_Late, file = "2hour_stress_MALE_Late_Recovery.csv")
write.csv(df2hFEMALE_Early, file = "2hour_stress_FEMALE_Early_Recovery.csv")
write.csv(df2hFEMALE_Late, file = "2hour_stress_FEMALE_Late_Recovery.csv")

write.csv(df6hMALE_Early, file = "6hour_stress_MALE_Early_Recovery.csv")
write.csv(df6hMALE_Late, file = "6hour_stress_MALE_Late_Recovery.csv")
write.csv(df6hFEMALE_Early, file = "6hour_stress_FEMALE_Early_Recovery.csv")
write.csv(df6hFEMALE_Late, file = "6hour_stress_FEMALE_Late_Recovery.csv")

#Let's do statistics based on temperature

##ACUTE STRESS (2hour)

#acute(2hr)_Early_Recovery(2hr)_Female

model9<-lm(ArcSign~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "33",])
summary(aov(model9))
model10<-lm(ArcSign~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "37",])
summary(aov(model10))
model11<-lm(ArcSign~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "41",])
summary(aov(model11))

#acute(2hr)_Late_Recovery(24hr)_Female

model12<-lm(ArcSign~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "33",])
summary(aov(model12))
emm12 <- emmeans(model12, pairwise ~ Line)
emm12
write.csv(emm12$contrasts, file = "33T_2hour_stress_FEMALE_Late_Recovery.csv")
model13<-lm(ArcSign~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "37",])
summary(aov(model13))
model14<-lm(ArcSign~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "41",])
summary(aov(model14))

#acute(2hr)_Early_Recovery(2hr)_Male

model15<-lm(ArcSign~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "33",])
summary(aov(model15))
model16<-lm(ArcSign~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "37",])
summary(aov(model16))
model17<-lm(ArcSign~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "41",])
summary(aov(model17))
emm17<-emmeans(model17, pairwise~Line)
emm17
write.csv(emm17$contrasts, file = "41T_2hour_stress_MALE_Early_Recovery.csv")

#acute(2hr)_Late_Recovery(24hr)_Male

model18<-lm(ArcSign~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "33",])
summary(aov(model18))
emm18<-emmeans(model18, pairwise~Line)
emm18
write.csv(emm18$contrasts, file = "33T_2hour_stress_MALE_Late_Recovery.csv")
model19<-lm(ArcSign~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "37",])
summary(aov(model19))
model20<-lm(ArcSign~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "41",])
summary(aov(model20))



##CHRONIC STRESS (6hour)
#chronic(6hr)_Early_Recovery(2hr)_Female

model21<-lm(ArcSign~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "33",])
summary(aov(model21))
model22<-lm(ArcSign~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "37",])
summary(aov(model22))
model23<-lm(ArcSign~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "41",])
summary(aov(model23))

#chronic(6hr)_Late_Recovery(24hr)_Female

model24<-lm(ArcSign~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "33",])
summary(aov(model24))
emm24 <- emmeans(model24, pairwise ~ Line)
emm24
write.csv(emm24$contrasts, file = "33T_6hour_stress_FEMALE_Late_Recovery.csv")
model25<-lm(ArcSign~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "37",])
summary(aov(model25))
model26<-lm(ArcSign~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "41",])
summary(aov(model26))


#chronic(6hr)_Early_Recovery(2hr)_Male

model27<-lm(ArcSign~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "33",])
summary(aov(model27))
model28<-lm(ArcSign~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "37",])
summary(aov(model28))
model29<-lm(ArcSign~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "41",])
summary(aov(model29))
emm29 <- emmeans(model29, pairwise ~ Line)
emm29
write.csv(emm29$contrasts, file = "41T_6hour_stress_MALE_Early_Recovery.csv")

#chronic(6hr)_Late_Recovery(24hr)_Male

model30<-lm(ArcSign~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "33",])
summary(aov(model30))
model31<-lm(ArcSign~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "37",])
summary(aov(model31))
model32<-lm(ArcSign~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "41",])
summary(aov(model32))
emm32 <- emmeans(model32, pairwise ~ Line)
emm32
write.csv(emm32$contrasts, file = "41T_6hour_stress_MALE_Late_Recovery.csv")

#emm29 and emm32 is not generating any significant data(!)



###########################PLOTING###############################
################################################################

df2hFEMALE
df2hMALE

df6hFEMALE
df6hMALE

#Split the 2h and 6hr dataset into appropriate groups (Based on Temperature)

df9<-dlply(df2hFEMALE,. (Temperature))
df9
df2hFEMALE33<-df9$'33'
df2hFEMALE37<-df9$'37'
df2hFEMALE41<-df9$'41'

df10<-dlply(df2hMALE,. (Temperature))
df10
df2hMALE33<-df10$'33'
df2hMALE37<-df10$'37'
df2hMALE41<-df10$'41'

df11<-dlply(df6hFEMALE,. (Temperature))
df11
df6hFEMALE33<-df11$'33'
df6hFEMALE37<-df11$'37'
df6hFEMALE41<-df11$'41'

df12<-dlply(df6hMALE,. (Temperature))
df12
df6hMALE33<-df12$'33'
df6hMALE37<-df12$'37'
df6hMALE41<-df12$'41'


{AF33<-df2hFEMALE33
  AF37<-df2hFEMALE37
  AF41<-df2hFEMALE41
  
  AM33<-df2hMALE33
  AM37<-df2hMALE37
  AM41<-df2hMALE41
  
  
  CF33<-df6hFEMALE33
  CF37<-df6hFEMALE37
  CF41<-df6hFEMALE41
  
  CM33<-df6hFEMALE33
  CM37<-df6hFEMALE37
  CM41<-df6hFEMALE41
}


View(CF33)
head(CF33)
#View(CF37)
#View(CF41)

#View(CM33)
#View(CM37)
#View(CM41)

################################2hour#######################################
#Ordering the levels
{levels(AF33$Line)
  AF33$Line <- ordered(AF33$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AF33$Line)
  
  levels(AF37$Line)
  AF37$Line <- ordered(AF37$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AF37$Line)
  
  levels(AF41$Line)
  AF41$Line <- ordered(AF41$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AF41$Line)
  
  levels(AM33$Line)
  AM33$Line <- ordered(AM33$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AM33$Line)
  
  levels(AM37$Line)
  AM37$Line <- ordered(AM37$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AM37$Line)
  
  levels(AM41$Line)
  AM41$Line <- ordered(AM41$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(AM41$Line)
}

library(ggplot2)
library(dplyr)

#Calculating the errorbars

{errbar_lims_AF33 <- group_by(AF33, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AF33
  
  errbar_lims_AF37 <- group_by(AF37, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AF37
  
  errbar_lims_AF41 <- group_by(AF41, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AF41
  
  errbar_lims_AM33 <- group_by(AM33, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AM33
  
  errbar_lims_AM37 <- group_by(AM37, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AM37
  
  errbar_lims_AM41 <- group_by(AM41, Line, Recovery) %>% 
    summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
              upper=mean+(2*se), lower=mean-(2*se))
  errbar_lims_AM41
}


#Plotting

pAF33<-ggplot(AF33, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AF33, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AF33, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAF33




pAF37<-ggplot(AF37, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AF37, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AF37, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAF37

pAF41<-ggplot(AF41, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AF41, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AF41, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAF41

pAM33<-ggplot(AM33, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AM33, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AM33, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAM33

pAM37<-ggplot(AM37, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AM37, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AM37, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAM37

pAM41<-ggplot(AM41, aes(x = Recovery, fill = Line))+
  geom_violin(aes(y = Probability))+
  geom_point(data = errbar_lims_AM41, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_AM41, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
pAM41


library("patchwork")

ggp<-(pAM33+pAF33)/(pAM37+pAF37)/(pAM41+pAF41)
ggp

ggsave("Acute_Stress4.pdf", dpi=300, dev='pdf', height=12, width=12, units="in")



##################################6hour##################################
#Ordering the levels
{levels(CF33$Line)
  CF33$Line <- ordered(CF33$Line, levels = c("PK10", "Mindin", "Kintampo", "Kedougou", "Ngoye", "Kumasi", "Thies", "Lab"))
  levels(CF33$Line)
  
  levels(CF37$Line)
  CF37$Line <- ordered(CF37$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(CF37$Line)
  
  levels(CF41$Line)
  CF41$Line <- ordered(CF41$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(CF41$Line)
  
  levels(CM33$Line)
  CM33$Line <- ordered(CM33$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(CM33$Line)
  
  levels(CM37$Line)
  CM37$Line <- ordered(CM37$Line, levels = c("PK10", "Mindin", "Kimtaba", "Kedougou", "Ngone", "Kumasi", "Thies", "Lab"))
  levels(CM37$Line)
  
  levels(CM41$Line)
  CM41$Line <- ordered(CM41$Line, levels = c("PK10", "Mindin", "Kintampo", "Kedougou", "Ngoye", "Kumasi", "Thies", "Lab"))
  levels(CM41$Line)
}



library(ggplot2)
library(dplyr)

#Calculating the errorbars

errbar_lims_CF33 <- group_by(CF33, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CF33

errbar_lims_CF37 <- group_by(CF37, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CF37

errbar_lims_CF41 <- group_by(CF41, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CF41

errbar_lims_CM33 <- group_by(CM33, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CM33

errbar_lims_CM37 <- group_by(CM37, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CM37

errbar_lims_CM41 <- group_by(CM41, Line, Recovery) %>% 
  summarize(mean=mean(Probability), se=sd(Probability)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))
errbar_lims_CM41


#Plotting

{pCF33<-ggplot(CF33, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CF33, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CF33, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCF33
  
  pCF37<-ggplot(CF37, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CF37, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CF37, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCF37
  
  pCF41<-ggplot(CF41, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CF41, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CF41, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCF41
  
  pCM33<-ggplot(CM33, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CM33, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CM33, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCM33
  
  pCM37<-ggplot(CM37, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CM37, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CM37, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCM37
  
  pCM41<-ggplot(CM41, aes(x = Recovery, fill = Line))+
    geom_violin(aes(y = Probability))+
    geom_point(data = errbar_lims_CM41, aes(x = Recovery, y = mean, group = Line),
               size = 3, position = position_dodge(width = 0.9)) +
    geom_errorbar(data = errbar_lims_CM41, aes(ymax = upper, ymin = lower, group = Line),
                  stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
    theme_classic(base_size=10)+scale_fill_brewer(palette="Dark2")+ylim(0,1)+theme(legend.position = "none") 
  pCM41
}


library("patchwork")

ggq<-(pCM33+pCF33)/(pCM37+pCF37)/(pCM41+pCF41)
ggq

ggsave("Chronic_Stress1.pdf", dpi=300, dev='pdf', height=12, width=12, units="in")


#legend purpose


pCM33 <- ggplot(CM33, aes(x = Recovery, fill = Line)) +
  geom_violin(aes(y = Probability)) +
  geom_point(data = errbar_lims_CM33, aes(x = Recovery, y = mean, group = Line),
             size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = errbar_lims_CM33, aes(ymax = upper, ymin = lower, group = Line),
                stat = 'identity', position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic(base_size = 10) +
  scale_fill_brewer(palette = "Dark2") +
  ylim(0, 1) +
  labs(fill = "Line")

pCM33

# Using the cowplot package printing the legend only

library(grid)
library(gridExtra)

legend <- cowplot::get_legend(pCM33)
grid.newpage()
grid.draw(legend)
