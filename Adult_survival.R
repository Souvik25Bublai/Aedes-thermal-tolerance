
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


##STAT
attach(Adult_survival)
df<-Adult_survival
View(df)
str(df)
df$Line<-as.factor(df$Line)
df$Sex<-as.factor(df$Sex)
df$Hour<-as.factor(df$Hour)
df$Recovery<-as.factor(df$Recovery)


model1<- aov(Probability ~ Sex, data = df)
summary(model1)
model2<- aov(Probability ~ Temperature, data = df)
summary(model2)



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

ggplot(df2h_early, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(se = FALSE, size = 1) + # Automatically uses LOESS for smoothing
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 1),
    legend.key.size = unit(1, "cm")
  ) +
  labs(x = "Temperature (°C)", y = "Probability %")

ggplot(df2h_late, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
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

ggplot(df2h_late, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(se = FALSE, size = 1) + # Automatically uses LOESS for smoothing
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
ggplot(df6h_early, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
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

ggplot(df6h_early, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(se = FALSE, size = 1) + # Automatically uses LOESS for smoothing
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 1),
    legend.key.size = unit(1, "cm")
  ) +
  labs(x = "Temperature (°C)", y = "Probability %")

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

ggplot(df6h_late, aes(x = Temperature, y = Probability, color = Line, linetype = Sex)) +
  geom_smooth(se = FALSE, size = 1) + # Automatically uses LOESS for smoothing
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
model1<-lm(Probability~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "early",])
summary(model1)
emm1 <- emmeans(model1, pairwise ~ Line)
emm1
write.csv(emm1$contrasts, file = "2hour_stress_female_Early-Recovery_statistics.csv")
getwd()
#2hour_stress_male_EARLY-RECOVERY_statistics
df2hMALE
model2<-glm(Probability~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "early",])
summary(model2)
emm2 <- emmeans(model2, pairwise ~ Line)
emm2
write.csv(emm2$contrasts, file = "2hour_stress_male_Early-Recovery_statistics.csv")


#2hour_stress_female_LATE-RECOVERY_statistics
df2hFEMALE
model3<-glm(Probability~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "late",])
summary(model3)
emm3 <- emmeans(model3, pairwise ~ Line)
emm3
write.csv(emm3$contrasts, file = "2hour_stress_female_Late-Recovery_statistics.csv")

#2hour_stress_male_LATE-RECOVERY_statistics
df2hMALE
model4<-glm(Probability~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "late",])
summary(model4)
emm4 <- emmeans(model4, pairwise ~ Line)
emm4
write.csv(emm4$contrasts, file = "2hour_stress_male_Late-Recovery_statistics.csv")


#####Long/Chronic(6hr)Stress#####

#6hour_stress_female_EARLY-RECOVERY_statistics
df6hFEMALE
model5<-glm(Probability~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "early",])
summary(model5)
emm5 <- emmeans(model5, pairwise ~ Line)
emm5
write.csv(emm5$contrasts, file = "6hour_stress_female_Early-Recovery_statistics.csv")

#6hour_stress_male_EARLY-RECOVERY_statistics
df6hMALE
model6<-glm(Probability~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "early",])
summary(model6)
emm6 <- emmeans(model6, pairwise ~ Line)
emm6
write.csv(emm6$contrasts, file = "6hour_stress_male_Early-Recovery_statistics.csv")


#6hour_stress_female_LATE-RECOVERY_statistics
df6hFEMALE
model7<-glm(Probability~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "late",])
summary(model7)
emm7 <- emmeans(model7, pairwise ~ Line)
emm7
write.csv(emm7$contrasts, file = "6hour_stress_female_Late-Recovery_statistics.csv")

#6hour_stress_male_LATE-RECOVERY_statistics
df6hMALE
model8<-glm(Probability~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "late",])
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

model9<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "33",])
summary(aov(model9))
model10<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "37",])
summary(aov(model10))
model11<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "41",])
summary(aov(model11))

#acute(2hr)_Late_Recovery(24hr)_Female

model12<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "33",])
summary(aov(model12))
emm12 <- emmeans(model12, pairwise ~ Line)
emm12
write.csv(emm12$contrasts, file = "33T_2hour_stress_FEMALE_Late_Recovery.csv")
model13<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "37",])
summary(aov(model13))
model14<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "41",])
summary(aov(model14))

#acute(2hr)_Early_Recovery(2hr)_Male

model15<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "33",])
summary(aov(model15))
model16<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "37",])
summary(aov(model16))
model17<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "41",])
summary(aov(model17))
emm17<-emmeans(model17, pairwise~Line)
emm17
write.csv(emm17$contrasts, file = "41T_2hour_stress_MALE_Early_Recovery.csv")

#acute(2hr)_Late_Recovery(24hr)_Male

model18<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "33",])
summary(aov(model18))
emm18<-emmeans(model18, pairwise~Line)
emm18
write.csv(emm18$contrasts, file = "33T_2hour_stress_MALE_Late_Recovery.csv")
model19<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "37",])
summary(aov(model19))
model20<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "41",])
summary(aov(model20))



##CHRONIC STRESS (6hour)
#chronic(6hr)_Early_Recovery(2hr)_Female

model21<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "33",])
summary(aov(model21))
model22<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "37",])
summary(aov(model22))
model23<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "41",])
summary(aov(model23))

#chronic(6hr)_Late_Recovery(24hr)_Female

model24<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "33",])
summary(aov(model24))
emm24 <- emmeans(model24, pairwise ~ Line)
emm24
write.csv(emm24$contrasts, file = "33T_6hour_stress_FEMALE_Late_Recovery.csv")
model25<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "37",])
summary(aov(model25))
model26<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "41",])
summary(aov(model26))


#chronic(6hr)_Early_Recovery(2hr)_Male

model27<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "33",])
summary(aov(model27))
model28<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "37",])
summary(aov(model28))
model29<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "41",])
summary(aov(model29))
emm29 <- emmeans(model29, pairwise ~ Line)
emm29
write.csv(emm29$contrasts, file = "41T_6hour_stress_MALE_Early_Recovery.csv")

#chronic(6hr)_Late_Recovery(24hr)_Male

model30<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "33",])
summary(aov(model30))
model31<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "37",])
summary(aov(model31))
model32<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "41",])
summary(aov(model32))
emm32 <- emmeans(model32, pairwise ~ Line)
emm32
write.csv(emm32$contrasts, file = "41T_6hour_stress_MALE_Late_Recovery.csv")

#emm29 and emm32 is not generating any significant data(!)





model1<-lm(Probability~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "early",])
summary(model1)

model2<-glm(Probability~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "early",])
summary(model2)

model3<-glm(Probability~Line+Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "late",])
summary(model3)

model4<-glm(Probability~Line+Temperature, data = df2hMALE[df2hMALE$Recovery == "late",])
summary(model4)

model5<-glm(Probability~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "early",])
summary(model5)

model6<-glm(Probability~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "early",])
summary(model6)

model7<-glm(Probability~Line+Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "late",])
summary(model7)

model8<-glm(Probability~Line+Temperature, data = df6hMALE[df6hMALE$Recovery == "late",])
summary(model8)


model9<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "33",])
summary(aov(model9))

model10<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "37",])
summary(aov(model10))

model11<-glm(Probability~Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "41",])
summary(aov(model11))

model12<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "33",])
summary(aov(model12))

model13<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "37",])
summary(aov(model13))

model14<-glm(Probability~Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "41",])
summary(aov(model14))

model15<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "33",])
summary(aov(model15))

model16<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "37",])
summary(aov(model16))

model17<-glm(Probability~Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "41",])
summary(aov(model17))

model18<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "33",])
summary(aov(model18))

model19<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "37",])
summary(aov(model19))

model20<-glm(Probability~Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "41",])
summary(aov(model20))

model21<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "33",])
summary(aov(model21))

model22<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "37",])
summary(aov(model22))

model23<-glm(Probability~Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "41",])
summary(aov(model23))

model24<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "33",])
summary(aov(model24))

model25<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "37",])
summary(aov(model25))

model26<-glm(Probability~Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "41",])
summary(aov(model26))

model27<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "33",])
summary(aov(model27))

model28<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "37",])
summary(aov(model28))

model29<-glm(Probability~Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "41",])
summary(aov(model29))

model30<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "33",])
summary(aov(model30))

model31<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "37",])
summary(aov(model31))

model32<-glm(Probability~Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "41",])
summary(aov(model32))



# Create a list of all models
model_list <- list(
  lm(Probability ~ Line + Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "early",]),
  glm(Probability ~ Line + Temperature, data = df2hMALE[df2hMALE$Recovery == "early",]),
  glm(Probability ~ Line + Temperature, data = df2hFEMALE[df2hFEMALE$Recovery == "late",]),
  glm(Probability ~ Line + Temperature, data = df2hMALE[df2hMALE$Recovery == "late",]),
  glm(Probability ~ Line + Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "early",]),
  glm(Probability ~ Line + Temperature, data = df6hMALE[df6hMALE$Recovery == "early",]),
  glm(Probability ~ Line + Temperature, data = df6hFEMALE[df6hFEMALE$Recovery == "late",]),
  glm(Probability ~ Line + Temperature, data = df6hMALE[df6hMALE$Recovery == "late",]),
  glm(Probability ~ Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "33",]),
  glm(Probability ~ Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "37",]),
  glm(Probability ~ Line, data = df2hFEMALE_Early[df2hFEMALE_Early$Temperature == "41",]),
  glm(Probability ~ Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "33",]),
  glm(Probability ~ Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "37",]),
  glm(Probability ~ Line, data = df2hFEMALE_Late[df2hFEMALE_Late$Temperature == "41",]),
  glm(Probability ~ Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "33",]),
  glm(Probability ~ Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "37",]),
  glm(Probability ~ Line, data = df2hMALE_Early[df2hMALE_Early$Temperature == "41",]),
  glm(Probability ~ Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "33",]),
  glm(Probability ~ Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "37",]),
  glm(Probability ~ Line, data = df2hMALE_Late[df2hMALE_Late$Temperature == "41",]),
  glm(Probability ~ Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "33",]),
  glm(Probability ~ Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "37",]),
  glm(Probability ~ Line, data = df6hFEMALE_Early[df6hFEMALE_Early$Temperature == "41",]),
  glm(Probability ~ Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "33",]),
  glm(Probability ~ Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "37",]),
  glm(Probability ~ Line, data = df6hFEMALE_Late[df6hFEMALE_Late$Temperature == "41",]),
  glm(Probability ~ Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "33",]),
  glm(Probability ~ Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "37",]),
  glm(Probability ~ Line, data = df6hMALE_Early[df6hMALE_Early$Temperature == "41",]),
  glm(Probability ~ Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "33",]),
  glm(Probability ~ Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "37",]),
  glm(Probability ~ Line, data = df6hMALE_Late[df6hMALE_Late$Temperature == "41",])
)

# Function to perform diagnostics
diagnostics <- function(model, model_name) {
  print(paste("Diagnostics for:", model_name))
  
  # Residuals vs Fitted
  plot(model$fitted.values, residuals(model), 
       xlab = "Fitted Values", 
       ylab = "Residuals", 
       main = paste(model_name, "- Residuals vs Fitted"))
  abline(h = 0, col = "red", lty = 2)
  
  # Q-Q Plot
  qqnorm(residuals(model), main = paste(model_name, "- Normal Q-Q Plot"))
  qqline(residuals(model), col = "red", lty = 2)
  
  # Breusch-Pagan Test
  if (inherits(model, "lm")) {
    library(lmtest)
    bp_result <- bptest(model)
    print(bp_result)
  }
  
  # Cook's Distance
  cooks_d <- cooks.distance(model)
  plot(cooks_d, type = "h", main = paste(model_name, "- Cook's Distance"), 
       xlab = "Observation", ylab = "Cook's Distance")
  abline(h = 4 / length(cooks_d), col = "red", lty = 2)
}

# Apply diagnostics to all models
for (i in seq_along(model_list)) {
  diagnostics(model_list[[i]], paste("Model", i))
}




