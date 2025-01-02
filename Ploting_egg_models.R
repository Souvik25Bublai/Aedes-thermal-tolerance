#Ploting egg

##Ploting the models
library(scales)
library(ggplot2)
attach(Pref_reg_val_Stress)
mydata<-Pref_reg_val_Stress
View(mydata)

#add the new variables

mydata$Preference <- ifelse(mydata$Line=="PK10",-0.33436322,ifelse(mydata$Line=="Mindin", -0.477038214,
                                                                   ifelse(mydata$Line=="Kintampo", -0.581702248,
                                                                          ifelse(mydata$Line=="Kedougou",-0.522748506,
                                                                                 ifelse(mydata$Line=="Ngoye",0.693053879,
                                                                                        ifelse(mydata$Line=="Thies",0.396234544, -0.300140692))))))

mydata$Ancestry <- ifelse(mydata$Line=="PK10",0.001113444,ifelse(mydata$Line=="Mindin", 0.017289316,
                                                                 ifelse(mydata$Line=="Kintampo", 0.031685947,
                                                                        ifelse(mydata$Line=="Kedougou",0.008563421,
                                                                               ifelse(mydata$Line=="Ngoye",0.373815800,
                                                                                      ifelse(mydata$Line=="Thies",0.221672909, 	
                                                                                             0.067482211))))))
mydata$Density <- ifelse(mydata$Line=="PK10",12.29111819,ifelse(mydata$Line=="Mindin", 53.60547457,
                                                                ifelse(mydata$Line=="Kintampo", 37.22657519,
                                                                       ifelse(mydata$Line=="Kedougou",12.24971172,
                                                                              ifelse(mydata$Line=="Ngoye",232.6737489,
                                                                                     ifelse(mydata$Line=="Thies",421.1161584, 2264.553235))))))


mydata$bio1 <- ifelse(mydata$Line == "Ngoye", 27.23134388,
                      ifelse(mydata$Line == "Thies", 26.34446173,
                             ifelse(mydata$Line == "Mindin", 27.4308689,
                                    ifelse(mydata$Line == "Kedougou", 28.31359838,
                                           ifelse(mydata$Line == "PK10", 28.32982187,
                                                  ifelse(mydata$Line == "Kumasi", 25.82099722, 26.67891258)))))) 


mydata$bio8 <- ifelse(mydata$Line == "Ngoye", 28.16551315,
                      ifelse(mydata$Line == "Thies", 27.73808739,
                             ifelse(mydata$Line == "Mindin", 27.34876508,
                                    ifelse(mydata$Line == "Kedougou", 26.66543166,
                                           ifelse(mydata$Line == "PK10", 26.6977011,
                                                  ifelse(mydata$Line == "Kumasi", 26.31027781, 25.93740981)))))) 

mydata$bio9 <- ifelse(mydata$Line == "Ngoye", 26.44713228,
                      ifelse(mydata$Line == "Thies", 25.41942082,
                             ifelse(mydata$Line == "Mindin", 27.84519131,
                                    ifelse(mydata$Line == "Kedougou", 27.65728965,
                                           ifelse(mydata$Line == "PK10", 28.05133339,
                                                  ifelse(mydata$Line == "Kumasi", 26.31272221, 27.5328634)))))) 


mydata$bio10 <- ifelse(mydata$Line == "Ngoye", 28.71277243,
                       ifelse(mydata$Line == "Thies", 27.85747534,
                              ifelse(mydata$Line == "Mindin", 29.94342078,
                                     ifelse(mydata$Line == "Kedougou", 32.02756295,
                                            ifelse(mydata$Line == "PK10", 32.0382415,
                                                   ifelse(mydata$Line == "Kumasi", 27.15455554, 28.62548641)))))) 


mydata$bio11 <- ifelse(mydata$Line == "Ngoye", 24.75124868,
                       ifelse(mydata$Line == "Thies", 24.0032459,
                              ifelse(mydata$Line == "Mindin", 24.82581424,
                                     ifelse(mydata$Line == "Kedougou", 26.117388,
                                            ifelse(mydata$Line == "PK10", 26.11555175,
                                                   ifelse(mydata$Line == "Kumasi", 24.36176669, 24.92969395)))))) 



mydata$bio15 <- ifelse(mydata$Line == "Ngoye", 157.7607664,
                       ifelse(mydata$Line == "Thies", 159.7975303,
                              ifelse(mydata$Line == "Mindin", 140.4319175,
                                     ifelse(mydata$Line == "Kedougou", 124.4395322,
                                            ifelse(mydata$Line == "PK10", 124.9359484,
                                                   ifelse(mydata$Line == "Kumasi", 53.98502001, 65.8939298)))))) 


mydata$bio16 <- ifelse(mydata$Line == "Ngoye", 442.0952381,
                       ifelse(mydata$Line == "Thies", 416.5409836,
                              ifelse(mydata$Line == "Mindin", 494.3442623,
                                     ifelse(mydata$Line == "Kedougou", 810.3442623,
                                            ifelse(mydata$Line == "PK10", 803.5517241,
                                                   ifelse(mydata$Line == "Kumasi", 511.8833333, 444.2295082)))))) 

mydata$bio17 <- ifelse(mydata$Line == "Ngoye", 1.206349206,
                       ifelse(mydata$Line == "Thies", 0.786885246,
                              ifelse(mydata$Line == "Mindin", 0.081967213,
                                     ifelse(mydata$Line == "Kedougou", 2.016393443,
                                            ifelse(mydata$Line == "PK10", 2.224137931,
                                                   ifelse(mydata$Line == "Kumasi", 99.01666667, 35.50819672))))))


mydata$bio18 <- ifelse(mydata$Line == "Ngoye", 135.7142857,
                       ifelse(mydata$Line == "Thies", 325.0819672,
                              ifelse(mydata$Line == "Mindin", 59.95081967,
                                     ifelse(mydata$Line == "Kedougou", 54.75409836,
                                            ifelse(mydata$Line == "PK10", 52.5,
                                                   ifelse(mydata$Line == "Kumasi", 332.2833333, 224.4098361)))))) 

str(mydata)
dj<-mydata
str(dj)
dj$Habitat<-as.factor(dj$Habitat)
dj$Stress<-as.factor(dj$Stress)
#Let's look at the Slope data after transformation into 0-1 #scaling slope to 0-1
SlopeNew<-rescale(dj$Slope)
dj$SlopeNew<-SlopeNew
str(dj)

custom_colors <- c("Rural" = "#4292C6", "Urban" = "#EF3B2C")

###PREFERENCE

model_2hr_Preference <- glm(SlopeNew ~ Preference, data = subset(dj, Stress == "acute"))
model_6hr_Preference <- glm(SlopeNew ~ Preference, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$Preference[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$Preference[Stress == "chronic"])

g1 <- ggplot(dj, aes(x = Preference, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 3.5) +
  labs(x = "Preference", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g1


###Ancestry
model_2hr_Ancestry <- glm(SlopeNew ~ Ancestry, data = subset(dj, Stress == "acute"))
model_6hr_Ancestry <- glm(SlopeNew ~ Ancestry, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$Ancestry[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$Ancestry[Stress == "chronic"])

g2 <- ggplot(dj, aes(x = Ancestry, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 3.5) +
  labs(x = "Ancestry", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g2


##Weighted Sum of Preference & Ancestry

# Step 1: Run a regression model with both Preference and Ancestry to get coefficients
model_combined <- lm(SlopeNew ~ Preference + Ancestry, data = dj)
summary(model_combined)

# Extract coefficients
beta_preference <- coef(model_combined)["Preference"]
beta_ancestry <- coef(model_combined)["Ancestry"]

# Create a new index (HSS) as a weighted sum of ancestry and preference
dj$HSS_weighted <- beta_preference * dj$Preference + beta_ancestry * dj$Ancestry

# Fit separate models for "acute" and "chronic" stress
model_2hr_HSS_weighted <- glm(SlopeNew ~ HSS_weighted, data = subset(dj, Stress == "acute"))
model_6hr_HSS_weighted <- glm(SlopeNew ~ HSS_weighted, data = subset(dj, Stress == "chronic"))

# Create the plot
ggplot(dj, aes(x = HSS_weighted, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +  # Acute points
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +  # Chronic points
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1),
              color = "black", linetype = "dashed", size = 1) +  # Acute smooth line
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1),
              color = "black", linetype = "dashed", size = 3.5) +  # Chronic smooth line
  labs(x = "Weighted Sum of Preference & Ancestry", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")


###Density
model_2hr_Density <- glm(SlopeNew ~ Density, data = subset(dj, Stress == "acute"))
model_6hr_Density <- glm(SlopeNew ~ Density, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$Density[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$Density[Stress == "chronic"])

g3 <- ggplot(dj, aes(x = log(Density), y = SlopeNew, color = Habitat)) + 
  geom_point(data = subset(dj, Stress == "acute"), size = 3) + 
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) + 
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 1) + 
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 3.5) + 
  labs(x = "Log-density", y = "Egg thermotolerance") + 
  theme_classic() + 
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) + 
  scale_color_manual(values = custom_colors) + 
  theme(legend.position = "none")

g3

#HSS_ Human Specialization Syndrome
Model15 <- glm(SlopeNew ~ Ancestry + Preference + Density+ Ancestry * Preference * Density, data = dj)
summary(Model15)

# Create a data frame with observed, predicted values, and Stress
predictions <- data.frame(
  Observed = dj$SlopeNew,
  Predicted = predict(Model15),
  Stress = as.factor(dj$Stress),
  Habitat = as.factor(dj$Habitat)
)

HSS <- ggplot(predictions, aes(x = Observed, y = Predicted, color = Habitat)) +
  geom_point(data = subset(predictions, Stress == "acute"), size = 3) +
  geom_point(data = subset(predictions, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(predictions, Stress == "acute"), method = "lm", se = TRUE, 
              aes(group = 1), color = "black", linetype = "dashed", size = 1) +
  geom_smooth(data = subset(predictions, Stress == "chronic"), method = "lm", se = TRUE, 
              aes(group = 1), color = "black", linetype = "dashed", size = 3.5) +
  labs(x = "Human Specialization Syndrome", y = "Egg thermotolerance") +
  theme_classic() +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

HSS



str(dj)
summary(model_bio1<-lm(Slope ~ bio1, data = dj))
model_2hr_bio1 <- glm(SlopeNew ~ bio1, data = subset(dj, Stress == "acute"))
model_6hr_bio1 <- glm(SlopeNew ~ bio1, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio1[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio1[Stress == "chronic"])

g_bio1<- ggplot(dj, aes(x = bio1, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio1", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio1

model_2hr_bio8 <- glm(SlopeNew ~ bio8, data = subset(dj, Stress == "acute"))
model_6hr_bio8 <- glm(SlopeNew ~ bio8, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio8[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio8[Stress == "chronic"])

g_bio8<- ggplot(dj, aes(x = bio8, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio8", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio8



model_2hr_bio9 <- glm(SlopeNew ~ bio9, data = subset(dj, Stress == "acute"))
model_6hr_bio9 <- glm(SlopeNew ~ bio9, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio9[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio9[Stress == "chronic"])

g_bio9<- ggplot(dj, aes(x = bio9, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio9", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio9

model_2hr_bio10 <- glm(SlopeNew ~ bio10, data = subset(dj, Stress == "acute"))
model_6hr_bio10 <- glm(SlopeNew ~ bio10, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio10[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio10[Stress == "chronic"])

g_bio10<- ggplot(dj, aes(x = bio10, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio10", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio10

model_2hr_bio11 <- glm(SlopeNew ~ bio11, data = subset(dj, Stress == "acute"))
model_6hr_bio11 <- glm(SlopeNew ~ bio11, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio11[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio11[Stress == "chronic"])

g_bio11<- ggplot(dj, aes(x = bio11, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio11", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio11

model_2hr_bio15 <- glm(SlopeNew ~ bio15, data = subset(dj, Stress == "acute"))
model_6hr_bio15 <- glm(SlopeNew ~ bio15, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio15[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio15[Stress == "chronic"])

g_bio15<- ggplot(dj, aes(x = bio15, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio15", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio15


model_2hr_bio16 <- glm(SlopeNew ~ bio16, data = subset(dj, Stress == "acute"))
model_6hr_bio16 <- glm(SlopeNew ~ bio16, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio16[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio16[Stress == "chronic"])

g_bio16<- ggplot(dj, aes(x = bio16, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio16", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio16


model_2hr_bio17 <- glm(SlopeNew ~ bio17, data = subset(dj, Stress == "acute"))
model_6hr_bio17 <- glm(SlopeNew ~ bio17, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio17[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio17[Stress == "chronic"])

g_bio17<- ggplot(dj, aes(x = bio17, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio17", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio17


model_2hr_bio18 <- glm(SlopeNew ~ bio18, data = subset(dj, Stress == "acute"))
model_6hr_bio18 <- glm(SlopeNew ~ bio18, data = subset(dj, Stress == "chronic"))
cor(dj$SlopeNew[Stress == "acute"], dj$bio18[Stress == "acute"])
cor(dj$SlopeNew[Stress == "chronic"], dj$bio18[Stress == "chronic"])

g_bio18<- ggplot(dj, aes(x = bio18, y = SlopeNew, color = Habitat)) +
  geom_point(data = subset(dj, Stress == "acute"), size = 3) +
  geom_point(data = subset(dj, Stress == "chronic"), size = 6) +
  geom_smooth(data = subset(dj, Stress == "acute"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 1) +
  geom_smooth(data = subset(dj, Stress == "chronic"), method = lm, se = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 3.5) +
  labs(x = "bio18", y = "Egg thermotolerance") +
  theme_classic() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "none")

g_bio18

# List of ggplot objectss
plots <- list(g_bio1, g_bio8, g_bio9, g_bio10, g_bio11, g_bio15, g_bio16, g_bio17, g_bio18)

# List of names for the pdf files
plot_names <- c("g_bio1", "g_bio8", "g_bio9", "g_bio10", "g_bio11", "g_bio15", "g_bio16", "g_bio17", "g_bio18")

# Loop through each plot and save it as a PDF with 3x3 inch size
for (i in 1:length(plots)) {
  ggsave(paste0(plot_names[i], ".pdf"), plot = plots[[i]], width = 8, height = 6, units = "in")
}
getwd()
