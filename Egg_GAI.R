attach(Egg_GAI)
gh<-Egg_GAI

str(gh)
unique(gh$Temperature)
unique(gh$Stress)
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Calculate summary statistics: mean and standard error
summary_data <- gh %>%
  group_by(Temperature, Stress) %>%
  summarise(
    mean_proportion = mean(Proportion, na.rm = TRUE),
    se_proportion = sd(Proportion, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()
View(summary_data)

# Define custom colors
colors <- c("2" = "#bf812c", "6" = "#969696") # 2 hr = gold, 6 hr = gray

ggplot(
  data = summary_data,
  aes(x = factor(Temperature), y = mean_proportion, fill = factor(Stress))
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
    position = position_dodge(width = 0.9), width = 0.2
  ) +
  scale_fill_manual(values = colors, name = "Stress Duration (hr)") +
  labs(x = "Temperature (°C)", y = "Proportion Survival") +
  theme_classic()



