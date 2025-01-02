attach(Larvae_GAI)

hgf<-Larvae_GAI

str(hgf)
unique(hgf$Line)
unique(gh$Temperature)
unique(gh$Hour)
unique(gh$Recovery)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)


# Calculate summary statistics: mean and standard error
summary_data <- hgf %>%
  group_by(Temp, Hr, Recovery) %>%
  summarise(
    mean_proportion = mean(Survival, na.rm = TRUE),
    se_proportion = sd(Survival, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Define custom colors
colors <- c("2" = "#bf812c", "6" = "#969696") # 2 hr = gold, 6 hr = gray
View(summary_data)
str(summary_data)
# Create the plot

ggplot(
  data = summary_data,
  aes(x = factor(Temp), y = mean_proportion, fill = factor(Hr))
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
    position = position_dodge(width = 0.9), width = 0.2
  ) +
  facet_wrap(~ Recovery, ncol = 2, labeller = labeller(Recovery = c(early = "Early Recovery", late = "Late Recovery"))) +
  scale_fill_manual(values = colors, name = "Stress Duration (hr)") +
  labs(x = "Temperature (°C)", y = "Proportion Survival") +
  theme_classic()




