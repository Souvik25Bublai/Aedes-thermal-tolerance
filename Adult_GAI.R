attach(Adult_GAI)

gh<-Adult_GAI

str(gh)
unique(gh$Sex)
unique(gh$Temperature)
unique(gh$Hour)
unique(gh$Recovery)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)


# Calculate summary statistics: mean and standard error
summary_data <- gh %>%
  group_by(Sex, Recovery, Temperature, Hour) %>%
  summarise(
    mean_proportion = mean(Proportion, na.rm = TRUE),
    se_proportion = sd(Proportion, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Define custom colors
colors <- c("2" = "#bf812c", "6" = "#969696") # 2 hr = gold, 6 hr = gray

# Create the plot for males
plot_male <- ggplot(
  data = summary_data %>% filter(Sex == "m"),
  aes(x = factor(Temperature), y = mean_proportion, fill = factor(Hour))
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
    position = position_dodge(width = 0.9), width = 0.2
  ) +
  facet_wrap(~ Recovery, ncol = 2, labeller = labeller(Recovery = c(early = "Early Recovery", late = "Late Recovery"))) +
  scale_fill_manual(values = colors, name = "Stress Duration (hr)") +
  labs(title = "(b)", x = "Temperature (°C)", y = "Proportion Survival") +
  theme_cleveland()

# Create the plot for females
plot_female <- ggplot(
  data = summary_data %>% filter(Sex == "f"),
  aes(x = factor(Temperature), y = mean_proportion, fill = factor(Hour))
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
    position = position_dodge(width = 0.9), width = 0.2
  ) +
  facet_wrap(~ Recovery, ncol = 2, labeller = labeller(Recovery = c(early = "Early Recovery", late = "Late Recovery"))) +
  scale_fill_manual(values = colors, name = "Stress Duration (hr)") +
  labs(title = "(a)", x = "Temperature (°C)", y = "Proportion Survival") +
  theme_cleveland()


final_plot <- plot_female + plot_male + plot_layout(ncol = 2)
print(final_plot)
