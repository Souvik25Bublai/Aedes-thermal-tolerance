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

attach(Larvae_Africa)
Larvae<-Larvae_Africa
str(Larvae)

# Step 1: Calculate Line-Level Average
Merge_Average_Line_Larvae <- Larvae %>%
  group_by(Line, Temperature, Hour, Recovery) %>%
  summarize(average_fraction_Line = mean(fraction, na.rm = TRUE), .groups = "drop")

# View the Line-Level Average Data
View(Merge_Average_Line_Larvae)

# Step 2: Calculate Combined Average
Merge_Average_Larvae <- Larvae %>%
  group_by(Temperature, Hour, Recovery) %>%
  summarize(average_fraction = mean(fraction, na.rm = TRUE), .groups = "drop")

# View the Combined Average Data
View(Merge_Average_Larvae)

# Step 3: Add the Combined Average to the Line-Level Average Data
Merge_Average_Line_Larvae <- Merge_Average_Line_Larvae %>%
  left_join(Merge_Average_Larvae, by = c("Temperature", "Hour", "Recovery"))

# View the Updated Data with Combined Average
View(Merge_Average_Line_Larvae)

# Step 4: Calculate the Ratio Difference
Merge_Average_Line_Larvae <- Merge_Average_Line_Larvae %>%
  mutate(ratio_diff = (average_fraction_Line - average_fraction) / average_fraction)

# View the Updated Data with the Ratio Difference Column
View(Merge_Average_Line_Larvae)

# Step 5: Create a New Column 'Habitat' Based on 'Line'
Merge_Average_Line_Larvae <- Merge_Average_Line_Larvae %>%
  mutate(Habitat = ifelse(Line %in% c("PK10", "Mindin", "Kedougou", "Kintampo"), "Rural", "Urban"))

# View the Data with the Habitat Column
View(Merge_Average_Line_Larvae)

# Step 6: Define Custom Colors for Rural and Urban Areas
custom_colors <- c(
  "Ngoye" = "#FB6A4A",
  "Thies" = "#CB181D",
  "Kumasi" = "#67000D",   # Reds for Urban
  "Kintampo" = "#C6DBEF",
  "Kedougou" = "#6BAED6",
  "Mindin" = "#2171B5",
  "PK10" = "#08306B"      # Blues for Rural
)


# Final View of Data with All Calculations
View(Merge_Average_Line_Larvae)
str(Merge_Average_Line_Larvae)


dot_plot_early <- ggplot(
  Merge_Average_Line_Larvae %>% filter(Recovery == "early"),
  aes(x = as.factor(Temperature), y = ratio_diff, color = Line, fill = Line, size = as.factor(Hour))
) +
  geom_point(position = position_jitter(width = 0.3), shape = 21, alpha = 0.55) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Temperature (°C)", y = "Deviation from Combined Average", size = "Hour") +
  scale_size_manual(values = c("2" = 3, "6" = 6)) + # Assign specific sizes
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  ylim(-1.5, 1.5) +
  ggtitle("Dot Plot - Early Recovery")
dot_plot_early

dot_plot_late <- ggplot(
  Merge_Average_Line_Larvae %>% filter(Recovery == "late"),
  aes(x = as.factor(Temperature), y = ratio_diff, color = Line, fill = Line, size = as.factor(Hour))
) +
  geom_point(position = position_jitter(width = 0.3), shape = 21, alpha = 0.55) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Temperature (°C)", y = "Deviation from Combined Average", size = "Hour") +
  scale_size_manual(values = c("2" = 3, "6" = 6)) + # Assign specific sizes
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  ylim(-1.5, 1.5) +
  ggtitle("Dot Plot - Late Recovery")
dot_plot_late

# Arrange both plots side by side
grid.arrange(dot_plot_early, dot_plot_late, ncol = 2)



heatmap_early_2hr <- ggplot(Merge_Average_Line_Larvae %>% filter(Recovery == "early", Hour == 2),
                            aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()) +
  ggtitle("Early Recovery - 2 Hour Stress")

heatmap_early_6hr <- ggplot(Merge_Average_Line_Larvae %>% filter(Recovery == "early", Hour == 6),
                            aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()) +
  ggtitle("Early Recovery - 6 Hour Stress")

heatmap_late_2hr <- ggplot(Merge_Average_Line_Larvae %>% filter(Recovery == "late", Hour == 2),
                           aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()) +
  ggtitle("Late Recovery - 2 Hour Stress")
heatmap_late_6hr <- ggplot(Merge_Average_Line_Larvae %>% filter(Recovery == "late", Hour == 6),
                           aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()) +
  ggtitle("Late Recovery - 6 Hour Stress")

grid.arrange(heatmap_early_2hr, heatmap_early_6hr, heatmap_late_2hr, heatmap_late_6hr, ncol = 2)
