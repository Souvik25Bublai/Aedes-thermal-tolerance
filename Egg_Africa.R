
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


attach(Egg_Africa)
Egg<-Egg_Africa
str(Egg)

# Step 1: Calculate Line-Level Average
Merge_Average_Line_Egg <- Egg %>%
  group_by(Line, Temperature, Stress) %>%
  summarize(average_Proportion_Line = mean(Proportion, na.rm = TRUE), .groups = "drop")

# View the Line-Level Average Data
View(Merge_Average_Line_Egg)

# Step 2: Calculate Combined Average
Merge_Average_Egg <- Egg %>%
  group_by(Temperature, Stress) %>%
  summarize(average_Proportion = mean(Proportion, na.rm = TRUE), .groups = "drop")

# View the Combined Average Data
View(Merge_Average_Egg)

# Step 3: Add the Combined Average to the Line-Level Average Data
Merge_Average_Line_Egg <- Merge_Average_Line_Egg %>%
  left_join(Merge_Average_Egg, by = c("Temperature", "Stress"))

# View the Updated Data with Combined Average
View(Merge_Average_Line_Egg)

# Step 4: Calculate the Ratio Difference
Merge_Average_Line_Egg <- Merge_Average_Line_Egg %>%
  mutate(ratio_diff = (average_Proportion_Line - average_Proportion) / average_Proportion)

# View the Updated Data with the Ratio Difference Column
View(Merge_Average_Line_Egg)

# Step 5: Create a New Column 'Habitat' Based on 'Line'
Merge_Average_Line_Egg <- Merge_Average_Line_Egg %>%
  mutate(Habitat = ifelse(Line %in% c("PK10", "Mindin", "Kedougou", "Kintampo"), "Rural", "Urban"))

# View the Data with the Habitat Column
View(Merge_Average_Line_Egg)

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
View(Merge_Average_Line_Egg)
str(Merge_Average_Line_Egg)

ggplot(
  Merge_Average_Line_Egg,
  aes(x = as.factor(Temperature), y = ratio_diff, color = Line, fill = Line, size = as.factor(Stress))
) +
  geom_point(position = position_jitter(width = 0.3), shape = 21, alpha = 0.55) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Temperature (°C)", y = "Deviation from Combined Average", size = "Stress") +
  scale_size_manual(values = c("2" = 3, "6" = 6)) + # Assign specific sizes
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  ylim(-2, 2) +
  ggtitle( "Dot Plot")



