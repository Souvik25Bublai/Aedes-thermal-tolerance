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

attach(Adult_Africa)
Adult<-Adult_Africa
str(Adult)

#Calculate LINE average
Merge_Average_Line <- Adult %>%
  group_by(Line, Sex, Temperature, Hour, Recovery) %>%
  summarize(average_proportion_Line = mean(Proportion, na.rm = TRUE), .groups = "drop")
View(Merge_Average_Line)

#Calculate combined average
Merge_Average <- Adult %>%
  group_by(Sex, Temperature, Hour, Recovery) %>%
  summarize(average_proportion = mean(Proportion, na.rm = TRUE), .groups = "drop")
View(Merge_Average)

# Add the Combined Average to the Line-Level Average Data
Merge_Average_Line <- Merge_Average_Line %>% 
  left_join(Merge_Average, by = c("Sex", "Temperature", "Hour", "Recovery"))

# View the updated data with combined average
View(Merge_Average_Line)
str(Merge_Average_Line)



# Add a new column with the desired calculation
Merge_Average_Line <- Merge_Average_Line %>%
  mutate(ratio_diff = (average_proportion_Line - average_proportion) / average_proportion)

# View the updated data with the new column
View(Merge_Average_Line)


# Create the new column 'habitat' based on the value of 'Line'
Merge_Average_Line <- Merge_Average_Line %>%
  mutate(Habitat = ifelse(Line %in% c("PK10", "Mindin", "Kedougou", "Kintampo"), "Rural", "Urban"))

View(Merge_Average_Line)

# Define color mapping for Rural and Urban areas
custom_colors <- c(
  "Ngoye" = "#FB6A4A",
  "Thies" = "#CB181D",
  "Kumasi" = "#67000D",   # Reds for Urban
  "Kintampo" = "#C6DBEF",
  "Kedougou" = "#6BAED6",
  "Mindin" = "#2171B5",
  "PK10" = "#08306B"      # Blues for Rural
)




# Filter for male and female data
Male_data <- Merge_Average_Line %>% filter(Sex == "m")
Female_data <- Merge_Average_Line %>% filter(Sex == "f")

# Create the ggplot for Male data
dev_male_plot <- ggplot(Male_data, aes(x = as.factor(Temperature), y = ratio_diff, color = Line, size = factor(Recovery), fill = Line)) +
  geom_point(position = position_jitter(width = 0.3), shape = 21, alpha = 0.55) +  # Use shape 21 for filled points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Horizontal dashed line at y = 0
  labs(x = "Temperature (°C)", y = "Deviation from combined average", size = "Recovery") +  # Axis and size labels
  scale_color_manual(values = custom_colors) +  # Custom colors for each Line (Habitat)
  scale_fill_manual(values = custom_colors) +  # Custom fill colors for each Line (Habitat)
  theme_classic() +  # Classic theme
  facet_wrap(~Hour) +  # Facet by Hour (Hour 2 and Hour 6)
  ylim(-0.15, 0.20)  # Y-axis limits

# Print the Male plot
dev_male_plot

# Create the ggplot for Female data
dev_female_plot <- ggplot(Female_data, aes(x = as.factor(Temperature), y = ratio_diff, color = Line, size = factor(Recovery), fill = Line)) +
  geom_point(position = position_jitter(width = 0.3), shape = 21, alpha = 0.55) +  # Use shape 21 for filled points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Horizontal dashed line at y = 0
  labs(x = "Temperature (°C)", y = "Deviation from combined average", size = "Recovery") +  # Axis and size labels
  scale_color_manual(values = custom_colors) +  # Custom colors for each Line (Habitat)
  scale_fill_manual(values = custom_colors) +  # Custom fill colors for each Line (Habitat)
  theme_classic() +  # Classic theme
  facet_wrap(~Hour) +  # Facet by Hour (Hour 2 and Hour 6)
  ylim(-0.15, 0.20)  # Y-axis limits

# Print the Female plot
dev_female_plot


# Factorize 'Line' for ordered plotting
Merge_Average_Line$Line <- factor(Merge_Average_Line$Line, levels = c("Kumasi", "Thies", "Ngoye", "Kintampo", "Mindin", "Kedougou", "PK10"))

# Filter for Male and Female subsets
male_data <- Merge_Average_Line %>% filter(Sex == "m")
female_data <- Merge_Average_Line %>% filter(Sex == "f")

# Create Heatmap for Male, Hour 2, Early Recovery
heatmap_plot_male_2h_early <- ggplot(male_data %>% filter(Hour == 2, Recovery == "early"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Male - Hour 2 - Early Recovery")

# Create Heatmap for Male, Hour 2, Late Recovery
heatmap_plot_male_2h_late <- ggplot(male_data %>% filter(Hour == 2, Recovery == "late"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Male - Hour 2 - Late Recovery")

# Create Heatmap for Male, Hour 6, Early Recovery
heatmap_plot_male_6h_early <- ggplot(male_data %>% filter(Hour == 6, Recovery == "early"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Male - Hour 6 - Early Recovery")

# Create Heatmap for Male, Hour 6, Late Recovery
heatmap_plot_male_6h_late <- ggplot(male_data %>% filter(Hour == 6, Recovery == "late"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Male - Hour 6 - Late Recovery")

# Create Heatmap for Female, Hour 2, Early Recovery
heatmap_plot_female_2h_early <- ggplot(female_data %>% filter(Hour == 2, Recovery == "early"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Female - Hour 2 - Early Recovery")

# Create Heatmap for Female, Hour 2, Late Recovery
heatmap_plot_female_2h_late <- ggplot(female_data %>% filter(Hour == 2, Recovery == "late"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Female - Hour 2 - Late Recovery")

# Create Heatmap for Female, Hour 6, Early Recovery
heatmap_plot_female_6h_early <- ggplot(female_data %>% filter(Hour == 6, Recovery == "early"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Female - Hour 6 - Early Recovery")

# Create Heatmap for Female, Hour 6, Late Recovery
heatmap_plot_female_6h_late <- ggplot(female_data %>% filter(Hour == 6, Recovery == "late"), aes(x = as.factor(Temperature), y = Line, fill = ratio_diff)) +
  geom_tile(color = "white", width = 1, height = 1) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank()) +
  ggtitle("Female - Hour 6 - Late Recovery")

# Arrange all 8 plots in a 4x2 grid
grid.arrange(heatmap_plot_male_2h_early, heatmap_plot_male_2h_late, 
             heatmap_plot_male_6h_early, heatmap_plot_male_6h_late,
             heatmap_plot_female_2h_early, heatmap_plot_female_2h_late, 
             heatmap_plot_female_6h_early, heatmap_plot_female_6h_late, 
             ncol = 2)

