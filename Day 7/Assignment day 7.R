# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
# Opening a PDF device. All plots will be saved to this file.
pdf("Final_Visualizations.pdf", width = 11, height = 8.5)

# Plot 1
df1 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot1.csv")

# 'myaxis' is mapped to the x-axis because it already contains the sample size labels
ggplot(df1, aes(x = myaxis, y = value, fill = name)) +
  # geom_violin is used to crate the density shape
geom_violin(color = "black", alpha = 1) +
  # 'outlier.shape = NA' is used to hide duplicate outliers, as the violin already represents the full range.
geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
  # Adding clear title and axis labels
labs(
    title = "Boxplot + Violin",
    x = "",
    y = "Value") +
  # theme_bw() provides a white background
theme_bw() +
theme(legend.position = "none") # Removing the legend

# Plot 2
df2 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot2.csv")

# Area (x), Population (y), and Population Density (size).
ggplot(df2, aes(x = area, y = poptotal, color = state)) +
  # Mapping 'size' to density to create the bubbles.
  # Alpha is set to 0.7 to allow overlapping points to remain visible.
  geom_point(aes(size = popdensity), alpha = 0.7) + # geom_smooth() without method="lm" to create a smoothed line
  geom_smooth(color = "royalblue", se = FALSE) +
  # Focusing on areas up to 0.1 and populations up to 500,000.
  xlim(0, 0.1) +
  ylim(0, 500000) + # Adding titles and clear axis labels
  labs(
    title = "Scatterplot",
    subtitle = "Area vs Population",
    x = "Area",
    y = "Population",
    size = "popdensity",
    color = "state"
  ) + # theme_bw() provides the distinct grid-line
  theme_bw()

# Plot 3
df3 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot3.csv")

ggplot(df3, aes(x = cty, y = hwy)) +
  # geom_count automatically calculates how many points overlap at each location.
  # Larger circles represent more data, making the density visible.
  geom_count(color = "tomato3", show.legend = FALSE) + # Adding clear axis labels
  labs(
    title = "Counts plot",
    subtitle = "mpg: city vs highway mileage",
    x = "cty",
    y = "hwy") +
 theme_minimal()

# Plot 4
df4 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot4.csv")

# Calculating Z-scores to normalize mileage relative to the dataset mean.
df4_norm <- df4 %>%
  mutate(mpg_z = (mpg - mean(mpg))/sd(mpg),
         type = ifelse(mpg_z > 0, "Above", "Below")) %>%
  arrange(mpg_z)

# Converting car_name to a factor with levels based on the sorted order.
# This ensures the bars are plotted in the order of the ranking
df4_norm$car_name <- factor(df4_norm$car_name, levels = df4_norm$car_name)

ggplot(df4_norm, aes(x = car_name, y = mpg_z, fill = type)) +
  geom_bar(stat = "identity", width = 0.9) +  # scale_fill_manual to apply distinct colors for 'Above' and 'Below' average.
  scale_fill_manual(values = c("Above" = "#00af60", "Below" = "#f8766d")) + # Flipping coordinates to allow space for car names on the Y axis.
  coord_flip() +
  labs(x = "Car Name", y = "MPG (Z-Score)") +
  theme_minimal()

# Plot 5
df5 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot5.csv")

# Initializing the ggplot, using 'reorder' to sort the x-axis by mileage.
# This ensures the visualization shows a clear ranking.
ggplot(df5, aes(x = reorder(manufacturer, mileage), y = mileage)) +
  # geom_segment creates the line of the lollipop.
  # I set xend to 'manufacturer' and y to 0 so the line starts at the baseline and ends at the data point.
geom_segment(aes(xend = manufacturer, y = 0, yend = mileage), color = "gray") +
  # geom_point creates the 'lollipop' head. I chose size 3 and orange
geom_point(size = 3, color = "orange") +
  # Adding labels
labs(
    title = "Lollipop Chart",
    subtitle = "Mean City Mileage by Manufacturer",
    x = "Manufacturer",
    y = "Mileage") +  # Using theme_light() to provide the background
theme_light() + # Rotating x-axis text by 45 degrees
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Plot 6
df6 <- read.csv("/Users/alkinoos.polychronopoulos/Library/CloudStorage/OneDrive-KarolinskaInstitutet(2)/Courses/Intermediate_R_course/Data_7/data_plot6.csv")

# Classification by defining significance based on p-value and log2FoldChange.
# The log2FoldChanges threshold is 0.5 as in the graph provided.
df6 <- df6 %>%
  mutate(diff_state = case_when(
    log2FoldChange > 0.5 & pvalue < 0.05 ~ "Up",
    log2FoldChange < -0.5 & pvalue < 0.05 ~ "Down",
    TRUE ~ "NS"
  )) %>%
  # Making a label column that only stores names for the significant genes as in the provided graph.
  mutate(gene_label = ifelse(diff_state != "NS", as.character(gene_symbol), ""))

# Plot Generation
ggplot(df6, aes(x = log2FoldChange, y = -log10(pvalue), color = diff_state)) +
  geom_point(alpha = 0.4, size = 1.5) +
  # Using geom_text_repel so that gene names do not overlap points or each other.
  geom_text_repel(
    aes(label = gene_label),
    size = 3,
    max.overlaps = Inf, # Here i will have all significant genes shown
    show.legend = FALSE
  ) +
  # Manual color scale to differentiate between Up-regulated (Red) and Down-regulated (Blue) genes.
  scale_color_manual(values = c("Down" = "blue", "NS" = "grey", "Up" = "red")) +
  labs(
    title = "Volcano Plot",
    subtitle = "Significant genes labeled (FC > 0.5, p < 0.05)", # I added this as extra to the example
    x = "log2 Fold Change",
    y = "-log10 P-Value"
  ) +
  theme_classic()
dev.off() #close device
