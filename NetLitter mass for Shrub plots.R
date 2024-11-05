# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Import the data
data <- read_excel("C:/Users/Allelua Niyokwizera/Desktop/ASU Fall research/NEW DATA/Litter manipulation/Litter mass.xlsx")

# Display the first few rows to understand the structure
print(head(data))

# Perform basic statistical analysis
# Summary statistics by treatment
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(`Net litter Mass (g)`, na.rm = TRUE),
    SD = sd(`Net litter Mass (g)`, na.rm = TRUE),
    Min = min(`Net litter Mass (g)`, na.rm = TRUE),
    Max = max(`Net litter Mass (g)`, na.rm = TRUE)
  )

# Print summary statistics to console
print(summary_stats)

# Create a line graph
plot <- ggplot(data, aes(x = Year, y = `Net litter Mass (g)`, color = Treatment)) +
  geom_line(aes(color = Treatment), linewidth = 1.2) +  # Set default linewidth
  geom_point(size = 2) +      # Point size
  scale_color_manual(values = c("Ctrl" = "orange", 
                                "0X" = "darkred", 
                                "2X" = "slateblue4")) + # Change colors as specified
  labs(
    title = "Litter Manipulation in Shrub Plots",
    x = "Time",
    y =  "Change in Total Litter Mass (added minus collected, g)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15) +  # Minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    plot.title = element_text(hjust = 0.5), # Center the title
    axis.title.x = element_text(face = "bold"), # Bold x-axis title
    axis.title.y = element_text(face = "bold"), # Bold y-axis title
    legend.position = "right" # Adjust legend position
  ) +
  scale_x_continuous(breaks = 2013:2024) # Set x-axis breaks to whole years

# Save the plot
ggsave("C:/Users/Allelua Niyokwizera/Desktop/ASU Fall research/NEW DATA/Litter manipulation/litter_manipulation_plot.png", plot = plot, width = 10, height = 6)

# Save summary statistics as a CSV file
write.csv(summary_stats, "C:/Users/Allelua Niyokwizera/Desktop/ASU Fall research/NEW DATA/Litter manipulation/summary_statistics.csv", row.names = FALSE)
