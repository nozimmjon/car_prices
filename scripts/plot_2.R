
# Function to create both charts for a single model
create_model_charts <- function(model_data, model_name) {
  # Calculate overall price change
  first_index <- model_data$index[1]
  last_index <- model_data$index[nrow(model_data)]
  overall_change <- (last_index / first_index - 1) * 100
  
  # Calculate growth rates
  model_data <- model_data %>%
    mutate(growth_rate = (index / lag(index) - 1) * 100)
  
  # Price Index Chart
  index_chart <- ggplot(model_data, aes(x = date, y = index)) +
    geom_line() +
    theme_minimal() +
    labs(title = paste("Price Index -", model_name),
         subtitle = sprintf("Total price change: %.1f%%", overall_change),
         x = NULL,
         y = "Index Value") +
    theme(plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Growth Rate Chart
  growth_chart <- ggplot(model_data, aes(x = date, y = growth_rate, fill = growth_rate >= 0)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", round(growth_rate, 1)), 
                  y = ifelse(growth_rate >= 0, growth_rate + 0.1, growth_rate - 0.1)),
              vjust = ifelse(model_data$growth_rate >= 0, 0, 1),
              color = "black",
              size = 2) +
    scale_fill_manual(values = c("TRUE" = "skyblue", "FALSE" = "lightcoral")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10)) +
    labs(title = paste("Monthly Growth Rate -", model_name),
         x = NULL,
         y = "Growth Rate (%)") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    coord_cartesian(ylim = c(min(model_data$growth_rate, na.rm = TRUE) - 1, 
                             max(model_data$growth_rate, na.rm = TRUE) + 1))
  
  # Combine charts using patchwork
  combined_chart <- index_chart / growth_chart +
    plot_layout(heights = c(1, 1.2))
  
  return(combined_chart)
}

# Create and save charts for each model
for (model_name in names(model_indices)) {
  model_data <- model_indices[[model_name]]
  combined_chart <- create_model_charts(model_data, model_name)
  
  # Save the combined chart
  ggsave(
    filename = paste0("chevrolet_", gsub(" ", "_", tolower(model_name)), "_charts.png"),
    plot = combined_chart,
    width = 10,
    height = 12,
    units = "in",
    dpi = 300
  )
  
  print(paste("Charts for", model_name, "saved."))
}



library(tidyverse)
library(ggtext)

# Calculate overall price change for each model
model_price_changes <- map_dfr(names(model_indices), function(model_name) {
  model_data <- model_indices[[model_name]]
  first_index <- model_data$index[1]
  last_index <- model_data$index[nrow(model_data)]
  overall_change <- (last_index / first_index - 1) * 100
  tibble(model = model_name, price_change = overall_change)
})

# Calculate overall index change
overall_first_index <- overall_index$overall_index[1]
overall_last_index <- overall_index$overall_index[nrow(overall_index)]
overall_change <- (overall_last_index / overall_first_index - 1) * 100

# Sort models by price change
model_price_changes <- model_price_changes %>%
  arrange(desc(price_change)) %>%
  mutate(above_average = price_change > overall_change)

# Create improved diverging bar chart
improved_chart <- ggplot(model_price_changes, aes(x = reorder(model, price_change), y = price_change, fill = above_average)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", price_change), 
                hjust = ifelse(price_change >= 0, 1.1, -0.1)),
            size = 3.5, color = "black") +
  geom_hline(yintercept = overall_change, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = length(model_price_changes$model), y = overall_change, 
           label = sprintf("Overall: %.1f%%", overall_change), 
           hjust = 1, vjust = -0.5, color = "red", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#FFA726")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9),
    plot.title = element_markdown(size = 16, face = "bold"),
    plot.subtitle = element_markdown(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = "Price Changes by Chevrolet Model",
    subtitle = paste0("Compared to overall change of <span style='color:red;'>",
                      sprintf("%.1f%%", overall_change), "</span>"),
    x = NULL,
    y = "Price Change"
  )

# Save the improved chart
ggsave(
  filename = "chevrolet_model_price_changes_improved.png",
  plot = improved_chart,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
