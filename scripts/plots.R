
theme_set(theme_minimal(     # always start with a base theme_****
  base_size = 16,             # 16-point font (adjusted for axes)
  base_family = "Roboto Slab"      # custom font family
)
)

theme_update(
  plot.margin = margin(.8, 1, .7, .8, "cm"),
  plot.title = element_text(
    size = 20,
    margin = margin(b = .5, unit = "cm")
  ),
  plot.title.position = "plot",
  axis.title.x = element_text(
    margin = margin(t = .5, unit = "cm")
  )
)



# Calculate overall price change
first_index <- overall_index$overall_index[1]
last_index <- overall_index$overall_index[nrow(overall_index)]
overall_change <- (last_index / first_index - 1) * 100


# Calculate growth rates
overall_index <- overall_index %>%
  mutate(growth_rate = (overall_index / lag(overall_index) - 1) * 100)

# Plot overall index with overall price change
overall_growth <- ggplot(overall_index, aes(x = date, y = overall_index)) +
  geom_line(size = 1.5) +
  #theme_minimal() +
  labs(title = "Overall Chevrolet Car Price Index",
       subtitle = sprintf("Total price change: %.1f%%", overall_change),
       x = "Date",
       y = "Index Value (Base = 100 for average of first 12 months)")

# Improved growth rate chart with better label visibility
monthly_growth <- ggplot(overall_index, aes(x = date, y = growth_rate, fill = growth_rate >= 0)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", round(growth_rate, 1)), 
                y = ifelse(growth_rate >= 0, growth_rate + 0.1, growth_rate - 0.1)),
            vjust = ifelse(overall_index$growth_rate >= 0, 0, 1),
            color = "black",
            size = 3) +
  scale_fill_manual(values = c("TRUE" = "skyblue", "FALSE" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Overall Chevrolet Car Price Monthly Growth Rate",
       x = NULL,
       y = "Growth Rate (%)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(min(overall_index$growth_rate) - 1, max(overall_index$growth_rate) + 1))


overall_chart <- overall_growth / monthly_growth +
  plot_layout(heights = c(1, 1.2))

# Save the overall chart
ggsave(
  filename = "chevrolet_overall_charts.png",
  plot = overall_chart,
  width = 10,
  height = 12,
  units = "in",
  dpi = 300
)
