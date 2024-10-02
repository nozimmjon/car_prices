
theme_set(theme_classic(     # always start with a base theme_****
  base_size = 16,             # 16-point font (adjusted for axes)
  base_family = "Roboto Slab"      # custom font family
)
)

theme_update(
  panel.background = element_rect(fill = "#edf3f6"),
  plot.background = element_rect(fill = "#edf3f6"),
  plot.margin = margin(.8, 1, .7, .8, "cm"),
  plot.title = element_text(
    size = 20,
    face = "bold",
    margin = margin(b = .25, unit = "cm")
  ),
  plot.title.position = "plot",
  plot.subtitle = element_text(
    face = "italic"
  ),
  axis.title.x = element_text(
    margin = margin(t = .5, unit = "cm")
  ),
  plot.caption = element_text(
    face = "italic",
    size = 14, 
    margin = margin(t = 0.5, unit = "cm")
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
  geom_line(size = 1.5, color = "#7d3a46") +
  geom_point(fill = "#7d3a46",
             size = 2, 
             pch = 21, # Type of point that allows us to have both color (border) and fill.
             color = "white", 
             stroke = 1 # The width of the border, i.e. stroke. 
               )+
  labs(title = "Маҳаллий автомобиллар нарх индекси",
       subtitle = "Иккиламчи бозорда Chevrole автомобиллари нархларининг ўзгариши",
       caption = "Иқтисодий тадқиқотлар ва ислоҳотлар маркази томонидан очиқ сайтларда берилган эълонлар асосида ҳисобланган",
       x = NULL,
       y = NULL
       )+
  scale_x_date(
    date_breaks = "1 months",
    # date_labels = "%b '%y", 
    labels = scales::label_date_short(),
    limits = as.Date(c("2023-01-01", "2024-09-01")),
    expand = expansion(mult=0.01)
    ) +
  scale_y_continuous(
    breaks = seq(0, 110, by=10),
    limits = c(70, 110),
    expand = expansion(mult=0.01)
                     )


----------
  
theme_set(theme_minimal(     # always start with a base theme_****
  base_size = 16,             # 16-point font (adjusted for axes)
  base_family = "Roboto Slab"      # custom font family
)
)

theme_update(
  panel.background = element_rect(fill = "#edf3f6", color = "#edf3f6"), 
  plot.background = element_rect(fill = "#edf3f6", color = "#edf3f6"),
  plot.margin = margin(.8, 1, .7, .8, "cm"),
  plot.title = element_text(
    size = 20,
    face = "bold",
    margin = margin(b = .25, unit = "cm")
  ),
  plot.title.position = "plot",
  plot.subtitle = element_text(
    face = "italic"
  ),
  axis.title.x = element_text(
    margin = margin(t = .5, unit = "cm")
  ),
  plot.caption = element_text(
    face = "italic",
    size = 14, 
    margin = margin(t = 0.5, unit = "cm")
  )
  )

# Improved growth rate chart with better label visibility
monthly_growth <- ggplot(overall_index, 
                         aes(x = date, y = growth_rate, fill = growth_rate >= 0,
                             )
                         ) +
  geom_col() +
  geom_bar_text(aes(label = round(growth_rate, 1)
                    ),
                contrast = TRUE
                )+
  scale_fill_manual(values = c("TRUE" = "skyblue", "FALSE" = "lightcoral")) +
  theme(legend.position = "none") +
  labs(title = "Overall Chevrolet Car Price Monthly Growth Rate",
       x = NULL,
       y = NULL) +
  scale_x_date(
    date_breaks = "1 months",
    # date_labels = "%b '%y", 
    labels = scales::label_date_short(),
    #limits = as.Date(c("2023-01-01", "2024-09-01")),
    expand = expansion(mult=0.01)
    ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(min(overall_index$growth_rate) - 1, max(overall_index$growth_rate) + 1))


overall_chart <- overall_growth + monthly_growth 

# Save the overall chart
ggsave(
  filename = "chevrolet_overall_charts.png",
  plot = overall_chart,
  width = 10,
  height = 12,
  units = "in",
  dpi = 300
)
