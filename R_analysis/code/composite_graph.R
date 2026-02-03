library(tidyverse)
library(lubridate)
library(patchwork) # Required for combining plots

# -------------------------
# 0) Setup & Data Loading
# -------------------------
data_path  <- "november_dataset_fixed_years.csv"

year_min   <- 2017
year_max   <- 2024
target_mon <- 11
who_ref    <- 8 

# City orders
city_order_heat <- c("York", "Sheffield", "Nottingham", "Leeds", "Manchester")
city_order_5    <- c("Sheffield", "Manchester", "Leeds", "Nottingham", "York")

# Custom Dark Theme for consistency across all plots
theme_black_custom <- function() {
  theme_minimal() +
    theme(
      plot.background   = element_rect(fill = "black", color = NA),
      panel.background  = element_rect(fill = "black", color = NA),
      legend.background = element_rect(fill = "black", color = NA),
      legend.key        = element_rect(fill = "black", color = NA),
      text              = element_text(color = "white"),
      axis.text         = element_text(color = "grey80"),
      axis.title        = element_text(color = "white"),
      plot.title        = element_text(face = "bold", size = 14), # Slightly smaller for composite
      plot.subtitle     = element_text(color = "grey85", size = 10),
      panel.grid        = element_blank(),
      strip.text        = element_text(face = "bold", color = "white"),
      legend.text       = element_text(color = "grey80")
    )
}

# Load Data
df <- read_csv(data_path, show_col_types = FALSE)

df2 <- df %>%
  mutate(
    datetime_utc = ymd_hms(datetime, tz = "UTC"),
    year_utc  = year(datetime_utc),
    month_utc = month(datetime_utc),
    hour_utc  = hour(datetime_utc)
  ) %>%
  filter(
    !is.na(datetime_utc),
    year_utc >= year_min, year_utc <= year_max,
    month_utc == target_mon,
    !is.na(hour_utc),
    !is.na(pm25)
  ) %>%
  mutate(
    period = if_else(hour_utc >= 19 | hour_utc <= 6, "Night", "Day"),
    period = factor(period, levels = c("Day", "Night"))
  )

night_df <- df2 %>% filter(period == "Night")

# -------------------------
# Plot A: Heatmap (Top Left)
# -------------------------
heat_df <- night_df %>%
  group_by(city, year = year_utc) %>%
  summarise(pm25_mean = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    label = round(pm25_mean, 1),
    city  = factor(city, levels = city_order_heat)
  ) %>%
  filter(!is.na(city))

p_heat <- ggplot(heat_df, aes(x = year, y = city, fill = pm25_mean)) +
  geom_tile(color = NA) +
  geom_text(aes(label = label), color = "white", size = 3.5) +
  scale_x_continuous(breaks = year_min:year_max) +
  scale_fill_gradient(low = "darkgreen", high = "red", name = "PM2.5") +
  labs(
    title = "Night PM2.5: UK Cities (2017–2024)",
    subtitle = "19:00–06:59 | WHO >8 µg/m³",
    x = "Year", y = "City"
  ) +
  theme_black_custom() +
  theme(legend.position = "right")

# -------------------------
# Plot B: Trend Lines (Top Right)
# -------------------------
cb_dark_5 <- c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#D55E00")

trend_df <- night_df %>%
  group_by(city, year = year_utc) %>%
  summarise(mean_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  filter(city %in% city_order_5) %>%
  mutate(city = factor(city, levels = city_order_5))

p_trend <- ggplot(trend_df, aes(x = year, y = mean_pm25, color = city, group = city)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = who_ref, linetype = "dashed", color = "grey80") +
  scale_x_continuous(breaks = year_min:year_max) +
  scale_color_manual(values = setNames(cb_dark_5, city_order_5)) +
  labs(
    title = "Night PM2.5 Trends: 5 Cities",
    subtitle = "Nov nights | Dashed=WHO 8 µg/m³",
    x = "Year", y = "Avg PM2.5 (µg/m³)", color = NULL
  ) +
  theme_black_custom() +
  theme(legend.position = "bottom")

# -------------------------
# Plot C: Day vs Night Bar (Bottom Left)
# -------------------------
summ_pm25 <- df2 %>%
  group_by(city, period) %>%
  summarise(
    pm25_median = median(pm25, na.rm = TRUE),
    .groups = "drop"
  )

# Order by highest night median
city_order_bar <- summ_pm25 %>%
  filter(period == "Night") %>%
  arrange(desc(pm25_median)) %>%
  pull(city)

summ_pm25 <- summ_pm25 %>%
  mutate(city = factor(city, levels = city_order_bar))

p_bar <- ggplot(summ_pm25, aes(x = city, y = pm25_median, fill = period)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Day" = "#0072B2", "Night" = "#F0E442")) +
  labs(
    title = "Day vs Night PM2.5 Median",
    subtitle = "Nov 2017–2024 | Night=19–07",
    x = "City (high→low night median)", y = "Median PM2.5 (µg/m³)", fill = NULL
  ) +
  theme_black_custom() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x labels for space
  )

# -------------------------
# Plot D: Faceted Area (Bottom Right)
# -------------------------
# ** FIX APPLIED HERE: Rotated X-axis labels **
cb_bright_5 <- c("#00A6FF", "#FF9F1C", "#2EC4B6", "#C7F000", "#E94FFF")
yearly_night <- trend_df %>% mutate(city = factor(city, levels = city_order_5))

p_area <- ggplot(yearly_night, aes(x = year, y = mean_pm25, fill = city)) +
  geom_area(alpha = 0.75) +
  facet_wrap(~ city, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = c(2018, 2020, 2022, 2024)) + # Reduce breaks to prevent crowd
  scale_fill_manual(values = cb_bright_5, guide = "none") +
  labs(
    title = "Night PM2.5 Accumulation by Year",
    subtitle = "Nov nights 2017–2024",
    x = "Year", y = "Avg PM2.5 (µg/m³)"
  ) +
  theme_black_custom() +
  theme(
    # Fix for overlapping labels: Rotate 45 deg and reduce size slightly
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
    panel.spacing = unit(1, "lines") # Give facets breathing room
  )

# -------------------------
# 5) Composite Assembly
# -------------------------
# Combine using patchwork
composite_plot <- (p_heat | p_trend) / (p_bar | p_area) +
  plot_annotation(
    title = "Bonfire Night PM2.5 Analysis",
    subtitle = "Northern England cities | November nights (19:00–06:59)",
    theme = theme(
      plot.background = element_rect(fill = "black", color = NA),
      plot.title = element_text(size = 22, face = "bold", color = "white", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "grey80", hjust = 0.5)
    )
  )

# Print the final plot
print(composite_plot)

# Optional: Save with dimensions that support the layout
# ggsave("Bonfire_Analysis_Composite.png", composite_plot, width = 14, height = 10, dpi = 300)