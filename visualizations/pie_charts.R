# SECTION 5: VISUALIZATIONS - PIE CHARTS

cat("\n  CREATING PIE CHARTS  \n")

safe_brewer_palette <- function(palette_name, n) {
  max_n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
  base_cols <- RColorBrewer::brewer.pal(max_n, palette_name)

  if (n <= max_n) {
    base_cols[seq_len(n)]
  } else {
    colorRampPalette(base_cols)(n)
  }
}

# 5.1 Pie Chart: Nationality by Appearances (ggplot2 - static)
# Top 10 + Others
nationality_pie_data <- data_clean |>
  group_by(Nationality) |>
  summarise(Appearances = sum(Appearances, na.rm = TRUE)) |>
  arrange(desc(Appearances)) |>
  mutate(
    Nationality_Group = ifelse(row_number() <= 10, Nationality, "Others")
  ) |>
  group_by(Nationality_Group) |>
  summarise(Appearances = sum(Appearances)) |>
  arrange(desc(Appearances)) |>
  mutate(
    Percentage = round(Appearances / sum(Appearances) * 100, 1),
    Label = paste0(Nationality_Group, "\n", Percentage, "%")
  )

# Static pie chart (ggplot2)
pie_nationality_static <- ggplot(
  nationality_pie_data,
  aes(x = "", y = Appearances, fill = Nationality_Group)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Countries Represented in EPL by Number of Appearances",
    fill = "Nationality"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Set3")

print(pie_nationality_static)

# 5.2 Interactive Pie Chart (plotly)
pie_nationality_interactive <- plot_ly(
  nationality_pie_data,
  labels = ~Nationality_Group,
  values = ~Appearances,
  type = "pie",
  textposition = "inside",
  textinfo = "percent+label",
  marker = list(colors = RColorBrewer::brewer.pal(11, "Set3"))
) |>
  layout(
    title = list(
      text = "<b>Countries Represented in EPL by Number of Appearances</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    showlegend = TRUE
  )

print(pie_nationality_interactive)

# 5.3 Pie Chart: Position Distribution
position_pie_data <- data_clean |>
  group_by(Position) |>
  summarise(
    Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Static pie chart
pie_position_static <- ggplot(
  position_pie_data,
  aes(x = "", y = Count, fill = Position)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Player Distribution by Position",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_manual(
    values = safe_brewer_palette("Set2", dplyr::n_distinct(position_pie_data$Position))
  )

print(pie_position_static)

# Interactive pie chart
pie_position_interactive <- plot_ly(
  position_pie_data,
  labels = ~Position,
  values = ~Count,
  type = "pie",
  textposition = "inside",
  textinfo = "percent+label",
  marker = list(
    colors = safe_brewer_palette("Set2", dplyr::n_distinct(position_pie_data$Position))
  )
) |>
  layout(
    title = list(
      text = "<b>Player Distribution by Position</b>",
      x = 0.5,
      font = list(size = 18)
    )
  )

print(pie_position_interactive)

