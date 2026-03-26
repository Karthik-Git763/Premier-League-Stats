# SECTION 6: VISUALIZATIONS - BAR CHARTS

cat("\n  CREATING BAR CHARTS  \n")

safe_brewer_palette <- function(palette_name, n) {
  max_n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
  base_cols <- RColorBrewer::brewer.pal(max_n, palette_name)

  if (n <= max_n) {
    base_cols[seq_len(n)]
  } else {
    colorRampPalette(base_cols)(n)
  }
}

# 6.1 Bar Chart: Appearances by Club (stacked by position)
club_position_data <- data_clean |>
  group_by(Club, Position) |>
  summarise(Appearances = sum(Appearances, na.rm = TRUE), .groups = "drop") |>
  group_by(Club) |>
  mutate(Total = sum(Appearances)) |>
  ungroup() |>
  arrange(desc(Total))

# Get club order for proper sorting
club_order <- club_position_data |>
  select(Club, Total) |>
  distinct() |>
  arrange(Total) |>
  pull(Club)

club_position_data$Club <- factor(club_position_data$Club, levels = club_order)

# Static bar chart (ggplot2)
bar_club_static <- ggplot(
  club_position_data,
  aes(x = Club, y = Appearances, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Appearances by Club",
    x = "Club",
    y = "Total Appearances",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  ) +
  scale_fill_manual(
    values = safe_brewer_palette("Set2", dplyr::n_distinct(club_position_data$Position))
  )

print(bar_club_static)

# Interactive bar chart (plotly)
bar_club_interactive <- plot_ly(
  club_position_data,
  x = ~Appearances,
  y = ~Club,
  color = ~Position,
  colors = safe_brewer_palette("Set2", dplyr::n_distinct(club_position_data$Position)),
  type = "bar",
  orientation = "h",
  height = 600
) |>
  layout(
    barmode = "stack",
    title = list(
      text = "<b>Appearances by Club</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Total Appearances"),
    yaxis = list(title = "", categoryorder = "total ascending")
  )

print(bar_club_interactive)

# 6.2 Bar Chart: Top 15 Goal Scorers
top_scorers <- data_clean |>
  arrange(desc(Goals)) |>
  head(15) |>
  mutate(Name = factor(Name, levels = rev(Name)))

bar_scorers_static <- ggplot(
  top_scorers,
  aes(x = Name, y = Goals, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Goal Scorers in EPL",
    x = "Player",
    y = "Goals",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = Goals), hjust = -0.2, size = 3)

print(bar_scorers_static)

# Interactive version
bar_scorers_interactive <- plot_ly(
  top_scorers,
  x = ~Goals,
  y = ~Name,
  color = ~Position,
  text = ~ paste("Club:", Club, "<br>Age:", Age),
  type = "bar",
  orientation = "h"
) |>
  layout(
    title = list(
      text = "<b>Top 15 Goal Scorers in EPL</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Goals"),
    yaxis = list(title = "", categoryorder = "total ascending")
  )

print(bar_scorers_interactive)

# 6.3 Bar Chart: Top 15 Assist Providers
top_assists <- data_clean |>
  arrange(desc(Assists)) |>
  head(15) |>
  mutate(Name = factor(Name, levels = rev(Name)))

bar_assists_static <- ggplot(
  top_assists,
  aes(x = Name, y = Assists, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Assist Providers in EPL",
    x = "Player",
    y = "Assists",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = Assists), hjust = -0.2, size = 3)

print(bar_assists_static)

# Interactive version
bar_assists_interactive <- plot_ly(
  top_assists,
  x = ~Assists,
  y = ~Name,
  color = ~Position,
  text = ~ paste("Club:", Club, "<br>Age:", Age),
  type = "bar",
  orientation = "h"
) |>
  layout(
    title = list(
      text = "<b>Top 15 Assist Providers in EPL</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Assists"),
    yaxis = list(title = "", categoryorder = "total ascending")
  )

print(bar_assists_interactive)

# 6.4 Bar Chart: Goals by Position
goals_by_position <- data_clean |>
  group_by(Position) |>
  summarise(
    Total_Goals = sum(Goals, na.rm = TRUE),
    Avg_Goals = round(mean(Goals, na.rm = TRUE), 2)
  ) |>
  arrange(desc(Total_Goals))

bar_goals_position_static <- ggplot(
  goals_by_position,
  aes(
    x = reorder(Position, Total_Goals),
    y = Total_Goals, fill = Position
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Total Goals by Position",
    x = "Position",
    y = "Total Goals"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = safe_brewer_palette("Set2", dplyr::n_distinct(goals_by_position$Position))
  ) +
  geom_text(aes(label = Total_Goals), vjust = -0.5, size = 4)

print(bar_goals_position_static)

# 6.5 Bar Chart: Top 10 Nationalities (excluding England)
top_nationalities <- data_clean |>
  filter(Nationality != "England") |>
  group_by(Nationality) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  arrange(desc(Total_Appearances)) |>
  head(10)

bar_nationality_static <- ggplot(
  top_nationalities,
  aes(
    x = reorder(Nationality, Total_Appearances),
    y = Total_Appearances, fill = Nationality
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Foreign Nations by Appearances (excl. England)",
    x = "Nationality",
    y = "Total Appearances"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = Total_Appearances), hjust = -0.1, size = 3)

print(bar_nationality_static)

