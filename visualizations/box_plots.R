# SECTION 7: VISUALIZATIONS - BOX PLOTS
cat("\n  CREATING BOX PLOTS  \n")

# Calculate average age for reference line
avg_age <- mean(data_clean$Age, na.rm = TRUE)

# 7.1 Box Plot: Age by Position
box_age_position_static <- ggplot(data_clean, aes(x = Position, y = Age, fill = Position)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Player Age Distribution by Position",
    subtitle = paste("Dotted line shows average age:", round(avg_age, 1)),
    x = "Position",
    y = "Age (years)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = avg_age, linetype = "dashed", color = "blue", size = 1)

print(box_age_position_static)

# Interactive version
box_age_position_interactive <- plot_ly(
  data_clean,
  x = ~Position,
  y = ~Age,
  color = ~Position,
  type = "box",
  colors = RColorBrewer::brewer.pal(4, "Set2")
) |>
  layout(
    title = list(
      text = "<b>Player Age Distribution by Position</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Position"),
    yaxis = list(title = "Age (years)"),
    shapes = list(
      list(
        type = "line",
        x0 = 0, x1 = 1, xref = "paper",
        y0 = avg_age, y1 = avg_age,
        line = list(color = "blue", width = 2, dash = "dot")
      )
    )
  )

print(box_age_position_interactive)

# 7.2 Box Plot: Age by Club
# Order clubs by median age
club_age_order <- data_clean |>
  group_by(Club) |>
  summarise(Median_Age = median(Age, na.rm = TRUE)) |>
  arrange(Median_Age) |>
  pull(Club)

data_clean$Club_ordered <- factor(data_clean$Club, levels = club_age_order)

box_age_club_static <- ggplot(data_clean, aes(x = Club_ordered, y = Age, fill = Club_ordered)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Player Age Distribution by Club",
    subtitle = paste("Dotted line shows average age:", round(avg_age, 1)),
    x = "Club",
    y = "Age (years)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_text(size = 7)
  ) +
  geom_vline(xintercept = NULL) +
  geom_hline(yintercept = avg_age, linetype = "dashed", color = "blue", size = 1)

print(box_age_club_static)

# Interactive version
box_age_club_interactive <- plot_ly(
  data_clean,
  y = ~Club_ordered,
  x = ~Age,
  color = ~Club_ordered,
  type = "box",
  orientation = "h"
) |>
  layout(
    title = list(
      text = "<b>Player Age Distribution by Club</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Age (years)"),
    yaxis = list(title = ""),
    showlegend = FALSE,
    height = 700,
    shapes = list(
      list(
        type = "line",
        y0 = 0, y1 = 1, yref = "paper",
        x0 = avg_age, x1 = avg_age,
        line = list(color = "blue", width = 2, dash = "dot")
      )
    )
  )

print(box_age_club_interactive)

# 7.3 Box Plot: Goals per Match by Position (players with 38+ appearances)
box_goals_position_static <- ggplot(
  data_38apps |> filter(Position != "Goalkeeper"),
  aes(x = Position, y = Goals_per_match, fill = Position)
) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Goals per Match by Position (38+ Appearances)",
    x = "Position",
    y = "Goals per Match"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set1")

print(box_goals_position_static)

# Interactive version
box_goals_interactive <- plot_ly(
  data_38apps |> filter(Position != "Goalkeeper"),
  x = ~Position,
  y = ~Goals_per_match,
  color = ~Position,
  type = "box",
  text = ~Name
) |>
  layout(
    title = list(
      text = "<b>Goals per Match by Position (38+ Appearances)</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Position"),
    yaxis = list(title = "Goals per Match")
  )

print(box_goals_interactive)

#                          ==
