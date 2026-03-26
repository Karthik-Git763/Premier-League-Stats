# SCATTERPLOTS - Relationship Analysis
# Required by methodology: scatterplots for correlation visualization

cat("\n  CREATING SCATTERPLOTS  \n")

# =============================================================================
# SCATTERPLOT 1: Goals vs Shots on Target
# =============================================================================

scatter_goals_shots <- ggplot(
  data_clean |> filter(Goals > 0 & Shots_on_target > 0),
  aes(x = Shots_on_target, y = Goals, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Goals vs Shots on Target",
    subtitle = paste("Correlation:", 
                     round(cor(data_clean$Goals, data_clean$Shots_on_target, 
                               use = "complete.obs"), 3)),
    x = "Shots on Target",
    y = "Goals"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_goals_shots)

# Interactive version
scatter_goals_shots_interactive <- plot_ly(
  data_clean |> filter(Goals > 0 & Shots_on_target > 0),
  x = ~Shots_on_target,
  y = ~Goals,
  color = ~Position,
  text = ~paste("Name:", Name, "<br>Club:", Club, 
                "<br>Goals:", Goals, "<br>Shots on Target:", Shots_on_target),
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7)
) |>
  layout(
    title = list(text = "<b>Goals vs Shots on Target</b>", x = 0.5),
    xaxis = list(title = "Shots on Target"),
    yaxis = list(title = "Goals")
  )

print(scatter_goals_shots_interactive)

# =============================================================================
# SCATTERPLOT 2: Assists vs Big Chances Created
# =============================================================================

scatter_assists_chances <- ggplot(
  data_clean |> filter(Assists > 0 & Big_chances_created > 0),
  aes(x = Big_chances_created, y = Assists, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Assists vs Big Chances Created",
    subtitle = paste("Correlation:", 
                     round(cor(data_clean$Assists, data_clean$Big_chances_created, 
                               use = "complete.obs"), 3)),
    x = "Big Chances Created",
    y = "Assists"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set2")

print(scatter_assists_chances)

# =============================================================================
# SCATTERPLOT 3: Age vs Appearances
# =============================================================================

scatter_age_apps <- ggplot(
  data_clean,
  aes(x = Age, y = Appearances, color = Position)
) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Age vs Appearances",
    subtitle = "Does experience correlate with more appearances?",
    x = "Age (years)",
    y = "Appearances"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_age_apps)

# =============================================================================
# SCATTERPLOT 4: Tackles vs Interceptions (Defensive Correlation)
# =============================================================================

scatter_tackles_interceptions <- ggplot(
  data_clean |> filter(Position %in% c("Defender", "Midfielder")),
  aes(x = Tackles, y = Interceptions, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Tackles vs Interceptions (Defensive Players)",
    subtitle = paste("Correlation:", 
                     round(cor(data_clean$Tackles, data_clean$Interceptions, 
                               use = "complete.obs"), 3)),
    x = "Tackles",
    y = "Interceptions"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Dark2")

print(scatter_tackles_interceptions)

# =============================================================================
# SCATTERPLOT 5: Yellow Cards vs Fouls
# =============================================================================

scatter_cards_fouls <- ggplot(
  data_clean |> filter(Fouls > 0),
  aes(x = Fouls, y = Yellow_cards, color = Position)
) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Yellow Cards vs Fouls Committed",
    subtitle = "Discipline analysis",
    x = "Fouls",
    y = "Yellow Cards"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_cards_fouls)

# =============================================================================
# SCATTERPLOT 6: Duels Won vs Aerial Battles Won
# =============================================================================

scatter_duels_aerial <- ggplot(
  data_clean |> filter(Duels_won > 0 & Aerial_battles_won > 0),
  aes(x = Duels_won, y = Aerial_battles_won, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Ground Duels Won vs Aerial Battles Won",
    x = "Duels Won",
    y = "Aerial Battles Won"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set2")

print(scatter_duels_aerial)

# Interactive version
scatter_duels_interactive <- plot_ly(
  data_clean |> filter(Duels_won > 0 & Aerial_battles_won > 0),
  x = ~Duels_won,
  y = ~Aerial_battles_won,
  color = ~Position,
  text = ~paste("Name:", Name, "<br>Club:", Club,
                "<br>Duels Won:", Duels_won,
                "<br>Aerial Battles:", Aerial_battles_won),
  type = "scatter",
  mode = "markers"
) |>
  layout(
    title = list(text = "<b>Duels Won vs Aerial Battles Won</b>", x = 0.5),
    xaxis = list(title = "Duels Won"),
    yaxis = list(title = "Aerial Battles Won")
  )

print(scatter_duels_interactive)

# =============================================================================
# SCATTERPLOT 7: Passes vs Crosses (Playmaking)
# =============================================================================

scatter_passes_crosses <- ggplot(
  data_clean |> filter(Passes > 100 & Crosses > 0),
  aes(x = Passes, y = Crosses, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(
    title = "Total Passes vs Crosses",
    subtitle = "Playmaking involvement",
    x = "Total Passes",
    y = "Crosses"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_passes_crosses)

cat("\n  SCATTERPLOTS COMPLETE  \n")
