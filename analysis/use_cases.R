# USE CASE QUESTIONS ANALYSIS
# This file addresses the 4 key analytical questions from the project requirements

cat("\n")
cat("================================================================\n")
cat("           USE CASE QUESTIONS ANALYSIS                          \n")
cat("================================================================\n")

# =============================================================================
# USE CASE 1: Which players have the highest offensive contributions?
# - Who scored the most goals?
# - How do their shot accuracy and conversion rates compare?
# =============================================================================

cat("\n")
cat(">>> USE CASE 1: HIGHEST OFFENSIVE CONTRIBUTIONS <<<\n")
cat("================================================================\n")

# Top 15 scorers with offensive metrics
cat("\n--- TOP 15 SCORERS WITH OFFENSIVE METRICS ---\n")
top_offensive <- data_clean |>
  filter(Goals > 0) |>
  arrange(desc(Goals)) |>
  head(15) |>
  mutate(
    Conversion_Rate = ifelse(Shots > 0, round((Goals / Shots) * 100, 2), 0),
    Shot_Accuracy = Shooting_accuracy_pct
  ) |>
  select(Name, Club, Position, Goals, Shots, Shots_on_target, 
         Shot_Accuracy, Conversion_Rate, Assists)

print(top_offensive)

# Visualization: Top Scorers with Conversion Rate
cat("\n--- VISUALIZATION: Top Scorers vs Conversion Rate ---\n")

top_scorers_analysis <- data_clean |>
  filter(Goals >= 10 & Shots > 0) |>
  mutate(
    Conversion_Rate = round((Goals / Shots) * 100, 2)
  ) |>
  arrange(desc(Goals)) |>
  head(20)

# Bar chart: Goals with conversion rate overlay
bar_offensive <- ggplot(top_scorers_analysis, aes(x = reorder(Name, Goals))) +
  geom_bar(aes(y = Goals, fill = "Goals"), stat = "identity", alpha = 0.8) +
  geom_point(aes(y = Conversion_Rate * 2, color = "Conversion Rate %"), size = 3) +
  geom_line(aes(y = Conversion_Rate * 2, group = 1, color = "Conversion Rate %"), linewidth = 1) +
  scale_y_continuous(
    name = "Goals",
    sec.axis = sec_axis(~ . / 2, name = "Conversion Rate (%)")
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top Scorers: Goals vs Conversion Rate",
    subtitle = "Players with 10+ goals | Conversion Rate = Goals/Shots",
    x = "Player"
  ) +
  scale_fill_manual(values = c("Goals" = "steelblue")) +
  scale_color_manual(values = c("Conversion Rate %" = "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(bar_offensive)

# Scatterplot: Goals vs Shooting Accuracy
scatter_goals_accuracy <- ggplot(
  data_clean |> filter(Goals > 0 & !is.na(Shooting_accuracy_pct)),
  aes(x = Shooting_accuracy_pct, y = Goals, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Goals vs Shooting Accuracy",
    subtitle = "Does higher accuracy lead to more goals?",
    x = "Shooting Accuracy (%)",
    y = "Total Goals"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_goals_accuracy)

# Interactive version
scatter_offensive_interactive <- plot_ly(
  data_clean |> filter(Goals > 0 & Shots > 0),
  x = ~Shots,
  y = ~Goals,
  color = ~Position,
  size = ~Shooting_accuracy_pct,
  text = ~paste("Name:", Name, "<br>Club:", Club, 
                "<br>Goals:", Goals, "<br>Shots:", Shots,
                "<br>Accuracy:", Shooting_accuracy_pct, "%"),
  type = "scatter",
  mode = "markers"
) |>
  layout(
    title = list(text = "<b>Goals vs Shots (Size = Shooting Accuracy)</b>", x = 0.5),
    xaxis = list(title = "Total Shots"),
    yaxis = list(title = "Total Goals")
  )

print(scatter_offensive_interactive)

# Summary statistics for top scorers
cat("\n--- OFFENSIVE METRICS SUMMARY ---\n")
offensive_summary <- data_clean |>
  filter(Goals >= 10) |>
  summarise(
    Players = n(),
    Avg_Goals = round(mean(Goals), 2),
    Avg_Shots = round(mean(Shots), 2),
    Avg_Accuracy = round(mean(Shooting_accuracy_pct, na.rm = TRUE), 2),
    Avg_Conversion = round(mean(Goals / Shots * 100, na.rm = TRUE), 2)
  )
print(offensive_summary)

# =============================================================================
# USE CASE 2: Does high possession involvement correlate with better performance?
# - Are players with many touches and passes also high on goal contributions?
# =============================================================================

cat("\n")
cat(">>> USE CASE 2: POSSESSION VS PERFORMANCE CORRELATION <<<\n")
cat("================================================================\n")

# Create goal contributions metric (Goals + Assists)
data_clean <- data_clean |>
  mutate(
    Goal_Contributions = Goals + Assists,
    Total_Touches = Passes  # Using Passes as proxy for touches/involvement
  )

# Correlation between passes and goal contributions
cat("\n--- CORRELATION: Passes vs Goal Contributions ---\n")
cor_passes_goals <- cor(data_clean$Passes, data_clean$Goal_Contributions, 
                        use = "complete.obs")
cat("Correlation coefficient:", round(cor_passes_goals, 4), "\n")

# Scatterplot: Passes vs Goal Contributions
scatter_possession <- ggplot(
  data_clean |> filter(Appearances >= 10),
  aes(x = Passes, y = Goal_Contributions, color = Position)
) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Possession Involvement vs Goal Contributions",
    subtitle = paste("Correlation:", round(cor_passes_goals, 3), "| Players with 10+ appearances"),
    x = "Total Passes",
    y = "Goal Contributions (Goals + Assists)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set2")

print(scatter_possession)

# Interactive version
scatter_possession_interactive <- plot_ly(
  data_clean |> filter(Appearances >= 10),
  x = ~Passes,
  y = ~Goal_Contributions,
  color = ~Position,
  text = ~paste("Name:", Name, "<br>Club:", Club,
                "<br>Passes:", Passes, "<br>Goals:", Goals, 
                "<br>Assists:", Assists),
  type = "scatter",
  mode = "markers"
) |>
  layout(
    title = list(text = "<b>Passes vs Goal Contributions by Position</b>", x = 0.5),
    xaxis = list(title = "Total Passes"),
    yaxis = list(title = "Goal Contributions (Goals + Assists)")
  )

print(scatter_possession_interactive)

# Per-match analysis for fairness
cat("\n--- POSSESSION PER MATCH VS GOALS PER MATCH ---\n")
data_38apps <- data_38apps |>
  mutate(
    Contributions_per_match = (Goals + Assists) / Appearances
  )

scatter_per_match <- ggplot(
  data_38apps |> filter(Position != "Goalkeeper"),
  aes(x = Passes_per_match, y = Goals_per_match, color = Position)
) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Passes per Match vs Goals per Match",
    subtitle = "Players with 38+ appearances (excl. Goalkeepers)",
    x = "Passes per Match",
    y = "Goals per Match"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_brewer(palette = "Set1")

print(scatter_per_match)

# Summary by position
cat("\n--- POSSESSION VS PERFORMANCE BY POSITION ---\n")
possession_summary <- data_clean |>
  filter(Appearances >= 10) |>
  group_by(Position) |>
  summarise(
    Players = n(),
    Avg_Passes = round(mean(Passes), 2),
    Avg_Goals = round(mean(Goals), 2),
    Avg_Assists = round(mean(Assists), 2),
    Avg_Contributions = round(mean(Goal_Contributions), 2),
    Correlation = round(cor(Passes, Goal_Contributions, use = "complete.obs"), 3)
  )
print(possession_summary)

# =============================================================================
# USE CASE 3: How do defensive statistics vary by player position?
# - Do defenders show significantly higher defensive metrics?
# =============================================================================

cat("\n")
cat(">>> USE CASE 3: DEFENSIVE STATISTICS BY POSITION <<<\n")
cat("================================================================\n")

# Summary of defensive metrics by position
cat("\n--- DEFENSIVE METRICS BY POSITION ---\n")
defensive_summary <- data_clean |>
  group_by(Position) |>
  summarise(
    Players = n(),
    Avg_Tackles = round(mean(Tackles, na.rm = TRUE), 2),
    Avg_Interceptions = round(mean(Interceptions, na.rm = TRUE), 2),
    Avg_Clearances = round(mean(Clearances, na.rm = TRUE), 2),
    Avg_Blocked_Shots = round(mean(Blocked_shots, na.rm = TRUE), 2),
    Avg_Duels_Won = round(mean(Duels_won, na.rm = TRUE), 2),
    Total_Tackles = sum(Tackles, na.rm = TRUE),
    Total_Clearances = sum(Clearances, na.rm = TRUE)
  ) |>
  arrange(desc(Avg_Tackles))

print(defensive_summary)

# Box Plot: Tackles by Position
box_tackles <- ggplot(data_clean, aes(x = Position, y = Tackles, fill = Position)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Tackles Distribution by Position",
    x = "Position",
    y = "Total Tackles"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")

print(box_tackles)

# Box Plot: Clearances by Position
box_clearances <- ggplot(data_clean, aes(x = Position, y = Clearances, fill = Position)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Clearances Distribution by Position",
    x = "Position",
    y = "Total Clearances"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")

print(box_clearances)

# Box Plot: Interceptions by Position
box_interceptions <- ggplot(data_clean, aes(x = Position, y = Interceptions, fill = Position)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Interceptions Distribution by Position",
    x = "Position",
    y = "Total Interceptions"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Pastel1")

print(box_interceptions)

# Combined defensive metrics bar chart
defensive_long <- defensive_summary |>
  select(Position, Avg_Tackles, Avg_Interceptions, Avg_Clearances) |>
  pivot_longer(cols = -Position, names_to = "Metric", values_to = "Value") |>
  mutate(Metric = gsub("Avg_", "", Metric))

bar_defensive <- ggplot(defensive_long, aes(x = Position, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Average Defensive Metrics by Position",
    x = "Position",
    y = "Average Value",
    fill = "Metric"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

print(bar_defensive)

# Interactive version
bar_defensive_interactive <- plot_ly(
  defensive_long,
  x = ~Position,
  y = ~Value,
  color = ~Metric,
  type = "bar"
) |>
  layout(
    barmode = "group",
    title = list(text = "<b>Defensive Metrics by Position</b>", x = 0.5),
    xaxis = list(title = "Position"),
    yaxis = list(title = "Average Value")
  )

print(bar_defensive_interactive)

# ANOVA Test: Tackles by Position
cat("\n--- ANOVA TEST: TACKLES BY POSITION ---\n")
anova_tackles <- aov(Tackles ~ Position, data = data_clean)
print(summary(anova_tackles))

# ANOVA Test: Clearances by Position
cat("\n--- ANOVA TEST: CLEARANCES BY POSITION ---\n")
anova_clearances <- aov(Clearances ~ Position, data = data_clean)
print(summary(anova_clearances))

# =============================================================================
# USE CASE 4: Can we identify trends in goalkeeper performance?
# - What patterns exist between saves, save percentage, and goals conceded?
# =============================================================================

cat("\n")
cat(">>> USE CASE 4: GOALKEEPER PERFORMANCE TRENDS <<<\n")
cat("================================================================\n")

# Filter goalkeepers only
goalkeepers_data <- data_clean |>
  filter(Position == "Goalkeeper" & Appearances > 0)

cat("\n--- GOALKEEPER STATISTICS OVERVIEW ---\n")
cat("Total Goalkeepers:", nrow(goalkeepers_data), "\n")

# Summary statistics for goalkeepers
gk_summary <- goalkeepers_data |>
  summarise(
    Count = n(),
    Avg_Saves = round(mean(Saves, na.rm = TRUE), 2),
    Avg_Goals_Conceded = round(mean(Goals_conceded, na.rm = TRUE), 2),
    Avg_Clean_Sheets = round(mean(Clean_sheets, na.rm = TRUE), 2),
    Avg_Appearances = round(mean(Appearances), 2),
    Total_Saves = sum(Saves, na.rm = TRUE),
    Total_Goals_Conceded = sum(Goals_conceded, na.rm = TRUE)
  )

print(gk_summary)

# Calculate save percentage if not available
goalkeepers_data <- goalkeepers_data |>
  mutate(
    Total_Shots_Faced = Saves + Goals_conceded,
    Save_Percentage = ifelse(Total_Shots_Faced > 0, 
                             round((Saves / Total_Shots_Faced) * 100, 2), 
                             NA),
    Goals_Conceded_Per_Match = round(Goals_conceded / Appearances, 2),
    Saves_Per_Match = round(Saves / Appearances, 2),
    Clean_Sheet_Percentage = round((Clean_sheets / Appearances) * 100, 2)
  )

# Top goalkeepers by various metrics
cat("\n--- TOP 10 GOALKEEPERS BY SAVES ---\n")
top_gk_saves <- goalkeepers_data |>
  arrange(desc(Saves)) |>
  head(10) |>
  select(Name, Club, Appearances, Saves, Goals_conceded, Clean_sheets, Save_Percentage)
print(top_gk_saves)

cat("\n--- TOP 10 GOALKEEPERS BY CLEAN SHEETS ---\n")
top_gk_cleansheets <- goalkeepers_data |>
  arrange(desc(Clean_sheets)) |>
  head(10) |>
  select(Name, Club, Appearances, Clean_sheets, Goals_conceded, Save_Percentage)
print(top_gk_cleansheets)

# Scatterplot: Saves vs Goals Conceded
scatter_gk <- ggplot(
  goalkeepers_data |> filter(Appearances >= 10),
  aes(x = Saves, y = Goals_conceded)
) +
  geom_point(aes(size = Appearances, color = Clean_sheets), alpha = 0.7) +
  geom_text(aes(label = ifelse(Saves > 100 | Goals_conceded > 50, Name, "")), 
            hjust = -0.1, vjust = 0.5, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Goalkeeper Performance: Saves vs Goals Conceded",
    subtitle = "Size = Appearances, Color = Clean Sheets | Goalkeepers with 10+ apps",
    x = "Total Saves",
    y = "Goals Conceded",
    color = "Clean Sheets",
    size = "Appearances"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_gradient(low = "yellow", high = "darkgreen")

print(scatter_gk)

# Bar chart: Goals Conceded per Match
gk_per_match <- goalkeepers_data |>
  filter(Appearances >= 20) |>
  arrange(Goals_Conceded_Per_Match) |>
  head(15)

bar_gk_conceded <- ggplot(
  gk_per_match,
  aes(x = reorder(Name, -Goals_Conceded_Per_Match), y = Goals_Conceded_Per_Match, fill = Club)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Goals Conceded per Match (Top 15 GKs, 20+ apps)",
    subtitle = "Lower is better",
    x = "Goalkeeper",
    y = "Goals Conceded per Match"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )

print(bar_gk_conceded)

# Interactive: Goalkeeper comparison
gk_interactive <- plot_ly(
  goalkeepers_data |> filter(Appearances >= 10),
  x = ~Saves,
  y = ~Goals_conceded,
  size = ~Appearances,
  color = ~Clean_sheets,
  text = ~paste("Name:", Name, "<br>Club:", Club,
                "<br>Appearances:", Appearances,
                "<br>Saves:", Saves,
                "<br>Goals Conceded:", Goals_conceded,
                "<br>Clean Sheets:", Clean_sheets,
                "<br>Save %:", Save_Percentage),
  type = "scatter",
  mode = "markers",
  marker = list(sizemode = "diameter", sizeref = 2)
) |>
  layout(
    title = list(text = "<b>Goalkeeper Analysis: Saves vs Goals Conceded</b>", x = 0.5),
    xaxis = list(title = "Total Saves"),
    yaxis = list(title = "Goals Conceded"),
    coloraxis = list(colorbar = list(title = "Clean Sheets"))
  )

print(gk_interactive)

# Histogram: Save Percentage Distribution
hist_save_pct <- ggplot(
  goalkeepers_data |> filter(!is.na(Save_Percentage) & Appearances >= 5),
  aes(x = Save_Percentage)
) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(goalkeepers_data$Save_Percentage, na.rm = TRUE), 
             linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Goalkeeper Save Percentage",
    subtitle = paste("Red line = Average:", 
                     round(mean(goalkeepers_data$Save_Percentage, na.rm = TRUE), 1), "%"),
    x = "Save Percentage (%)",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_save_pct)

# Clean sheet percentage vs goals conceded
scatter_gk_efficiency <- ggplot(
  goalkeepers_data |> filter(Appearances >= 10),
  aes(x = Clean_Sheet_Percentage, y = Goals_Conceded_Per_Match)
) +
  geom_point(aes(size = Appearances), color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Clean Sheet % vs Goals Conceded per Match",
    subtitle = "Inverse relationship expected | GKs with 10+ appearances",
    x = "Clean Sheet Percentage (%)",
    y = "Goals Conceded per Match"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(scatter_gk_efficiency)

# Correlation analysis for goalkeepers
cat("\n--- GOALKEEPER CORRELATIONS ---\n")
gk_numeric <- goalkeepers_data |>
  filter(Appearances >= 5) |>
  select(Saves, Goals_conceded, Clean_sheets, Appearances, Penalties_saved)

gk_cor <- cor(gk_numeric, use = "pairwise.complete.obs")
print(round(gk_cor, 3))

cat("\n")
cat("================================================================\n")
cat("           USE CASE ANALYSIS COMPLETE                           \n")
cat("================================================================\n")
