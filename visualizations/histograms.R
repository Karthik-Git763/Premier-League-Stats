# HISTOGRAMS - Distribution Analysis
# Required by methodology: histograms for distribution visualization

cat("\n  CREATING HISTOGRAMS  \n")

# =============================================================================
# HISTOGRAM 1: Age Distribution
# =============================================================================

hist_age <- ggplot(data_clean, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(data_clean$Age, na.rm = TRUE), 
             linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Player Ages",
    subtitle = paste("Mean Age:", round(mean(data_clean$Age, na.rm = TRUE), 1), 
                     "| Median:", median(data_clean$Age, na.rm = TRUE)),
    x = "Age (years)",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_age)

# Interactive histogram
hist_age_interactive <- plot_ly(
  data_clean,
  x = ~Age,
  type = "histogram",
  nbinsx = 20,
  marker = list(color = "steelblue", line = list(color = "white", width = 1))
) |>
  layout(
    title = list(text = "<b>Distribution of Player Ages</b>", x = 0.5),
    xaxis = list(title = "Age (years)"),
    yaxis = list(title = "Count")
  )

print(hist_age_interactive)

# =============================================================================
# HISTOGRAM 2: Goals Distribution
# =============================================================================

hist_goals <- ggplot(data_clean |> filter(Goals > 0), aes(x = Goals)) +
  geom_histogram(bins = 25, fill = "darkgreen", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(data_clean$Goals[data_clean$Goals > 0], na.rm = TRUE), 
             linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Goals Scored",
    subtitle = "Players with at least 1 goal",
    x = "Goals",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_goals)

# =============================================================================
# HISTOGRAM 3: Appearances Distribution
# =============================================================================

hist_appearances <- ggplot(data_clean, aes(x = Appearances)) +
  geom_histogram(bins = 30, fill = "purple", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 38, linetype = "dashed", color = "orange", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Player Appearances",
    subtitle = "Orange line = 38 appearances (1 full season)",
    x = "Appearances",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_appearances)

# =============================================================================
# HISTOGRAM 4: Assists Distribution
# =============================================================================

hist_assists <- ggplot(data_clean |> filter(Assists > 0), aes(x = Assists)) +
  geom_histogram(bins = 20, fill = "orange", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Assists",
    subtitle = "Players with at least 1 assist",
    x = "Assists",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_assists)

# =============================================================================
# HISTOGRAM 5: Shooting Accuracy Distribution
# =============================================================================

hist_accuracy <- ggplot(
  data_clean |> filter(!is.na(Shooting_accuracy_pct) & Shooting_accuracy_pct > 0),
  aes(x = Shooting_accuracy_pct)
) +
  geom_histogram(bins = 20, fill = "coral", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(data_clean$Shooting_accuracy_pct, na.rm = TRUE), 
             linetype = "dashed", color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Shooting Accuracy",
    subtitle = paste("Mean:", round(mean(data_clean$Shooting_accuracy_pct, na.rm = TRUE), 1), "%"),
    x = "Shooting Accuracy (%)",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_accuracy)

# =============================================================================
# HISTOGRAM 6: Tackles Distribution by Position (Faceted)
# =============================================================================

hist_tackles_facet <- ggplot(data_clean, aes(x = Tackles, fill = Position)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.8) +
  facet_wrap(~Position, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution of Tackles by Position",
    x = "Tackles",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")

print(hist_tackles_facet)

# =============================================================================
# HISTOGRAM 7: Passes Distribution
# =============================================================================

hist_passes <- ggplot(data_clean |> filter(Passes > 0), aes(x = Passes)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Total Passes",
    x = "Passes",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(hist_passes)

cat("\n  HISTOGRAMS COMPLETE  \n")
