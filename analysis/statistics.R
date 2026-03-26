# SECTION 8: STATISTICAL ANALYSIS
#                          ==

cat("\n  STATISTICAL ANALYSIS  \n")

# 8.1 Correlation Analysis
# Select numeric columns for correlation
numeric_cols <- c(
  "Age", "Appearances", "Goals", "Assists", "Passes",
  "Shots", "Shots_on_target", "Tackles", "Interceptions",
  "Clean_sheets", "Yellow_cards", "Fouls"
)

# Filter to only include columns that exist
numeric_cols <- numeric_cols[numeric_cols %in% names(data_clean)]

correlation_data <- data_clean |>
  select(all_of(numeric_cols))

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
cor_matrix[is.na(cor_matrix)] <- 0

cat("\n  CORRELATION MATRIX  \n")
print(round(cor_matrix, 2))

# Plot correlation heatmap
corrplot(cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Player Statistics",
  mar = c(0, 0, 2, 0),
  number.cex = 0.7
)

# 8.2 Summary Statistics by Position
cat("\n  SUMMARY STATISTICS BY POSITION  \n")
position_summary <- data_clean |>
  group_by(Position) |>
  summarise(
    Player_Count = n(),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    Median_Age = median(Age, na.rm = TRUE),
    SD_Age = round(sd(Age, na.rm = TRUE), 2),
    Total_Goals = sum(Goals, na.rm = TRUE),
    Avg_Goals = round(mean(Goals, na.rm = TRUE), 2),
    Total_Assists = sum(Assists, na.rm = TRUE),
    Avg_Assists = round(mean(Assists, na.rm = TRUE), 2),
    Avg_Appearances = round(mean(Appearances, na.rm = TRUE), 2)
  )
print(position_summary)

# 8.3 ANOVA Test: Age differences between positions
cat("\n  ANOVA TEST: AGE BY POSITION  \n")
anova_age <- aov(Age ~ Position, data = data_clean)
print(summary(anova_age))

# Post-hoc Tukey test
cat("\n  TUKEY POST-HOC TEST  \n")
tukey_age <- TukeyHSD(anova_age)
print(tukey_age)

# 8.4 ANOVA Test: Goals per match by Position (outfield players)
cat("\n  ANOVA TEST: GOALS PER MATCH BY POSITION (OUTFIELD, 38+ APPS)  \n")
outfield_38 <- data_38apps |> filter(Position != "Goalkeeper")
anova_goals <- aov(Goals_per_match ~ Position, data = outfield_38)
print(summary(anova_goals))

# 8.5 Summary by Club
cat("\n  SUMMARY STATISTICS BY CLUB  \n")
club_summary <- data_clean |>
  group_by(Club) |>
  summarise(
    Player_Count = n(),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    Total_Goals = sum(Goals, na.rm = TRUE),
    Total_Assists = sum(Assists, na.rm = TRUE),
    Total_Clean_Sheets = sum(Clean_sheets, na.rm = TRUE),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  arrange(desc(Total_Goals))
print(club_summary)

