
# SECTION 10: EXPORT DATA FOR POWER BI

cat("\n  EXPORTING DATA FOR POWER BI  \n")

# Create directory for Power BI exports
dir.create("powerbi_data", showWarnings = FALSE)

# 10.1 Export cleaned main dataset
write.csv(data_clean, "powerbi_data/epl_clean.csv", row.names = FALSE)
cat("Exported: powerbi_data/epl_clean.csv\n")

# 10.2 Export summary by club
summary_by_club <- data_clean |>
  group_by(Club) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE),
    Total_Goals = sum(Goals, na.rm = TRUE),
    Total_Assists = sum(Assists, na.rm = TRUE),
    Total_Clean_Sheets = sum(Clean_sheets, na.rm = TRUE),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE),
    Total_Yellow_Cards = sum(Yellow_cards, na.rm = TRUE),
    Total_Red_Cards = sum(Red_cards, na.rm = TRUE)
  ) |>
  arrange(desc(Total_Goals))

write.csv(summary_by_club, "powerbi_data/summary_by_club.csv", row.names = FALSE)
cat("Exported: powerbi_data/summary_by_club.csv\n")

# 10.3 Export summary by position
summary_by_position <- data_clean |>
  group_by(Position) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE),
    Total_Goals = sum(Goals, na.rm = TRUE),
    Avg_Goals_Per_Match = round(mean(Goals_per_match, na.rm = TRUE), 3),
    Total_Assists = sum(Assists, na.rm = TRUE),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    Total_Clean_Sheets = sum(Clean_sheets, na.rm = TRUE),
    Total_Tackles = sum(Tackles, na.rm = TRUE),
    Total_Interceptions = sum(Interceptions, na.rm = TRUE)
  )

write.csv(summary_by_position, "powerbi_data/summary_by_position.csv", row.names = FALSE)
cat("Exported: powerbi_data/summary_by_position.csv\n")

# 10.4 Export summary by nationality
summary_by_nationality <- data_clean |>
  group_by(Nationality) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE),
    Total_Goals = sum(Goals, na.rm = TRUE),
    Total_Assists = sum(Assists, na.rm = TRUE),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    Top_Club = Club[which.max(Appearances)]
  ) |>
  arrange(desc(Total_Appearances))

write.csv(summary_by_nationality, "powerbi_data/summary_by_nationality.csv", row.names = FALSE)
cat("Exported: powerbi_data/summary_by_nationality.csv\n")

# 10.5 Export top performers
top_performers <- bind_rows(
  data_clean |> arrange(desc(Goals)) |> head(20) |> mutate(Category = "Top Scorers"),
  data_clean |> arrange(desc(Assists)) |> head(20) |> mutate(Category = "Top Assists"),
  data_clean |> arrange(desc(Clean_sheets)) |> head(20) |> mutate(Category = "Top Clean Sheets"),
  data_clean |> arrange(desc(Passes)) |> head(20) |> mutate(Category = "Top Passers")
) |>
  select(
    Category, Name, Club, Position, Nationality, Age, Appearances,
    Goals, Assists, Clean_sheets, Passes
  )

write.csv(top_performers, "powerbi_data/top_performers.csv", row.names = FALSE)
cat("Exported: powerbi_data/top_performers.csv\n")
