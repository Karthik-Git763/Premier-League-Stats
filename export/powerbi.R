
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

# 10.6 Export goalkeeper statistics
goalkeeper_stats <- data_clean |>
  filter(Position == "Goalkeeper") |>
  filter(Appearances > 0) |>
  mutate(
    Save_Percentage = round(ifelse(Saves > 0 & Goals_conceded >= 0, 
                                   Saves / (Saves + Goals_conceded) * 100, 0), 2),
    Goals_Conceded_Per_Match = round(Goals_conceded / Appearances, 2),
    Saves_Per_Match = round(Saves / Appearances, 2),
    Clean_Sheet_Percentage = round(Clean_sheets / Appearances * 100, 2),
    GK_Rating = round(
      (Save_Percentage * 0.4) + 
      (Clean_Sheet_Percentage * 0.3) + 
      ((100 - Goals_Conceded_Per_Match * 10) * 0.3), 2
    )
  ) |>
  select(
    Name, Club, Nationality, Age, Appearances, Wins, Losses,
    Clean_sheets, Clean_Sheet_Percentage, Goals_conceded, Goals_Conceded_Per_Match,
    Saves, Saves_Per_Match, Save_Percentage, Penalties_saved,
    Punches, High_Claims, Catches, Sweeper_clearances, Goal_Kicks,
    GK_Rating
  ) |>
  arrange(desc(GK_Rating))

write.csv(goalkeeper_stats, "powerbi_data/goalkeeper_stats.csv", row.names = FALSE)
cat("Exported: powerbi_data/goalkeeper_stats.csv\n")

# 10.7 Export offensive metrics
offensive_metrics <- data_clean |>
  filter(Position != "Goalkeeper") |>
  filter(Appearances > 0) |>
  mutate(
    Goals_Per_Match = round(Goals / Appearances, 3),
    Headed_Goal_Pct = round(ifelse(Goals > 0, Headed_goals / Goals * 100, 0), 2),
    Right_Foot_Goal_Pct = round(ifelse(Goals > 0, Goals_with_right_foot / Goals * 100, 0), 2),
    Left_Foot_Goal_Pct = round(ifelse(Goals > 0, Goals_with_left_foot / Goals * 100, 0), 2),
    Penalty_Goal_Pct = round(ifelse(Goals > 0, Penalties_scored / Goals * 100, 0), 2),
    Shots_Per_Match = round(Shots / Appearances, 2),
    Shot_Accuracy = round(ifelse(Shots > 0, Shots_on_target / Shots * 100, 0), 0),
    Conversion_Rate = round(ifelse(Shots > 0, Goals / Shots * 100, 0), 2),
    Assists_Per_Match = round(Assists / Appearances, 3),
    Big_Chances_Per_Match = round(Big_chances_created / Appearances, 3),
    Goal_Contributions = Goals + Assists,
    Contributions_Per_Match = round(Goal_Contributions / Appearances, 3),
    Offensive_Rating = round(
      (Goals_Per_Match * 20) + 
      (Assists_Per_Match * 15) + 
      (Conversion_Rate * 0.3) + 
      (Shot_Accuracy * 0.1) + 
      (Big_Chances_Per_Match * 5), 2
    )
  ) |>
  select(
    Name, Club, Position, Nationality, Age, Appearances,
    Goals, Goals_Per_Match, Headed_goals, Goals_with_right_foot, Goals_with_left_foot,
    Headed_Goal_Pct, Right_Foot_Goal_Pct, Left_Foot_Goal_Pct,
    Penalties_scored, Penalty_Goal_Pct, Freekicks_scored,
    Shots, Shots_on_target, Shots_Per_Match, Shot_Accuracy, Conversion_Rate,
    Hit_woodwork, Big_chances_missed,
    Assists, Assists_Per_Match, Big_chances_created, Big_Chances_Per_Match,
    Goal_Contributions, Contributions_Per_Match,
    Offensive_Rating
  ) |>
  arrange(desc(Offensive_Rating))

write.csv(offensive_metrics, "powerbi_data/offensive_metrics.csv", row.names = FALSE)
cat("Exported: powerbi_data/offensive_metrics.csv\n")

# 10.8 Export defensive metrics
defensive_metrics <- data_clean |>
  filter(Position != "Goalkeeper") |>
  filter(Appearances > 0) |>
  mutate(
    Tackle_Success_Rate = Tackle_success_pct,
    Tackles_Per_Match = round(Tackles / Appearances, 2),
    Interceptions_Per_Match = round(Interceptions / Appearances, 2),
    Clearances_Per_Match = round(Clearances / Appearances, 2),
    Recoveries_Per_Match = round(Recoveries / Appearances, 2),
    Blocked_Shots_Per_Match = round(Blocked_shots / Appearances, 2),
    Total_Duels = Duels_won + Duels_lost,
    Duel_Win_Rate = round(ifelse(Total_Duels > 0, (Duels_won / Total_Duels) * 100, 0), 2),
    Total_Aerial_Duels = Aerial_battles_won + Aerial_battles_lost,
    Aerial_Win_Rate = round(ifelse(Total_Aerial_Duels > 0, 
                                    (Aerial_battles_won / Total_Aerial_Duels) * 100, 0), 2),
    Defensive_Actions = Tackles + Interceptions + Clearances + Blocked_shots,
    Defensive_Actions_Per_Match = round(Defensive_Actions / Appearances, 2),
    Cards_Total = Yellow_cards + Red_cards,
    Fouls_Per_Match = round(Fouls / Appearances, 2),
    Defensive_Rating = round(
      (Tackles_Per_Match * 2.5) + 
      (Interceptions_Per_Match * 3) + 
      (Clearances_Per_Match * 1.5) + 
      (Duel_Win_Rate * 0.15) + 
      (Aerial_Win_Rate * 0.10) + 
      (Recoveries_Per_Match * 1.0), 2
    )
  ) |>
  select(
    Name, Club, Position, Nationality, Age, Appearances,
    Tackles, Tackle_Success_Rate, Blocked_shots, Interceptions, Clearances, Recoveries,
    Duels_won, Duels_lost, Total_Duels, Duel_Win_Rate,
    Aerial_battles_won, Aerial_battles_lost, Total_Aerial_Duels, Aerial_Win_Rate,
    Tackles_Per_Match, Interceptions_Per_Match, Clearances_Per_Match, Recoveries_Per_Match,
    Defensive_Actions, Defensive_Actions_Per_Match,
    Yellow_cards, Red_cards, Cards_Total, Fouls, Fouls_Per_Match,
    Defensive_Rating
  ) |>
  arrange(desc(Defensive_Rating))

write.csv(defensive_metrics, "powerbi_data/defensive_metrics.csv", row.names = FALSE)
cat("Exported: powerbi_data/defensive_metrics.csv\n")

cat("\n  ALL POWER BI EXPORTS COMPLETE  \n")
cat("Total files exported: 8\n")
