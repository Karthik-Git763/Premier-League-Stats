# SECTION 3: DATA TRANSFORMATIONS

# 3.1 Create normalized per-appearance data (for players with >0 appearances)
data_normalized <- data_clean |>
  filter(Appearances > 0)

# Columns to normalize (divide by Appearances)
cols_to_normalize <- c(
  "Wins", "Losses", "Goals", "Headed_goals", "Goals_with_right_foot",
  "Goals_with_left_foot", "Penalties_scored", "Freekicks_scored",
  "Shots", "Shots_on_target", "Hit_woodwork", "Big_chances_missed",
  "Clean_sheets", "Goals_conceded", "Tackles", "Last_man_tackles",
  "Blocked_shots", "Interceptions", "Clearances", "Headed_Clearance",
  "Clearances_off_line", "Recoveries", "Duels_won", "Duels_lost",
  "Successful_50_50s", "Aerial_battles_won", "Aerial_battles_lost",
  "Own_goals", "Errors_leading_to_goal", "Assists", "Passes",
  "Big_chances_created", "Crosses", "Through_balls", "Accurate_long_balls",
  "Saves", "Penalties_saved", "Punches", "High_Claims", "Catches",
  "Sweeper_clearances", "Throw_outs", "Goal_Kicks", "Yellow_cards",
  "Red_cards", "Fouls", "Offsides"
)

# Only normalize columns that exist in the dataset
cols_to_normalize <- cols_to_normalize[cols_to_normalize %in% names(data_normalized)]

# Create normalized columns
for (col in cols_to_normalize) {
  data_normalized[[col]] <- data_normalized[[col]] / data_normalized$Appearances
}

# 3.2 Create position-based subsets (original data)
goalkeepers <- data_clean |> filter(Position == "Goalkeeper")
defenders <- data_clean |> filter(Position == "Defender")
midfielders <- data_clean |> filter(Position == "Midfielder")
forwards <- data_clean |> filter(Position == "Forward")

cat("\n  PLAYERS BY POSITION  \n")
cat("Goalkeepers:", nrow(goalkeepers), "\n")
cat("Defenders:", nrow(defenders), "\n")
cat("Midfielders:", nrow(midfielders), "\n")
cat("Forwards:", nrow(forwards), "\n")

# 3.3 Filter players with 38+ appearances (a full season's worth)
data_38apps <- data_clean |> filter(Appearances >= 38)
goalkeepers_38 <- goalkeepers |> filter(Appearances >= 38)
defenders_38 <- defenders |> filter(Appearances >= 38)
midfielders_38 <- midfielders |> filter(Appearances >= 38)
forwards_38 <- forwards |> filter(Appearances >= 38)

# Normalized data for 38+ appearances
all_players_norm <- data_normalized |> filter(Appearances >= 38)
goalkeepers_norm <- data_normalized |> filter(Position == "Goalkeeper" & Appearances >= 38)
defenders_norm <- data_normalized |> filter(Position == "Defender" & Appearances >= 38)
midfielders_norm <- data_normalized |> filter(Position == "Midfielder" & Appearances >= 38)
forwards_norm <- data_normalized |> filter(Position == "Forward" & Appearances >= 38)

cat("\n  PLAYERS WITH 38+ APPEARANCES  \n")
cat("Total:", nrow(data_38apps), "\n")
cat("Goalkeepers:", nrow(goalkeepers_38), "\n")
cat("Defenders:", nrow(defenders_38), "\n")
cat("Midfielders:", nrow(midfielders_38), "\n")
cat("Forwards:", nrow(forwards_38), "\n")

