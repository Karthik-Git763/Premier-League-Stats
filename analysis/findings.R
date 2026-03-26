# SECTION 9: SUMMARY

cat("\n")
cat("                     \n")
cat("                    KEY FINDINGS SUMMARY                        \n")
cat("                     \n")

# Most appearances by position
cat("\n--- MOST APPEARANCES BY POSITION ---\n")
top_by_position <- data_clean |>
  group_by(Position) |>
  slice_max(Appearances, n = 1) |>
  select(Position, Name, Club, Appearances)
print(top_by_position)

# Top nationality summary
cat("\n--- TOP 3 FOREIGN NATIONALITIES (excl. England) ---\n")
top_foreign <- data_clean |>
  filter(Nationality != "England") |>
  group_by(Nationality) |>
  summarise(Total_Appearances = sum(Appearances)) |>
  arrange(desc(Total_Appearances)) |>
  head(3)
print(top_foreign)

# Youngest and oldest squads
cat("\n--- YOUNGEST SQUAD ---\n")
youngest <- club_summary |>
  arrange(Avg_Age) |>
  head(1)
print(youngest)

cat("\n--- OLDEST SQUAD ---\n")
oldest <- club_summary |>
  arrange(desc(Avg_Age)) |>
  head(1)
print(oldest)
