# SECTION 4: SUMMARY STATISTICS

cat("\n  BASIC STATISTICS  \n")

# Age statistics
cat("\nAge Statistics:\n")
cat("Min:", min(data_clean$Age, na.rm = TRUE), "\n")
cat("Max:", max(data_clean$Age, na.rm = TRUE), "\n")
cat("Mean:", round(mean(data_clean$Age, na.rm = TRUE), 2), "\n")
cat("Median:", median(data_clean$Age, na.rm = TRUE), "\n")

# Top clubs by appearances
cat("\n  TOP CLUBS BY TOTAL APPEARANCES  \n")
club_appearances <- data_clean |>
  group_by(Club) |>
  summarise(Total_Appearances = sum(Appearances, na.rm = TRUE)) |>
  arrange(desc(Total_Appearances))
print(head(club_appearances, 10))

# Top nationalities by appearances
cat("\n  TOP NATIONALITIES BY APPEARANCES  \n")
nationality_appearances <- data_clean |>
  group_by(Nationality) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  arrange(desc(Total_Appearances))
print(head(nationality_appearances, 10))

