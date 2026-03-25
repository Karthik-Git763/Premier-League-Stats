# Premier League Player Statistics Analysis
# Dataset: EPL Player Statistics (2020-09-24)

# SECTION 1: SETUP AND LIBRARY LOADING

# Install packages if not already installed
required_packages <- c(
  "tidyverse", "plotly", "corrplot", "scales",
  "RColorBrewer", "gridExtra"
)

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  }
}

install_if_missing(required_packages)

# Load libraries
library(tidyverse)
library(plotly)
library(corrplot)
library(scales)
library(RColorBrewer)
library(gridExtra)

# Set options
options(scipen = 999) # Disable scientific notation

# SECTION 2: DATA LOADING AND PREPROCESSING

# 2.1 Load the dataset
data <- read.csv("dataset - 2020-09-24.csv", stringsAsFactors = FALSE)

# Display basic info
cat("  DATASET OVERVIEW  \n")
cat("Shape of the dataset:", nrow(data), "rows x", ncol(data), "columns\n\n")
cat("Column names:\n")
print(names(data))

# 2.2 Initial data inspection
cat("\n  DATA STRUCTURE  \n")
str(data)

cat("\n  MISSING VALUES SUMMARY  \n")
missing_summary <- colSums(is.na(data) | data == "")
print(missing_summary[missing_summary > 0])

# 2.3 Remove entries without Age, Nationality, or Jersey Number
data_clean <- data |>
  filter(!is.na(Nationality) & Nationality != "") |>
  filter(!is.na(Age)) |>
  filter(!is.na(Jersey.Number))

cat("\nRows after removing missing Nationality/Age/Jersey Number:", nrow(data_clean), "\n")

# 2.4 Clean percentage columns (remove '%' sign and convert to numeric)
# Function to clean percentage columns
clean_percentage <- function(x) {
  as.numeric(gsub("%", "", x))
}

data_clean <- data_clean |>
  mutate(
    Cross.accuracy.. = clean_percentage(Cross.accuracy..),
    Shooting.accuracy.. = clean_percentage(Shooting.accuracy..),
    Tackle.success.. = clean_percentage(Tackle.success..)
  )

# Rename columns for easier use (remove dots and special characters)
names(data_clean) <- gsub("\\.\\.", "_pct", names(data_clean))
names(data_clean) <- gsub("\\.", "_", names(data_clean))

cat("\n  CLEANED COLUMN NAMES  \n")
print(names(data_clean))

# 2.5 View cleaned data
cat("\n  FIRST FEW ROWS OF CLEANED DATA  \n")
print(head(data_clean))

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

# SECTION 5: VISUALIZATIONS - PIE CHARTS

cat("\n  CREATING PIE CHARTS  \n")

# 5.1 Pie Chart: Nationality by Appearances (ggplot2 - static)
# Top 10 + Others
nationality_pie_data <- data_clean |>
  group_by(Nationality) |>
  summarise(Appearances = sum(Appearances, na.rm = TRUE)) |>
  arrange(desc(Appearances)) |>
  mutate(
    Nationality_Group = ifelse(row_number() <= 10, Nationality, "Others")
  ) |>
  group_by(Nationality_Group) |>
  summarise(Appearances = sum(Appearances)) |>
  arrange(desc(Appearances)) |>
  mutate(
    Percentage = round(Appearances / sum(Appearances) * 100, 1),
    Label = paste0(Nationality_Group, "\n", Percentage, "%")
  )

# Static pie chart (ggplot2)
pie_nationality_static <- ggplot(
  nationality_pie_data,
  aes(x = "", y = Appearances, fill = Nationality_Group)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Countries Represented in EPL by Number of Appearances",
    fill = "Nationality"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Set3")

print(pie_nationality_static)

# 5.2 Interactive Pie Chart (plotly)
pie_nationality_interactive <- plot_ly(
  nationality_pie_data,
  labels = ~Nationality_Group,
  values = ~Appearances,
  type = "pie",
  textposition = "inside",
  textinfo = "percent+label",
  marker = list(colors = RColorBrewer::brewer.pal(11, "Set3"))
) |>
  layout(
    title = list(
      text = "<b>Countries Represented in EPL by Number of Appearances</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    showlegend = TRUE
  )

print(pie_nationality_interactive)

# 5.3 Pie Chart: Position Distribution
position_pie_data <- data_clean |>
  group_by(Position) |>
  summarise(
    Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Static pie chart
pie_position_static <- ggplot(
  position_pie_data,
  aes(x = "", y = Count, fill = Position)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Player Distribution by Position",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")

print(pie_position_static)

# Interactive pie chart
pie_position_interactive <- plot_ly(
  position_pie_data,
  labels = ~Position,
  values = ~Count,
  type = "pie",
  textposition = "inside",
  textinfo = "percent+label",
  marker = list(colors = RColorBrewer::brewer.pal(4, "Set2"))
) |>
  layout(
    title = list(
      text = "<b>Player Distribution by Position</b>",
      x = 0.5,
      font = list(size = 18)
    )
  )

print(pie_position_interactive)

# SECTION 6: VISUALIZATIONS - BAR CHARTS

cat("\n  CREATING BAR CHARTS  \n")

# 6.1 Bar Chart: Appearances by Club (stacked by position)
club_position_data <- data_clean |>
  group_by(Club, Position) |>
  summarise(Appearances = sum(Appearances, na.rm = TRUE), .groups = "drop") |>
  group_by(Club) |>
  mutate(Total = sum(Appearances)) |>
  ungroup() |>
  arrange(desc(Total))

# Get club order for proper sorting
club_order <- club_position_data |>
  select(Club, Total) |>
  distinct() |>
  arrange(Total) |>
  pull(Club)

club_position_data$Club <- factor(club_position_data$Club, levels = club_order)

# Static bar chart (ggplot2)
bar_club_static <- ggplot(
  club_position_data,
  aes(x = Club, y = Appearances, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Appearances by Club",
    x = "Club",
    y = "Total Appearances",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  ) +
  scale_fill_brewer(palette = "Set2")

print(bar_club_static)

# Interactive bar chart (plotly)
bar_club_interactive <- plot_ly(
  club_position_data,
  x = ~Appearances,
  y = ~Club,
  color = ~Position,
  colors = RColorBrewer::brewer.pal(4, "Set2"),
  type = "bar",
  orientation = "h"
) |>
  layout(
    barmode = "stack",
    title = list(
      text = "<b>Appearances by Club</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Total Appearances"),
    yaxis = list(title = "", categoryorder = "total ascending"),
    height = 600
  )

print(bar_club_interactive)

# 6.2 Bar Chart: Top 15 Goal Scorers
top_scorers <- data_clean |>
  arrange(desc(Goals)) |>
  head(15) |>
  mutate(Name = factor(Name, levels = rev(Name)))

bar_scorers_static <- ggplot(
  top_scorers,
  aes(x = Name, y = Goals, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Goal Scorers in EPL",
    x = "Player",
    y = "Goals",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = Goals), hjust = -0.2, size = 3)

print(bar_scorers_static)

# Interactive version
bar_scorers_interactive <- plot_ly(
  top_scorers,
  x = ~Goals,
  y = ~Name,
  color = ~Position,
  text = ~ paste("Club:", Club, "<br>Age:", Age),
  type = "bar",
  orientation = "h"
) |>
  layout(
    title = list(
      text = "<b>Top 15 Goal Scorers in EPL</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Goals"),
    yaxis = list(title = "", categoryorder = "total ascending")
  )

print(bar_scorers_interactive)

# 6.3 Bar Chart: Top 15 Assist Providers
top_assists <- data_clean |>
  arrange(desc(Assists)) |>
  head(15) |>
  mutate(Name = factor(Name, levels = rev(Name)))

bar_assists_static <- ggplot(
  top_assists,
  aes(x = Name, y = Assists, fill = Position)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Assist Providers in EPL",
    x = "Player",
    y = "Assists",
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = Assists), hjust = -0.2, size = 3)

print(bar_assists_static)

# Interactive version
bar_assists_interactive <- plot_ly(
  top_assists,
  x = ~Assists,
  y = ~Name,
  color = ~Position,
  text = ~ paste("Club:", Club, "<br>Age:", Age),
  type = "bar",
  orientation = "h"
) |>
  layout(
    title = list(
      text = "<b>Top 15 Assist Providers in EPL</b>",
      x = 0.5,
      font = list(size = 18)
    ),
    xaxis = list(title = "Assists"),
    yaxis = list(title = "", categoryorder = "total ascending")
  )

print(bar_assists_interactive)

# 6.4 Bar Chart: Goals by Position
goals_by_position <- data_clean |>
  group_by(Position) |>
  summarise(
    Total_Goals = sum(Goals, na.rm = TRUE),
    Avg_Goals = round(mean(Goals, na.rm = TRUE), 2)
  ) |>
  arrange(desc(Total_Goals))

bar_goals_position_static <- ggplot(
  goals_by_position,
  aes(
    x = reorder(Position, Total_Goals),
    y = Total_Goals, fill = Position
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Total Goals by Position",
    x = "Position",
    y = "Total Goals"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = Total_Goals), vjust = -0.5, size = 4)

print(bar_goals_position_static)

# 6.5 Bar Chart: Top 10 Nationalities (excluding England)
top_nationalities <- data_clean |>
  filter(Nationality != "England") |>
  group_by(Nationality) |>
  summarise(
    Player_Count = n(),
    Total_Appearances = sum(Appearances, na.rm = TRUE)
  ) |>
  arrange(desc(Total_Appearances)) |>
  head(10)

bar_nationality_static <- ggplot(
  top_nationalities,
  aes(
    x = reorder(Nationality, Total_Appearances),
    y = Total_Appearances, fill = Nationality
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Foreign Nations by Appearances (excl. England)",
    x = "Nationality",
    y = "Total Appearances"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = Total_Appearances), hjust = -0.1, size = 3)

print(bar_nationality_static)

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
  select(all_of(numeric_cols)) |>
  filter(complete.cases(.))

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, use = "complete.obs")

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
