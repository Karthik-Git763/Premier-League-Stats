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

