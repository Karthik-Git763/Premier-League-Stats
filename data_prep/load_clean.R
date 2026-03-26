# SECTION 2: DATA LOADING AND PREPROCESSING

# 2.1 Load the dataset
data <- read.csv(
  "dataset - 2020-09-24.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "NULL", "null", "-")
)

# Standardize text fields and convert empty text to NA
text_cols <- names(data)[sapply(data, is.character)]
for (col in text_cols) {
  data[[col]] <- trimws(data[[col]])
  data[[col]][data[[col]] == ""] <- NA
}

# Display basic info
cat("  DATASET OVERVIEW  \n")
cat("Shape of the dataset:", nrow(data), "rows x", ncol(data), "columns\n\n")
cat("Column names:\n")
print(names(data))

# 2.2 Initial data inspection
cat("\n  DATA STRUCTURE  \n")
str(data)

cat("\n  MISSING VALUES SUMMARY  \n")
missing_summary <- colSums(is.na(data))
print(missing_summary[missing_summary > 0])

# 2.3 Keep all entries and impute missing values for dashboard readiness
data_clean <- data

cat("\nRows retained for preprocessing:", nrow(data_clean), "\n")

# 2.4 Clean percentage columns (remove '%' sign and convert to numeric)
# Function to clean percentage columns
clean_percentage <- function(x) {
  x <- gsub("%", "", x)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}

if ("Cross.accuracy.." %in% names(data_clean)) {
  data_clean$Cross.accuracy.. <- clean_percentage(data_clean$Cross.accuracy..)
}
if ("Shooting.accuracy.." %in% names(data_clean)) {
  data_clean$Shooting.accuracy.. <- clean_percentage(data_clean$Shooting.accuracy..)
}
if ("Tackle.success.." %in% names(data_clean)) {
  data_clean$Tackle.success.. <- clean_percentage(data_clean$Tackle.success..)
}

# Rename columns for easier use (remove dots and special characters)
names(data_clean) <- gsub("\\.\\.", "_pct", names(data_clean))
names(data_clean) <- gsub("\\.", "_", names(data_clean))

# 2.5 Convert numeric-like columns
explicit_text_cols <- intersect(
  c("Name", "Club", "Nationality", "Position"),
  names(data_clean)
)

for (col in names(data_clean)) {
  if (!(col %in% explicit_text_cols) && is.character(data_clean[[col]])) {
    numeric_candidate <- gsub(",", "", data_clean[[col]])
    suppressWarnings(num_values <- as.numeric(numeric_candidate))
    convertible_share <- mean(is.na(data_clean[[col]]) | !is.na(num_values))

    if (!is.na(convertible_share) && convertible_share >= 0.7) {
      data_clean[[col]] <- num_values
    }
  }
}

# 2.6 Missing-value imputation helpers
get_mode <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Ensure Position exists for group-aware imputations
if ("Position" %in% names(data_clean)) {
  data_clean$Position[is.na(data_clean$Position)] <- "Unknown Position"
}

# Impute character columns (group mode by Position, then global mode)
char_cols <- names(data_clean)[sapply(data_clean, is.character)]
for (col in char_cols) {
  if (col == "Name") {
    missing_idx <- which(is.na(data_clean[[col]]) | data_clean[[col]] == "")
    if (length(missing_idx) > 0) {
      data_clean[[col]][missing_idx] <- paste0("Unknown_Player_", missing_idx)
    }
    next
  }

  if ("Position" %in% names(data_clean) && col != "Position") {
    positions <- unique(data_clean$Position)
    for (pos in positions) {
      idx <- which(
        data_clean$Position == pos &
          (is.na(data_clean[[col]]) | data_clean[[col]] == "")
      )

      if (length(idx) > 0) {
        mode_pos <- get_mode(data_clean[[col]][data_clean$Position == pos])
        if (!is.na(mode_pos)) {
          data_clean[[col]][idx] <- mode_pos
        }
      }
    }
  }

  remaining_missing <- which(is.na(data_clean[[col]]) | data_clean[[col]] == "")
  if (length(remaining_missing) > 0) {
    global_mode <- get_mode(data_clean[[col]])
    fallback <- ifelse(
      is.na(global_mode),
      paste("Unknown", gsub("_", " ", col)),
      global_mode
    )
    data_clean[[col]][remaining_missing] <- fallback
  }
}

# Impute numeric columns (group median by Position, then global median, then 0)
num_cols <- names(data_clean)[sapply(data_clean, is.numeric)]
for (col in num_cols) {
  if ("Position" %in% names(data_clean)) {
    positions <- unique(data_clean$Position)
    for (pos in positions) {
      group_idx <- which(data_clean$Position == pos)
      missing_idx <- group_idx[is.na(data_clean[[col]][group_idx])]

      if (length(missing_idx) > 0) {
        group_median <- suppressWarnings(median(data_clean[[col]][group_idx], na.rm = TRUE))
        if (is.finite(group_median)) {
          data_clean[[col]][missing_idx] <- group_median
        }
      }
    }
  }

  remaining_missing <- which(is.na(data_clean[[col]]))
  if (length(remaining_missing) > 0) {
    global_median <- suppressWarnings(median(data_clean[[col]], na.rm = TRUE))
    fill_value <- ifelse(is.finite(global_median), global_median, 0)
    data_clean[[col]][remaining_missing] <- fill_value
  }
}

# Last pass to guarantee no blank strings in text fields
char_cols <- names(data_clean)[sapply(data_clean, is.character)]
for (col in char_cols) {
  blank_idx <- which(trimws(data_clean[[col]]) == "")
  if (length(blank_idx) > 0) {
    data_clean[[col]][blank_idx] <- paste("Unknown", gsub("_", " ", col))
  }
}

cat("\n  CLEANED COLUMN NAMES  \n")
print(names(data_clean))

# 2.7 Final missing check
final_missing <- colSums(is.na(data_clean))
cat("\n  FINAL MISSING VALUES SUMMARY  \n")
print(final_missing[final_missing > 0])
cat("Total remaining NA values:", sum(is.na(data_clean)), "\n")

# 2.8 View cleaned data
cat("\n  FIRST FEW ROWS OF CLEANED DATA  \n")
print(head(data_clean))

