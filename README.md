Fetching the dataset from this

[Link Kaggle](https://www.kaggle.com/code/desalegngeb/english-premier-league-players-statistics/input)

# Convention to follow

### The project is structured modularly. Analysis findings are aggregated in `analysis/` and datasets for PowerBI are exported via the `main.R` orchestrator natively via a nix flake (`nix develop . --command Rscript main.R`).

## How to run the analysis
1. Ensure you have Nix installed with flakes enabled.
2. Run the full analysis sequence by executing:
   ```bash
   nix develop . --command Rscript main.R
   ```
3. The exported outputs will be saved in the `powerbi_data/` directory.

## Phase 1: Kaggle Site from 3rd Section

### 1.1 Complete from 3rd Section Onwards

### 1.2 Export dataset for PowerBi

## Phase 2: Check questions given when creating project

### 2.1 Need to implement analysis for those questions which will be asked by mam

## Phase 3: Power BI Data Exports

### 3.1 Verify Export Files

- [x] Check `powerbi_data/` folder exists
- [x] Verify `epl_clean.csv` (cleaned dataset, ~562 rows)
- [x] Verify `summary_by_club.csv` (20 clubs)
- [x] Verify `summary_by_position.csv` (4 positions)
- [x] Verify `summary_by_nationality.csv` (all nationalities)
- [x] Verify `top_performers.csv` (top 20 by each category)

### 3.2 Data Quality Check

- [x] No missing values in key columns
- [x] Numeric columns are properly formatted
- [x] Club names are consistent
- [x] Position values are: Goalkeeper, Defender, Midfielder, Forward

---

## Phase 4: Power BI Dashboard Creation

### 4.1 Setup

- [ ] Install Power BI Desktop (https://powerbi.microsoft.com/downloads/)
- [ ] Create new Power BI project

### 4.2 Data Import

- [ ] Import `epl_clean.csv`
- [ ] Import `summary_by_club.csv`
- [ ] Import `summary_by_position.csv`
- [ ] Import `summary_by_nationality.csv`
- [ ] Import `top_performers.csv`

### 4.3 Data Model

- [ ] Create relationship: epl_clean[Club] → summary_by_club[Club]
- [ ] Create relationship: epl_clean[Position] → summary_by_position[Position]
- [ ] Create relationship: epl_clean[Nationality] → summary_by_nationality[Nationality]

### 4.4 Dashboard Pages

#### Page 1: Overview

- [ ] KPI Card: Total Players
- [ ] KPI Card: Average Age
- [ ] KPI Card: Total Goals
- [ ] KPI Card: Total Assists
- [ ] Pie Chart: Position Distribution
- [ ] Donut Chart: Top 5 Nationalities

#### Page 2: Club Analysis

- [ ] Bar Chart: Total Goals by Club (sorted descending)
- [ ] Stacked Bar: Appearances by Club and Position
- [ ] Table: Club Statistics (Players, Avg Age, Goals, Assists)
- [ ] Slicer: Club Filter

#### Page 3: Player Performance

- [ ] Bar Chart: Top 15 Scorers
- [ ] Bar Chart: Top 15 Assist Providers
- [ ] Scatter Plot: Goals vs Assists (bubble size = Appearances)
- [ ] Table: Top Performers (from top_performers.csv)
- [ ] Slicer: Position Filter

#### Page 4: Age Analysis

- [ ] Bar Chart: Average Age by Club
- [ ] Clustered Column: Age Distribution by Position
- [ ] Card: Youngest Player
- [ ] Card: Oldest Player
- [ ] Card: Average Age

#### Page 5: Nationality Insights

- [ ] Map Visual: Players by Country (if lat/long available)
- [ ] Bar Chart: Top 10 Nationalities by Appearances
- [ ] Table: Nationality breakdown
- [ ] Pie Chart: English vs Foreign Players

### 4.5 Interactivity

- [ ] Add cross-filtering between visuals
- [ ] Add drill-through for player details
- [ ] Add tooltips with additional info
- [ ] Add bookmarks for key insights

### 4.6 Formatting

- [ ] Apply consistent color theme
- [ ] Add titles to all visuals
- [ ] Format numbers (comma separators, decimals)
- [ ] Add page navigation buttons
