# if not already installed, you will need to use install.packages("library name")
# then load the following libraries
library(tidyverse)
library(plotly)
library(broom)
library(ggrepel)

# read in the raw data and assign to new objects
t_stats1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
t_stats2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
p_stats <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
salaries <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
payroll <- read_csv("data/raw/2019-20_nba_team-payroll.csv")

# join all team statistics together by Team
joined <- full_join(t_stats1, t_stats2, by = "Team")

# check the structure of the joined data
str(joined)

# check the first 6 rows of the joined data
head(joined)

# check the last 6 rows of the joined data 
tail(joined)

# rename inefficient variables for easier analysis in R
joined <- rename(joined,
                 "P3Ar" = "3PAr",
                 "TSp" = "TS%",
                 "eFGp" = "eFG%",
                 "TOVp" = "TOV%",
                 "ORBp" = "ORB%",
                 "FT_per_FGA" = "FT/FGA",
                 "DRBp" = "DRB%",
                 "FGp" = "FG%",
                 "P3" = "3P",
                 "P3A" = "3PA",
                 "P3p" = "3P%",
                 "P2" = "2P",
                 "P2A" = "2PA",
                 "P2p" = "2P%",
                 "FTp" = "FT%")
