# check the structure of p_stats
str(p_stats)


# check the first 6 rows of p_stats
head(p_stats)


# check the last 6 rows of p_stats
tail(p_stats)


# check for missing values
sum(is.na(p_stats))


# visualize missing values.
naniar::vis_miss(p_stats)


# create new variables of interest divided by the minutes played
p_stats <- p_stats %>%
  mutate(PTS_p_min = PTS / MP, 
         FGA_p_min = FGA / MP,
         FTA_p_min = FTA / MP,
         AST_p_min = AST / MP,
         ORB_p_min = ORB / MP)
p_stats

# create new summary variables that deal with the duplicates
p_summary <- p_stats %>%
  group_by(player_name) %>%
  summarise(total_starts = sum(GS),  # sum the total number of games started
            last_Tm = last(Tm),  # only leave the team name that was the most recent team played for
            last_Pos = last(Pos),  # only leave the position name that was the most recent position played in
            sum_PTS_min = round(sum(PTS_p_min), digits = 2),  # sum the total number of PTS / min and round to 2 decimal places
            sum_FGA_min = round(sum(FGA_p_min), digits = 2), 
            sum_FTA_min = round(sum(FTA_p_min), digits = 2), 
            sum_AST_min = round(sum(AST_p_min), digits = 2), 
            sum_ORB_min = round(sum(ORB_p_min), digits = 2)) %>%
  mutate(TSp = round(sum_PTS_min / (2 * sum_FGA_min + 0.44 * sum_FTA_min) * 100, digits = 1)) # create a new TSp column using the formula
p_summary


# join the data so we can see player statistics and salary together
joined1 <- left_join(p_summary, salaries, by = "player_name") %>%
  arrange(desc(salary))


# check the structure of joined1
str(joined1)


# check the first 6 rows of joined1
head(joined1)


# check the last 6 rows of joined1
tail(joined1)


# remove any rows containing an NA from the data
joined1 <- drop_na(joined1)


# `filter()` data for Chicago's starting players and `select()` variables of interest
bulls <- joined1 %>%
  filter(last_Tm == "CHO" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(salary))  # arrange players from highest salary to lowest 
bulls


# filter the PG position and arrange by AST
PG_stats <- joined1 %>%
  filter(last_Pos == "PG" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(sum_AST_min))
PG_stats

# create a data set only showing the current player versus the new recommendation for PG
joined1 %>%
  filter(player_name == "Kemba Walker" | player_name == "Ben Simmons") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(salary))


# filter the SG position and arrange by TSp
SG_stats <- joined1 %>%
  filter(last_Pos == "SG" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(TSp))
SG_stats

# create a data set only showing the current player versus the new recommendation for SG
joined1 %>%
  filter(player_name == "Jeremy Lamb" | player_name == "Joe Harris") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(sum_ORB_min))


# filter the SF position and arrange by TSp
SF_stats <- joined1 %>%
  filter(last_Pos == "SF" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(TSp))
SF_stats

# create a data set only showing the current player versus the new recommendation for SF
joined1 %>%
  filter(player_name == "Nicolas Batum" | player_name == "Robert Covington") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(salary))


# filter the PF position arranged by TSp
PF_stats <- joined1 %>%
  filter(last_Pos == "PF" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(TSp))
PF_stats

# create a data set only showing the current player versus the new recommendation for PF
joined1 %>%
  filter(player_name == "Marvin Williams" | player_name == "John Collins") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(salary))


# filter the C position and arrange by ORB
C_stats <- joined1 %>%
  filter(last_Pos == "C" & total_starts > 40) %>%
  select(player_name, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(sum_ORB_min))
C_stats

# create a data set only showing the current player versus the new recommendation for C
joined1 %>%
  filter(player_name == "Cody Zeller" | player_name == "Karl-Anthony Towns") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary) %>%
  arrange(desc(salary))


# final recommendations
recommendation <- joined1 %>%
  filter(player_name == "Ben Simmons" | player_name == "Joe Harris" | 
           player_name == "Robert Covington" | player_name == "John Collins" | 
           player_name == "Karl-Anthony Towns") %>%
  select(player_name, last_Tm, last_Pos, total_starts, sum_AST_min, sum_ORB_min, TSp, salary)
recommendation



