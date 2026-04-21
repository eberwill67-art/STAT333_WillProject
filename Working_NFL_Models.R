library(tidyverse)
library(MASS)
library(nlme)

nfl_data <- read.csv("nfl-team-statistics.csv") %>%
  filter(season >= 2006) %>%   # removes structural missingness years
  mutate(
    wins = as.numeric(wins),
    losses = as.numeric(losses),
    ties = as.numeric(ties),
    win_diff = wins - losses,
    winning_record = ifelse(win_diff >= 0, 1, 0),
    win_pct = (wins + 0.5 * ties) / (wins + losses + ties),
    turnovers = offense_n_interceptions +
      offense_n_fumbles_lost_pass +
      offense_n_fumbles_lost_run
  )



#Linear
linear_model <- lm(
  wins ~ offense_ave_yards_gained_pass +
    offense_ave_yards_gained_run +
    turnovers +
    score_differential,
  data = nfl_data
)

summary(linear_model)


#Logistic
logistic_model <- glm(
  winning_record ~ offense_ave_yards_gained_pass +
    offense_ave_yards_gained_run +
    turnovers +
    score_differential,
  data = nfl_data,
  family = binomial()
)

summary(logistic_model)



#Time series
nfl_ts <- nfl_data %>%
  arrange(team, season) %>%
  group_by(team) %>%
  mutate(
    TimeIndex = row_number(),
    lag1_win_pct = lag(win_pct, 1),
    lag2_win_pct = lag(win_pct, 2),
    lag1_winning_record = lag(winning_record, 1),
    lag2_winning_record = lag(winning_record, 2),
    next_winning_record = lead(winning_record, 1),
    next_win_pct = lead(win_pct, 1)
  ) %>%
  ungroup()


ts_logistic_model <- glm(
  next_winning_record ~ lag1_winning_record +
    lag2_winning_record +
    offense_ave_yards_gained_pass +
    offense_ave_yards_gained_run +
    turnovers +
    score_differential,
  data = nfl_ts %>% drop_na(
    next_winning_record,
    lag1_winning_record,
    lag2_winning_record,
    offense_ave_yards_gained_pass,
    offense_ave_yards_gained_run,
    turnovers,
    score_differential
  ),
  family = binomial()
)

summary(ts_logistic_model)
