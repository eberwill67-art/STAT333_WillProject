library(tidyverse)
library(MASS)
library(nlme)
library(GGally)
library(car)


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


# Test for collinearity
model_vars <- nfl_data %>%
  select(offense_ave_yards_gained_pass,
         offense_ave_yards_gained_run,
         turnovers)

# correlation matrix
cor(model_vars, use = "complete.obs")

# pairwise scatterplot matrix
ggpairs(model_vars)

# fit linear model for VIF check
vif_model <- lm(wins ~ offense_ave_yards_gained_pass +
                  offense_ave_yards_gained_run +
                  turnovers,
                data = nfl_data)

# VIF values
vif(vif_model)


#Linear
linear_model <- lm(
  wins ~ offense_ave_yards_gained_pass +
    offense_ave_yards_gained_run +
    turnovers +
    score_differential,
  data = nfl_data
)

summary(linear_model)


# predicted values (linear regression)
nfl_data$pred_wins <- predict(turnover_model)

# plot
ggplot(nfl_data, aes(x = pred_wins, y = wins)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual Wins (Linear Regression)",
       x = "Predicted Wins",
       y = "Actual Wins") +
  theme_minimal()



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


# predicted vs actual (logit model)
nfl_data$pred_prob <- predict(logit_model, type = "response")

# plot
ggplot(nfl_data, aes(x = pred_prob, y = winning_record)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  labs(title = "Predicted Probability vs Actual Winning Season",
       x = "Predicted Probability of Winning Season",
       y = "Actual Winning Season (0 = No, 1 = Yes)") +
  theme_minimal()



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
