library("rstan") # observe startup messages
library("tidyverse")

setwd("~/Desktop/zearn/marcos_RL_Zearn")
load("RL_data_Zearn.Rdata")
load("RL_data_Zearn-notscaled.Rdata")

df <- mfmb_scaled[,c(1,2,7,10,11)]
# testing different values for reward
df[df$reward == 1,]$reward <- 0
df[df$reward == 2,]$reward <- 0.5
df[df$reward == 3,]$reward <- 1

# Transforming data so first week of use = 1 for each teacher
df <- df %>%
  group_by(Adult.User.ID) %>%
  mutate(week_num = week_num - min(week_num) + 1)

# create choice[N,T]
choice <- df %>%
  select(Adult.User.ID, week_num, effort) %>%
  group_by(week_num) %>%
  spread(week_num, effort)
# fill in NAs with 1 for now (lowest payoff)
choice[is.na(choice)] <- 1
#Sort to match reward
choice <- choice[order(choice$Adult.User.ID),]

# create reward[N,T]
reward <- df %>%
  select(Adult.User.ID, week_num, reward) %>%
  group_by(week_num) %>%
  spread(week_num, reward)
# fill in NAs with 0 for now
reward[is.na(reward)] <- 0
#Sort to match effort
reward <- reward[order(reward$Adult.User.ID),]

### Test: Use 1% of data
N_partial = 290
choice <- choice[c(1:N_partial),]
reward <- reward[c(1:N_partial),]

model_data <- list( N = N_partial, #number of teachers
                    T = max(df$week_num), # number of weeks total
                    Tsubj = rep(max(df$week_num), N_partial), # number of weeks each teacher used Zearn
                    choice = choice[,c(-1)],
                    outcome = reward[,c(-1)])
my_model <- stan_model(file = "RL_model_hierarchical.stan", verbose = TRUE)

sample <- sampling(object = my_model, data = model_data)

#get alpha and beta estimates
fit$par[1]
fit$par[2]

library("shinystan")
launch_shinystan(sample)

save.image(file = 'ben_chain_tau3.RDS')










