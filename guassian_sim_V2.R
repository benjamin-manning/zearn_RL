library(tidyverse)
library(reshape2)


# 1. Initialize: alpha_theta >0, alpha_w > 0; theta_mu, theta_sigma, w (vectors size 4); 
# S (which is the vector size 4 we call x’s — badges, tower alerts, squares…)

# 2. Calculate mu and theta. Sample minutes from the resulting normal distribution.

#3. calculate delta = badges for the NEXT week + gamma * 
#(linear combination of w with x’s for the NEXT week) - 
#(linear combination of w with x’s for the THIS week)
#(oh and gamma = 0.95)

#4. update w. Like so: w = w + alpha_w * delta * w  
#[this is like the updating function we did with each state before]

#5. update theta_mu. Like so: theta_mu = theta_mu + alpha_theta * delta^(week #) * 
#1/(sigma)^2 * (minutes - mu) * x’s  [this is the first gradient formula from the picture]

#6. update theta_sigma. Like so: theta_sigma = theta_sigma + alpha_theta * delta^(week #) * 
#((minutes - mu)^2/(sigma)^2 - 1) * x’s  [this is the second gradient formula from the picture]

# 7. Loop accordingly until the end of the 40 weeks




#data generation specifications
nweeks <- 40
nteachers = 1000

theta_m = rep(NA, 5)
theta_s = rep(NA, 5)

#theta dataframes
mu_theta_df = matrix(data = NA, nrow = nteachers , ncol = 5)
colnames(mu_theta_df) = c('teacher', 'theta1', 'theta2', 'theta3', 'theta4')
sig_theta_df = matrix(data = NA, nrow = nteachers , ncol = 5)
colnames(sig_theta_df) = c('teacher', 'theta1', 'theta2', 'theta3', 'theta4')

#state_variables_df
state_var_df = matrix(data = NA, nrow = nweeks , ncol = 5)
colnames(state_var_df) = c('week', 'x1', 'x2', 'x3', 'x4')

#intialize badges - THIS CAN'T BE NA, WHAT DO I DO???
last_badges = NA
last_tower_alerts = NA

#iterating through weeks to create state variables - WEEkS SQUARED MAKES IT HUGE!!
for (week in 1:nweeks) {
  
  #how do we get badges???
  state_var_df[week,] = c(week, last_tower_alerts, last_badges, last_tower_alerts^2, last_badges^2)
  
  #updating badges
  last_badges  = rnorm(1, mean = 2, sd = .5)
  last_badges = ifelse(last_badges < 0, 0, last_badges)
  last_tower_alerts  = rnorm(1, mean = 6, sd = 1)
  last_tower_alerts= ifelse(last_tower_alerts < 0, 0, last_tower_alerts)
}

#Simulated minutes_data frame
sim_mins_df = matrix(data = NA, nrow = nteachers , ncol = 40)


#iterating through teachers
for (teacher in 1:nteachers){
  
  #selecting thetas -CHANGE THESE TO NORMAL AROUND 0 - MAKE SURE VARIANCE IS POSITIVE
  mu_theta_df[teacher,] = c(teacher, rbeta(1, 4, 8), rbeta(1, 4, 8), rbeta(1, 4, 8), rbeta(1, 4, 8)) 
  sig_theta_df[teacher,] = c(teacher, rbeta(1, 4, 20), rbeta(1, 4, 20), rbeta(1, 4, 20),rbeta(1, 4, 20)) 
  
  #making first column teacher
  sim_mins_df[teacher,1] = teacher
  
  #iterating through weeks 
  for (week in 2:nweeks) {
    
    #hyperparameters get big with the growing weeks
    
    #final sim mu
  
    mu = as.numeric(mu_theta_df[teacher,2]*state_var_df[week,2]) +
      as.numeric(mu_theta_df[teacher,3]*state_var_df[week,3]) +
      as.numeric(mu_theta_df[teacher,4]*state_var_df[week,4]) +
      as.numeric(mu_theta_df[teacher,5]*state_var_df[week,5]) 
    
    #final sim sigma (linear combination)
    sigma = as.numeric(sig_theta_df[teacher,2]*state_var_df[week,2]) +
      as.numeric(sig_theta_df[teacher,3]*state_var_df[week,3]) +
      as.numeric(sig_theta_df[teacher,4]*state_var_df[week,4]) +
      as.numeric(sig_theta_df[teacher,5]*state_var_df[week,5]) 
    
    minutes = rnorm(1, mean = mu, sd = sigma)
    minutes = ifelse(minutes < 0, 0, minutes)
    
    #simualted minutes per wee
    sim_mins_df[teacher,week] = minutes
  }
  
}
