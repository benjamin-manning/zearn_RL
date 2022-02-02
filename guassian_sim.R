library(tidyverse)
library(reshape2)


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

#iterating through weeks to create state variables - WEEkS SQUARED MAKES IT HUGE!!
for (week in 1:nweeks) {
  
  #how do we get badges???
  state_var_df[week,] = c(week, week, last_badges, week^2, last_badges^2)
  
  #updating badges
  last_badges  = rnorm(1,2,.5)
  last_badges = ifelse(last_badges < 0, 0, last_badges)
}

#Simulated minutes_data frame
sim_mins_df = matrix(data = NA, nrow = nteachers , ncol = 40)


#iterating through teachers
for (teacher in 1:nteachers){
  
  #selecting thetas - WHAT DISTRIBUTION SHOULD I USE!!
  mu_theta_df[teacher,] = c(teacher, rbeta(1, 4, 8), rbeta(1, 4, 8), rbeta(1, 4, 8), rbeta(1, 4, 8)) 
  sig_theta_df[teacher,] = c(teacher, rbeta(1, 4, 8), rbeta(1, 4, 8), rbeta(1, 4, 8),rbeta(1, 4, 8)) 
  
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
    
    #final sim sigma
    sigma = as.numeric(sig_theta_df[teacher,2]*state_var_df[week,2]) +
      as.numeric(sig_theta_df[teacher,3]*state_var_df[week,3]) +
      as.numeric(sig_theta_df[teacher,4]*state_var_df[week,4]) +
      as.numeric(sig_theta_df[teacher,5]*state_var_df[week,5]) 
    
    minutes = rnorm(1, mean = mu, sd = sigma)
    minutes = ifelse(minutes < 0, 0, minutes)
    
    sim_mins_df[teacher,week] = minutes
  }
  
}
