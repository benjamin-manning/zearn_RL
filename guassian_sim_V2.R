library(tidyverse)
library(reshape2)

nweeks <- 40
nteachers = 100

#0. inflated tobit - formula changed - on hold
#1. two armed simple
#2. bernoulli do the indicator variable
#3. truncated normal


# 1. Initialize: alpha_theta >0, alpha_w > 0; theta_mu, theta_sigma, w (vectors size 4); 
# S (x) (which is the vector size 4 we call x’s — badges, tower alerts, squares…)

alpha_theta = .1
alpha_w = .1
gamma = .95

#thetas hyper
mu_theta_df = matrix(data = NA, nrow = nteachers*nweeks, ncol = 6)
colnames(mu_theta_df) = c('week', 'teacher', 'theta1', 'theta2', 'theta3', 'theta4')
sig_theta_df = matrix(data = NA, nrow = nteachers*nweeks, ncol = 6)
colnames(sig_theta_df) = c('week', 'teacher', 'theta1', 'theta2', 'theta3', 'theta4')


#state_variables_df THIS IS X
state_var_df = matrix(data = NA, nrow = nteachers*nweeks , ncol = 6)
colnames(state_var_df) = c('week', 'teacher','x1', 'x2', 'x3', 'x4')

#w starting
w_df = matrix(data = NA, nrow = nteachers*nweeks , ncol = 6)
colnames(w_df) = c('week', 'teacher', 'w1', 'w2', 'w3', 'w4')

#FIX HERE
sim_mins_df = matrix(data = NA, nrow = nteachers , ncol = 40)
# 2. Calculate mu and theta. Sample minutes from the resulting normal distribution.

#iterating through teachers
for (teacher in 1:nteachers){
  #DELETE teacher == whatver
  teacher = 1
  
  #selecting thetas MAKE ALL THESE 0 to START
  mu_theta_df[40* (teacher-1)+1,]  = c(1, teacher, 0, 0, 0, 0)
  sig_theta_df[40*(teacher-1)+1,]  = c(1, teacher, .001, .001, .001, .001)
  
  #starting state variables for each teacher (the same?)
  badges = 2
  tower_alerts = 6
  
  #adding first week state
  state_var_df[40*(teacher-1)+1,] = c(1, teacher, tower_alerts, badges, tower_alerts^2, badges^2)
  
  #initializing w for each teacher
  w_df[40*(teacher-1)+1,]= c(1, teacher,0, 0, 0, 0)
  
  #making first column teacher
  sim_mins_df[teacher,1] = teacher
  
  #iterating through weeks 
  for (week in 2:nweeks) {
    #DELETE WEEk == whatver
    week = 2

    #generatin badges
    badges  = rnorm(1, mean = 2, sd = .5)
    #keeping badges above 0
    badges = ifelse(badges < 0, 0, badges)
    #generating tower alerts
    tower_alerts  = rnorm(1, mean = 6, sd = 1)
    #keeping tower alerts above 0
    tower_alerts= ifelse(tower_alerts < 0, 0, tower_alerts)
    
    #update this weeks badges
    state_var_df[40*(teacher-1)+week,] = c(week, teacher, tower_alerts, badges, tower_alerts^2, badges^2)
    
    #final sim mu (linear combination) - last weeks mu and last weeks states
  
    mu = as.numeric(mu_theta_df[40*(teacher-1)+week-1,3]*state_var_df[40*(teacher-1)+week-1,3]) +
      as.numeric(mu_theta_df[40*(teacher-1)+week-1,4]*state_var_df[40*(teacher-1)+week-1,4]) +
      as.numeric(mu_theta_df[40*(teacher-1)+week-1,5]*state_var_df[40*(teacher-1)+week-1,5]) +
      as.numeric(mu_theta_df[40*(teacher-1)+week-1,6]*state_var_df[40*(teacher-1)+week-1,6]) 
    
    #final sim sigma (linear combination) - last weeks sigmas and last weeks states
    sigma = exp(as.numeric(sig_theta_df[40*(teacher-1)+week-1,3]*state_var_df[40*(teacher-1)+week-1,3]) +
      as.numeric(sig_theta_df[40*(teacher-1)+week-1,4]*state_var_df[40*(teacher-1)+week-1,4]) +
      as.numeric(sig_theta_df[40*(teacher-1)+week-1,5]*state_var_df[40*(teacher-1)+week-1,5]) +
      as.numeric(sig_theta_df[40*(teacher-1)+week-1,6]*state_var_df[40*(teacher-1)+week-1,6]))
    
    #generating minutes
    minutes = rnorm(1, mean = mu, sd = sigma)
    #making minutes greater than 0
    #minutes = ifelse(minutes < 0, 0, minutes)
    
    #simualted minutes per wee
    sim_mins_df[teacher,week] = minutes
    
    #3. calculate delta = badges for the NEXT week + gamma * 
    #(linear combination of w with x’s for the NEXT week) - 
    #(linear combination of w with x’s for the THIS week)
    #(oh and gamma = 0.95)
    
    delta = badges + gamma * 
      #this weeks states and last weeks w
      state_var_df[40*(teacher-1)+week,c(3:6)] %*% w_df[40*(teacher-1)+week-1,c(3:6)] - 
      #last weeks states and last weeks w
      state_var_df[40*(teacher-1)+week-1,c(3:6)] %*% w_df[40*(teacher-1)+week-1,c(3:6)]
    
    delta = delta[[1]]
    
    #4. update w. Like so: w = w + alpha_w * delta * X 
    #[this is like the updating function we did with each state before]
    w_df[40*(teacher-1)+week,c(1:2)] = c(week, teacher)
    
    w_df[40*(teacher-1)+week,c(3:6)] = 
      #lasts weeks w
      as.vector(w_df[40*(teacher-1)+week-1,c(3:6)]) +
      alpha_w * delta * 
      #last weeks states
      as.vector(state_var_df[40*(teacher-1)+week-1,c(3:6)])

    
    #5. update theta_mu. Like so: theta_mu = theta_mu + alpha_theta * delta*gamma^(week -1#) * 
    #1/(sigma)^2 * (minutes - mu) * x’s (IS THIS WEEK -1 ???)  [this is the first gradient formula from the picture]
    mu_theta_df[40*(teacher-1)+1 + (week-1), c(1:2)] = c(week, teacher)
    
    #updating this weeks mu
    mu_theta_df[40*(teacher-1)+1 + (week-1), c(3:6)] =
      #last week's mu
      mu_theta_df[40*(teacher-1) + (week-1),c(3:6)] + 
      #gamma is to the last weeks power
      alpha_theta * delta*gamma^(week-1) *
      1/(sigma^2) * (minutes - mu) *
      #last weeks states
      state_var_df[40*(teacher-1) + (week-1),c(3:6)]
    
    #6. update theta_sigma. Like so: theta_sigma = theta_sigma + alpha_theta * delta*gamma^(week -1 #) * 
    #((minutes - mu)^2/(sigma)^2 - 1) * x’s  [this is the second gradient formula from the picture]'
    
    #WHAT TO DO ABOUT SIGMA STARTING AT 0???
    sig_theta_df[40*(teacher-1)+1 + (week-1),c(1:2)] = c(week, teacher)
    
    sig_theta_df[40*(teacher-1)+1 + (week-1),c(3:6)] = 
      #last weeks sigma
      sig_theta_df[40*(teacher-1) + (week-1),c(3:6)] + 
      #gamma is to the last weeks power
      alpha_theta * delta * gamma^(week-1) *
      #chilling
     (((minutes-mu)^2/(sigma^2)) - 1) * 
      
      state_var_df[40*(teacher-1) + (week-1),c(2:5)]
  }
  
}


# 7. Loop accordingly until the end of the 40 weeks
