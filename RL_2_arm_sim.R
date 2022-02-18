library(tidyverse)
library(reshape2)

# 1. #have a multiplier on cost - each teacher has a different sensitivity to the cost

#cost multiplier per teacher on every column

# Alpha do the same thing as with C

# Beta do the same thing with as C, inlcude in data

# > summary(teacher_usage$min_per_week) PER TEACHER
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    4.70   30.20   52.12   76.20  669.60


# PER CLASSROOM
# > summary(classroom_student_usage$badges_per_week)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.100   2.400   2.477   3.500  76.000 

#DAT ATO SIMULATE: 1000 teachers, 40 weeks,
#simualted badges per week, effort, cost


#data generation specifications
nweeks <- 40
nteachers = 1000
nArms <- 2 #try a different here instead
banditArms <- c(1:nArms)

#from original code
#armRewardProbabilities <- c(0.1, 0.3, 0.5,0.8) #each arm needs its own reward probability

# alpha = .02
# beta <- 5 #inverse temperature, and with this
Qi <- 0 #initial Q values
currentQs <- vector(length = length(banditArms))
weekQs <- matrix(data = NA, nrow = nweeks, ncol = nArms)
choiceProbs <- vector(length = length(banditArms))
weekChoiceProbs <- matrix(data = NA, nrow = nweeks, ncol = nArms)
choices <- vector(length = nweeks)
rewards <- vector(length = nweeks)
badges <- vector(length = nweeks)
cost <- vector(length = nweeks)


#assign initial Q value
for (arm in banditArms) {
  currentQs[arm] <- Qi
}


teachers = matrix(data = NA, nrow = nteachers, ncol = nArms+1)

teachers_final_sim = data.frame(matrix(data = NA, nrow = nteachers*nweeks, ncol = nArms+12))

for (teacher in 1:nteachers){
  
  
  #Cost for each arm
  cost1 = 0
  cost2 = runif(1, .5, 1)
  
  #alpha for each teacher
  alpha = runif(1,0.000001,1)
  
  #beta for each teacher
  beta = runif(1,0.01,40)

  
  for (week in 1:nweeks) {
    
    #calculate sumExp for softmax function
    sumExp <- 0
    for (arm in banditArms) {
      sumExp <- sumExp + exp(beta * currentQs[arm])
    }
    #calculate choice probabilities
    for (arm in banditArms) {
      choiceProbs[arm] = exp(beta * currentQs[arm]) / sumExp
    }
    
    #save choice probabilities in matrix for later visualization
    weekChoiceProbs[week,] <- choiceProbs
    
    # choose action given choice probabilities, save in choices vector
    choices[week] <- sample(banditArms, size = 1, replace = FALSE, prob = choiceProbs)
    
    #sampling the probabilities every single teacher
    #deal with greater than 0 or less than 1
    arm1_dist = rnorm(1, mean = .2, sd = .1)
    arm1_dist = ifelse(arm1_dist < 0, 0, arm1_dist )
    arm1_dist = ifelse(arm1_dist > 1, 1, arm1_dist )
    arm1_dist
    
    arm2_dist = rnorm(1, mean = .5, sd = .125)
    arm2_dist = ifelse(arm2_dist < 0, 0, arm2_dist )
    arm2_dist = ifelse(arm2_dist > 1, 1, arm2_dist )
    armRewardProbabilities <- c(arm1_dist, arm2_dist)
    
    
    #given bandit arm choice, get reward outcome (based on armRewardProbabilities)
    #reward depends on the amount of effort given
    
    #for no effort (arm 1)
    if (armRewardProbabilities[choices[week]] == arm1_dist){
      # if they're in the no effort section then their cost is quite literally 0
      arm1_reward_base = rnorm(1, mean = 0.5, sd = 1) 
      arm1_reward_base = ifelse(arm1_reward_base < 0, 0, arm1_reward_base)
      
      arm1_cost = cost1
      
      #reward is the probability times that reward base - 1. could do binomial * 2; reward, or reward * probability
      #rewards[week] <- rbinom(1,size = 1,prob = armRewardProbabilities[choices[week]]) * arm1_reward_base arm1 - arm1_cost
      rewards[week] <- arm1_reward_base*armRewardProbabilities[choices[week]] #- arm1_cost
      badges[week] = arm1_reward_base*armRewardProbabilities[choices[week]]
      cost[week] = arm1_cost
    }
    else if (armRewardProbabilities[choices[week]] == arm2_dist){
      #setting reward from Q1 badges per week
      arm2_reward_base = rnorm(1, mean = 3, sd = 1) 
      arm2_reward_base = ifelse(arm2_reward_base < 0, 0, arm2_reward_base)
      
      #making fixed to start - can add variability if we want
      arm2_cost = cost2
      
      #reward is the probability times that reward base - 1. could do binomial * 2; reward, or reward * probability
      #rewards[week] <- rbinom(1,size = 1,prob = armRewardProbabilities[choices[week]]) * arm2_reward_base - arm2_cost
      rewards[week] <- arm2_reward_base*armRewardProbabilities[choices[week]] - arm2_cost
      badges[week] = arm2_reward_base*armRewardProbabilities[choices[week]]
      cost[week] = arm2_cost
    }
    
    #rewards[week] <- rbinom(1,size = 1,prob = armRewardProbabilities[choices[week]])
    
    #if we chose 0
    if (choices[week] == 1){
      #given reward outcome, update Q values
      currentQs[choices[week]] <- currentQs[choices[week]] + alpha * (rewards[week] - currentQs[choices[week]])
      
      #save Q values in matrix of all Q-values
      weekQs[week,] <- currentQs
    }
    else {
      
    }
 
    
    #combine choices and rewards into dataframe
    df <- data.frame(choices, rewards, badges, cost)
    
    # #save out data df as csv
    # fileName <- paste(data_out, "Generated_Data.csv",sep = "/")
    # write.csv(df,fileName, row.names = FALSE)
    
    #turn week choice probs into dataframe
    ChoiceProbs_df <- as.data.frame(weekChoiceProbs)
    
    #add column names
    for (i in 1:length(ChoiceProbs_df)){
      colnames(ChoiceProbs_df)[i] <- paste("Arm", i, sep="")
    }
    
    #add column of week counts
    ChoiceProbs_df$weekCount <- as.numeric(row.names(ChoiceProbs_df))
    
    #turn df into long format for plotting
    ChoiceProbs_long <- melt(ChoiceProbs_df, id = "weekCount")
  }

  teachers[teacher,] = c(teacher, weekChoiceProbs[40,])
  lower_bound = 1+(40*(teacher-1))
  upper_bound = 40+(40*(teacher-1))
  teachers_final_sim[c(lower_bound:upper_bound),] = c(teacher, alpha, beta, data.frame(choices), cost1, cost2, ChoiceProbs_df, df)
}

# WRIT THIS DF FOR MARCOS TO RUN DATA
# fileName <- paste(teachers_final_sim, "Generated_Data.csv",sep = "/")
# write.csv(df,fileName, row.names = FALSE)

teachers = data.frame(teachers)
colnames(teachers) = c('Teacher', 'Arm1', 'Arm2')
colnames(teachers_final_sim) = c('teacher', 'alpha', 'beta', 'arm_choice', 'cost1','cost2', 'Arm1', 'Arm2', 'week', 'effort', 'reward', 'badges', 'cost')


#THIS IS THE SIMULATED DATA@
write.csv(teachers_final_sim, 'teacher_sim.csv', row.names = FALSE)

teachers = teachers[order(teachers[,2], decreasing = FALSE),]
teachers$Teacher = 1:nteachers

ggplot(teachers) +
  geom_point(aes(x = Teacher, y = Arm1, color = 'Arm1')) +
  geom_point(aes(x = Teacher, y = Arm2, color = 'Arm2')) +
  labs(y= 'P')

#plot Q values over time
# ggplot(data=ChoiceProbs_long, aes(x = weekCount, y = value, color = variable)) +
#   #geom_smooth(se=F) +
#   geom_line(size = .5)+
#   ggtitle("Probability of Choosing Arm by week")
mean(rnorm(40, mean = .2, sd = .1))

