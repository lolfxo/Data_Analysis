diff_of_means = function(data0, data1, h0, alpha, pop_mean0, pop_mean1, pop_var){
  # 0. Inputs:
  # data0 and data1 are vectors of continuous data.
  # h0 can take values "g", "e" and "l":
  #   "g": mu0 is greater than mu1 (decrease in effect)
  #   "e": mu0 is equal to mu1 (no difference).
  #   "l": mu0 is less than mu1 (increase in effect)
  # pop_mean0 and pop_mean1: this is in case the population means are known for samples data0 and data1.
  #   if missing these are taken as 0.
  # pop_var: this is the case when the population variance is known.
  
  # 1. Data:
  xbar0 = mean(data0)
  xbar1 = mean(data1)
  if (length(data0) > 30 & length(data1) > 30) {
    message("Data large enough for CLT to be in effect. Proceeding...")
  }
  
  if (missing(pop_mean0) & missing(pop_mean1)){
    pop_mean0 = 0
    pop_mean1 = 0
  }
  
  # 2. Test-Statistic: If population variance is not known, use sample standard deviation:
  tail = ifelse(tolower(h0)=="g" | tolower(h0)=="l",1 , 2)
  
  if (missing(pop_var)){
    s1 = sd(data0)
    s2 = sd(data1)
    degf = length(data0)+length(data1)-2
    
    test_stat = ((xbar0-xbar1)-(pop_mean0 - pop_mean1))/(s1^2/length(data0) + s2^2/length(data1))
    crit_value = qt(p=alpha/tail, df=degf, lower.tail=ifelse(tolower(h0)=="l",TRUE,FALSE))
    p_value = pt(test_stat, df=degf, lower.tail = FALSE)
    
  } else {
    test_stat = ((xbar0-xvar2)-(pop_mean0 - pop_mean1))/(pop_var[1]/length(data0) + pop_var[2]/length(data1))
    crit_value = qnorm(p=alpha/tail, mean=0, sd=1, lower.tail=ifelse(tolower(h0)=="l",TRUE,FALSE))
    p_value = pnorm(test_stat, mean=0, sd=1, lower.tail=FALSE)
  }
  
  # 3. Decision Rule
  if (tolower(h0)=="g" & (test_stat > crit_value)){
    action = "There is sufficient evidence to reject H0"  
  } else if (tolower(h0)=="l" & (test_stat < crit_value)){
    action = "There is sufficient evidence to reject H0"
  } else if (tolower(h0)=="e" & (abs(test_stat) > crit_value)){
    action = "There is sufficient evidence to reject H0"
  } else {
    action = "There is insufficient evidence to reject H0"
  }
  
  test_result = data.frame(action, p_value, tail, test_stat, crit_value)
  return(test_result)
}

####################################################################

diff_of_prop = function(data0, data1, success0, success1, h0, alpha){
  # 0. Inputs:
  # data0 and data1 are vectors of discrete data.
  # h0 can take values "g", "e" and "l":
  #   "g": prop0 is greater than prop1 (decrease in successes)
  #   "e": prop0 is equal to prop1 (no difference).
  #   "l": prop0 is less than prop1 (increase in successes)
  # pop_mean0 and pop_mean1: this is in case the population means are known for samples data0 and data1.
  #   if missing these are taken as 0.
  # pop_var: this is the case when the population variance is known.
  
  # 1. Data:
  prop0 = sum(data0 == success0)/length(data0)
  prop1 = sum(data1 == success1)/length(data1)
  
  # 2. Test-Statistic:
  tail = ifelse(tolower(h0)=="g" | tolower(h0)=="l",1 , 2)
  
  z = (prop0 - prop1)/sqrt(prop0*(1-prop0)/length(data0) + prop1*(1-prop1)/length(data1))
  crit_value = qnorm(p=alpha/tail, mean=0, sd=1, lower.tail=ifelse(tolower(h0)=="l",TRUE,FALSE))
  p_value = pnorm(z, mean=0, sd=1, lower.tail=FALSE)
  
  # 3. Decision Rule:
  # p0 > p1 (decrease in effect)
  if (tolower(h0)=="g" & (z > crit_value)){
    action = "There is sufficient evidence to reject H0"
  } else if (tolower(h0)=="l" & (z < crit_value)){
    action = "There is sufficient evidence to reject H0"
  } else if (tolower(h0)=="e" & (abs(z) > crit_value)){
    action = "There is sufficient evidence to reject H0"
  } else {
    action = "There is insufficient evidence to reject H0"
  }
  
  test_result = data.frame(action, p_value, tail, z, crit_value)
  return(test_result)
}