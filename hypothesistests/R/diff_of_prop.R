#' Function to test whether two populations or samples have a significant difference in their means.
#'
#' This function allows us to take two data vectors, define a null hypothesis and a significance level to tell us whether the difference in the mean of the two data vectors are statistically significant or not.
#' @param data0: data representing the null hypothesis; data1: data representing the alternative hypothesis; h0: g/e/l for where the mean of data0 is greater, equal to or less than the mean of data1; alpha: significance level at which we test the difference in means; pop_mean0:
#' @keywords: hypothesis testing
#' diff_of_prop()

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