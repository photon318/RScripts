setwd("/Users/Alexz/Documents/GitHub/RScripts/Data")

dataset = read.csv("Salary_Data.csv")

#install.packages("caTools")
library(caTools)

set.seed(123)

split = sample.split(dataset$Salary, 2/3)
training_set = subset(dataset, split == TRUE )
test_set = subset(dataset, split == FALSE )


regressor  = lm(formula = Salary~YearsExperience,
                 data = training_set)



                                        

y_pred = predict( regressor, newdata = test_set)

#visualization


#install.packages("ggplot2")

library(ggplot2)

ggplot() +
 geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
               colour = 'red') + 
geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, 
                                                            newdata = training_set)), colour = 'blue') +

ggtitle('SALARY vs EXPERIENCE') + 
xlab('Years') + 
ylab('Salary')


ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') + 
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, 
                                                              newdata = training_set)), colour = 'blue') +
  
  ggtitle('SALARY vs EXPERIENCE (prediction)') + 
  xlab('Years') + 
  ylab('Salary')
