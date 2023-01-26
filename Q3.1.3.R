
# R SOLUTION
#
# ASSIGNMENT 3.1
# QUESTION 3

# if necessary: install package titanic

library(titanic)
tit <- titanic_train

ggplot(tit, aes(
  
  # Different colours for Survived/Dead
  fill = factor(Survived), 
  y = frequency(Sex),
  x = Sex
)) +
  geom_bar(stat = "identity") +
  labs(y = "count") +
  
  # Include legend
  scale_fill_discrete(name = "How did it go?", labels = c("dead", "alive"))

