library(tidyverse)
library(arules)
library(arulesViz)

load(".RData")

bin_first <- first_vars %>%
  mutate(across(everything(), as.logical)) %>%
  as.data.frame()

trans <- transactions(bin_first)

summary(trans)


rules <- apriori(trans, 
                 parameter = list(supp = 0.05, 
                                  conf = 0.5,
                                  target = "rules"))

summary(rules)

rules_sorted <- arules::sort(rules, by = "lift")
inspect(head(rules_sorted, 20))

length(rules_sorted)

plot(rules_sorted, method = "graph", engine = "htmlwidget")

# save.image()
