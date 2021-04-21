source("scripts/libraries.R")
library(xtable)

match_data <- read.csv("data/ACP_final_data.csv")
match_data <- match_data %>% 
  select(-c(1)) 

match_data$change_1620 <- match_data$elections_2020_total - match_data$elections_2016_total 

treatment <- lm(votes_over_18plus ~ ballot_mailed_2020, data = match_data)
diff_in_diff <- lm(change_1620 ~ ballot_mailed_2020, data = match_data)

view(treatment)
