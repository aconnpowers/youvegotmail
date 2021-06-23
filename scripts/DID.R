source("scripts/libraries.R")
library(tidyverse)
library(haven)
library(plm)
library(stargazer)

did_data <- read.csv("data/ACP_final_data.csv") %>% 
  as.data.frame()  %>% 
  select(-c(1)) 

i <- c(4:49)

did_data[ , i] <- apply(did_data[ , i], 2,            # Specify own function within apply
                          function(x) as.numeric(as.character(x)))

did_data[is.na(did_data)] <- 0

did_data$change_CVAPpercent <- did_data$votes_over_CVAP_2020 - did_data$votes_over_CVAP_2016 


##mailed ballots to all registered voters

did_mailedballot <- did_data[did_data$application_mailed_2020 != 1, ]  
did_mailedballot <- did_mailedballot[did_mailedballot$universal_mail != 1, ]

ggplot(did_mailedballot, aes(x = change_CVAPpercent, fill = as.factor(ballot_mailed_2020))) +
  geom_density(alpha=0.4) + 
  scale_fill_manual(values = c("blue","red"))

lm_did_mailedballot <- lm(votes_over_CVAP_2020 ~ ballot_mailed_2020 + votes_over_CVAP_2016 + ballot_mailed_2020*votes_over_CVAP_2016, data = did_mailedballot)

lm_did_mb_match <- lm(votes_over_CVAP_2020 ~ ballot_mailed_2020 + votes_over_CVAP_2016 + ballot_mailed_2020*votes_over_CVAP_2016, data = mb_match)

summary(lm_did_mailedballot)
summary(lm_did_mb_match)

##mailed applications to all registered voters

did_mailedapp <- did_data[did_data$ballot_mailed_2020 != 1, ]  
did_mailedapp <- did_mailedapp[did_mailedballot$universal_mail != 1, ]

ggplot(did_mailedapp, aes(x = change_CVAPpercent, fill = as.factor(application_mailed_2020))) +
  geom_density(alpha=0.4) + 
  scale_fill_manual(values = c("blue","red"))

lm_did_mailedapp <- lm(votes_over_CVAP_2020 ~ application_mailed_2020 + votes_over_CVAP_2016 + application_mailed_2020*votes_over_CVAP_2016, data = did_mailedapp)

lm_did_app_match <- lm(votes_over_CVAP_2020 ~ application_mailed_2020 + votes_over_CVAP_2016 + application_mailed_2020*votes_over_CVAP_2016, data = app_match)

summary(lm_did_mailedapp)
summary(lm_did_app_match)


##universal mail in ballots to all registered voters


did_uni <- did_data[did_data$ballot_mailed_2020 != 1, ]  
did_uni <- did_uni[did_mailedballot$application_mailed_2020 != 1, ]

ggplot(did_uni, aes(x = change_CVAPpercent, fill = as.factor(universal_mail))) +
  geom_density(alpha=0.4) + 
  scale_fill_manual(values = c("blue","red"))

lm_did_uni <- lm(votes_over_CVAP_2020 ~ universal_mail + votes_over_CVAP_2016 + universal_mail*votes_over_CVAP_2016, data = did_uni)

lm_did_uni <- lm(votes_over_CVAP_2020 ~ universal_mail + votes_over_CVAP_2016 + universal_mail*votes_over_CVAP_2016, data = uni_match)

summary(lm_did_uni)
summary(lm_did_uni)











##mailed ballots to all registered voters

ggplot(mb_match, aes(x = change_turnoutpercent, fill = as.factor(ballot_mailed_2020))) +
  geom_density(alpha=0.4) + 
  scale_fill_manual(values = c("blue","red"))

lm_did_mailedballot <- lm(change_turnoutpercent ~ ballot_mailed_2020 + votes_over_CVAP_2016 + ballot_mailed_2020*votes_over_CVAP_2016, data = did_mailedballot)

lm_did_mb_match <- lm(votes_over_CVAP_2020 ~ ballot_mailed_2020 + votes_over_CVAP_2016 + ballot_mailed_2020*votes_over_CVAP_2016, data = mb_match)

summary(lm_did_mailedballot)
summary(lm_did_mb_match)
