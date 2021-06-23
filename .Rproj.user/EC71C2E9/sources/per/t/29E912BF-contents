##GRAVEYARD





##SECOND without corona ##should i still include

##mailed ballots to all registered voters

mb_norona <- matchit(ballot_mailed_2020 ~ avg_income + population_density + asian + black + hispanic + median_age + change_regCVAP,
                     method = "nearest", 
                     replace = TRUE, 
                     ratio = 3, 
                     data = match_mailedballot)

mbnc_match <- match.data(mb_norona)

#estimate of ATE
mean(mbnc_match$change_turnoutpercent[mbnc_match$ballot_mailed_2020 == 1]) - mean(mbnc_match$change_turnoutpercent[mbnc_match$ballot_mailed_2020 == 0])

with(mbnc_match, t.test(change_turnoutpercent[ballot_mailed_2020 ==1],change_turnoutpercent[ballot_mailed_2020 ==0]))

# Plot propensity scores
plot(density(mbnc_match$distance[mbnc_match$ballot_mailed_2020==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(mbnc_match$distance[mbnc_match$ballot_mailed_2020==0]),lty=2)

mbnc_match_tab <- mbnc_match %>%
  group_by(ballot_mailed_2020) %>% 
  summarise_all(list(mean = mean), na.rm = TRUE) %>%
  t() %>% `colnames<-`(c("ballot mailed = no","yes"))


##mailed applications to all registered voters

appnorona_dat <- matchit(application_mailed_2020 ~ avg_income + population_density + asian + black + hispanic + median_age + change_regCVAP,
                         method = "nearest", 
                         replace = TRUE, 
                         ratio = 3, 
                         data = match_mailedapplication)

appnorona_match <- match.data(appnorona_dat)

#estimate of ATE
mean(appnorona_match$change_turnoutpercent[appnorona_match$application_mailed_2020 == 1]) - mean(appnorona_match$change_turnoutpercent[appnorona_match$application_mailed_2020 == 0])

with(appnorona_match, t.test(change_turnoutpercent[application_mailed_2020 ==1],change_turnoutpercent[application_mailed_2020 ==0]))

# Plot propensity scores
plot(density(appnorona_match$distance[appnorona_match$application_mailed_2020==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(appnorona_match$distance[appnorona_match$application_mailed_2020==0]),lty=2)

appnorona_match_tab <- appnorona_match %>%
  group_by(application_mailed_2020) %>% 
  summarise_all(list(mean = mean), na.rm = TRUE) %>%
  t() %>% `colnames<-`(c("ballot mailed = no","yes"))




##universal mail in ballots to all registered voters

uninorona_dat <- matchit(universal_mail ~ avg_income + population_density + asian + black + hispanic + median_age + change_regCVAP,
                         method = "nearest", 
                         replace = TRUE, 
                         ratio = 3, 
                         data = match_universal)

uninorona_match <- match.data(uninorona_dat)

#estimate of ATE
mean(uninorona_match$change_turnoutpercent[uninorona_match$universal_mail == 1]) - mean(uninorona_match$change_turnoutpercent[uninorona_match$universal_mail == 0])

with(uninorona_match, t.test(change_turnoutpercent[universal_mail ==1],change_turnoutpercent[universal_mail ==0]))

# Plot propensity scores
plot(density(uninorona_match$distance[uninorona_match$universal_mail==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(uninorona_match$distance[uninorona_match$universal_mail==0]),lty=2)

uninorona_match_tab <- uninorona_match %>%
  group_by(universal_mail) %>% 
  summarise_all(list(mean = mean), na.rm = TRUE) %>%
  t() %>% `colnames<-`(c("universal = no","yes"))






###############






mb_dat <- matchit(ballot_mailed_2020 ~ avg_income + covid_case_percap + population_density + asian + black + hispanic + white + votesplit_2016 + votesplit_2020,
                  method = "nearest", 
                  replace = TRUE, 
                  ratio = 2, 
                  data = match_mailedballot)


mb_match <- match.data(mb_dat)

length(unique(mb_match$state))
sum(mb_match$ballot_mailed_2020)

#estimate of ATE
mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 1]) - mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 0])

with(mb_match, t.test(change_turnoutpercent[ballot_mailed_2020 ==1],change_turnoutpercent[ballot_mailed_2020 ==0]))










##universal mail in ballots to all registered voters

match_universal <- match_data[match_data$ballot_mailed_2020 != 1, ]  
match_universal <- match_universal[match_universal$application_mailed_2020 != 1, ]
match_universal <- match_universal[match_universal$excuse_required_2020 != 1, ]


uni_tab <- match_universal %>%
  group_by(universal_mail) %>% 
  summarise_all(list(mean = mean), na.rm = TRUE) %>%
  t() %>% `colnames<-`(c("universal = no","yes"))

uni_dat <- matchit(universal_mail ~ covid_case_percap + covid_death_percap + population_density + hispanic + change_regCVAP + no_diploma,
                   method = "nearest", 
                   replace = TRUE, 
                   ratio = 3, 
                   data = match_universal)

uni_match <- match.data(uni_dat)

#estimate of ATE
mean(uni_match$change_turnoutpercent[uni_match$universal_mail == 1]) - mean(uni_match$change_turnoutpercent[uni_match$universal_mail == 0])

with(uni_match, t.test(change_turnoutpercent[universal_mail ==1],change_turnoutpercent[universal_mail ==0]))

# Plot propensity scores
plot(density(uni_match$distance[uni_match$universal_mail==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(uni_match$distance[uni_match$universal_mail==0]),lty=2)

uni_match_tab <- uni_match %>%
  group_by(universal_mail) %>% 
  summarise_all(list(mean = mean), na.rm = TRUE) %>%
  t() %>% `colnames<-`(c("universal = no","yes"))



##excuse required to vote by mail

match_excuse <- match_data[match_data$ballot_mailed_2020 != 1, ]  
match_excuse <- match_excuse[match_excuse$application_mailed_2020 != 1, ]
match_excuse <- match_excuse[match_excuse$universal_mail != 1, ]


excuse_tab <- match_excuse %>%
  group_by(excuse_required_2020) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_excuse = V1, excuse = V2) %>% 
  mutate(diff = no_excuse - excuse,
         diffPerc = diff / (excuse + no_excuse)) %>% 
  mutate_if(is.numeric, round, 3)

excuse_dat <- matchit(excuse_required_2020 ~ covid_death_percap + population_density + hispanic + gov_2020 + asian,
                      method = "nearest", 
                      replace = TRUE, 
                      ratio = 3, 
                      data = match_excuse)

excuse_match <- match.data(excuse_dat)

#estimate of ATE
mean(excuse_match$change_turnoutpercent[excuse_match$excuse_required_2020 == 1]) - mean(excuse_match$change_turnoutpercent[excuse_match$excuse_required_2020 == 0])

with(excuse_match, t.test(change_turnoutpercent[excuse_required_2020 ==1],change_turnoutpercent[excuse_required_2020 ==0]))

# Plot propensity scores
plot(density(excuse_match$distance[excuse_match$excuse_required_2020==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(excuse_match$distance[excuse_match$excuse_required_2020==0]),lty=2)

excuse_match_tab <- excuse_match %>%
  group_by(excuse_required_2020) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_excuse = V1, excuse = V2) %>% 
  mutate(diff = no_excuse - excuse,
         diffPerc = diff / (excuse + no_excuse)) %>% 
  mutate_if(is.numeric, round, 3)





##########aggregate universal and unique


matchtest <- match_data[match_data$excuse_required_2020 != 1, ]

matchtest <- matchtest[matchtest$application_mailed_2020 != 1, ]

matchtest$ballot_test <- matchtest$ballot_mailed_2020 + matchtest$universal_mail

test_tab <-  test_match  %>%
  group_by(ballot_test) %>%
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_ballot = V1, ballot_mailed = V2) %>% 
  mutate(diff = no_ballot - ballot_mailed,
         diffPerc = diff / (no_ballot + ballot_mailed)) %>% 
  mutate_if(is.numeric, round, 3)


test_dat <- matchit(ballot_test ~ asian +  legis_2020 + hispanic + gov_2020 + votesplit_2020 + covid_case_percap + black +covid_death_percap, 
                    method = "nearest", 
                    replace = TRUE, 
                    ratio = 3, 
                    data = matchtest)


test_match <- match.data(test_dat)

length(unique(test_match$state))
sum(test_match$ballot_mailed_2020)

#estimate of ATE
mean(test_match$change_turnoutpercent[test_match$ballot_test == 1]) - mean(test_match$change_turnoutpercent[test_match$ballot_mailed_2020 == 0])

with(test_match, t.test(change_turnoutpercent[ballot_test ==1],change_turnoutpercent[ballot_test ==0]))

# Plot propensity scores
plot(density(mb_match$distance[mb_match$ballot_mailed_2020==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(mb_match$distance[mb_match$ballot_mailed_2020==0]),lty=2)


mb_match_tab <-  mb_match %>%
  group_by(ballot_mailed_2020) %>%
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_ballot = V1, ballot_mailed = V2) %>% 
  mutate(diff = no_ballot - ballot_mailed,
         diffPerc = diff / (no_ballot + ballot_mailed)) %>% 
  mutate_if(is.numeric, round, 3)


ballot_table <- select(left_join(mailedballot_tab, mb_match_tab[-c(34:35), ],"variable"),c(1:3,6))

ballot_table <- ballot_table[-c(1:4,8:9,13,21:23,25,27,28,31,32),]
ballot_table <- ballot_table[,c(1,2,4,3)]



##OLS for mailed ballot
lm_ballot <- lm(change_turnoutpercent ~ ballot_mailed_2020 + asian + population_density + legis_2020 + hispanic + gov_2020 + votesplit_2020 + covid_case_percap + black, mb_match)

summary(lm_ballot)

