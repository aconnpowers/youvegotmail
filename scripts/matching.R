source("scripts/libraries.R")

match_data <- read.csv("data/ACP_final_data.csv", stringsAsFactors=T) %>% 
  as.data.frame() %>%
  select(-c(1,))   %>% 
  select(-c(7,10:12,15:18,27:28,30,35:38,41:42,46:48))

match_data$legis_2020 = as.numeric(as.character(match_data$legis_2020))

match_data$gov_2020 = as.numeric(as.character(match_data$gov_2020))

match_data$control_2020 = as.numeric(as.character(match_data$control_2020))

match_data$demvotes_2016 = as.numeric(as.character(match_data$demvotes_2016))

match_data$repvotes_2016 = as.numeric(as.character(match_data$repvotes_2016))

match_data$demvotes_2020 = as.numeric(as.character(match_data$demvotes_2020))

match_data$repvotes_2020 = as.numeric(as.character(match_data$repvotes_2020))

match_data$police = as.numeric(as.character(match_data$police))

match_data$no_diploma = as.numeric(as.character(match_data$no_diploma))

match_data$ballot_mailed_2020 = as.numeric(as.character(match_data$ballot_mailed_2020))

match_data$asian = as.numeric(as.character(match_data$asian))

match_data <- match_data[complete.cases(match_data), ]




##mailed ballots to all registered voters

match_mailedballot <- match_data[match_data$application_mailed_2020 != 1, ]  

match_mailedballot <- match_mailedballot[match_mailedballot$universal_mail != 1, ]

match_mailedballot <- match_mailedballot[match_mailedballot$excuse_required_2020 != 1, ]

mailedballot_tab <-  match_mailedballot %>%
  group_by(ballot_mailed_2020) %>%
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_ballot = V1, ballot_mailed = V2) %>% 
  mutate(diff = no_ballot - ballot_mailed,
         diffPerc = diff / (no_ballot + ballot_mailed)) %>% 
  mutate_if(is.numeric, round, 3)
  

mb_dat <- matchit(ballot_mailed_2020 ~ asian + population_density + legis_2020 + hispanic + gov_2020 + votesplit_2020 + covid_case_percap + black, 
                   method = "nearest", 
                   replace = TRUE, 
                   ratio = 3, 
                   data = match_mailedballot)

  
mb_match <- match.data(mb_dat)

#estimate of ATT
mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 1]) - mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 0])

with(mb_match, t.test(change_turnoutpercent[ballot_mailed_2020 ==1],change_turnoutpercent[ballot_mailed_2020 ==0]))

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

ballot_table <- ballot_table[-c(1:4,8:9,13,18:19,21:23,25,27,28,31,32),]
ballot_table <- ballot_table[,c(1,2,4,3)]

rows_table <- c("Average income","COVID-19 cases per capita","COVID-19 deaths per capita","Democratic vote share","Republican vote share","Population density","Percent Asian","Percent Black","Percent Hispanic","Percent white","Average age","Margin of victory","Voter turnout 2020","Control of Legislature","Governor political party","Percent without high school diploma")

ballot_table$variable <- rows_table

kbl_ballot <- ballot_table %>% 
  kable(format = "html",
    col.names = c("","Prematch control mean","Postmatch control mean","Treatment mean")) %>%
  kable_styling(full_width = F,
                row_label_position = "c") %>%
  add_header_above(c(" " = 1, "Table 1: Mailed ballots to registered voters" = 3))

column_spec(kbl_ballot, 1:4, width = "4cm") %>%
  save_kable("output/kbl_ballot.png",
             bs_theme = "paper",
             zoom = 1.5)

table2 <- matrix(c(mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 1]) - mean(mb_match$change_turnoutpercent[mb_match$ballot_mailed_2020 == 0]), length(mb_match$state),length(unique(mb_match$state))), ncol=3,byrow=TRUE)
colnames(table2) <- c("ATT","Counties","States")

table2 <- table2 %>% 
  kable(format = "html",
        col.names = c("ATT","Counties","States")) %>%
  kable_styling(full_width = F,
                row_label_position = "c") %>%
  add_header_above(c("Table 2: Effect of Mailed Ballots in 2020 Presidential Election" = 3))

column_spec(table2, 1:3, width = "4cm") %>%
  save_kable("output/table2.png",
             bs_theme = "paper",
             zoom = 1.5)


##OLS for mailed ballot
lm_ballot <- lm(change_turnoutpercent ~ ballot_mailed_2020 + asian + black + hispanic +population_density + legis_2020 +  gov_2020 + votesplit_2020 + covid_case_percap, mb_match)

summary(lm_ballot)

stargazer(lm_ballot,  type = "html",
          title = "Table 3: OLS results for mailed ballots",
          dep.var.caption = NULL,
          dep.var.labels = "Change in Turnout",
          out = "output/table3.html",
          style = "default",
          omit.stat=c("f", "ser"),
          covariate.labels = c("Ballot mailed","Percent Asian","Percent Black","Percent Hispanic","Population density","Control of legislature","Governor political party","Margin of victory","COVID-19 cases per capita"))


##
##
##
##mailed applications to all registered voters

match_mailedapplication <- match_data[match_data$ballot_mailed_2020 != 1, ]  

match_mailedapplication <- match_mailedapplication[match_mailedapplication$universal_mail != 1, ]

match_mailedapplication <- match_mailedapplication[match_mailedapplication$excuse_required_2020 != 1, ]

mailedapp_tab <-  match_mailedapplication %>%
  group_by(application_mailed_2020) %>%
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_app = V1, app_mailed = V2) %>% 
  mutate(diff = no_app - app_mailed,
         diffPerc = diff / (no_app + app_mailed)) %>% 
  mutate_if(is.numeric, round, 3)

app_dat <- matchit(application_mailed_2020 ~ black + hispanic + votesplit_2020 + no_diploma, 
                  method = "nearest", 
                  replace = TRUE, 
                  ratio = 3, 
                  data = match_mailedapplication)

app_match <- match.data(app_dat)

length(unique(app_match$state))
sum(app_match$application_mailed_2020)

#estimate of ATE
mean(app_match$change_turnoutpercent[app_match$application_mailed_2020 == 1]) - mean(app_match$change_turnoutpercent[app_match$application_mailed_2020 == 0])

with(app_match, t.test(change_turnoutpercent[application_mailed_2020 ==1],change_turnoutpercent[application_mailed_2020 ==0]))

# Plot propensity scores
plot(density(app_match$distance[app_match$application_mailed_2020==1]), main = "Propensity score distribution, treatment and control groups") 
lines(density(app_match$distance[app_match$application_mailed_2020==0]),lty=2)

app_match_tab <- app_match %>%
  group_by(application_mailed_2020) %>%
  summarise_all(funs(mean(., na.rm = T))) %>% 
  t() %>% 
  as.data.frame() %>% 
  add_rownames("variable") %>% 
  rename(no_app = V1, app_mailed = V2) %>% 
  mutate(diff = no_app - app_mailed,
         diffPerc = diff / (no_app + app_mailed)) %>% 
  mutate_if(is.numeric, round, 3)

##OLS for mailed app
lm_applic <- lm(change_turnoutpercent ~ application_mailed_2020 + black + hispanic + votesplit_2020 + no_diploma, app_match)

summary(lm_applic)

stargazer(lm_applic,  type = "html",
          title = "Table 6: OLS results for mailed applications",
          dep.var.caption = NULL,
          dep.var.labels = "Change in Turnout",
          out = "output/table6.html",
          style = "default",
          omit.stat=c("f", "ser"),
          covariate.labels = c("Application mailed","Percent Black","Percent Hispanic","Margin of victory","Percent without high school diploma"))


##
##
##
app_table <- select(left_join(mailedapp_tab, app_match_tab[-c(34:35), ],"variable"),c(1:3,6))

app_table <- app_table[-c(1:4,8:9,13,18:19,21:23,25,27,28,31,32),]
app_table <- app_table[,c(1,2,4,3)]

app_table$variable <- rows_table

kbl_app <- app_table %>% 
  kable(format = "html",
        col.names = c("","Prematch control mean","Postmatch control mean","Treatment mean")) %>%
  kable_styling(full_width = F,
                row_label_position = "c") %>%
  add_header_above(c(" " = 1, "Table 4: Mailed applications to registered voters" = 3))

column_spec(kbl_app, 1:4, width = "4cm") %>%
  save_kable("output/kbl_app.png",
             bs_theme = "paper",
             zoom = 1.5)

table5 <- matrix(
  c(
  mean(app_match$change_turnoutpercent[app_match$application_mailed_2020 == 1]) - mean(app_match$change_turnoutpercent[app_match$application_mailed_2020 == 0]),
  length(app_match$state), 
  length(unique(app_match$state))), ncol=3, byrow=TRUE)

colnames(table5) <- c("ATT","Counties","States")

table5 <- table5 %>% 
  kable(format = "html",
        col.names = c("ATT","Counties","States")) %>%
  kable_styling(full_width = F,
                row_label_position = "c") %>%
  add_header_above(c("Table 5: Effect of Mailed Applications in 2020 Presidential Election" = 3))

column_spec(table5, 1:3, width = "4cm") %>%
  save_kable("output/table5.png",
             bs_theme = "paper",
             zoom = 1.5)



