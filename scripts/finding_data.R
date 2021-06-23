library(jsonlite)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)

data_json <- fromJSON('https://raw.githubusercontent.com/evangambit/JsonOfCounties/master/counties.json') #import
data_raw <- enframe(unlist(data_json)) #unlist the lists of lists

data_raw$name <- sub('\\.\\s', ' ', data_raw$name)

data_full <- data_raw %>%
  separate(name, into=c("state", "county", "var1", "var2", "var3", "var4","var5"), 
           sep="[.]", fill="right")%>%  #separate whatever you have in the first column on multiple columns
  unite("X", var1:var5, sep=".", na.rm=TRUE, remove=FALSE)%>% #get all the var components together into one variable
  select("state", "county", "X", "value")%>% #select only the var X and drop all the var1:var4 columns
  spread("X", "value", fill=NA) #reshape dataset from long to wide 


data_clean <- data_full %>% 
  select(-contains("zip")) %>%
  select(-contains("transit")) %>% 
  select(-contains("walk")) %>% 
  select(-contains("driving")) %>% 
  select(-contains("zip")) %>% 
  select(-contains("bls.200")) %>% 
  select(-contains("bls.2012")) %>% 
  select(-contains("latitude")) %>% 
  select(-contains("longitude")) %>% 
  select(-contains("confirmed.2021")) %>% #remove covid cases after election
  select(-contains("confirmed.2020-12")) %>%
  select(-contains("deaths.2021")) %>% #remove covid deaths after election
  select(-contains("deaths.2020-12")) %>% 
  select(-contains("age_demographics"))



## scraping for census data
state_census <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-01.csv")

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-02.csv"))

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-04.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-05.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-06.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-08.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-09.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-10.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-11.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-12.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-13.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-15.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-16.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-17.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-18.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-19.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-20.csv"))   #kansas

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-21.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-22.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-23.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-24.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-25.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-26.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-27.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-28.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-29.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-30.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-31.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-32.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-33.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-34.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-35.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-36.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-37.csv"))   #north carolina

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-38.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-40.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-41.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-42.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-44.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-45.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-46.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-47.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-48.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-49.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-50.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-51.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-53.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-54.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-55.csv"))  

state_census <- rbind(state_census, read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-56.csv"))

clean_census <- state_census %>% 
  unique() %>%
  filter(YEAR == 12)

##add and format FIPS info

clean_census$STATE <- str_pad(clean_census$STATE, width=2, side="left", pad="0")

clean_census$COUNTY <- str_pad(clean_census$COUNTY, width=3, side="left", pad="0")

clean_census$fips <- paste(clean_census$STATE,clean_census$COUNTY, sep = "")

trim_census <- clean_census %>% 
  select(c(7:9,25:27,94:97)) 

#join together

data_excelraw <- merge(data_clean, trim_census, by = "fips")

write.csv(data_excelraw,"data/ACP_data_excel.csv", row.names = TRUE)

#import manually coded dummy variables by state/county

state_codes <- read.csv("data/states.csv")

county_codes <- read.csv("data/counties.csv")

#voting age population 2016

CVAP_2016 <- read.csv("data/CVAP_county_2016.csv")
CVAP_2016 <- subset(CVAP_2016, LNTITLE == "Total")
CVAP_2016 <- CVAP_2016 %>% 
  select(2:4)


CVAP_2020 <- read.csv("data/CVAP_county_2020.csv")
CVAP_2020 <- subset(CVAP_2020, lntitle == "Total")
CVAP_2020 <- CVAP_2020 %>% 
  select(2:4)

data_condensed <- read.csv("data/ACP_condensed_data.csv")

data_coded <- left_join(data_condensed, state_codes, by = "state") 

data_coded <- left_join(data_coded, county_codes, by = "fips")

data_coded[is.na(data_coded)] <- 0

data_coded$universal_mail <- data_coded$universal_mail.x + data_coded$universal_mail.y
data_coded$ballot_mailed_2020 <- data_coded$ballot_mailed_2020.x + data_coded$ballot_mailed_2020.y
data_coded$application_mailed_2020 <- data_coded$application_mailed_2020.x + data_coded$application_mailed_2020.y
data_coded$excuse_required_2020 <- data_coded$excuse_required_2020.x + data_coded$excuse_required_2020.y


data_final <- data_coded %>%
  select(1:30,39:42)

#add citizen voting age population
data_final <- left_join(data_final, CVAP_2016, by = "fips")
data_final <- left_join(data_final, CVAP_2020, by = "fips")


#remove alaska, only estimates of vote
data_final <- data_final[data_final$state != "Alaska", ] 

data_final <- unique(data_final)

#aggregate vote totals by state level
data_final$demraw_2016 <- as.numeric(as.character(data_final$demraw_2016))
state_dems_2016 <- aggregate(data_final$demraw_2016, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    dems_state_2016 = x
  )

data_final$repraw_2016 <- as.numeric(as.character(data_final$repraw_2016))
state_rep_2016 <- aggregate(data_final$repraw_2016, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    rep_state_2016 = x
  )

data_final$demraw_2020 <- as.numeric(as.character(data_final$demraw_2020))

state_dems_2020 <- aggregate(data_final$demraw_2020, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    dems_state_2020 = x
  )

data_final$repraw_2020 <- as.numeric(as.character(data_final$repraw_2020))
state_rep_2020 <- aggregate(data_final$repraw_2020, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    rep_state_2020 = x
  )

data_final$elections_2016_total <- as.numeric(as.character(data_final$elections_2016_total))
state_total_2016 <- aggregate(data_final$elections_2016_total, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    state_total_2016 = x
  )

data_final$elections_2020_total <- as.numeric(as.character(data_final$elections_2020_total))
state_total_2020 <- aggregate(data_final$elections_2020_total, by=list(Category=data_final$state), FUN=sum) %>%
  rename(
    state = Category,
    state_total_2020 = x
  )



state_total_2016 <- left_join(state_total_2016, state_dems_2016, by ="state")
state_total_2016 <- left_join(state_total_2016, state_rep_2016, by ="state") 
  
state_total_2020 <- left_join(state_total_2020, state_dems_2020, by ="state")
state_total_2020 <- left_join(state_total_2020, state_rep_2020, by ="state") 
  
state_total_2016$dem_percent_state <- state_total_2016$dems_state_2016/state_total_2016$state_total_2016
state_total_2016$rep_percent_state <- state_total_2016$rep_state_2016/state_total_2016$state_total_2016

state_total_2020$dem_percent_state <- state_total_2020$dems_state_2020/state_total_2020$state_total_2020
state_total_2020$rep_percent_state <- state_total_2020$rep_state_2020/state_total_2020$state_total_2020

state_total_2016$votesplit_2016 <- abs(state_total_2016$dem_percent_state - state_total_2016$rep_percent_state)
state_total_2020$votesplit_2020 <- abs(state_total_2020$dem_percent_state - state_total_2020$rep_percent_state)

state_total_2016 <- state_total_2016 %>% select(1,7)
state_total_2020 <- state_total_2020 %>% select(1,7)

data_final <- left_join(data_final, state_total_2020, by ="state")
data_final <- left_join(data_final, state_total_2016, by ="state")


##registration data 

registration <- read.csv("data/registration.csv") 
data_final <- left_join(data_final, registration, by ="fips")

i <- c(4:42)

data_final[ , i] <- apply(data_final[ , i], 2,            # Specify own function within apply
                          function(x) as.numeric(as.character(x)))


data_final[is.na(data_final)] <- 0

##north dakota doesn't have registration, using CVAP instead
data_final$registration_2020 <- ifelse(data_final$registration_2020 == 0, data_final$cvap_est_2020, data_final$registration_2020)

data_final$registration_2016 <- ifelse(data_final$registration_2016 == 0, data_final$CVAP_EST_2016, data_final$registration_2016)


data_final$turnout2020 <- data_final$elections_2020_total/data_final$registration_2020

data_final$turnout2016 <- data_final$elections_2016_total/data_final$registration_2016

data_final$change_turnoutpercent <- data_final$turnout2020 - data_final$turnout2016 

#variable for total registered over total cvap per each year
data_final$reg_over_CVAP_2016 <- data_final$registration_2016/data_final$CVAP_EST_2016

data_final$reg_over_CVAP_2020 <- data_final$registration_2020/data_final$cvap_est_2020

data_final$change_regCVAP <- data_final$reg_over_CVAP_2020 - data_final$reg_over_CVAP_2016

poli_control <- read.csv("data/legis_control_2016_2020.csv")

poli_control <- poli_control %>% 
  select(1:4)

data_final <- left_join(data_final, poli_control, by = "state") 

edu <- read.csv("data/education.csv")

data_final <- left_join(data_final, edu, by = "fips") 

write.csv(data_final,"data/ACP_final_data.csv", row.names = TRUE)

