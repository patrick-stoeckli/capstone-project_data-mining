library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

load("C:/Users/patst/Documents/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_NR_final.Rdata")
load("C:/Users/patst/Documents/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_NR_final_2.Rdata")

speeches_NR_rq1 <- speeches_NR_final %>% 
  rename(npvalue = value) 

for (i in 1201:1651) {
  speeches_NR_rq1$npvalue[i] <- speeches_NR_final_2$value[i]
}

save(speeches_NR_rq1, file = 'data/data_processed/speeches_NR_rq1.Rdata')

###################################################

load("C:/Users/patst/Documents/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_SR_final.Rdata")
load("C:/Users/patst/Documents/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_SR_final_2.Rdata")

speeches_SR_rq1 <- speeches_SR_final %>% 
  rename(npvalue = value) 

for (i in 701:1252) {
  speeches_SR_rq1$npvalue[i] <- speeches_SR_final_2$value[i]
}

save(speeches_SR_rq1, file = 'data/data_processed/speeches_SR_rq1.Rdata')

###################################################

load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_NR_final_bias.Rdata")

speeches_NR_rq2 <- speeches_NR_final_bias %>% 
  rename(lrclvalue = bias) %>%
  mutate(lrclcopy = lrclvalue) %>%
  separate(col = lrclvalue, into = c("lrvalue", "clvalue"), sep = ",") %>%
  mutate(lrvalue = as.numeric(lrvalue)) %>%
  mutate(clvalue = as.numeric(clvalue)) 

# missing_obs <- speeches_NR_rq2[is.na(speeches_NR_rq2$lrvalue), ]
# missing_obs
# 
# missing_obs2 <- speeches_NR_rq2[is.na(speeches_NR_rq2$clvalue), ]
# missing_obs2

speeches_NR_rq2$clvalue[139] <- 0.9
speeches_NR_rq2$clvalue[1468] <- 0.1
    
save(speeches_NR_rq2, file = 'data/data_processed/speeches_NR_rq2.Rdata')

###################################################

load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_SR_final_bias.Rdata")

speeches_SR_rq2 <- speeches_SR_final_bias %>% 
  rename(lrclvalue = bias) %>%
  mutate(lrclcopy = lrclvalue) %>%
  separate(col = lrclvalue, into = c("lrvalue", "clvalue"), sep = ",") %>%
  mutate(lrvalue = as.numeric(lrvalue)) %>%
  mutate(clvalue = as.numeric(clvalue)) 

missing_obs <- speeches_SR_rq2[is.na(speeches_SR_rq2$lrvalue), ]
missing_obs

missing_obs2 <- speeches_SR_rq2[is.na(speeches_SR_rq2$clvalue), ]
missing_obs2

speeches_SR_rq2$clvalue[376] <- 0.6
speeches_SR_rq2$clvalue[694] <- 0.0

#311 and 700 are too long

save(speeches_SR_rq2, file = 'data/data_processed/speeches_SR_rq2.Rdata')

###################################################

load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_SR_final_text.Rdata")
load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_SR_rq3.Rdata")

for (i in 1:310) {
  speeches_SR_rq3$cfvalue[i] <- speeches_SR_final_text$cf[i]
}

for (i in 1:nrow(speeches_SR_rq3)){
  speeches_SR_rq3$fvalue[i] <- substr(speeches_SR_rq3$cfvalue[i], nchar(speeches_SR_rq3$cfvalue[i]), nchar(speeches_SR_rq3$cfvalue[i]))
  speeches_SR_rq3$cvalue[i] <- (as.numeric(substr(speeches_SR_rq3$cfvalue[i], nchar(1), nchar(1))))
}

na_indices <- which(is.na(speeches_SR_rq3$cvalue))

for (i in na_indices){
  value <- sub(".*Complexity: ([0-9]+).*", "\\1", speeches_SR_rq3$cfvalue[i])
  numeric_value <- as.numeric(value)
  speeches_SR_rq3$cvalue[i] <- value
}  

speeches_SR_rq3 <- speeches_SR_rq3 %>% 
  mutate(cvalue = as.numeric(cvalue)) %>%
  mutate(fvalue = as.numeric(fvalue)) 

missing_obs <- speeches_SR_rq3[is.na(speeches_SR_rq3$cvalue), ]
missing_obs

speeches_SR_rq3$cvalue[844] <- 5
speeches_SR_rq3$cvalue[594] <- 5
speeches_SR_rq3$cvalue[1145] <- 3 

missing_obs2 <- speeches_SR_rq3[is.na(speeches_SR_rq3$fvalue), ]
missing_obs2

speeches_SR_rq3$fvalue[844] <- 8
speeches_SR_rq3$fvalue[905] <- 9
speeches_SR_rq3$fvalue[1052] <- 7 
speeches_SR_rq3$fvalue[1086] <- 8 
speeches_SR_rq3$fvalue[1145] <- 8 

speeches_SR_rq3 <- speeches_SR_rq3[-c(311,700),]

save(speeches_SR_rq3, file = 'data/data_processed/speeches_SR_rq3.Rdata')

###

load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_NR_rq3_310.Rdata")

speeches_NR_rq3_a <- speeches_NR_rq3

load("~/data-mining-lumacss/capstone-project/capstone-project_data-mining/data/data_processed/speeches_NR_rq3.Rdata")
 
speeches_NR_rq3_b <- speeches_NR_rq3

for (i in 311:1651) {
  speeches_NR_rq3_a$cfvalue[i] <- speeches_NR_rq3_b$cfvalue[i]
}

speeches_NR_rq3 <- speeches_NR_rq3_a

for (i in 1:nrow(speeches_NR_rq3)){
  speeches_NR_rq3$fvalue[i] <- substr(speeches_NR_rq3$cfvalue[i], nchar(speeches_NR_rq3$cfvalue[i]), nchar(speeches_NR_rq3$cfvalue[i]))
  speeches_NR_rq3$cvalue[i] <- (as.numeric(substr(speeches_NR_rq3$cfvalue[i], nchar(1), nchar(1))))
}

na_indices <- which(is.na(speeches_NR_rq3$cvalue))

for (i in na_indices){
  value <- sub(".*Complexity: ([0-9]+).*", "\\1", speeches_NR_rq3$cfvalue[i])
  numeric_value <- as.numeric(value)
  speeches_NR_rq3$cvalue[i] <- value
}  

speeches_NR_rq3 <- speeches_NR_rq3 %>% 
  mutate(cvalue = as.numeric(cvalue)) %>%
  mutate(fvalue = as.numeric(fvalue)) 

missing_obs <- speeches_NR_rq3[is.na(speeches_NR_rq3$cvalue), ]
missing_obs

speeches_NR_rq3$cvalue[39] <- 5
speeches_NR_rq3$cvalue[365] <- 7

missing_obs2 <- speeches_NR_rq3[is.na(speeches_NR_rq3$fvalue), ]
missing_obs2

speeches_NR_rq3$fvalue[39] <- 8
speeches_NR_rq3$fvalue[267] <- 8 
speeches_NR_rq3$fvalue[335] <- 8 
speeches_NR_rq3$fvalue[365] <- 9
speeches_NR_rq3$fvalue[771] <- 7 

save(speeches_NR_rq3, file = 'data/data_processed/speeches_NR_rq3.Rdata')
