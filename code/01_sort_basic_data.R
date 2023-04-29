#Load package 'tidyverse'
library(tidyverse)

#Load datasets from created in .r_file '00_download_data'
load('data/data_raw/BusinessResponsibility_raw.Rdata')
load('data/data_raw/SubjectBusiness_raw.Rdata')
load('data/data_raw/BusinessRole_raw.Rdata') 
load('data/data_raw//MemberCouncil_raw.Rdata')
load('data/data_raw//Rapporteur_raw.Rdata')

#Filter out specific departments if wanted
BusinessResponsibility <- BusinessResponsibility %>% 
  #filter(DepartmentNumber == 9) %>% 
  select(BusinessNumber, DepartmentAbbreviation)

#Apply the department filter to the subjects if wanted
SubjectBusiness <- SubjectBusiness %>% 
  #filter(BusinessNumber %in% BusinessResponsibility$BusinessNumber) %>%
  select(IdSubject, BusinessNumber, TitleDE)

#Filter the MemberCouncilNumber
BusinessRole <- BusinessRole %>%   
  #filter(BusinessTypeName == "Motion") %>% #Only take motions into account
  filter(BusinessNumber %in% SubjectBusiness$BusinessNumber) %>% #Take over businesses defined beforehand
  filter(RoleName == "Urheber(-in)") %>% #Only regard observations that correspond the original motion
  filter(is.na(ParlGroupNumber) == TRUE) %>% #Do not take into account motions of parliamentarian groups
  filter(is.na(CantonNumber) == TRUE) %>% #Do not take into account motions of cantons
  filter(is.na(CommitteeNumber) == TRUE) %>% #Do not take into account motions of committees
  select(BusinessNumber, BusinessShortNumber, BusinessTitle, MemberCouncilNumber)

#Merge the datasets with the information about the businesses
df0 <- merge(SubjectBusiness, BusinessResponsibility, by = "BusinessNumber", all.x = TRUE)
df1 <- merge(BusinessRole, df0, by = "BusinessNumber", all.x = TRUE)

#Get the parliamentarian group of each parliament member
MemberCouncil <- MemberCouncil %>%
  select(FirstName, LastName, ParlGroupName)

#Get the MemberCouncilNumber of each parliament member
Rapporteur <- Rapporteur %>%
  select(FirstName, LastName, MemberCouncilNumber) %>%
  unique()

#Merge the datasets with the information about the parliamentarians
df2 <- merge(Rapporteur, MemberCouncil, by = c("FirstName", "LastName"), all.x = TRUE)

#Merge all datasets and save the file
Basic_data <- merge(df1, df2, by = "MemberCouncilNumber", all.x = TRUE)
save(Basic_data, file = 'data/data_raw/Basic_data.Rdata') 
