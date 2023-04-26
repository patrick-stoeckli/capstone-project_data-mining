library(tidyverse)

load('data/data_raw/BusinessResponsibility_raw.Rdata')
load('data/data_raw/SubjectBusiness_raw.Rdata')
load('data/data_raw/BusinessRole_raw.Rdata') 
load('data/data_raw/ParlGroup_raw.Rdata')
load('data/data_raw/MemberParty_raw.Rdata')
load('data/data_raw//MemberParlGroup_raw.Rdata')
load('data/data_raw//Rapporteur_raw.Rdata')
load('data/data_raw//MemberCouncil_raw.Rdata')

BusinessResponsibility <- BusinessResponsibility %>% 
  filter(DepartmentNumber == 9) %>% 
  select(BusinessNumber)

SubjectBusiness <- SubjectBusiness %>% 
  filter(BusinessNumber %in% BusinessResponsibility$BusinessNumber) %>%
  select(IdSubject, BusinessNumber, TitleDE)

# BusinessRole <- BusinessRole %>%            #MemberCouncilNumber
#   filter(BusinessNumber %in% SubjectBusiness$BusinessNumber) %>%
#   #filter(BusinessTypeName == "Motion")
#   select(BusinessNumber, MemberCouncilNumber, BusinessShortNumber, BusinessTitle, BusinessType, BusinessTypeName)

BusinessRole <- BusinessRole %>%            #MemberCouncilNumber
  filter(BusinessNumber %in% SubjectBusiness$BusinessNumber) %>%
  filter(RoleName == "Urheber(-in)") %>%
  filter(is.na(ParlGroupNumber) == TRUE) %>%
  filter(is.na(CantonNumber) == TRUE) %>%
  filter(is.na(CommitteeNumber) == TRUE) %>%
  #filter(BusinessTypeName == "Motion")
  select(BusinessNumber, BusinessShortNumber, BusinessTitle, BusinessTypeName, MemberCouncilNumber, ParlGroupNumber)

df1 <- merge(BusinessRole, SubjectBusiness, by = "BusinessNumber", all.x = TRUE)

# MemberParty <- MemberParty %>%
#   select(PersonIdCode, FirstName, LastName, PartyNumber, PartyName, PartyAbbreviation)

# MemberParlGroup <- MemberParlGroup %>%
#   select(PersonIdCode, FirstName, LastName, ParlGroupNumber, ParlGroupName)

MemberCouncil <- MemberCouncil %>%
  select(PersonNumber, PersonIdCode, FirstName, LastName, PersonIdCode, ParlGroupName)

Rapporteur <- Rapporteur %>%
  select(FirstName, LastName, MemberCouncilNumber) %>%
  unique()

df2 <- merge(Rapporteur, MemberCouncil, by = c("FirstName", "LastName"), all.x = TRUE)

Basic_raw <- merge(df1, df2, by = "MemberCouncilNumber", all.x = TRUE)

save(Basic_raw, file = 'data/data_raw/Basic_raw.Rdata') 
