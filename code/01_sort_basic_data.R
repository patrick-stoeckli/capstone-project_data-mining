library("swissparl")
library(tidyverse)

load('data/data_raw/BusinessResponsibility_raw.Rdata')
load('data/data_raw/SubjectBusiness_raw.Rdata')
load('data/data_raw/BusinessRole_raw.Rdata')
load('data/data_raw/MemberParty_raw.Rdata')

BusinessResponsibility <- BusinessResponsibility %>% 
  filter(DepartmentNumber == 9) %>% 
  select(BusinessNumber, DepartmentNumber, DepartmentName, DepartmentAbbreviation)

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
  #filter(BusinessTypeName == "Motion")
  select(BusinessNumber, MemberCouncilNumber, ParlGroupNumber, CantonNumber, CommitteeNumber, BusinessShortNumber, BusinessTitle, BusinessType, BusinessTypeName)

MemberParty <- MemberParty %>%
  select(PersonIdCode, FirstName, LastName, PartyNumber, PartyName, PartyAbbreviation)

df1 <- merge(BusinessRole, SubjectBusiness, by = "BusinessNumber", all.x = TRUE)
df2 <- merge(df1, BusinessResponsibility, by = "BusinessNumber", all.x = TRUE)
df3 <- merge(df2, MemberParty, by.x = "MemberCouncilNumber", by.y = "PersonIdCode" ,all.x = TRUE)

save(df3, file = 'data/data_raw/Basic_raw.Rdata') 
