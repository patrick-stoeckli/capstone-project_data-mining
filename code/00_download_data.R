library("swissparl")

####

BusinessResponsibility <- swissparl::get_data(
  table = "BusinessResponsibility",
  Language = "DE"
)

save(BusinessResponsibility, file = 'data/data_raw/BusinessResponsibility_raw.Rdata')

####

SubjectBusiness <- swissparl::get_data(
  table = "SubjectBusiness",
  Language = "DE"
)

save(SubjectBusiness, file = 'data/data_raw/SubjectBusiness_raw.Rdata')

####

BusinessRole <- swissparl::get_data(  #MemberCouncilNumber
  table = "BusinessRole",     
  Language = "DE",
  BusinessTypeName = "Motion" 
)  
  
save(BusinessRole, file = 'data/data_raw/BusinessRole_raw.Rdata') 

###

MemberParty <- swissparl::get_data(   #PersonIdCode
  table = "MemberParty",
  Language = "DE"
) 

save(MemberParty, file = 'data/data_raw/MemberParty_raw.Rdata') 













# Business2 <- swissparl::get_data(
#   table = "Business", 
#   BusinessTypeName = "Motion", 
#   BusinessShortNumber = as.numeric(df3$BusinessShortNumber), 
#   Language = "DE")
# 
# Business2 <- swissparl::get_data(
#   table = "Business", 
#   SubmittedBy = "Badran Jacqueline", 
#   Language = "DE")

# Party <- swissparl::get_data("Party", Language = "DE")
# Person <- swissparl::get_data("Person", Language = "DE")

# df2 <- swissparl::get_data(
#   table = "Meeting",
#   LegislativePeriodNumber = 49,
#   Language = "DE"
# )