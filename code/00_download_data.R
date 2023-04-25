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

MemberParlGroup <- swissparl::get_data(   #PersonIdCode
  table = "MemberParlGroup",
  Language = "DE"
) 

save(MemberParlGroup, file = 'data/data_raw/MemberParlGroup_raw.Rdata') 

###

ParlGroup <- swissparl::get_data(
  table="ParlGroup",
  Language = "DE"
)

save(MemberParty, file = 'data/data_raw/ParlGroup_raw.Rdata') 

###
