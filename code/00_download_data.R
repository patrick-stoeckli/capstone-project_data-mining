#Install the package 'swissparl', see documentation: https://github.com/zumbov2/swissparl
install.packages("swissparl")

#Load package 'swissparl'
library(swissparl)

#########################################################################
#########################################################################

#Get data from the table 'BusinessResponsibility': -> BusinessNumber; DepartmentNumber
BusinessResponsibility <- swissparl::get_data(
  table = "BusinessResponsibility",
  Language = "DE"
)
save(BusinessResponsibility, file = 'data/data_raw/BusinessResponsibility_raw.Rdata')

#########################################################################

#Get data from the table 'SubjectBusiness': -> IdSubject; BusinessNumber; TitleDE
SubjectBusiness <- swissparl::get_data(
  table = "SubjectBusiness",
  Language = "DE"
)
save(SubjectBusiness, file = 'data/data_raw/SubjectBusiness_raw.Rdata')

#########################################################################

#Get data from the table 'BusinessRole': -> MemberCouncilNumber
BusinessRole <- swissparl::get_data(  
  table = "BusinessRole",     
  Language = "DE",
  BusinessTypeName = "Motion" 
)  
save(BusinessRole, file = 'data/data_raw/BusinessRole_raw.Rdata') 

#########################################################################
#########################################################################

#Get data from the table 'MemberCouncil': -> FirstName, LastName, MemberCouncilNumber
MemberCouncil <-  swissparl::get_data(
  table="MemberCouncil",
  Language = "DE",
)
save(MemberCouncil, file = 'data/data_raw/MemberCouncil_raw.Rdata') 

#########################################################################

#Get data from the table 'Rapporteur': -> XXX
Rapporteur <-  swissparl::get_data(
  table="Rapporteur",
  Language = "DE",
)
save(Rapporteur, file = 'data/data_raw/Rapporteur_raw.Rdata') 

#########################################################################
#########################################################################