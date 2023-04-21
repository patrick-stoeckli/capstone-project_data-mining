library("swissparl")

df1 <- swissparl::get_data(
  table = "BusinessResponsibility",
  Language = "DE",
  BillNumber = 0,
  DepartmentNumber = 9
)

df2 <- swissparl::get_data(
  table = "Meeting",
  LegislativePeriodNumber = 49,
  Language = "DE"
)

df3 <- swissparl::get_data(
  table = "SubjectBusiness", 
  Language = "DE",
  BusinessNumber = as.numeric(df1$BusinessNumber)
)

df4 <- swissparl::get_data(
  table = "Transcript", 
  IdSubject = as.numeric(df3$IdSubject),
  Language = "DE"
)

df5 <- swissparl::get_data(
  table = "Transcript", 
  IdSubject = as.numeric(df3$IdSubject),
  IdSession = as.numeric(df2$IdSession),
  Language = "DE"
)
