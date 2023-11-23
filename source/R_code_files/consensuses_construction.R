######################################################################
# CHOOSE THE VERSION OF THE MCC APPROACH TO USE AND SET MT PARAMETER #
######################################################################

ConsMthd <- readline("Consensus method (1 to 5): ")

if (ConsMthd=='1')
{
  source("algorithm_1.R")
}else{
  MT <- as.numeric(readline("Merging threshold (0.1 to 1): "))
#  if (is.na(MT)){MT <- 0.4}
  if (ConsMthd=='2'){source("algorithm_2.R")}
  else if (ConsMthd=='3'){source("algorithm_3.R")}
  else if (ConsMthd=='4'){source("algorithm_4.R")}
  else if (ConsMthd=='5'){source("algorithm_5.R")}
  else {print("No such method.")}
}
rm(ConsMthd)