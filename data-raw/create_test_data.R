# create data used in tests from CoRiverNF package

index_flow <- as.matrix(CoRiverNF::cyAnnTot$LeesFerry)
nf_index_flow <- cbind(
  as.numeric(format(zoo::index(CoRiverNF::cyAnnTot$LeesFerry), "%Y")),
  index_flow
)

nf_mon_flow <- CoRiverNF::monthlyInt["1906/"]

save(nf_index_flow, nf_mon_flow, file = "tests/testthat/nf_test_data.rda")
