This repository contains files used to produce the TDABM results presented in the Paper "Who cares (without being paid for it)? 
A topological data analysis of regional unpaid care inequalities observed using UK 2021 census data."

The files are categorised as follows
######################## Data ################################
The following three csv files contain data that is used by the TDABM model
- key_vars_v4.csv #Final Exploratory Dataset Presented in Paper
- exp_vars(full_v2).csv #Full complement of census 2021 output questions considered
- inc_vars.csv #Supplementary Variables used during analysis

######################## Schema ################################
Summary of the variables held within each of the data files
- data_schema.xlsx

####################### CPP Reader ###########################
Used in the R Script to improve run times
- BallMapper.cpp

####################### R Script #############################
Finalised code used to run model
- Ball Mapper Key Var (LA) v4 (Shared).R
