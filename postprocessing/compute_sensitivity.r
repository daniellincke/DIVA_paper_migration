library(data.table)

d <- read.csv("../results/global_output_cummulative_all_cases.csv")
d <- d[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migrcost","migration_submergence_cummulative","landloss_submergence_cummulative")]
d <- d[d$migrcost=="Migrationcost 3.0",]

d_mgrl_1p0 <- d[d$migrlevel=="Migrationlevel   1.0",]
d_mgrl_1p0 <- d_mgrl_1p0[,c("locationid","time","ssp","pfl","dr","slr","migrcost","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrl_1p0)[names(d_mgrl_1p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_1p0" 
names(d_mgrl_1p0)[names(d_mgrl_1p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_1p0" 

d_mgrl_10p0 <- d[d$migrlevel=="Migrationlevel  10.0",]
d_mgrl_10p0 <- d_mgrl_10p0[,c("locationid","time","ssp","pfl","dr","slr","migrcost","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrl_10p0)[names(d_mgrl_10p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_10p0" 
names(d_mgrl_10p0)[names(d_mgrl_10p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_10p0" 

d_mgrl_100p0 <- d[d$migrlevel=="Migrationlevel 100.0",]
d_mgrl_100p0 <- d_mgrl_100p0[,c("locationid","time","ssp","pfl","dr","slr","migrcost","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrl_100p0)[names(d_mgrl_100p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_100p0" 
names(d_mgrl_100p0)[names(d_mgrl_100p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_100p0" 

d <- merge(d_mgrl_1p0,d_mgrl_10p0,by=c("locationid","time","ssp","pfl","dr","slr","migrcost"))
d <- merge(d,d_mgrl_100p0,by=c("locationid","time","ssp","pfl","dr","slr","migrcost"))

d$landloss_change1_10 <-   (d$landloss_submergence_cummulative_1p0  - d$landloss_submergence_cummulative_10p0) * 100 / d$landloss_submergence_cummulative_1p0
d$landloss_change10_100 <- (d$landloss_submergence_cummulative_10p0 - d$landloss_submergence_cummulative_100p0)* 100 / d$landloss_submergence_cummulative_10p0
d$landloss_change1_100 <-  (d$landloss_submergence_cummulative_1p0  - d$landloss_submergence_cummulative_100p0)* 100 / d$landloss_submergence_cummulative_1p0

d$migration_change1_10 <-   (d$migration_submergence_cummulative_1p0  - d$migration_submergence_cummulative_10p0) * 100 / d$migration_submergence_cummulative_1p0
d$migration_change10_100 <- (d$migration_submergence_cummulative_10p0 - d$migration_submergence_cummulative_100p0)* 100 / d$migration_submergence_cummulative_10p0
d$migration_change1_100 <-  (d$migration_submergence_cummulative_1p0  - d$migration_submergence_cummulative_100p0)* 100 / d$migration_submergence_cummulative_1p0

print("landloss_change1_10")
print(quantile(d$landloss_change1_10, c(.05, .95)))

print("landloss_change10_100")
print(quantile(d$landloss_change10_100, c(.05, .95)))

print("migration_change1_10")
print(quantile(d$migration_change1_10, c(.05, .95)))

print("migration_change10_100")
print(quantile(d$migration_change10_100, c(.05, .95)))

write.csv(d,"../results/sensitivity_migrlevel.csv",row.names=F)


d <- read.csv("../results/global_output_cummulative_all_cases.csv")
d <- d[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migrcost","migration_submergence_cummulative","landloss_submergence_cummulative")]
d <- d[d$migrlevel!="Migrationlevel 100.0",]

d_mgrc_2p0 <- d[d$migrcost=="Migrationcost 2.0",]
d_mgrc_2p0 <- d_mgrc_2p0[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrc_2p0)[names(d_mgrc_2p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_2p0" 
names(d_mgrc_2p0)[names(d_mgrc_2p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_2p0" 

d_mgrc_3p0 <- d[d$migrcost=="Migrationcost 3.0",]
d_mgrc_3p0 <- d_mgrc_3p0[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrc_3p0)[names(d_mgrc_3p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_3p0" 
names(d_mgrc_3p0)[names(d_mgrc_3p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_3p0" 

d_mgrc_4p0 <- d[d$migrcost=="Migrationcost 4.0",]
d_mgrc_4p0 <- d_mgrc_4p0[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migration_submergence_cummulative","landloss_submergence_cummulative")]
names(d_mgrc_4p0)[names(d_mgrc_4p0)=="landloss_submergence_cummulative"] <- "landloss_submergence_cummulative_4p0" 
names(d_mgrc_4p0)[names(d_mgrc_4p0)=="migration_submergence_cummulative"] <- "migration_submergence_cummulative_4p0" 

d <- merge(d_mgrc_2p0,d_mgrc_3p0,by=c("locationid","time","ssp","pfl","dr","slr","migrlevel"))
d <- merge(d,d_mgrc_4p0,by=c("locationid","time","ssp","pfl","dr","slr","migrlevel"))

d$landloss_change3_2 <- (d$landloss_submergence_cummulative_2p0 - d$landloss_submergence_cummulative_3p0) * 100 / d$landloss_submergence_cummulative_3p0
d$landloss_change3_4 <- (d$landloss_submergence_cummulative_4p0 - d$landloss_submergence_cummulative_3p0) * 100 / d$landloss_submergence_cummulative_3p0

d$migration_change3_2 <- (d$migration_submergence_cummulative_2p0 - d$migration_submergence_cummulative_3p0) * 100 / d$migration_submergence_cummulative_3p0
d$migration_change3_4 <- (d$migration_submergence_cummulative_4p0 - d$migration_submergence_cummulative_3p0) * 100 / d$migration_submergence_cummulative_3p0

print("landloss_change3_2")
print(quantile(d$landloss_change3_2, c(.05, .95)))

print("landloss_change3_4")
print(quantile(d$landloss_change3_4, c(.05, .95)))

print("migration_change3_2")
print(quantile(d$migration_change3_2, c(.05, .95)))

print("migration_change3_4")
print(quantile(d$migration_change3_4, c(.05, .95)))


write.csv(d,"../results/sensitivity_migrcost.csv",row.names=F)
