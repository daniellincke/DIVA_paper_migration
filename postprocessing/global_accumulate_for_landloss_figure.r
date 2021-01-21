library(data.table)

data <- read.csv("../results/global_output.csv")

data <- data[,c("caseid","locationid","locationname","time","coastlength","landloss_submergence","length_protected","migration_submergence","migrationcost_submergence","par","rslr","seafloodcost","seadike_cost","seadike_maintenance_cost","totalpop","ssp","pfl","dr","slr","seadikecost_factor","migr","migrcost","migrlevel")]
data <- data[data$time>=2015,]

data <- data[data$migrlevel!="Migrationlevel 100.0",]
data <- data[data$migrcost=="Migrationcost 3.0",]

data$migrationcost_submergence <- data$migrationcost_submergence * 5
data$migration_submergence <- data$migration_submergence * 5
data$landloss_submergence <- data$landloss_submergence * 5
data$par <- data$par * 5
data$seafloodcost <- data$seafloodcost * 5
data$protectioncost <- (data$seadike_cost + data$seadike_maintenance_cost) * 5

DT <- data.table(data)
DT[, migration_submergence_cummulative := sum(migration_submergence), by = c('caseid')]
DT[, migrationcost_submergence_cummulative := sum(migrationcost_submergence), by = c('caseid')]
DT[, landloss_submergence_cummulative := sum(landloss_submergence), by = c('caseid')]
DT[, par_cummulative := sum(par), by = c('caseid')]
DT[, seafloodcost_cummulative := sum(seafloodcost), by = c('caseid')]
DT[, protectioncost_cummulative := sum(protectioncost), by = c('caseid')]
data <- data.frame(DT)
data <- data[data$time==2100,]

data$migration_submergence_cummulative_relative <- (data$migration_submergence_cummulative / data$totalpop) * 100
data <- data[,c("caseid","locationid","locationname","time","coastlength","length_protected","ssp","pfl","dr","slr","seadikecost_factor","migr","migrcost","migrlevel","migration_submergence_cummulative","migrationcost_submergence_cummulative","landloss_submergence_cummulative","par_cummulative","totalpop","migration_submergence_cummulative_relative","seafloodcost_cummulative","protectioncost_cummulative")]

DT <- data.table(data)
DT[, migration_submergence_cummulative_mean := mean(migration_submergence_cummulative), by = c('slr','migrlevel')]
DT[, migration_submergence_cummulative_min := min(migration_submergence_cummulative), by = c('slr','migrlevel')]
DT[, migration_submergence_cummulative_max := max(migration_submergence_cummulative), by = c('slr','migrlevel')]
DT[, migration_submergence_cummulative_relative_mean := mean(migration_submergence_cummulative_relative), by = c('slr','migrlevel')]
DT[, migration_submergence_cummulative_relative_min := min(migration_submergence_cummulative_relative), by = c('slr','migrlevel')]
DT[, migration_submergence_cummulative_relative_max := max(migration_submergence_cummulative_relative), by = c('slr','migrlevel')]
DT[, landloss_submergence_cummulative_mean := mean(landloss_submergence_cummulative), by = c('slr','migrlevel')]
DT[, landloss_submergence_cummulative_min := min(landloss_submergence_cummulative), by = c('slr','migrlevel')]
DT[, landloss_submergence_cummulative_max := max(landloss_submergence_cummulative), by = c('slr','migrlevel')]
DT[, par_cummulative_mean := mean(par_cummulative), by = c('slr','migrlevel')]
DT[, par_cummulative_min := min(par_cummulative), by = c('slr','migrlevel')]
DT[, par_cummulative_max := max(par_cummulative), by = c('slr','migrlevel')]
DT[, migrationcost_submergence_cummulative_mean := mean(migrationcost_submergence_cummulative), by = c('slr','migrlevel')]
DT[, migrationcost_submergence_cummulative_min := min(migrationcost_submergence_cummulative), by = c('slr','migrlevel')]
DT[, migrationcost_submergence_cummulative_max := max(migrationcost_submergence_cummulative), by = c('slr','migrlevel')]
DT[, seafloodcost_cummulative_mean := mean(seafloodcost_cummulative), by = c('slr','migrlevel')]
DT[, seafloodcost_cummulative_min := min(seafloodcost_cummulative), by = c('slr','migrlevel')]
DT[, seafloodcost_cummulative_max := max(seafloodcost_cummulative), by = c('slr','migrlevel')]
DT[, protectioncost_cummulative_mean := mean(protectioncost_cummulative), by = c('slr','migrlevel')]
DT[, protectioncost_cummulative_min := min(protectioncost_cummulative), by = c('slr','migrlevel')]
DT[, protectioncost_cummulative_max := max(protectioncost_cummulative), by = c('slr','migrlevel')]
data_new <- data.frame(DT)

data_new[is.na(data_new)] <- 0

write.csv(data_new,"../results/global_output_cummulative_for_landloss_figure.csv",row.names=F)
