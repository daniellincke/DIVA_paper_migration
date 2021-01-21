library(data.table)

data <- read.csv("../results/country_output.csv")

data <- data[,c("caseid","locationid","locationname","time","coastlength","gdp","landloss_submergence","length_protected","migration_submergence","migrationcost_submergence","par","rslr","seafloodcost","seadike_cost","seadike_maintenance_cost","totalpop","ssp","pfl","dr","slr","seadikecost_factor","migr","migrcost","migrlevel")]
data <- data[data$time>=2015,]

data$migrationcost_submergence <- data$migrationcost_submergence * 5
data$migration_submergence <- data$migration_submergence * 5
data$landloss_submergence <- data$landloss_submergence * 5
data$par <- data$par * 5
data$seafloodcost <- data$seafloodcost * 5
data$protectioncost <- (data$seadike_cost + data$seadike_maintenance_cost) * 5

DT <- data.table(data)
DT[, gdp_cummulative := sum(gdp), by = c('caseid','locationid')]
DT[, migration_submergence_cummulative := sum(migration_submergence), by = c('caseid','locationid')]
DT[, migrationcost_submergence_cummulative := sum(migrationcost_submergence), by = c('caseid','locationid')]
DT[, landloss_submergence_cummulative := sum(landloss_submergence), by = c('caseid','locationid')]
DT[, par_cummulative := sum(par), by = c('caseid','locationid')]
DT[, seafloodcost_cummulative := sum(seafloodcost), by = c('caseid','locationid')]
DT[, protectioncost_cummulative := sum(protectioncost), by = c('caseid','locationid')]
data <- data.frame(DT)

data2100 <- data[data$time==2100,]
data2100$migrationcost_submergence_cummulative_relative <- (data2100$migrationcost_submergence_cummulative / data2100$gdp_cummulative) * 100
data2100 <- data2100[,c("caseid","locationid","locationname","migrationcost_submergence_cummulative_relative")]

data <- data[data$time==2015,]
dataArea <- read.csv("../tables/country_area.csv")
dataArea <- dataArea[,c("locationid","area")]
data <- merge(data,dataArea,by=c("locationid")) 
data$migration_submergence_cummulative_relative <- (data$migration_submergence_cummulative / data$totalpop) * 100
data$landloss_submergence_cummulative_relative <- (data$landloss_submergence_cummulative / data$area) * 100
data <- data[,c("caseid","locationid","locationname","time","coastlength","length_protected","ssp","pfl","dr","slr","seadikecost_factor","migr","migrcost","migrlevel","migration_submergence_cummulative","migrationcost_submergence_cummulative","landloss_submergence_cummulative","par_cummulative","totalpop","migration_submergence_cummulative_relative","landloss_submergence_cummulative_relative","seafloodcost_cummulative","protectioncost_cummulative")]

data <- merge(data,data2100,by=c("caseid","locationid","locationname"))

dataMainTable01 <- data[(data$migrcost=="Migrationcost 3.0" & data$migrlevel == "Migrationlevel   1.0"),]
dataMainTable10 <- data[(data$migrcost=="Migrationcost 3.0" & data$migrlevel == "Migrationlevel  10.0"),]

data <- rbind(dataMainTable01,dataMainTable10)

DT <- data.table(data)
DT[, migration_submergence_cummulative_mean := mean(migration_submergence_cummulative), by = c('locationid')]
DT[, migration_submergence_cummulative_median := median(migration_submergence_cummulative), by = c('locationid')]
DT[, migration_submergence_cummulative_min := min(migration_submergence_cummulative), by = c('locationid')]
DT[, migration_submergence_cummulative_max := max(migration_submergence_cummulative), by = c('locationid')]
DT[, migration_submergence_cummulative_relative_mean := mean(migration_submergence_cummulative_relative), by = c('locationid')]
DT[, migration_submergence_cummulative_relative_median := median(migration_submergence_cummulative_relative), by = c('locationid')]
DT[, migration_submergence_cummulative_relative_min := min(migration_submergence_cummulative_relative), by = c('locationid')]
DT[, migration_submergence_cummulative_relative_max := max(migration_submergence_cummulative_relative), by = c('locationid')]
DT[, landloss_submergence_cummulative_mean := mean(landloss_submergence_cummulative), by = c('locationid')]
DT[, landloss_submergence_cummulative_median := median(landloss_submergence_cummulative), by = c('locationid')]
DT[, landloss_submergence_cummulative_min := min(landloss_submergence_cummulative), by = c('locationid')]
DT[, landloss_submergence_cummulative_max := max(landloss_submergence_cummulative), by = c('locationid')]
DT[, landloss_submergence_cummulative_relative_mean := mean(landloss_submergence_cummulative_relative), by = c('locationid')]
DT[, landloss_submergence_cummulative_relative_median := median(landloss_submergence_cummulative_relative), by = c('locationid')]
DT[, landloss_submergence_cummulative_relative_min := min(landloss_submergence_cummulative_relative), by = c('locationid')]
DT[, landloss_submergence_cummulative_relative_max := max(landloss_submergence_cummulative_relative), by = c('locationid')]
DT[, par_cummulative_mean := mean(par_cummulative), by = c('locationid')]
DT[, par_cummulative_mean := median(par_cummulative), by = c('locationid')]
DT[, par_cummulative_min := min(par_cummulative), by = c('locationid')]
DT[, par_cummulative_max := max(par_cummulative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_mean := mean(migrationcost_submergence_cummulative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_median := median(migrationcost_submergence_cummulative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_min := min(migrationcost_submergence_cummulative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_max := max(migrationcost_submergence_cummulative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_relative_mean := mean(migrationcost_submergence_cummulative_relative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_relative_median := median(migrationcost_submergence_cummulative_relative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_relative_min := min(migrationcost_submergence_cummulative_relative), by = c('locationid')]
DT[, migrationcost_submergence_cummulative_relative_max := max(migrationcost_submergence_cummulative_relative), by = c('locationid')]
DT[, seafloodcost_cummulative_mean := mean(seafloodcost_cummulative), by = c('locationid')]
DT[, seafloodcost_cummulative_median := median(seafloodcost_cummulative), by = c('locationid')]
DT[, seafloodcost_cummulative_min := min(seafloodcost_cummulative), by = c('locationid')]
DT[, seafloodcost_cummulative_max := max(seafloodcost_cummulative), by = c('locationid')]
DT[, protectioncost_cummulative_mean := mean(protectioncost_cummulative), by = c('locationid')]
DT[, protectioncost_cummulative_median := median(protectioncost_cummulative), by = c('locationid')]
DT[, protectioncost_cummulative_min := min(protectioncost_cummulative), by = c('locationid')]
DT[, protectioncost_cummulative_max := max(protectioncost_cummulative), by = c('locationid')]
DT[, migration_submergence_cummulative_mean_per_mlmc := mean(migration_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_median_per_mlmc := median(migration_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_min_per_mlmc := min(migration_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_max_per_mlmc := max(migration_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_relative_mean_per_mlmc := mean(migration_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_relative_median_per_mlmc := median(migration_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_relative_min_per_mlmc := min(migration_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migration_submergence_cummulative_relative_max_per_mlmc := max(migration_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_mean_per_mlmc := mean(landloss_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_median_per_mlmc := median(landloss_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_min_per_mlmc := min(landloss_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_max_per_mlmc := max(landloss_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_relative_mean_per_mlmc := mean(landloss_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_relative_median_per_mlmc := median(landloss_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_relative_min_per_mlmc := min(landloss_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, landloss_submergence_cummulative_relative_max_per_mlmc := max(landloss_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, par_cummulative_mean_per_mlmc := mean(par_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, par_cummulative_median_per_mlmc := median(par_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, par_cummulative_min_per_mlmc := min(par_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, par_cummulative_max_per_mlmc := max(par_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_mean_per_mlmc := mean(migrationcost_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_median_per_mlmc := median(migrationcost_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_min_per_mlmc := min(migrationcost_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_max_per_mlmc := max(migrationcost_submergence_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_relative_mean_per_mlmc := mean(migrationcost_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_relative_median_per_mlmc := median(migrationcost_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_relative_min_per_mlmc := min(migrationcost_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, migrationcost_submergence_cummulative_relative_max_per_mlmc := max(migrationcost_submergence_cummulative_relative), by = c('locationid','migrcost','migrlevel')]
DT[, seafloodcost_cummulative_mean_per_mlmc := mean(seafloodcost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, seafloodcost_cummulative_median_per_mlmc := median(seafloodcost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, seafloodcost_cummulative_min_per_mlmc := min(seafloodcost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, seafloodcost_cummulative_max_per_mlmc := max(seafloodcost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, protectioncost_cummulative_mean_per_mlmc := mean(protectioncost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, protectioncost_cummulative_median_per_mlmc := median(protectioncost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, protectioncost_cummulative_min_per_mlmc := min(protectioncost_cummulative), by = c('locationid','migrcost','migrlevel')]
DT[, protectioncost_cummulative_max_per_mlmc := max(protectioncost_cummulative), by = c('locationid','migrcost','migrlevel')]
data <- data.frame(DT)

data[is.na(data)] <- 0

dataArea <- read.csv("../tables/country_area.csv")
dataArea <- dataArea[,c("locationid","area")]

data <- merge(data,dataArea,by=c("locationid")) 
data <- data[data$migrlevel == "Migrationlevel   1.0",]

data <- data[,c("locationid","caseid","locationname","time","coastlength","length_protected","ssp","pfl","dr","slr","seadikecost_factor","migr","migrcost","migrlevel","area","totalpop","migration_submergence_cummulative_mean","migration_submergence_cummulative_median","migration_submergence_cummulative_min","migration_submergence_cummulative_max","migration_submergence_cummulative_relative_mean","migration_submergence_cummulative_relative_median","migration_submergence_cummulative_relative_min","migration_submergence_cummulative_relative_max","landloss_submergence_cummulative_mean","landloss_submergence_cummulative_median","landloss_submergence_cummulative_min","landloss_submergence_cummulative_max","landloss_submergence_cummulative_relative_mean","landloss_submergence_cummulative_relative_median","landloss_submergence_cummulative_relative_min","landloss_submergence_cummulative_relative_max","par_cummulative_mean","par_cummulative_min","par_cummulative_max","migrationcost_submergence_cummulative_mean","migrationcost_submergence_cummulative_median","migrationcost_submergence_cummulative_min","migrationcost_submergence_cummulative_max","migrationcost_submergence_cummulative_relative_mean","migrationcost_submergence_cummulative_relative_median","migrationcost_submergence_cummulative_relative_min","migrationcost_submergence_cummulative_relative_max","seafloodcost_cummulative_mean","seafloodcost_cummulative_median","seafloodcost_cummulative_min","seafloodcost_cummulative_max","protectioncost_cummulative_mean","protectioncost_cummulative_median","protectioncost_cummulative_min","protectioncost_cummulative_max","migration_submergence_cummulative_mean_per_mlmc","migration_submergence_cummulative_median_per_mlmc","migration_submergence_cummulative_min_per_mlmc","migration_submergence_cummulative_max_per_mlmc","migration_submergence_cummulative_relative_mean_per_mlmc","migration_submergence_cummulative_relative_median_per_mlmc","migration_submergence_cummulative_relative_min_per_mlmc","migration_submergence_cummulative_relative_max_per_mlmc","landloss_submergence_cummulative_mean_per_mlmc","landloss_submergence_cummulative_median_per_mlmc","landloss_submergence_cummulative_min_per_mlmc","landloss_submergence_cummulative_max_per_mlmc","landloss_submergence_cummulative_relative_mean_per_mlmc","landloss_submergence_cummulative_relative_median_per_mlmc","landloss_submergence_cummulative_relative_min_per_mlmc","landloss_submergence_cummulative_relative_max_per_mlmc","par_cummulative_mean_per_mlmc","par_cummulative_median_per_mlmc","par_cummulative_min_per_mlmc","par_cummulative_max_per_mlmc","migrationcost_submergence_cummulative_mean_per_mlmc","migrationcost_submergence_cummulative_median_per_mlmc","migrationcost_submergence_cummulative_min_per_mlmc","migrationcost_submergence_cummulative_max_per_mlmc","migrationcost_submergence_cummulative_relative_mean_per_mlmc","migrationcost_submergence_cummulative_relative_median_per_mlmc","migrationcost_submergence_cummulative_relative_min_per_mlmc","migrationcost_submergence_cummulative_relative_max_per_mlmc","seafloodcost_cummulative_mean_per_mlmc","seafloodcost_cummulative_median_per_mlmc","seafloodcost_cummulative_min_per_mlmc","seafloodcost_cummulative_max_per_mlmc","protectioncost_cummulative_mean_per_mlmc","protectioncost_cummulative_median_per_mlmc","protectioncost_cummulative_min_per_mlmc","protectioncost_cummulative_max_per_mlmc")]

write.csv(data,"../results/country_output_cummulative_main_cases.csv",row.names=F)

