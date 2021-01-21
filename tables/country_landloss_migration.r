library(ggplot2)
library(scales)

#library(cowplot)

countryResults <- read.csv("../results/country_output_cummulative_main_cases.csv")

countryResults <- countryResults[countryResults$ssp=="SSP1",]
countryResults <- countryResults[countryResults$dr=="Discount rate 0.0",]
countryResults <- countryResults[countryResults$slr=="H++",]
countryResults <- countryResults[countryResults$landloss_submergence_cummulative_median>3000,]
countryResults <- countryResults[countryResults$time==2015,]

countryResults[countryResults$landloss_submergence_cummulative_min<0,]$landloss_submergence_cummulative_min = 0

levels(countryResults$locationname) <- c(levels(countryResults$locationname), "Russia")
countryResults[countryResults$locationid=="RUS",]$locationname <- as.factor("Russia")

levels(countryResults$locationname) <- c(levels(countryResults$locationname), "USA")
countryResults[countryResults$locationid=="USA",]$locationname <- as.factor("USA")

countryResults <- countryResults[,c("locationid","locationname","landloss_submergence_cummulative_median","landloss_submergence_cummulative_min","landloss_submergence_cummulative_max")]
countryResults <- countryResults[order(-countryResults$landloss_submergence_cummulative_median),]

print(countryResults)


countryResults <- read.csv("../results/country_output_cummulative_main_cases.csv")
countryResults$migration_submergence_cummulative_median <- countryResults$migration_submergence_cummulative_median / 1000
countryResults$migration_submergence_cummulative_min <- countryResults$migration_submergence_cummulative_min / 1000
countryResults$migration_submergence_cummulative_max <- countryResults$migration_submergence_cummulative_max / 1000

countryResults <- countryResults[countryResults$ssp=="SSP1",]
countryResults <- countryResults[countryResults$dr=="Discount rate 0.0",]
countryResults <- countryResults[countryResults$slr=="H++",]
countryResults <- countryResults[countryResults$time==2015,]
countryResults <- countryResults[countryResults$migration_submergence_cummulative_median>0.48,]

countryResults <- countryResults[,c("locationid","locationname","migration_submergence_cummulative_median","migration_submergence_cummulative_min","migration_submergence_cummulative_max")]
countryResults <- countryResults[order(-countryResults$migration_submergence_cummulative_median),]

print(countryResults)


countryResults <- read.csv("../results/country_output_cummulative_main_cases.csv")

countryResults <- countryResults[countryResults$ssp=="SSP1",]
countryResults <- countryResults[countryResults$dr=="Discount rate 0.0",]
countryResults <- countryResults[countryResults$slr=="H++",]
countryResults <- countryResults[countryResults$landloss_submergence_cummulative_relative_median>1.6,]
countryResults <- countryResults[countryResults$time==2015,]

#countryResults[countryResults$landloss_submergence_cummulative_relative_min<0,]$landloss_submergence_cummulative_relative_min = 0

countryResults <- countryResults[,c("locationid","locationname","landloss_submergence_cummulative_relative_median","landloss_submergence_cummulative_relative_min","landloss_submergence_cummulative_relative_max")]
countryResults <- countryResults[order(-countryResults$landloss_submergence_cummulative_relative_median),]

print(countryResults)


countryResults <- read.csv("../results/country_output_cummulative_main_cases.csv")

countryResults <- countryResults[countryResults$ssp=="SSP1",]
countryResults <- countryResults[countryResults$dr=="Discount rate 0.0",]
countryResults <- countryResults[countryResults$slr=="H++",]
countryResults <- countryResults[countryResults$time==2015,]
countryResults <- countryResults[countryResults$migration_submergence_cummulative_relative_median>23,]

countryResults <- countryResults[,c("locationid","locationname","migration_submergence_cummulative_relative_median","migration_submergence_cummulative_relative_min","migration_submergence_cummulative_relative_max")]
countryResults <- countryResults[order(-countryResults$migration_submergence_cummulative_relative_median),]

print(countryResults)

