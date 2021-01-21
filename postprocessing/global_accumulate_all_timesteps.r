library(data.table)
library(dplyr)

data <- read.csv("../results/global_output.csv")
data2015 <- data[data$time==2010,]
data <- data[data$time>=2015,]

data <- data[,c("caseid","locationid","locationname","time","landloss_submergence","migration_submergence","rslr","seafloodcost","totalpop","ssp","pfl","dr","slr","migr","migrcost","migrlevel")]
data$landloss_submergence <- data$landloss_submergence * 5
data$migration_submergence <- data$migration_submergence * 5

data <- data %>% 
  group_by(caseid) %>%
#  summarise(rslr = identity(rslr)) %>%
  mutate(landloss_submergence_cummulative = cumsum(landloss_submergence), migration_submergence_cummulative = cumsum(migration_submergence))

data2015 <- data2015[,c("caseid","rslr")]
data <- merge(data,data2015,by=c("caseid"))
data$rslr <- data$rslr.x - data$rslr.y
data <- data[,c("caseid","locationid","locationname","time","landloss_submergence_cummulative","migration_submergence_cummulative","rslr","seafloodcost","totalpop","ssp","pfl","dr","slr","migr","migrcost","migrlevel")]

write.csv(data,"../results/global_output_cummulative_all_timesteps.csv",row.names=F)
