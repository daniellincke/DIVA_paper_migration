data <- read.csv("../results/data_complete_reduced.csv")
data_cls <- read.csv("../results/cls_average_exposure_2015.csv")

data <- merge(data,data_cls,by=c("locationid","locationname","countryid"))
vars <- c("protect_m001_mc2_0_Migration","protect_m001_mc3_0_Migration","protect_m001_mc4_0_Migration","protect_m010_mc2_0_Migration","protect_m010_mc3_0_Migration","protect_m010_mc4_0_Migration","protect_m100_mc2_0_Migration","protect_m100_mc3_0_Migration","protect_m100_mc4_0_Migration")
#vars <- c("protect_m100_mc3_0_Migration")

for (prot_v in vars) {

    data_protectionpercentage00 <- data[data[,grepl(prot_v, colnames(data))]==0,] 
    data_protectionpercentage01 <- data[(data[,grepl(prot_v, colnames(data))]>0 & data[,grepl(prot_v, colnames(data))]<=33),] 
    data_protectionpercentage02 <- data[(data[,grepl(prot_v, colnames(data))]>33 & data[,grepl(prot_v, colnames(data))]<=67),] 
    data_protectionpercentage03 <- data[(data[,grepl(prot_v, colnames(data))]>67 & data[,grepl(prot_v, colnames(data))]<100),] 
    data_protectionpercentage04 <- data[(data[,grepl(prot_v, colnames(data))]==100),] 

    classes <- c("Never protect (0%)","Rather not protect (1%-33%)","Possible protect (34%-67%)","Rather protect (67%-99%)","Always protect (100%)")

    length_km <- c(round(sum(data_protectionpercentage00$length),digits=0),round(sum(data_protectionpercentage01$length),digits=0),round(sum(data_protectionpercentage02$length),digits=0),round(sum(data_protectionpercentage03$length),digits=0),round(sum(data_protectionpercentage04$length),digits=0))
    length_percent <- c(round(sum(data_protectionpercentage00$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage01$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage02$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage03$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage04$length)*100/sum(data$length),digits=2))
    area_below_h100_km2 <- c(round(sum(data_protectionpercentage00$area_below_h100),digits=0),round(sum(data_protectionpercentage01$area_below_h100),digits=0),round(sum(data_protectionpercentage02$area_below_h100),digits=0),round(sum(data_protectionpercentage03$area_below_h100),digits=0),round(sum(data_protectionpercentage04$area_below_h100),digits=0))
    area_below_h100_percent <- c(round(sum(data_protectionpercentage00$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage01$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage02$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage03$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage04$area_below_h100)*100/sum(data$area_below_h100),digits=2))
    pop_below_h100 <- c(round(sum(data_protectionpercentage00$pop_below_h100),digits=0),round(sum(data_protectionpercentage01$pop_below_h100),digits=0),round(sum(data_protectionpercentage02$pop_below_h100),digits=0),round(sum(data_protectionpercentage03$pop_below_h100),digits=0),round(sum(data_protectionpercentage04$pop_below_h100),digits=0))
    pop_below_h100_percent <- c(round(sum(data_protectionpercentage00$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage01$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage02$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage03$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage04$pop_below_h100)*100/sum(data$pop_below_h100),digits=2))
    assets_below_h100 <- c(round(sum(data_protectionpercentage00$assets_below_h100),digits=0),round(sum(data_protectionpercentage01$assets_below_h100),digits=0),round(sum(data_protectionpercentage02$assets_below_h100),digits=0),round(sum(data_protectionpercentage03$assets_below_h100),digits=0),round(sum(data_protectionpercentage04$assets_below_h100),digits=0))
    assets_below_h100_percent <- c(round(sum(data_protectionpercentage00$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage01$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage02$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage03$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage04$assets_below_h100)*100/sum(data$assets_below_h100),digits=2))

    pop_below_h100 <- pop_below_h100 / 1000000 
    assets_below_h100 <- assets_below_h100 / 1000000000

    length_percent <- round(length_percent, digits = 1)
    area_below_h100_percent <- round(area_below_h100_percent, digits = 1)
    pop_below_h100_percent <- round(pop_below_h100_percent, digits = 1)
    assets_below_h100_percent <- round(assets_below_h100_percent, digits = 1)
 
    pop_below_h100 <- round(pop_below_h100, digits = 1)
    assets_below_h100 <- round(assets_below_h100, digits = 1)
 
    length_percent[3] <- length_percent[3] + (100 - sum(length_percent))
    area_below_h100_percent[3] <- area_below_h100_percent[3] + (100 - sum(area_below_h100_percent))
    pop_below_h100_percent[3] <- pop_below_h100_percent[3] + (100 - sum(pop_below_h100_percent))
    assets_below_h100_percent[3] <- assets_below_h100_percent[3] + (100 - sum(assets_below_h100_percent))

    data_global <- data.frame(classes,length_km,length_percent,area_below_h100_km2,area_below_h100_percent, pop_below_h100, pop_below_h100_percent, assets_below_h100, assets_below_h100_percent)
    write.csv(data_global,paste(paste("results",prot_v,sep="_"),"csv",sep="."),row.names=F)
}


data <- read.csv("../results/data_complete_reduced_main_table.csv")
data_cls <- read.csv("../results/cls_average_exposure_2015.csv")

data <- merge(data,data_cls,by=c("locationid","locationname","countryid"))
vars <- c("protect_mc3_0_Migration")

for (prot_v in vars) {

    data_protectionpercentage00 <- data[data[,grepl(prot_v, colnames(data))]==0,] 
    data_protectionpercentage01 <- data[(data[,grepl(prot_v, colnames(data))]>0 & data[,grepl(prot_v, colnames(data))]<=33),] 
    data_protectionpercentage02 <- data[(data[,grepl(prot_v, colnames(data))]>33 & data[,grepl(prot_v, colnames(data))]<=67),] 
    data_protectionpercentage03 <- data[(data[,grepl(prot_v, colnames(data))]>67 & data[,grepl(prot_v, colnames(data))]<100),] 
    data_protectionpercentage04 <- data[(data[,grepl(prot_v, colnames(data))]==100),] 

    classes <- c("Never protect (0%)","Rather not protect (1%-33%)","Possible protect (34%-67%)","Rather protect (67%-99%)","Always protect (100%)")

    length_km <- c(round(sum(data_protectionpercentage00$length),digits=0),round(sum(data_protectionpercentage01$length),digits=0),round(sum(data_protectionpercentage02$length),digits=0),round(sum(data_protectionpercentage03$length),digits=0),round(sum(data_protectionpercentage04$length),digits=0))
    length_percent <- c(round(sum(data_protectionpercentage00$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage01$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage02$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage03$length)*100/sum(data$length),digits=2),round(sum(data_protectionpercentage04$length)*100/sum(data$length),digits=2))
    area_below_h100_km2 <- c(round(sum(data_protectionpercentage00$area_below_h100),digits=0),round(sum(data_protectionpercentage01$area_below_h100),digits=0),round(sum(data_protectionpercentage02$area_below_h100),digits=0),round(sum(data_protectionpercentage03$area_below_h100),digits=0),round(sum(data_protectionpercentage04$area_below_h100),digits=0))
    area_below_h100_percent <- c(round(sum(data_protectionpercentage00$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage01$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage02$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage03$area_below_h100)*100/sum(data$area_below_h100),digits=2),round(sum(data_protectionpercentage04$area_below_h100)*100/sum(data$area_below_h100),digits=2))
    pop_below_h100 <- c(round(sum(data_protectionpercentage00$pop_below_h100),digits=0),round(sum(data_protectionpercentage01$pop_below_h100),digits=0),round(sum(data_protectionpercentage02$pop_below_h100),digits=0),round(sum(data_protectionpercentage03$pop_below_h100),digits=0),round(sum(data_protectionpercentage04$pop_below_h100),digits=0))
    pop_below_h100_percent <- c(round(sum(data_protectionpercentage00$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage01$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage02$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage03$pop_below_h100)*100/sum(data$pop_below_h100),digits=2),round(sum(data_protectionpercentage04$pop_below_h100)*100/sum(data$pop_below_h100),digits=2))
    assets_below_h100 <- c(round(sum(data_protectionpercentage00$assets_below_h100),digits=0),round(sum(data_protectionpercentage01$assets_below_h100),digits=0),round(sum(data_protectionpercentage02$assets_below_h100),digits=0),round(sum(data_protectionpercentage03$assets_below_h100),digits=0),round(sum(data_protectionpercentage04$assets_below_h100),digits=0))
    assets_below_h100_percent <- c(round(sum(data_protectionpercentage00$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage01$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage02$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage03$assets_below_h100)*100/sum(data$assets_below_h100),digits=2),round(sum(data_protectionpercentage04$assets_below_h100)*100/sum(data$assets_below_h100),digits=2))

    pop_below_h100 <- pop_below_h100 / 1000000 
    assets_below_h100 <- assets_below_h100 / 1000000000

    length_percent <- round(length_percent, digits = 1)
    area_below_h100_percent <- round(area_below_h100_percent, digits = 1)
    pop_below_h100_percent <- round(pop_below_h100_percent, digits = 1)
    assets_below_h100_percent <- round(assets_below_h100_percent, digits = 1)
 
    pop_below_h100 <- round(pop_below_h100, digits = 1)
    assets_below_h100 <- round(assets_below_h100, digits = 1)
 
    length_percent[3] <- length_percent[3] + (100 - sum(length_percent))
    area_below_h100_percent[3] <- area_below_h100_percent[3] + (100 - sum(area_below_h100_percent))
    pop_below_h100_percent[3] <- pop_below_h100_percent[3] + (100 - sum(pop_below_h100_percent))
    assets_below_h100_percent[3] <- assets_below_h100_percent[3] + (100 - sum(assets_below_h100_percent))

    data_global <- data.frame(classes,length_km,length_percent,area_below_h100_km2,area_below_h100_percent, pop_below_h100, pop_below_h100_percent, assets_below_h100, assets_below_h100_percent)
    write.csv(data_global,paste(paste("results",prot_v,sep="_"),"csv",sep="."),row.names=F)
}


