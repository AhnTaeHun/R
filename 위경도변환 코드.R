#install.packages("ggmap")
#install.packages("devtools")
library(devtools)
library(ggmap)

## 구글맵 api키 등록
register_google(key='AIzaSyCOrgTTOQFR-ptFgIamKJw8FRNe_ryNuxo')


data<-read.csv(file.choose(),header = TRUE)
head(data,1)

data$address <- enc2utf8(data$address)
data$address <- as.character(data$address)
data_lonlat <- mutate_geocode(data, address, source='google')
write.csv(data_lonlat,"cleaning_data.csv",row.names=TRUE)
