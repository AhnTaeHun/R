## 0~1까지 정규화

# install.packages("scales")

data<-read.csv(file.choose())

library(scales)
a<- rescale(data$x1)
b<- rescale(data$x2)
c<- rescale(data$x3)
d<- rescale(data$x4)
e<- rescale(data$x5)

main <- data.frame(a,b,c,d,e)

write.csv(main,"main.csv")
