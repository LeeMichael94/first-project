## Correlation in R
forex <- read.csv("C:\\forex_data\\EURUSD.csv", header =TRUE)
c<-forex[,5]
o<-forex[,2]
h<-forex[,3]
l<-forex[,4]
v <- forex[,6]
DataFrame <- data.frame(o,h,l,c,v)

head(forex)
plot(forex)

forex <- forex[,c(-7)]
plot(forex)

##Create scatterplot
#forex.cor = cor(forex)
#cor(forex, method="spearman")

#cr <- cor(forex)

cor.test(DataFrame$c, DataFrame$o)
cor.test(DataFrame$c, DataFrame$h)
cor.test(DataFrame$c, DataFrame$l)
cor.test(DataFrame$c, DataFrame$v)
