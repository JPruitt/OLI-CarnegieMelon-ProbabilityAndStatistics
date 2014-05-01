setwd("C:/Users/joseph.pruitt/Desktop/OLI/Probability & Statistics")

load("./depression.RData")

names(depression)
str (depression)

depression$Gender = replace(depression$Gender,depression$Gender==1,'Female') 
depression$Gender = replace(depression$Gender,depression$Gender==2,'Male')
depression

load("./friends.RData")

names(friends)
str(friends)
friends

t = table(friends)
t

percent = 100*t/sum(t)
percent

pie(t)
barplot(t)
barplot(percent)

pf = format(percent,digits=3)
lbl = paste(c("No difference","Opposite sex","Same sex"),pf,"%",sep="")
pie(t,label=lbl)

load("./actor.RData")

actor_age

hist(actor_age$Age,breaks=8) 
hist(actor_age$Age,breaks=5) 
hist(actor_age$Age,breaks=20) 

dotchart(actor_age$Age)
boxplot(actor_age$Age)
plot(actor_age)

str(actor_age)
summary(actor_age$Age)

summary(depression)

load("./graduation.RData")

summary(grad_data)
boxplot(grad_data)

load("./sdintuition.RData")

sapply(ratings, sd)

load("./drinking.RData")
data

hist(data$Alcohol)
summary(data$Alcohol)
sd(data$Alcohol, na.rm=T)

summary(data)

tbl<-table(data$Cheat)
tbl
tbl1<-100*tbl/sum(tbl)
tbl1

pie(tbl1)

load("./nightlight.RData")
t = table(nightlight)
t 
t1<-100*t / rowSums(t) 
t1

load("./height.RData")
plot(h$height,h$weight)
points(h$height[h$gender==1],h$weight[h$gender==1],col="red")
m = h[h$gender==0,]
f = h[h$gender==1,]
xaxis = c(min(h$height),max(h$height))
yaxis = c(min(h$weight),max(h$weight))
plot(m$height,m$weight,xlab="height",ylab="weight", main="Heights and Weights",xlim=xaxis,ylim=yaxis,col="blue") 
points(f$height,f$weight,col="red")
legend(55,225, pch=1, col=c("red","blue"),legend=c("females","males"))

load("./animals.RData")
plot(a$longevity,a$gestation) 
cor(a$longevity,a$gestation)
cor(a$longevity[a$animal!="elephant"],a$gestation[a$animal!="elephant"])

load("./olympics.RData")
plot(olym$Year, olym$Time) 
L = lm(olym$Time~olym$Year)
abline(L)
cf=coefficients(L)
legend(1940,260,legend=paste("time = ",round(cf[1],0),round(cf[2],2),"year")) 
plot(olym$Year[olym$Year!=1896], olym$Time[olym$Year!=1896]) 
L = lm(olym$Time[olym$Year!=1896]~olym$Year[olym$Year!=1896])
abline(L); cf=coefficients(L)
legend(1950,240,legend=paste("time = ",round(cf[1],0),round(cf[2],2),"year")) 
872-.33*2008

load("./body_image.RData")
data
summary(data)
plot(data$HS_GPA, data$GPA)
cor(data$HS_GPA, data$GPA, use="c")
L<-lm(data$GPA~data$HS_GPA)
abline(L)
cf<-coefficients(L)
lt<-paste("GPA = ", round(cf[1], 2), "+", round(cf[2], 2), "HS_GPA")
legend(1.7, 4.3, lt)
1.07+0.62*3.45

tbl<-table(data.frame(data$Gender, data$WtFeel))
tbl
tbl1<-100*tbl/rowSums(tbl)
tbl1

plot(factor(data$Seat), data$GPA)
tapply(data$GPA, factor(data$Seat), summary)
