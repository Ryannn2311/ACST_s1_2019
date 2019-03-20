Github username: Ryannn2311
Public repository: ACST_s1_2019
File name: 45542899-Quiz1

#Q1
e=2.7183
Coupon=C
Face value=F
The number of coupon payments=n
#coupon C is paid every 6 months 
tn=n/2
tj=seq(0.5,tn,by=0.5)
y=c(y(0.5),y(1),y(1.5),...,y(tn))
P=sum(C*e^-(y*tj))+F*e^-(y(tn)*tn)

#Q3
(a)
dataset=read.csv(file.choose())

(b)
dataset=na.omit(dataset)
dataset

(c)
plot(dataset$time,dataset$gdp,main = "Singapore GDP growth",xlab = "Time",ylab = "GDP (%)")

(d)
m1 = mean(subset(dataset,period==1)$gdp, trim=0.10)
sd1 = sd(subset(dataset,period==1)$gdp)
m2 = mean(subset(dataset,period==2)$gdp, trim=0.10)
sd2 = sd(subset(dataset,period==2)$gdp)
m3 = mean(subset(dataset,period==3)$gdp, trim=0.10)
sd3 = sd(subset(dataset,period==3)$gdp)
stat.table<-matrix(c(m1,sd1,m2,sd2,m3,sd3),3,2)
colnames(stat.table)<-c("mean","standard deviation")
rownames(stat.table)<-c("per1","per2","per3")
stat.table

(e)
pairs(dataset[,-(1:2)])

(f)
G1=lm(dataset$gdp~dataset$exp,data = dataset)
summary(G1)
This linear regression established that Export growth rate could statistically  predict GDP, F(1, 108) = 43.66, p = 1.524e-09
and Export growth rate accounted for 28.133% of the explained variability in GDP.
The p-values are both small (less than 0.05) so the coefficients is statiscally significant.
The regression equation predicted GDP = -1.19832+0.19076*(Export growth rate).

(g)
G2=lm(dataset$gdp~dataset$exp+dataset$epg+dataset$hpr+dataset$gdpus+dataset$oil+dataset$crd,data = dataset )
summary(G2)
The model above shows that 6 variables:exp, epg, hpr, oil, gdpus, crd could  predict GDP, F(6, 108) = 10.17
and and 6 variables accounted for 33.54% of the explained variability in GDP.
The p-values of (exp, epg, gpr) are both small so their coefficients are statiscally significant. 
By contrast, the p-values of the rest indexs are big (more than 0.05) so the coefficients are inconsiderable.


(h)
#calculate quantile
q=quantile(dataset$gdp,0.05)
#create vecto state
state=rep("crisis",nrow(dataset))
state[dataset$gdp>q]="normal"
state=as.factor(state)
dataset=data.frame(dataset,state)
#fit model
G=glm(dataset$bci[1:72]~dataset$state[1:72])
#compute confusion matrix
predict(G)
table(dataset$state[1:72],predict(G))

