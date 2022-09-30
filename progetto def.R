install.packages("lmtest")
library(lmtest)
install.packages("Rcmdr")
install.packages("ggplot2")
library(ggplot2)
library(scales)
 

police_killings=read.csv(file.choose(),header = T,sep=";", colClasses = c(name="character",age="numeric",gender="character",raceethnicity="character",month="character",day="numeric",year="numeric",streetaddress="character",city="character",state="character",latitude="numeric",longitude="numeric",state_fp="numeric",county_fp="numeric",tract_ce="numeric",geo_id="numeric",county_id="numeric",namelsad="character",lawenforcementagency="character",cause="character",armed="character",pop="numeric", share_white="numeric", share_black="numeric",share_hispanic="numeric",p_income="numeric",h_income="numeric",county_income="numeric",comp_income="numeric",county_bucket="numeric",nat_bucket="numeric",pov="numeric",urate="numeric",college="numeric"))
attach(police_killings)
names(police_killings)
View(police_killings)

tot_killings=read.csv(file.choose(),header=T, sep=";",col.names = c( "null" ,"names_tot","age_tot","gender_tot","raceethnicity_tot","month_tot","day_tot","year_tot","streetaddress_tot","city_tot","state_tot","cause_tot","lawenforcementagency_tot","armed_tot"))
attach(tot_killings)
names(tot_killings)
View(tot_killings)

# step 0:  cleaning the data set
tot_killings<-tot_killings[complete.cases(tot_killings), ]
police_killings<-police_killings[complete.cases(police_killings),]
tot_killings<-na.omit(tot_killings)
police_killings<-na.omit(police_killings)


#1: GRAPHS: 
# as we are working with a large amount of data, it is necessary to have a preliminary
# overall idea on the data we are working with. It is thus useful to plot some graphs
# to start familiarize with the results and to gain some insights on possible tests and 
# regressions that could be later studied.

# 1.1 PIE CHART OF GENDERS (tot annual)
x<-as.data.frame(prop.table(table(gender_tot)))
labels_GENDER=paste0(names(table(gender_tot))," = ",c(100 *x[2]) ,"%")
q<-round(100*x[2])
q
bp<- ggplot(x, aes(x="", y=Freq, fill=names(table(gender_tot))))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie + scale_fill_manual(values=c("lightblue", "pink", "red"))
#rk: 1. aggiungi sided percentuali

#1.2 MONTHS BAR PLOT
z<-table(month_tot)
z<-z[c(5,4,8,1,9,7,6,2,12,11,10,3)]
z<-data.frame(z)

p<-ggplot(data=z, aes(x=z[,1], y=z[,2])) +
  geom_bar(stat ="identity")+
  theme_light()
p

#1.2 CHART OF ETHNICITY 
w<-as.data.frame(prop.table(table(raceethnicity_tot)))
labels_ethnicity=paste0(names(table(raceethnicity_tot))," = ",c(100 *w[2]) ,"%")
y<-round(100*w[2])
y

b<- ggplot(w, aes(x="", y=Freq, fill=names(table( raceethnicity_tot))))+
  geom_bar(width = 1, stat = "identity")
b


w2<-c(0.001,0.001,0.13,0.185,0.018,0.06, 0.605)
c<- ggplot(as.data.frame(w2), aes(x="", y=w2, fill=names(table(raceethnicity_tot))))+
  geom_bar(width = 1, stat = "identity")
c
#i due plot delle proporzioni( etnia dei morti, etnia dellapopolazione), 
# poi devo concatenarli in un unico grafico

#1.3HISTOGRAM AGE
plot(table(age_tot),xlab= "age", ylab= "occurrencies")
mean(as.numeric(age_tot))

plot(h_income, urate)#1
cor(h_income, urate)#2
plot(h_income, college, xlab="Income", ylab="College")
cor(h_income,college)

#1.4 GRAPHS FOR CATEGORICAL VARIABLES

barplot(table(cause_tot),names.arg=names(table(cause_tot)), las=2)

#1.5 GRAPHS ON ETHNICITIES OF MOST HITTEN STATES 
tbl_etn <- matrix(c(  0.365,0.065,0.394,0.176,    0.421, 0.129,0.397,0.053,    0.926,0.014,0.02,0.04,       0.837,0.034,0.041, 0.088,                     0.601, 0.134,0.185,0.08),ncol=4,nrow=4, byrow=FALSE)
colnames(tbl_etn) <-c("CALIFORNIA","TEXAS","VERMONT", "NORTH DAKOTA")
rownames(tbl_etn)<-c("WHITE","BLACK","HISPANIC", "OTHERS")
mycol<-c("white", "black", "orange","grey")
tbl_etn
barplot(tbl_etn, col= mycol, las=2)
legend(x = "right", legend = rownames(tbl_etn), col = mycol, fill=mycol)

 
#2 STATISTICAL TESTS : INCOME 
data <-h_income
mean(h_income)
median(h_income)
plot(ecdf(h_income), main = "Empirical distribution function")

qqnorm(h_income, pch = 1, frame = FALSE)
qqline(h_income, col = "blue", lwd = 2)
#2nd we run a  Kolmogorov-Smirnov test and since the p-value is less than 0.05  we have to  reject H_0..hence h_income in not "normally distributed"
alpha = .05
ks_test2<-ks.test(data, "pnorm", mean = mean(data), sd = sd(data))
ks_test2$p.value< alpha
#3rd we test the hp that mu=mu_0 vs mu!=mu_0 using a t-test where mu_0 is the mean house income in US during 2015 and we reject H_0 hence the h_income of the victims differs significantly from the average US house income
n = length(h_income)
alpha = 0.05
mu_0 =56516
t_stat = sqrt(n) * (mean(h_income) - mu_0) / sd(h_income)
c_alpha = qt(p = 1-alpha/2, df = n-1)
abs(t_stat) > c_alpha
p_value = 2 * min(pt(t_stat, n-1), pt(t_stat, n-1, lower.tail = F))
p_value < alpha



#3 REGRESSIONS: 
#3.1 : TRICKY REGRESSION: UNEMPLOYEMENT ~SHARE OF BLACK POPULATION 
share_new<- as.numeric(share_black)/100
plot(share_new~urate)
cor(share_new, urate)
r1<-lm(urate~share_new, data=police_killings)
r1
summary(r1)
abline(a=r1$coefficients[1],b=r1$coefficients[2],col="green")
 
plot(r1$residuals)
abline(a=0, b=0, col="blue") 
qqnorm(residuals(r1))
qqline(residuals(r1))
shapiro.test(residuals(r1))


#3.2 DUMMY VARIABLES/MUTLTIPLE: AGE~URATE+ GENDER 
plot(urate,age)
points(urate[gender=="Male"],age[gender=="Male"], col="lightblue", pch="x")
points(urate[gender=="Female"],age[gender=="Female"], col="pink", pch="o")


r4<-lm(age~urate+gender, data=police_killings)
r4
summary(r4)
r4$coefficients
abline(a=(r4$coefficients[1]+r4$coefficients[3]),b=r4$coefficients[2], col="lightblue")
abline(a=r4$coefficients[1],b=r4$coefficients[2], col="pink")

#test only the urate since we can discard the gender  LISA
r4<-lm(age~urate, data=police_killings)
r4
summary(r4)
r4$coefficients
cor(urate, age)

#3.3 MULTIPLE REGRESSION: AGE~STATE+GENDER
take3<-which((state_tot=="CA"|state_tot=="TX") & age_tot!="Unknown")
state_tot3<-state_tot[take3]
gender_tot3<-gender_tot[take3]
age_tot3<-age_tot[take3]

plot(age_tot3, factor(state_tot3))
points(age_tot3[gender_tot3=="Male"],factor(state_tot3[gender_tot3=="Male"]), col="lightblue", pch="x")
points(age_tot3[gender_tot3=="Female"],factor(state_tot3[gender_tot3=="Female"]), col="pink", pch="o")
# rk: QUI NEL GRAFICO STATO 1= CA E STATO 2 = TX, il plot poi giuro che lo miglioro ahhahaha
r3<-lm(age_tot3~state_tot3+gender_tot3)
r3
summary(r3)
plot(r3$residuals)
abline(a=0, b=0, col="blue")
qqnorm(residuals(r3))
qqline(residuals(r3))
shapiro.test(residuals(r3))
 

#3.5 : NON LINEAR REGRESSION: URATE~NEW INCOME 
plot(h_income, urate)
new_income<-(1/(h_income))
plot(new_income, urate)
r6<-lm(urate~new_income)
r6
summary(r6)
abline(a=r6$coefficients[1],b=r6$coefficients[2], col="green")

plot(r6$residuals)
abline(a=0, b=0, col="blue")
qqnorm(residuals(r6))
qqline(residuals(r6))
shapiro.test(residuals(r6))

#4: ANOVA
#4.1: anova on race and age : CHECK WHY IT DOES NOT WORK 
del=which(raceethnicity_tot=="Unknown"|age_tot=="Unknown"| raceethnicity_tot==" Asian/Pacific Islander" | raceethnicity_tot=="Native American" | raceethnicity_tot=="Other")
del=which(raceethnicity_tot=="Unknown"|age_tot=="Unknown"| raceethnicity_tot==" Asian/Pacific Islander"| raceethnicity_tot=="Native American"| raceethnicity_tot=="Other")
raceethnicity2=factor(raceethnicity_tot[-del])
raceethnicity2
age1=as.numeric(age_tot[-del])
mean(age1)
l<-lm(age1~factor(raceethnicity2))
anova1=aov(age1 ~factor(raceethnicity2))
anova1$coefficients
summary(anova1)
TukeyHSD(anova1)


#----- variables intermezzo 
new_armed<-c()
for (x in armed_tot) {
  if (x != "No"){
    new_armed<-c(new_armed,"armed")
  }
  else{
    new_armed<-c(new_armed,"No")
  }
}
del1=which(cause_tot =="Other")
new_cause<-factor(cause_tot[-del1])
new_armed<-factor(new_armed[-del1])
new_age<-as.numeric(age_tot[-del1])
#----------
plot(new_age,new_armed)
points(new_age[new_cause=="Gunshot"],new_armed[new_cause=="Gunshot"], col="grey")
points(new_age[new_cause=="Death in custody"],new_armed[new_cause=="Death in custody"],col="blue")
points(new_age[new_cause=="Struck by vehicle"],new_armed[new_cause=="Struck by vehicle"],col="yellow")
points(new_age[new_cause=="Taser"],new_armed[new_cause=="Taser"],col="green")
# here armed is 1 non armed is 2

anova2<-aov(new_age~new_cause+new_armed)
anova2
anova2$coefficients
summary(anova2)
TukeyHSD(anova2)
interaction.plot(new_cause, new_armed, new_age, type="b", col=c(2,4), 
                 leg.bty="o", leg.bg="beige",las=2, lwd=2, pch=c(18,24,22))



#5: USE LOGISTIC REGRESSION TO STUDY CATEGORICAL VARIABLES

#5.1 LOGISTIC REGRESSION: ARMED~CAUSE

plot(new_armed,new_cause, las=1, las=2)
logistic_regression<- glm(new_armed ~new_cause,family=binomial(link='logit'),data=police_killings)
summary(logistic_regression)

log_reg1<- glm(new_armed ~ new_cause,family=binomial(link='logit'),data=police_killings)
log_reg0<- glm(new_armed ~1,family=binomial(link='logit'),data=police_killings)
lrtest(log_reg0,log_reg1)

#5.2LOGISTIC REGRESSION: STATE~RACEETHNICITY
new_ethnicity<-factor(subset(raceethnicity_tot,(state_tot=="CA"|state_tot=="FL" )&(raceethnicity_tot=="White"|raceethnicity_tot=="Black"|raceethnicity_tot=="Hispanic/Latino")))
new_state<-factor(subset(state_tot, (state_tot=="CA"|state_tot=="FL")&(raceethnicity_tot=="White"|raceethnicity_tot=="Black"|raceethnicity_tot=="Hispanic/Latino")))
new_state<-na.omit(new_state)
new_ethnicity<-na.omit(new_ethnicity)
plot(new_ethnicity, new_state)

logistic_regression<- glm(new_state ~new_ethnicity,family=binomial(link='logit'),data=police_killings)
summary(logistic_regression)
