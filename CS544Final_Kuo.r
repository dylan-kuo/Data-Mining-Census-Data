# Final Project - MET CS544 Foundations of Analytics  
# Author: Tzupin Kuo
# Description: Categorical and Numerical Analysis for the dataset 'Adults'

# -- Importing dataset into R --
adult <- read.csv("adult.csv")
dim(adult)
names(adult)
head(adult)

# -- Preprocessing --

# Splitting two classes depending on the income earned
higherPaid <-adult[adult$income == ">50K", ] 
lowerPaid <- adult[adult$income == "<=50K", ]
nrow(higherPaid)
nrow(lowerPaid)

# Finding and imputing the unknown values with mean or median
length(table(adult$workclass))
length(table(adult$occupation))
length(sort(table(adult$native.country)))
length(table(adult$relationship))

# Removing the data by setting ? as NA 
adult.temp <- adult
table(adult.temp$workclass)
adult.temp$workclass <- replace(as.vector(adult.temp$workclass),as.vector(adult.temp$workclass)=="?", NA)
table(adult.temp$workclass)

table(adult.temp$occupation)
adult.temp$occupation <- replace(as.vector(adult.temp$occupation),as.vector(adult.temp$occupation)=="?", NA)
table(adult.temp$occupation)

table(adult.temp$native.country)
adult.temp$native.country <- replace(as.vector(adult.temp$native.country),as.vector(adult.temp$native.country)=="?", NA)
table(adult.temp$native.country)

# Using COMPLETE.CASES function to makre sure the dataset is clean (no missing values)
adult.cleaned <- adult.temp[complete.cases(adult.temp), ]
dim(adult.cleaned)
adult <- adult.cleaned
higherPaid <- adult[adult$income == ">50K", ]
lowerPaid <- adult[adult$income == "<=50K", ]
nrow(higherPaid)
nrow(lowerPaid)

# -- Analyzing - Attributes with Classes --

# Age vs Income
dimnames(adult)
hist(adult$age, xlim = c(0,100))
table(adult$age)
levels(factor(adult$income))

age.higher.paid <- numeric(range(adult$age)[2] - range(adult$age)[1] + 1)
age.lower.paid <- numeric(range(adult$age)[2] - range(adult$age)[1] + 1)
age.count <- 1
for (i in c(min(adult$age):max(adult$age))) {
  age.higher.paid[age.count] <- nrow(adult[adult$age == i & adult$income == ">50K", ])
  age.lower.paid[age.count] <- nrow(adult[adult$age == i & adult$income == "<=50K", ])
  age.count <- age.count + 1
}
age.range = range(adult$age)[2] - range(adult$age)[1] + 1

plot(min(adult$age):max(adult$age), age.lower.paid, type = "l", lwd = 3, 
     main = "Age VS Income", col = "seagreen2", xlab = "Age", ylab = "# of People")
lines(min(adult$age):max(adult$age), age.higher.paid, type = "l", lwd = 3, col = "deeppink")
legend("right", legend = c("Income <= 50K", "Income > 50K"),
       col = c("seagreen2", "deeppink"), lty="solid", lwd = 3)
                     
# Gender Vs Income
female.higher.paid = nrow(adult[adult$gender == "Female" & adult$income ==">50K", ])
female.lower.paid = nrow(adult[adult$gender == "Female" & adult$income =="<=50K", ])
male.higher.paid = nrow(adult[adult$gender == "Male" & adult$income ==">50K", ])
male.lower.paid = nrow(adult[adult$gender == "Male" & adult$income =="<=50K", ])

males <- nrow(adult[adult$gender == "Male", ])
males
females <- nrow(adult[adult$gender == "Female", ])
females
male.income <- c(male.lower.paid, male.higher.paid)
names(male.income) <- c("<=50K", ">50K")
female.income <- c(female.lower.paid, female.higher.paid)
names(female.income) <- c("<=50K", ">50K")
male.income
female.income
gender.income <- data.frame(male.income, female.income)
gender.income

barplot(t(as.matrix(gender.income)), main = "Male vs Female Income", 
        beside = TRUE, col=c("dodgerblue1","deeppink"), 
        ylim = c(0,1.25*max(gender.income)), xlab = "Income", ylab="# of people")
legend("right", legend = c("Male", "Female"),
       col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)
grid()

arrows(1.5, 20987, 1.7, 22500, code=1)
text(1.7, 23500, "0.6875225")
arrows(2.5, 13026, 2.7, 15500, code=1)
text(2.7, 16500, "0.886424")
arrows(4.5, 9539, 4.7, 11500, code=1)
text(4.7, 12500, "0.3124775")
arrows(5.5, 1669, 5.7, 3000, code=1)
text(5.7, 3500, "0.113576")

male.income/males
female.income/females

# Education vs Income
levels(adult$education)
# -- Male & Female earning more than 50K 
male.edu <- adult[adult$gender == "Male" & adult$income == ">50K", ]
female.edu <- adult[adult$gender == "Female" & adult$income == ">50K", ]

male.edu.df <- as.data.frame(table(male.edu$education))
male.education <- c(as.vector(male.edu.df$Freq))
names(male.education) <- male.edu.df$Var1
male.education

female.edu.df <- as.data.frame(table(female.edu$education))
female.education <- c(as.vector(female.edu.df$Freq))
names(female.education) <- female.edu.df$Var1
female.education

gender.edu.upto.hs <- data.frame(male.education[c(1,2,3,4,5,6,7,14)], female.education[c(1,2,3,4,5,6,7,14)]) # M/F upto high school
gender.edu.after.hs <- data.frame(male.education[c(8,9,10,11,12,13,15,16)], female.education[c(8,9,10,11,12,13,15,16)]) # M/F after high school

# barplot - M/F eanring > 50K and went to school up to high school
barplot(t(as.matrix(gender.edu.upto.hs)), main = "Male vs Female Education Income > 50K", 
        beside = TRUE, ylim = range(0:100) , col = c("dodgerblue1", "deeppink"), xlab = "Education", ylab = "# of People")
legend("topright", legend = c("Male", "Female"), col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)

# barplot - M/F eanring > 50K and went to school after high school
barplot(t(as.matrix(gender.edu.after.hs)), main = "Male vs Female Education Income > 50K", 
        beside = TRUE, ylim = range(0:3000) , col = c("dodgerblue1", "deeppink"), xlab = "Education", ylab = "# of People")
legend("topright", legend = c("Male", "Female"), col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)

# -- Male & Female earning equal to / less than 50K 
male.edu.l <- adult[adult$gender == "Male" & adult$income == "<=50K", ]
female.edu.l <- adult[adult$gender == "Female" & adult$income == "<=50K", ]

male.edu.l.df <- as.data.frame(table(male.edu.l$education))
male.education.l <- c(as.vector(male.edu.l.df$Freq))
names(male.education.l) <- male.edu.l.df$Var1
male.education.l

female.edu.l.df <- as.data.frame(table(female.edu.l$education))
female.education.l <- c(as.vector(female.edu.l.df$Freq))
names(female.education.l) <- female.edu.l.df$Var1
female.education.l

gender.edu.upto.hs.l <- data.frame(male.education.l[c(1,2,3,4,5,6,7,14)], female.education.l[c(1,2,3,4,5,6,7,14)]) # M/F upto high school
gender.edu.after.hs.l <- data.frame(male.education.l[c(8,9,10,11,12,13,15,16)], female.education.l[c(8,9,10,11,12,13,15,16)]) # M/F after high school

# barplot - M/F eanring <= 50K and went to school up to high school
barplot(t(as.matrix(gender.edu.upto.hs.l)), main = "Male vs Female Education Income <= 50K", 
        beside = TRUE, ylim = range(0:1000) , col = c("dodgerblue1", "deeppink"), xlab = "Education", ylab = "# of People")
legend("topright", legend = c("Male", "Female"), col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)

# barplot - M/F eanring <= 50K and went to school after high school
barplot(t(as.matrix(gender.edu.after.hs.l)), main = "Male vs Female Education Income <= 50K", 
        beside = TRUE, ylim = range(0:8000), col = c("dodgerblue1", "deeppink"), xlab = "Education", ylab = "# of People")
legend("topright", legend = c("Male", "Female"), col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)


sum(male.education/males) + sum(male.education.l/males)
sum(female.education/females) + sum(female.education.l/females)


# -- Education vs Income
a <- list()
edu <- levels(factor(adult$education))
ear <- levels(factor(adult$income))
for(i in c(1:length(levels(factor(adult$education))))){
  a[[i]]<-c(nrow(adult[adult$education == edu[i] & adult$income == ear[1] , ]), nrow(adult[adult$education == edu[i] & adult$income == ear[2] , ]))
}

a.sum <- numeric(16)
for(i in c(1:16)){
  a.sum[i] <- sum(a[[i]])
}
sum(a.sum)

a1 <- numeric(16)
a2 <- numeric(16)
for(i in c(1:2)){
  if(i == 1){
    for (j in c(1:16)){
      a1[j] <- a[[j]][i]
    }
  }else if (i == 2){
    for (j in c(1:16)){
      a2[j] <- a[[j]][i]
    }
  }
}

edu.names <- c("10th","11th", "12th","1-4th","5-6th","7-8th","9th","acdm","voc","Bach",
             "Doc","HSgrad","MS","Pre","Prof","Some")

names(a1) <- edu.names
names(a2) <- edu.names
education.income <- data.frame(a1,a2)
# barplot - M/F comparison towards education/income 
barplot(t(as.matrix(education.income)), main="Education vs Income", 
        beside = TRUE, col = c("dodgerblue1", "deeppink"), xlab="Education level", ylab="# of People")
legend("topright", legend = c("Male", "Female"), col = c("dodgerblue1", "deeppink"), lty="solid", lwd = 3)


# High school graduate details
male.hs.grad <- adult[adult$education == "HS-grad" & adult$gender == "Male", ]
female.hs.grad = adult[adult$education == "HS-grad" & adult$gender == "Female", ]

male.hsgrad <- data.frame(table(male.hs.grad$income))
female.hsgrad<-data.frame(table(female.hs.grad$income))

library(plotrix)
slices <- c(male.hsgrad$Freq[1],female.hsgrad$Freq[1],male.hsgrad$Freq[2],female.hsgrad$Freq[2])
lbls <- c("Male: <=50K", "Female: <=50K","Male: >50K","Female: >50K")
# pir chart for high school graduate variation analysis
pie3D(slices, labels = lbls, main = "High School Graduate Variation", 
      col = c("deepskyblue1", "deeppink", "dodgerblue", "deeppink2"), explode = 0.1)


# Years of Education vs Income
edu.number <- data.frame(table(adult$educational.num))
hist(adult$educational.num, breaks = seq(0,16, by=1), xlim = c(0,16),
     col = rgb(0,0,1,0.5), xlab = "#of years")

edu.more <- data.frame(table(higherPaid$educational.num))
edunum.more <- edu.more$Freq
edu.less <- data.frame(table(lowerPaid$educational.num))
edunum.less <- edu.less$Freq
names(edunum.more) <- c(1:16)
names(edunum.less) <- c(1:16)
ednum <- data.frame(edunum.less, edunum.more)

barplot(c(1:16), edunum.more, col=rgb(0,0,1,0.5),main="#of years of school vs Earning >50K", xlab = "# of people")
barplot(edunum.more,col=rgb(0,0,1,0.5),main="#of years of school vs Earning >50K", xlab = "# of people")
barplot(c(1:16), edunum.less,col=rgb(0,1,0,0.5),main="#of years of school vs Earning <=50K", xlab = "# of people")
barplot(edunum.less,col=rgb(0,1,0,0.5),main="#of years of school vs Earning <=50K", xlab = "# of people")

library(ggplot2)
# barplot for years of education vs income by using ggplot
ggplot(data = ednum, aes(x = c(1:16), y = edunum.less)) + xlab("# of years") + ylab("# of people") + 
  ggtitle("# of Years of school vs Earning") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity", fill = "lightpink1", colour="lightpink2") +
  geom_bar(data = ednum, aes(x = c(1:16), y = edunum.more), stat = "identity", fill = "seagreen2", colour="seagreen3") +
  scale_x_continuous(breaks=c(1:16)) 
  
 
# Hours per week Vs Income
hr.more <- data.frame(table(higherPaid$hours.per.week))
hr.less <- data.frame(table(lowerPaid$hours.per.week))  
levels(hr.more$Var1)
levels(hr.less$Var1)

par(mfrow=c(1,2))
plot(as.integer(hr.less$Var1), hr.less$Freq,col="red", type="l",main="Hours/week vs earnings <= 50K", xlab="Hours/week",ylab="#of people")
plot(as.integer(hr.more$Var1), hr.more$Freq,col="seagreen3", type="l",main="Hours/week vs earnings > 50K", xlab="Hours/week",ylab="#of people")
par(mfrow=c(1,1))
 

# Final weights 
library(vioplot)
summary(adult$fnlwgt)
vioplot(adult$fnlwgt,col="deepskyblue1")

boxplot(adult$fnlwgt, col="deepskyblue1", xaxt="n", xlab="Weight values", main="Boxplot for FinalWeight", horizontal = TRUE)
axis(side=1,at=fivenum(adult$fnlwgt), labels=TRUE, las=2)
text(fivenum(adult$fnlwgt), rep(1.2,5), srt=90, adj=0, labels=c("Min","Q1","Median","Q3","Max"))

hist(adult$fnlwgt, col="lightblue", main="Histogram for FinalWeight", xlab="Weights")
final.weight.lower <- adult[adult$fnlwgt <= 0.4*(max(adult$fnlwgt)), ]$fnlwgt
hist(final.weight.lower, col="deepskyblue2", main="Histogram for FinalWeight-Lower", xlab="Weights")
final.weight.upper <- adult[adult$fnlwgt > 0.4*(max(adult$fnlwgt)), ]$fnlwgt
hist(final.weight.upper, col="deepskyblue2", main="Histogram for FinalWeight-Upper", xlab="Weights")


# Central limit theorem
final.weight <- adult$fnlwgt
samples <- 5000
final.weight.bar.5 <- as.numeric(samples)
final.weight.bar.10 <- as.numeric(samples)
final.weight.bar.50 <- as.numeric(samples)
final.weight.bar.100 <- as.numeric(samples)
final.weight.bar.200 <- as.numeric(samples)
final.weight.bar.500 <- as.numeric(samples)
samples.size <- c(5,10,50,100,200,500)
for(i in 1:samples){
  final.weight.bar.5[i] <- mean(sample(final.weight, samples.size[1]))
  final.weight.bar.10[i] <- mean(sample(final.weight, samples.size[2]))
  final.weight.bar.50[i] <- mean(sample(final.weight, samples.size[3]))
  final.weight.bar.100[i] <- mean(sample(final.weight, samples.size[4]))
  final.weight.bar.200[i] <- mean(sample(final.weight, samples.size[5]))
  final.weight.bar.500[i] <- mean(sample(final.weight, samples.size[6]))
}
# barplot for central limit theorem 
par(mfrow=c(2,3))
hist(final.weight.bar.5, main="Histogram for sample size 5", xlab= "weights",col="gray78")
hist(final.weight.bar.10, main="Histogram for sample size 10", xlab= "weights",col="gray67")
hist(final.weight.bar.50, main="Histogram for sample size 50", xlab= "weights",col="gray57")
hist(final.weight.bar.100, main="Histogram for sample size 100", xlab= "weights",col="gray50")
hist(final.weight.bar.200, main="Histogram for sample size 200", xlab= "weights",col="gray43")
hist(final.weight.bar.500, main="Histogram for sample size 500", xlab= "weights",col="gray34")
par(mfrow=c(1,1))

# mean and standard deviation comparison
mean(final.weight)
final.weight.mean <- c(mean(final.weight.bar.5), mean(final.weight.bar.10), mean(final.weight.bar.50),
                     mean(final.weight.bar.100), mean(final.weight.bar.200), mean(final.weight.bar.500))
final.weight.mean

sd(final.weight)
final.weight.sd <- c(sd(final.weight.bar.5), sd(final.weight.bar.10), sd(final.weight.bar.50),
                   sd(final.weight.bar.100), sd(final.weight.bar.200), sd(final.weight.bar.500))
final.weight.sd

par(mfrow=c(1,2))
plot(samples.size, final.weight.mean, type="h", col="red",
     main="Mean of samples", xlab="Sample sizes", ylab="Means", xaxt="n")
axis(side=1, at=c(5,10,50,100,200,500), labels=c(5,10,50,100,200,500), cex.axis=0.7)
abline(h=189734.7, col="red", lwd=3, lty=2)
text(200, 189800, "Population Mean: 189734.7", cex = 1)
points(samples.size,final.weight.mean,pch=8)
grid()

plot(samples.size, final.weight.sd, type="h", col="blue",
     main="Standard Deviation of samples", xlab="Sample sizes",ylab="SD", xaxt="n")
axis(side=1, at=c(5,10,50,100,200,500), labels=c(5,10,50,100,200,500), cex.axis=0.7)
points(samples.size,final.weight.sd,pch=8)
grid()
par(mfrow=c(1,1))

# Sampling
library(sampling)

# -- Simple random sampling with replacement -- 
s <- srswr(10000, nrow(adult.cleaned))
length(s)
table(s)
rows <- (1:nrow(adult.cleaned))[s!=0]
length(rows)
rows <- rep(rows, s[s!=0])
length(rows)
adult.srs.w.rep <- adult.cleaned[rows, ]
(table(adult.srs.w.rep$native.country))

# -- Simple random sampling without replacement --
swo <- srswor(10000, nrow(adult.cleaned))
length(swo)
table(swo)
adult.srs.wo.rep <- adult.cleaned[swo!=0, ]
table(adult.srs.wo.rep$native.country)

# -- Systematic sampling --
N <- nrow(adult.cleaned)
n <- 10000
k <- ceiling(N/n)
k
r<- sample(k, 1)
r
s.sys <- seq(r, by = k, length = n)
adult.sys <- adult.cleaned[s.sys, ]
table(adult.sys$native.country)

# Changing categorical to numeric for inclusion probability
adult.native.fac <- factor(adult.cleaned$native.country)
length(adult.native.fac)
levels(adult.native.fac) <- c(1:41)
adult.native.cleaned <- adult.cleaned
adult.native.cleaned$native.country <- as.numeric(adult.native.fac)

levels(adult.native.cleaned$native.country)
pik <- inclusionprobabilities(adult.native.cleaned$native.country, n)
length(pik)
sum(pik)

s.sys.unequal <- UPsystematic(pik)
table(s.sys.unequal)
adult.sys.unequal <- adult.cleaned[s.sys.unequal!=0, ]
table(adult.sys.unequal$native.country)


# -- Strata sampling : unequal probility --
freq <- table(adult.cleaned$native.country)
st.sizes <- round(n*freq/sum(freq))
if(sum(st.sizes) > n) {
  zero.pointer <- match(0, st.sizes)
  max.pointer <- match(max(st.sizes), st.sizes)
  if(zero.pointer != 0 | is.na(zero.pointer) != FALSE) {
    st.sizes[zero.pointer] <- table(adult.cleaned$native.country)[[zero.pointer]]
    st.sizes[max.pointer] <- st.sizes[max.pointer] - st.sizes[zero.pointer]
  }
  second.high <- match(sort(st.sizes, decreasing = TRUE)[2], st.sizes)
  st.sizes[second.high] <- st.sizes[second.high] - (sum(st.sizes) - n)
}
sum(st.sizes)
length(st.sizes)
sort(table(adult.cleaned$native.country))
s.strata <- strata(adult.cleaned[order(adult.cleaned$native.country), ], 
                   stratanames = c("native.country"), size = st.sizes, method = "srswor", description = TRUE)

adult.strata <- getdata(adult.cleaned, s.strata)
table(adult.strata$native.country)

# There are 41 different countries (levels) in the native.country attribute 
# Stratified sampling is the best option in this case
unique(adult$native.country) # All conuntries taken by the population
length(unique(adult$native.country)) # the number of all countries, which is 41
nrow(table(adult.srs.w.rep$native.country))
nrow(table(adult.srs.wo.rep$native.country))
nrow(table(adult.sys$native.country))
nrow(table(adult.sys.unequal$native.country))
nrow(table(adult.strata$native.country)) # only stratified sampling does a good job picking up every 41 country sample as an effective sampling


# Confidence Intervals
population.mean <- mean(adult$fnlwgt)
population.sd <- sd(adult$fnlwgt)
sd.sample.means <- population.sd/sqrt(5)
sd(final.weight.bar.5)
sd.sample.means
sd.sample.means <- population.sd/sqrt(100)
sd(final.weight.bar.100)
sd.sample.means
sd.sample.means <- population.sd/sqrt(500)
sd(final.weight.bar.500)
sd.sample.means

population.mean
final.weight.mean
final.weight.sd
samples.size

conf80.lower <- numeric(length(samples.size))
conf80.upper <- numeric(length(samples.size))
conf90.lower <- numeric(length(samples.size))
conf90.upper <- numeric(length(samples.size))

for (i in 1:length(samples.size)){
  conf80.lower[i] <- final.weight.mean[i] - 1.28*(population.sd/sqrt(samples.size[i]))
  conf80.upper[i] <- final.weight.mean[i] + 1.28*(population.sd/sqrt(samples.size[i]))
  conf90.lower[i] <- final.weight.mean[i] - 1.645*(population.sd/sqrt(samples.size[i]))
  conf90.upper[i] <- final.weight.mean[i] + 1.645*(population.sd/sqrt(samples.size[i]))
}

par(mfrow=c(1,2))
plot(samples.size, conf90.lower, type="l", col="red", lty = 'dashed',
     ylim = c(0,max(conf90.upper)), xlab="Sample size", ylab="values", main="Confidence Interval 90%")
lines(samples.size, conf90.upper, type="l", col="red", lty = 'dashed')
lines(samples.size, rep(population.mean, length(samples.size)), type="l", col="grey65")
polygon(c(samples.size,rev(samples.size)), c(conf90.lower,rev(conf90.upper)),col=rgb(1, 0, 0,0.1),border=NA)

plot(samples.size, conf80.lower, type="l", col="red", lty = 'dashed',
     ylim = c(0,max(conf80.upper)), xlab="Sample size", ylab="values", main="Confidence Interval 80%")
lines(samples.size, conf80.upper, type="l", col="red", lty = 'dashed')
lines(samples.size, rep(population.mean, length(samples.size)), type="l", col="grey65")
polygon(c(samples.size,rev(samples.size)), c(conf80.lower,rev(conf80.upper)),col=rgb(1, 0, 0,0.1),border=NA)
par(mfrow=c(1,1))


xbar <- numeric(20)
for (i in 1:length(samples.size)){
  cat("Sample.size : ",samples.size[i],"\n")
  for (j in 1:20){
    xbar[j] <- mean(sample(final.weight,samples.size[i]))
    cat("80% confidence intervals =",
        xbar[j] - 1.28*(population.sd/sqrt(samples.size[i]))," - ",
        xbar[j] + 1.28*(population.sd/sqrt(samples.size[i])),  "\n")
  }
}

for (i in 1:length(samples.size)){
  cat("Sample.size : ",samples.size[i],"\n")
  for (j in 1:20){
    xbar[j] <- mean(sample(final.weight,samples.size[i]))
    cat("90% confidence intervals =",
        xbar[j] - 1.645*(population.sd/sqrt(samples.size[i]))," - ",
        xbar[j] + 1.645*(population.sd/sqrt(samples.size[i])),  "\n")
  }
}
