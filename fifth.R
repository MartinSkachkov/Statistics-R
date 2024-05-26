#41
x <- rexp(5000, 1/8)
hist(x, probability = T)

y <- 1 - exp(-1/8 * x)
hist(y, probability = T)

# 42
x <- rnorm(5000, 0, 1)
hist(x, probability = T)

y <- -(1/(1/8)) * log(1-x)
hist(y, probability = T)
curve(dexp(x, lambda), col = "red", lwd = 2, add = TRUE)

#43
# X ~ N(260, 5^2)
# X = # минути работа до изтощаване на батерията
# P(X > 240) = 1 - P(X <= 240)
1 - pnorm(240, 260, 50)

# P(3 <= X <= 5) = P(X <= 5) - P(X <= 3)
pnorm(5*60, 260, 50) - pnorm(3*60, 260, 50)

# t=? P(X > t) = 0.9
qnorm(0.9, 260, 50)

#44
# X ~ Exp(1/5)
# X = времето за разглеждане на страница
# P(X > 10) = 1 - P(X <= 10)
1 - pexp(10, 1/5)

# P(X == t) = 0.5
qexp(0.5, 1/5)

#45
library(MASS)
data(survey)
?survey

table(survey$Exer)

attach(survey)
table(Exer)

sort(table(Exer), decreasing = T)
length(Exer)
100*table(Exer)

100*table(Exer)/length(Exer)
100*prop.table(table(Exer))

barplot(table(Exer))
barplot( sort( table(Exer), decreasing=T ) )
barplot( 100*table(Exer)/length(Exer) )
pie( table(Exer) )
pie( table(Exer), col=c("red", "yellow", "blue") )

#46
library(MASS)
data(survey)
attach(survey)

table(Pulse)
table(Pulse, useNA="ifany")

pulse.interval <- cut(Pulse, breaks=seq(30,110,10))
pulse.interval # всяка стойност за пулс в таблицата в кой интервал се намира
table(pulse.interval)
barplot( table(pulse.interval) )

hist(Pulse)
hist(Pulse, breaks=seq(30,110,5)) #30-35-40-45-...-110


stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=18)
stripchart(Pulse, method="stack", pch=1)
stripchart(Pulse, method="stack", pch="*")

#47
table(Age)

age.interval <- cut(Age, breaks=seq(15,75,10))
age.interval
table(age.interval)

barplot( table(age.interval) )
hist(Age)

stripchart(Age, method="stack", pch="*")

#48
my_summary <- function(x){
  res <- c(median(x, na.rm = T), mean(x, na.rm = T), sd(x, na.rm = T))
  names(res) <- c("Median", "Mean", "StDev")
  res
}

v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

res <- my_summary(v1)

#49
load("cereals.RData")
summary(cereals)
attach(cereals)

summary(carbo)
median(carbo, na.rm = T)
mean(carbo, na.rm = T)
sd(carbo, na.rm=T)
my_summary(carbo)
hist(carbo)

summary(sodium)
median(sodium, na.rm = T)
mean(sodium, na.rm=T)
sd(sodium, na.rm=T)
my_summary(sodium)
hist(sodium)

summary(potass)
median(potass, na.rm = T)
mean(potass, na.rm=T)
sd(potass, na.rm=T)
my_summary(potass)
hist(potass)

#50
boxplot(Pulse[W.Hnd == "Left"], Pulse[W.Hnd == "Right"])

my_summary(Pulse[W.Hnd == "Left"])
my_summary(Pulse[W.Hnd == "Right"])