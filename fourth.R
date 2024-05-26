#32
numbers <- runif(500, 2, 3)
hist(numbers)
hist(numbers, probability = T)
curve(dunif(x, 2, 3), from=2, to=3, add=T, lwd=2, col="red")

#33
numbers <- rexp(500, 1/7)
hist(numbers, probability = T)
curve(dexp(x, 1/7), from=0, to=max(numbers), add=T, lwd=2, col="red")

#34
nubers <- rnorm(500, 0, 1)
hist(numbers, probability = T)
curve(dnorm(x, 0, 1), from=0, to=40, add=T, lwd=2, col="red")

#35
# X = количество душ гел в опаковка
# X ~ U(248, 255)
# # P(X < 250) = P(X <= 250)
punif(250, 248, 255)

# v=?  P(X > v) = 0.95
# P(X <= v) = 0.05
qunif(0.05, 248, 255)

#36
# X = време на живот на лазерен диод
# X ~ Exp(1/10)
# P(X > 10) = 1 - P(X <= 10)
1 - pexp(10, 1/10)

# P(7 < X < 11) = P(X < 11) - P(X < 7)
pexp(11, 1/10) - pexp(7, 1/10)

# t=?  P(X > t) = 0.97
# P(X <= t) = 0.03
qexp(0.03, 1/10)

#37
# X = изразходвано количество кашкавал за седмица
# X ~ N(mu=41, sigma^2=25)
# P(X > 51) = 1 - P(X <= 51)
1 - pnorm(51, 41, 5)

# P(45 < X < 50) = P(X < 50) - P(X < 45)
pnorm(50, 41, 5) - pnorm(45, 41, 5)

# t=?  P(X <= t) = 0.99
qnorm(0.99, 41, 5)

#38
# P(A) със симулации
vol <- rnorm(100000, 252, 3)
sum(vol>250)/length(vol)

# P(B) със симулации
sim.5cups <- function() {
  volume5cups <- rnorm(5, 252, 3)
  sum(volume5cups>250) <= 2
}
res <- replicate( 100000, sim.5cups() )
sum(res)/length(res)

#############################################

# X = обем течност, който машина налива в една чаша
# X ~ N(mu=252, sigma=3)
# P(A) = P(X > 250) = 1 - P(X <= 250)
1 - pnorm(250, 252, 3)

# Y = брой наливания с над 250 мл от общо 5 наливания
# Y ~ Bi(n=5, p=p.a)
# P(Y<=2)
pbinom(2, 5, p.a)

#39
x <- runif(10000, -1, 1)
y <- runif(10000, -1, 1)
points_in_circle <- sum(x^2 + y^2 < 1)
approx_pi <- 4 * points_in_circle / 10000
approx_pi
pi

plot(x, y, pch=".", col="gray")
curve( sqrt(1-x^2), from=-1, to=1, add=T, col="red" )
curve( -sqrt(1-x^2), from=-1, to=1, add=T, col="red" )

#40
x <- runif(10000, 0.8, 4)
f_x <- exp(-(x^2) / 2) / sqrt(2 * pi)
mean_fx <- sum(f_x) / length(f_x)
integral <- (mean_fx) * (4 - 0.8)
integral