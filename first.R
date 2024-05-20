#1
sim_balls <- function(){
  balls <- c(1:8)
  s <- sample(balls,2,replace=T)
  s[1] == s[2]
}

res <- replicate(10000, sim_balls())
sum(res)/length(res)

#2
sim_socks <- function(){
  socks <- c(1,1,2,2,3,3)
  s <- sample(socks,2,replace=F)
  s[1] == s[2]
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_socks())
  sum(res)/length(res)
}

prob_sim(10000)

#3
sim_keys <- function(){
  keys <- c(1:4)
  s <- sample(keys,4,replace=F)
  s[4] == 4
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_keys())
  sum(res)/length(res)
}

prob_sim(10000)

#4
sim_questions <- function(){
  questions <- c(rep(0,3), rep(1,17))
  s <- sample(questions,2,replace=F)
  sum(s) == 1
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_questions())
  sum(res)/length(res)
}

prob_sim(10000)

#5
sim_birthdays <- function(){
  birthdays <- c(1:365)
  s <- sample(birthdays,25,replace=T)
  any(duplicated(s))
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_birthdays())
  sum(res)/length(res)
}

prob_sim(10000)

#6
sim_gifts <- function(){
  gifts <- c(1:20)
  s <- sample(gifts,20,replace=F)
  v <- s - c(1:20)
  any(v == 0)  
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_gifts())
  sum(res)/length(res)
}

prob_sim(10000)

#7
sim_ants <- function(){
  ant1 <- sample(c(2,3),1)
  ant2 <- sample(c(1,3),1)
  ant3 <- sample(c(1,2),1)
  
  places <- c(ant1,ant2,ant3)
  length(unique(places)) == 3
}

prob_sim <- function(Nrep){
  res <- replicate(Nrep, sim_ants())
  sum(res)/length(res)
}

prob_sim(10000)

#8
sim_eggs <- function(){
  eggs <- c(rep("b",2), rep("r",6))
  s <- sample(eggs,8,replace=F)
  player1 <- s[seq(1,7,2)]
  player2 <- s[seq(2,8,2)]
  boiled1 <- sum(player1 == "b")
  boiled2 <- sum(player2 == "b")
  c(boiled1,boiled2)
}

res <- replicate(10000, sim_eggs())
res[1,1:10] == 2

# на един играч се падат двете сварени яйца
(sum(res[1,] == 2) + sum(res[2,] == 2)) / 10000
# пада се по едно сварено яйце на всеки играч
(sum(res[1,] == 1)) / 10000
(sum(res[2,] == 1)) / 10000
# падат се двете сварени яйца на този, който тегли първи
(sum(res[1,] == 2)) / 10000
# падат се двете сварени яйца на този, който тегли втори
(sum(res[2,] == 2)) / 10000

#9
sim_exam <- function(){
  x <- sample(c(0,1),10,replace=T,prob=c(0.75, 0.25))
  sum(x)
}

res <- replicate(10000, sim_exam())
sum(res >= 5)/length(res)

#10
sim_airplane <- function(){
  s <- sample(c(0,1),143,replace=T,prob=c(0.08,0.92))
  sum(s)
}

res <- replicate(10000, sim_airplane())
# Каква е вероятността да има място за всички пътници, които са дошли навреме?
sum(res <= 138) / 10000
# Каква е вероятността да остане едно незаето пътническо място?
sum(res == 137) / 10000
