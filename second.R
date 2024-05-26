#11
# Каква е вероятността да извадим зелена топка?
sim_boxes_a <- function(){
  dice <- c(1:6)
  roll_dice <- sample(dice,1,replace=F)
  
  if(roll_dice == 6){
    box <- c(rep("r",2), rep("g",2))
    ball <- sample(box,1,replace=F)
  }else{
    box <- c(rep("r",4), "g")
    ball <- sample(box,1,replace=F)
  }
  
  ball == "g"
}

res <- replicate(10000,sim_boxes_a())
sum(res)/length(res)

# Ако извадената топка е зелена, каква е вероятността да е извадена от втората кутия?
sim_boxes_b <- function(){
  dice <- c(1:6)
  roll_dice <- sample(dice,1,replace=F)
  
  if(roll_dice == 6){
    box <- c(rep("r",2), rep("g",2))
    ball <- sample(box,1,replace=F)
  }else{
    box <- c(rep("r",4), "g")
    ball <- sample(box,1,replace=F)
  }
  
  c(ball,roll_dice)
}

res <- replicate(10000, sim_boxes_b())
sum(res[2,]!=6 & res[1,] == "g") / sum(res[1,] == "g")

#12
# Каква е вероятността да се падне единица?
sim_coins_a <- function(){
  coins <- c(11, 11, 22, 12, 12)
  throw <- sample(coins,1)
  
  if(throw == 12){
    sides <- c(1,2)
    up <- sample(sides,1)
  }else if(throw == 22){
    up <- 2
  }else{
    up <- 1
  }
  
  up == 1
}

res <- replicate(10000, sim_coins_a())
sum(res)/length(res)

# Ако горната страна на хвърлената монета е единица, каква е вероятността другата страна да е двойка?
sim_coins_b <- function(){
  coins <- c(11, 11, 22, 12, 12)
  throw <- sample(coins,1)
  
  if(throw == 12){
    sides <- c(1,2)
    up <- sample(sides,1)
    
    if(up == 1){
      down = 2
    }else{
      down = 1
    }
  }else if(throw == 22){
    up <- 2
    down <- 2
  }else{
    up <- 1
    down <- 1
  }
  
  c(up,down)
}

res <- replicate(10000, sim_coins_b())
sum(res[1,] == 1 & res[2,] == 2) / sum(res[1,] == 1)

#13
sim_cards <- function(){
  cards <- c(11,22,12)
  card <- sample(cards,1)
  
  if(card == 12){
    sides <- c(1,2)
    up <- sample(sides,1)
  }else if(card == 11){
    up <- 1
  }else{
    up <- 2
  }
  
  c(up, card)
}

res <- replicate(10000, sim_cards())
sum(res[1,] == 1 & res[2,] == 11) / sum(res[1,] == 1)

#14
sim_balls <- function(){
  balls <- c(1:99)
  pick <- sample(balls,4,replace=F)
  pick[1] == max(pick)
}

res <- replicate(10000, sim_balls())
sum(res)/length(res)

#15
sim_sequence <- function(){
  sequence <- c(1:20)
  ivan_georgi <- sample(sequence, 2, replace=F)
  ivan <- ivan_georgi[1]
  georgi <- ivan_georgi[2]
  abs(ivan - georgi) == 1
}

res <- replicate(10000, sim_sequence())
sum(res)/length(res)

#16
sim_cards <- function(){
  deck <- c(rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4))
  shuffle <- sample(deck)
  split_size <- 52/4

  player1 <- shuffle[1:split_size]
  player2 <- shuffle[(split_size + 1):(2*split_size)]
  player3 <- shuffle[(2*split_size + 1):(3*split_size)]
  player4 <- shuffle[(3*split_size + 1):52]
  
  sum(player1 == "A") == 1 & sum(player2 == "A") & sum(player3 == "A") & sum(player4 == "A")
}

res <- replicate(10000, sim_cards())
sum(res)/length(res)

#17
# Каква е вероятността поне двама от чакащите да отиват на един и същи етаж?
sim_elevator_a <- function(){
  floors <- c(2:16)
  floor_people <- sample(floors,7,replace=T)

  any(duplicated(floor_people))
}

res <- replicate(10000, sim_elevator_a())
sum(res)/length(res)

# Ако Вие сте един от седемте, каква е вероятността поне един от останалите 6 да отива на Вашия етаж?
sim_elevator_b <- function(){
  floors <- c(2:16)
  floor_people <- sample(floors,7,replace=T)
  
  my_floor <- floor_people[1]
  floor_people <- floor_people[2:7]

  any(my_floor == floor_people)
}

res <- replicate(10000, sim_elevator_b())
sum(res)/length(res)

#18
# X = брой шестици при 10 хвърляния на зар
# X ~ Bi(n=10, p=1/6)

#a) P(X = 2)
dbinom(2, 10, 1/6)

#b) P(X <= 2)
pbinom(2, 10, 1/6)

#c) P(X >= 2) = 1 - P(X <= 1)
1 - pbinom(1, 10, 1/6)

#d) P(3 <= X <= 8) = P(X <= 8) - P(X <= 2) 
pbinom(8, 10, 1/6) - pbinom(2, 10, 1/6)

#19
# X = брой хвърляния до падане на шестица (вкл.)
# X ~ Ge(p=1/6)

#а) P(X <= 10)
pgeom(9, 1/6)

#b) P(X >= 6) = 1 - P(X <= 5)
1 - pgeom(4, 1/6)

#20
# X = брой хвърляния докато се паднат общо 3 шестици
# X ~ NB(r=3, p=1/6)

# P(X <= 20)
pnbinom(17, 3, 1/6)
