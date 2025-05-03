# Travelling salesman problem - Salesman has to visit all cities only once and he has
#to take the shortest route possible

# libraries import
library(tidyverse)
library(ggplot2)
library(magick)
library(gridExtra)

# Setting up

Lc = 20 # Number of cities
Lp = 10^3 # Size of population
P = 0.1 # Probability of mutation or crossbreeding
S = 0 #Selection method ( 0  or  not 0 ) # not implemented (yet), only roulette method


#generating cities - coordinates x & y


x = runif(Lc, max = 100)
y = runif(Lc, max = 100)


df = data.frame(x = x, y = y)

ggplot(df,aes(x,y))+geom_point()+xlim(0,100)+ylim(0,100)


# Generating starting population
#individual is vector of Lm length, next numbers are next cities visited by salesman
#population is a set of individuals

#individuals will be generated using sample() function



population = matrix(data = NA, nrow = Lc, ncol = Lp)

for (i in 1:Lp){
os = sample(1:Lc)
population[,i] = os
}

#usefull funtions:


# loss function - added distances between cities in euclidean metrics
loss = function(population,df,n){
  sum = 0
  for (j in 2:Lc-1)
  {
    sum = sum + sqrt( (df[population[j,n],][1] - df[population[j+1,n],][1] )^2 + ( df[population[j,n],][2] - df[population[j+1,n],][2])^2 )
  }
  sum = sum + sqrt( (df[population[1,n],][1] - df[population[Lc,n],][1] )^2 + ( df[population[1,n],][2] - df[population[Lc,n],][2])^2 )
  return(sum)
}

# loss_table - loss for whole population
loss_table = function(population,df){
  ret = matrix(ncol = 2,nrow = Lp)
  for (i in 1:Lp)
  {
    ret[i,1] = loss(population,df,i)[[1]]
    ret[i,2] = i
  }
  return(ret)
}

#function for crossover of whole population, uses 'crossover' function
pop_crossover = function(population,P,Lp){
  sample = sample(1:Lp, size = Lp * P, replace = F)
  
  for (l in 1:(Lp*P/2) )
  {
    a = crossover(population[,sample[2*l-1]],population[,sample[2*l]])
    population[,sample[2*l-1]] = a[[1]]
    population[,sample[2*l]] = a[[2]]
  }
  return(population)
}
#function for crossover of 2 individuals
crossover = function(vec1,vec2){
  one = sample(1:Lc,1)
  two = sample(1:Lc,1)
  
  child1 = rep(NA,Lc)
  child2 = rep(NA,Lc)
  
  child2[one:two] = vec1[one:two]
  child1[one:two] = vec2[one:two]
  
  temp = vec1[!(vec1 %in% child1)]
  
  k = 1
  for (i in 1:Lc)
  {
    if(is.na(child1[i]))
    {
      child1[i] = temp[k]
      k = k +1
    }
  }
  
  temp = vec2[!(vec2 %in% child2)]
  k = 1
  for (i in 1:Lc)
  {
    if(is.na(child2[i]))
    {
      child2[i] = temp[k]
      k = k +1
    }
  }
  m = data.frame(
    child1 = child1,
    child2 = child2
  )
  
  return(m)
}





#main loop for simulating generations

for (k in 1:100){
# sorting to get best results
lost = loss_table(population,df)
lost_sorted = lost[order(lost[,1]),]
lost_sorted = cbind(lost_sorted, seq_len(nrow(lost_sorted)))


g = ggplot(df,aes(x,y))+geom_point()+xlim(0,100)+ylim(0,100)
a = 1

for (i in 1:20){
  x = lost_sorted[i,2]
  path =  df[population[,x],]
  path[Lc+1,] = path[1,] # we copy 1st row so salesman can return home
  g = g +geom_path(data=path, alpha = a, color = 'red')
  a = a * 0.2
}

g

# we sample using roulette method - probability of getting individual is based on his loss function
new_population = population[,lost_sorted[sample(lost_sorted[,3],size = Lp,prob = (Lp - lost_sorted[,3]),replace = T),2]]

population = pop_crossover(new_population,P,Lp)
}

