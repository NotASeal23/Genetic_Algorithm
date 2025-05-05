# Travelling salesman problem - Salesman has to visit all cities only once and he has
#to take the shortest route possible

# libraries import
library(tidyverse)
library(ggplot2)
library(magick)
library(gridExtra)
library(gganimate)

# Setting up

Lc = 12 # Number of cities
Lp = 10^2 # Size of population
P = 0.5 # Probability of mutation or crossbreeding
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
  for (j in 1:(Lc-1))
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
# Mutations help the algorithm escape local minima 
#We want to apply a small number of mutations each iteration — let's say to
#about 10% of the offspring generated through crossover

pop_mutate = function(population,P,Lp,Lc)
{
  sample = sample(1:Lp, size = Lp / 10 * P, replace = F)
  for (m in 1:length(sample)){
    population[,sample[m]] = sample(1:Lc)
  }
  return(population)
}

# matrix storing means and best routes for each generation
stats = matrix(NA,50,2)



#main loop for simulating generations

for (k in 1:50){
  # sorting to get best results
  lost = loss_table(population,df)
  lost_sorted = lost[order(lost[,1]),]
  lost_sorted = cbind(lost_sorted, seq_len(nrow(lost_sorted)))
  
  stats[k,1] = lost_sorted[1,1] 
  stats[k,2] = lost_sorted[,1] |> mean()
  
  g = ggplot(df,aes(x,y))+geom_point()+xlim(0,100)+ylim(0,100)
  a = 1
  
  for (i in 1:50){
    x = lost_sorted[i,2]
    path =  df[population[,x],]
    path[Lc+1,] = path[1,] # we copy 1st row so salesman can return home
    g = g +geom_path(data=path, alpha = a, color = 'red')+labs(title=paste("Generation:", k))
    a = a * 0.2
  }
  
  g
  
  # saving plots
  ggsave(filename = sprintf("path/Genetic Algorithm/gif_images/%d_plot.png", k), plot = g, width = 5, height = 5)
  
  
  # we sample using roulette method - probability of getting individual is based on his loss function
  new_population = population[,lost_sorted[sample(lost_sorted[,3],size = Lp,prob = (Lp - lost_sorted[,3]),replace = T),2]]
  
  population = pop_crossover(new_population,P,Lp)
  population = pop_mutate(population,P,Lp,Lc)
}

#stat plot - showing mean and value of loss function for best solution in each generation
df_stats = data.frame(stats)
df_stats$X3 = 1:nrow(df_stats)

ggplot(df_stats,aes(X3,X1,color = "best"))+geom_point()+geom_point(aes(X3,X2,color = "mean"))+
  labs(
    x = "generation",      
    y = "loss funtion value",      
    title = "Loss funtion",  
    color = "Legend"    
  ) +
  scale_color_manual(values = c("mean" = "red", "best" = "blue"))+
  geom_hline(yintercept = df_stats$X1 |> min(), color = "gray", linetype = "dashed", size = 1)+
  annotate("text", x = max(df_stats$X3), y = min(df_stats$X1) - 50 , label = paste0("min = ", round(min(df_stats$X1), 2)),
           vjust = -1, hjust = 1, size = 4, color = "gray")

#gif showing changes of our solutions

#reading files
img_files = list.files(path = "path/Genetic Algorithm/gif_images/",pattern = "\\d+_plot\\.png", full.names = TRUE)
img_files = img_files[order(as.numeric(gsub("_plot\\.png", "", basename(img_files))))]
img_list = lapply(img_files, image_read)

animation = image_animate(image_join(img_list), fps = 5, loop = 1)
image_write(animation, "path/Genetic Algorithm/visual.gif")
