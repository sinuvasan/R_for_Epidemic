library(ggplot2)
library(deSolve)
state_vars  <- c(N = 1)
times       <- seq(0, 40, by = 0.5)

exp_pop_fn <- function(time, state, parameters) { 
  
  N <- state['N']  
  dN <- parameters['alpha'] * N                                                              
  return(list(c(dN)))                     
  
} 

parms <- c(alpha = log(2))
parms['alpha']

result <- ode(y = state_vars , times = times , func = exp_pop_fn , parms = parms)   
head(as.data.frame(result))

result <- as.data.frame(result)

expplot <- ggplot(data = result)+ geom_line(aes(x = time, y = N) ,  colour ="blue")+ labs(x = "time (days)")

expplot 
