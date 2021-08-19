library(deSolve)
library(reshape2)
library(ggplot2)

initial_state_values <- c(S = 999999,I=1,R=0)
parameters <- c(lambda = 0.2,gamma=0.1)
times <- seq(from = 0, to = 60, by = 1) 

sir_model <- function(time, state, parameters) { 
  with(as.list(c(state, parameters)), { 
    dS <- -lambda * S  
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR))) 
  })
}
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
output
output_long <- melt(as.data.frame(output), id = "time") 

ggplot(data = output_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +   
  
  geom_line() +                                                          
  xlab("Time (days)")+                                                  
  ylab("Number of people") +                                             
  
  labs(colour = "Compartment")  
