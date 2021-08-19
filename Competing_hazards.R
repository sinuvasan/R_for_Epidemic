cohort_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {  

    
    dI <- -gamma * I - mu * I   
    dR <- gamma * I               
    dM <- mu * I                     
    
    
    
    return(list(c(dI, dR, dM))) 
  })
  
}

initial_state_values <- c(I = 1000000,   
                          R = 0,                   
                          M = 0)                  

parameters <- c(gamma = 0.1,       
                mu = 0.2)                         

times <- seq(from = 0, to = 4*7, by = 1) 



library(deSolve)
library(reshape2)
library(ggplot2)
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))

output_long <- melt(as.data.frame(output), id = "time")     

ggplot(data = output_long,   
       aes(x = time, 
           y = value, 
           colour = variable, 
           group = variable)) +       
  geom_line() +                       
  xlab("Time (days)")+                
  ylab("Number of people") +          
  labs(colour = "Compartment")        

output

output[output$time == 28,]

output[29,"M"]/1000000

