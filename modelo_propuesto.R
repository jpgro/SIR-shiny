
library(data.table)
library(ggplot2)

dias <- 300
t <- 1:dias

dias_incubacion <- 5
dias_infeccion <- 14
dias_inmunidad <- 60

N <- 1000
S <- rep(0, dias)
E <- rep(0, dias_incubacion+1)
I_sint <- rep(0, dias_infeccion+1)
IS_cuarentena <- rep(0, dias_infeccion+1)
I_asint <- rep(0, dias_infeccion+1)
I <- rep(0, dias) # E + I_sint + I_asint
R <- rep(0, dias_inmunidad+1)

E[1] <- 4
I[1] <- E[1] + I_sint[1] + I_asint[1]
S[1] <- N - I[1]

# probabilidades
prob_infectarse <- 0.05
prob_sintomatico <- 0.7
prob_cuarentena_sint <- 0.9
prob_recuperarse <- 0.95

# Rango de personas con las que alguien interactua
mean_interactions <- 5
stdDesv_interactions <- 2

for( i in 2:dias ) {
  # Estamos procesando el dia i
  aux <- i
  # Pasar de susceptible a infectado
  p_encuentro_infectivo <- (I[i]-sum(IS_cuarentena))/(I[i]-sum(IS_cuarentena)+S[i]+sum(R))
  if( is.na(S[i]) )
    break
  if( S[i] != 0 ) {
    interacciones <- abs(floor(rnorm(S[i], mean_interactions, stdDesv_interactions)))
    contagios <- rep(0, S[i])
    for( j in seq(S[i]) ) {
      contagios[j] <- min(1, rbinom(1, interacciones[j], p_encuentro_infectivo*prob_infectarse))
    }
    #cat("", fill = TRUE)
    infected <- sum(contagios)
  }
  else  {
    infected <- 0
  }
  
  # Nuevas personas incubando
  E <- shift(E, n=1, fill=0)
  E[1] <- infected
  
  # Las que dejaron de incubar pasan a infectados
  muestran_sintomas <- sum(rbinom(E[dias_incubacion+1], 1, prob_sintomatico))
  
  I_sint <- shift(I_sint, n=1, fill=0)
  I_asint <- shift(I_asint, n=1, fill=0)
  I_sint[1] <- muestran_sintomas
  I_asint[1] <- E[dias_incubacion+1] - muestran_sintomas
  
  # Los que tienen sintomas pueden pasar a cuarentena
  entran_cuarentena <- sum(rbinom(muestran_sintomas, 1, prob_cuarentena_sint))
  IS_cuarentena[1] <- entran_cuarentena
  I_sint[1] <- I_sint[1] - entran_cuarentena
  
  # Los que se recuperan pueden pasar a recuperarse
  recovered <- sum(rbinom(I_sint[dias_infeccion+1]+I_asint[dias_infeccion+1]+
                            IS_cuarentena[dias_infeccion+1], 1, prob_recuperarse))
  
  R <- shift(R, n=1, fill=0)
  R[1] <- recovered
  
  # Los que pierden inmunidad vuelven a los susceptibles
  S[i+1] <- S[i]-infected+R[dias_inmunidad+1]
  I[i+1] <- I[i]+infected-(I_sint[dias_infeccion+1]+I_asint[dias_infeccion+1]+
                             IS_cuarentena[dias_infeccion+1])
}

p <- ggplot() +
  geom_line(aes(x=t, y=S, color='Susceptibles')) +
  geom_line(aes(x=t, y=I, color='Infectados')) +
  xlab('Tiempo (s)') +
  ylab('Personas') +
  labs(title = 'Modelo SIR')

p