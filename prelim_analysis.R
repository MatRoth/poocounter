library(RSQLite)
library(tidyverse)
library(DBI)
library(lubridate)

db <- dbConnect(SQLite(),"/srv/shiny-server/poosrv/poobase.sqlite")

data <- dbReadTable(db,"pootable") %>%
  as_tibble %>%
  mutate(time = map_dbl(time,as_datetime))%>%
  arrange(time)
dbDisconnect(db)
poo_data<-data %>%
  filter(str_detect(event,"Kacka")) %>%
  mutate(time = as_datetime(time),
         diff_to_last = c(NA_real_,map2_dbl(time[-length(time)],time[-1],
                             \(left,right){
                               difftime(left,right,units="mins") |> as.double()
                             })) %>% abs %>% round) %>%
  drop_na() %>%
  filter(diff_to_last < 300) %>%
  mutate(diff_to_last_lag = lag(diff_to_last)) %>%
  drop_na()

#checking some distributions

AIC(lm(diff_to_last~1,data = poo_data))
AIC(glm(diff_to_last~1,family = poisson("log"),data = poo_data))
AIC(glm(diff_to_last~1,family = Gamma("log"),data = poo_data)) # -> wins

#checking regression model
AIC(glm(diff_to_last~diff_to_last_lag,family = Gamma("log"),data = poo_data))
AIC(lm(diff_to_last~diff_to_last_lag,data = poo_data))
#Simulation from wining model (gamma, w/o reg)

#simulate parameter distr
sim_params<-MASS::mvrnorm(200,mu = MASS::fitdistr(poo_data$diff_to_last,"Gamma") %>% coef,Sigma = MASS::fitdistr(poo_data$diff_to_last,"gamma") %>% vcov)
#simulate values
sims<-map2(sim_params[,1],sim_params[,2],\(shape,rate) rgamma(1000,shape,rate)) %>%
  flatten_dbl() %>%
  round()

hist(sims)
summary(sims)
quantile(sims,c(0.01,0.99))

#sim without parameter variation
shape <- MASS::fitdistr(poo_data$diff_to_last,"Gamma")$estimate[1]
rate <- MASS::fitdistr(poo_data$diff_to_last,"Gamma")$estimate[2]

sims_simple <- rgamma(10000,shape,rate)
hist(sims_simple)
summary(sims_simple)
quantile(sims_simple,c(0.01,0.99))
summary(poo_data$diff_to_last)

#function to calculate probability that poo has happened in M minutes
prob_of_event <- function(cur_time,time_ahead,shape,rate){
  pgamma(cur_time+time_ahead,shape,rate)
}

prob_of_event_dif <- function(cur_time,time_ahead,shape,rate){
  end <- pgamma(cur_time+time_ahead,shape,rate)
  start <- pgamma(cur_time,shape,rate)
  end-start
}

prob_of_event_dif(200,120,shape,rate)
