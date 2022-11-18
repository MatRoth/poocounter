library(RSQLite)
library(tidyverse)
library(DBI)
library(lubridate)

db <- dbConnect(SQLite(),"/srv/shiny-server/poosrv/poobase.sqlite")

data <- dbReadTable(db,"pootable") %>%
  as_tibble %>%
  mutate(time = map_dbl(time,as_datetime))%>%
  arrange(time)

poo_data<-data %>%
  filter(str_detect(event,"Kacka")) %>%
  mutate(time = as_datetime(time),
         diff_to_last = c(NA_real_,map2_dbl(time[-length(time)],time[-1],
                             \(left,right){
                               difftime(left,right,units="mins") |> as.double()
                             })) %>% abs %>% round) %>%
  drop_na() %>%
  filter(diff_to_last < 500) %>%
  mutate(diff_to_last_lag = lag(diff_to_last)) %>%
  drop_na()

#checking some distributions

AIC(lm(diff_to_last~1,data = poo_data))
AIC(glm(diff_to_last~1,family = poisson("log"),data = poo_data))
AIC(glm(diff_to_last~1,family = Gamma("log"),data = poo_data)) # -> wins

#checking regression model
AIC(glm(diff_to_last~diff_to_last_lag,family = Gamma("log"),data = poo_data))

#Simulation from wining model (gamma, w/o reg)

#simulate parameter distr
MASS::mvrnorm(200,mu = MASS::fitdistr(poo_data$diff_to_last,"Gamma") %>% coef,Sigma = MASS::fitdistr(poo_data$diff_to_last,"gamma") %>% vcov)
#simulate values

