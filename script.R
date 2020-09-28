## Feito por: Julia Hellen franco Ferreira - Script do teste

## Pacotes utilizados:

library(tidyverse)

## Questão 1

v <- NULL
max <- function(v){
  maior <- v[1]
  for(i in 1:length(v)){
    if(maior < v[i]){
      maior <- v[i]
    }
  }
  return(maior)
}
max(c(1,2,600,5,10,500))

## Questão 2


Num_Int = seq(1:100)
df <- as.data.frame(Num_Int)

normal <- function(df, media){
  for (i in 1:100){
    for (j in 2:101) {
      lista <- rnorm(100, media)
      df[i,j] <- lista[i]
    }
  }
  return(df)
}

resultado <- normal(df, 10)
head(resultado, 10)

## Questão 3 

library(rio)
library(tidyverse)
(data <- import("https://www.rug.nl/ggdc/docs/10sd_jan15_2014.xlsx",
                sheet = 2,
                setclass = "tibble") %>%
    filter(Variable == "VA_Q05" | Variable == "EMP") %>%
    select(-"Region code", -"Summation of sector GDP"))


##

Resultado <- data %>% 
  gather(c(Agriculture, Mining, Manufacturing, Utilities, Utilities, Construction,
           `Trade, restaurants and hotels`, `Transport, storage and communication`,
           `Finance, insurance, real estate and business services`,
           `Government services`, `Community, social and personal services`),
         key = "Sector", value = "Valor") %>% 
  spread(key = Variable, value = Valor)

Resultado

## Questão 4

teste <- function(vetor){
  if (is.numeric(vetor)){
    resposta = mean(vetor)
  }
  if (is.character(vetor)){
    for (i in 1:length(vetor)){
      vetor1 <- as.data.frame(vetor)
      contagem <- vetor1 %>% 
        group_by(vetor) %>% 
        summarise(n = n())
      maior <- as.numeric(max(contagem$n))
      contagem <- contagem %>% 
        filter(contagem$n == maior)
      if(contagem$n > length(vetor)/3){
        resposta <- maior/length(vetor)
      }
      else{
        resposta <- contagem %>% 
          filter(contagem$n == maior)
      }
    }
  }
  return(resposta)
}

teste(c(1,2,3))
teste(c("a","a","b","j","i","o"))

## Questão 5

fib <- function(n){
  f <- c(1,1)
  for(i in 3: n){
    f[i] <- f[i - 1] + f[i-2]
  }
  return(f)
}

fib(12)