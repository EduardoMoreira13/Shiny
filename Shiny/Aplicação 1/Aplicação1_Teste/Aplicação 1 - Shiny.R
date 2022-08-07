# Aplicação 1 - Shiny em demografia

library(readxl)
library(tidyverse)

dados_TO <- read_excel("dados_TO.xlsx")

# Tabelas

dados_TO %>% 
  group_by(Ano) %>%
  filter(Idade == "15 a 19")

dados_TO %>% 
  group_by(Idade) %>%
  filter(Ano %in% c(2000,2005))

# Gráfico - População

dados_TO %>% 
  filter(Idade %in% c("15 a 19", "20 a 24", "25 a 29")) %>% 
  ggplot(aes(x = Ano, y = Pop_Fem, col = Idade)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "População das mulheres em Idade Reprodutiva 
do Tocantins segundo Grupo Etário - 2000/2021",
       x = "Anos",
       y = "População Feminina") +
  scale_y_continuous(breaks = seq(from = 20000, to = 80000, by = 10000),
                     limits = c(20000,80000)) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2022, by = 2),
                     limits = c(2000,2022)) +
  theme_bw()


# Gráfico - Taxa Específica de Fecundidade

dados_TO %>% 
  filter(Idade %in% c("15 a 19", "20 a 24", "35 a 39")) %>% 
  ggplot(aes(x = Ano, y = TEF, col = Idade)) +
  geom_line() +
  geom_point() +
  labs(title = "Taxa Específica de Fecundidade - TO - 2000/2021",
       x = "Anos",
       y = "População Feminina") +
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 20),
                     limits = c(0,200)) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2022, by = 2),
                     limits = c(2000,2022)) +
  theme_bw()


TEF <- dados_TO %>% 
       filter(Ano == 2000)

TFT = 5 * sum(TEF$TEF/1000)



