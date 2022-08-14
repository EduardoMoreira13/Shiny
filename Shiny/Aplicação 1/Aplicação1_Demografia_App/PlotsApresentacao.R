pacman::p_load(tidyverse, ggplot2, readxl, tibble, infer, readxl,janitor, ggcorrplot, knitr, ggsave)
install.packages(ggs)
menumc$Category

categorias <- menumc %>%
  filter(!is.na(Category)) %>%
  count(Category) %>%
  mutate(
    freq = n )

ggplot(categorias, aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = '#A11D21', width = 0.7) +
  labs( x = "Categorias", y = "Frequências")+
  theme_bw()


calorias <- menumc %>% 
  filter(!is.na(Calories)) %>% 
  count(Calories) %>% 
  mutate ( freq = n)




ggplot(calorias, aes(x = Calories, y = n)) +
  geom_point(stat = "identity", color = '#A11D21', size = 1.5) +
  labs( x = "Calorias", y = "Frequências")+
  theme_bw()

