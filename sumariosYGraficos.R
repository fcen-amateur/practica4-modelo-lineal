library("tidyverse")
library("ggplot2")
library("GGally")
                                        # Graficar qqnorm.
                                        # Graficar comparación de longitudes.
                                        # Graciar tendencia a cubrir.
#¿Anova sobre el factor "cubre"?

intervalos <- read_rds("simulacion.Rds") %>% mutate(estimador = 0.5*(ic_upp-ic_low) + ic_low, longitud = ic_upp - ic_low )

exponencial <- intervalos %>% filter(distr_eps=='exponencial',fun_a=='beta1')
consigna1 <- intervalos %>% filter(distr_eps=='exponencial',fun_a=='beta1',n==10)


ggplot(exponencial, aes(sample =estimador)) + geom_qq() + stat_qq_line() + facet_grid(n~.)
ggplot(intervalos, aes(sample =estimador)) + geom_qq() + stat_qq_line() + facet_grid(n~distr_eps)


ggplot(intervalos %>% filter(distr_eps=='exponencial',n<=100), aes(y =longitud, x=met_int )) + geom_boxplot() + facet_grid(n~met_int)

consigna1 %>%
  select(n,met_int) %>%
  ggpairs(mapping = aes(color = met_int))

intervalos_que_cubren <- intervalos %>%
          group_by(distr_eps, n, met_int, fun_a) %>%
          summarise(prop_cubre = mean(cubre)) 


intervalos %>% filter(fun_a=='beta1',distr_eps=='exponencial') %>%
  ggplot(aes(x =cubre)) + geom_bar() + facet_grid(n~met_int)

## ¿Los Estimadores Tienen Distribución Normal?

```{r qqses por n y distr_eps}
ggplot(intervalos %>% filter(n_sim <= 50, is.element(n,c(10,100,1000)),fun_a=='beta1' ), aes(sample =estimador, color = distr_eps)) + geom_qq() + stat_qq_line() + facet_grid(n~distr_eps, scales='free')
```
