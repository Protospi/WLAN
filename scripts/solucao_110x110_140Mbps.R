
# --------------------------------------------------------------------------

# Solução com espaço discreto M de 120 x 120

# --------------------------------------------------------------------------

# Importa pacotes
library(tidyverse)
library(ggplot2)
library(Rcpp)

# Importa dados
wlan_completa <- read_csv("dados/clientes.csv",
                          col_names = c("x", "y", "Mbps"))

# Insere colunas de indice
wlan <- wlan_completa %>%
  mutate(indice = 1:nrow(wlan_completa))

# Declara base de circulos espaco de solucoes
centro_x <- rep(seq(1, 800, length.out = 110), each = 110)  
centro_y <- rep(seq(1, 800, length.out = 110), times = 110) 

# --------------------------------------------------------------------------

# Implementacao de conta_pontos em cpp
sourceCpp('scripts/rastreador.cpp')

# --------------------------------------------------------------------------

# Declara vencedores
vencedores <- NULL

# Contador de pontos cobertos
contador <- 0

# Declara total megabites
Mbps <- NULL

# Vetor de Pa's que violam condicao de Mbps
viola <- 0

# Marca tempo de inicio
inicio <- Sys.time()

# Laco para para varrear espaco
while(contador < 475){
  
  # Numero de Pontos cobertos
  cobertura <- NULL
  
  # Declara Inicio
  mais_populoso <- 1
  
  # Chama funcao
  rastreado <- rastreia(intervalo_x = 110,
                        intervalo_y = 110,
                        wlanx = wlan$x,
                        wlany = wlan$y,
                        centro_x = centro_x,
                        centro_y = centro_y,
                        indice = wlan$indice,
                        mais_populoso = mais_populoso,
                        viola = viola)
  
  # Atualiza total de megas
  megas <- wlan %>% 
    filter(indice %in% as.integer(rastreado[[1]])) %>%
    summarise(total = sum(Mbps)) %>%
    pull(total)
  
  # Condicao verifica Mbps
  if(megas < 140){
    
    # Atualiza totais de Mbps
    Mbps <- c( Mbps, megas)
    
    # Atualiza vencedores
    vencedores <- c(vencedores, rastreado[[2]])
    
    # Remove pontos ja cobertos
    wlan <- wlan %>% filter(!indice %in% as.integer(rastreado[[1]]) )
    
    # Atualiza contador
    contador <- contador + length(rastreado[[1]])
    
    # Verbaliza passos da funcao
    print(paste0("Indice: ", 110, " - ",
                 110, " - ",
                 "Contador: ", contador))
    
  } else {
    
    # Insere indice de pa que viola condicoes
    viola <- c(viola, rastreado[[2]])
    
  }
  
}

# Marca tempo de inicio
fim <- Sys.time()

# Tempo gasto
fim - inicio

# Quantidade de Pontos de Acessos utilizados
length(vencedores)

# Imprime Mbps
Mbps

# --------------------------------------------------------------------------


# Gera data frame de resultados
resultados <- tibble(x = centro_x[vencedores],
                     y = centro_y[vencedores],
                     Mbps_total = Mbps)

# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Declara nome de colunas
colunas <- c("x", "y")

# Grafico Base
p <- ggplot()+
  geom_point(aes(x = x, y = y, color = Mbps),
             data = wlan_completa,
             alpha = 0.8, size = 3) 

# Desenha Pa's vermelhos
p <- p + geom_point(resultados,
                    mapping =  aes(x = x, y = y, fill = "red"),
                    color = "red",
                    size =  3,
                    alpha = 0.9) +
  ggtitle("Posição Ótima para os Pontos de Acesso",
          subtitle = "Mínimo global de 14 Pa's no espaço discreto de 110 x 110, máximo de 140 Mbps por Pa.")+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10))+
  scale_fill_identity(name = "Pa's", guide = 'legend', labels = c('')) 

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 85 * cos(1:360),
           y = centro_y[i] + 85 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 2.5,
                     alpha = 0.1)
  
}


# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 62.5 * cos(1:360),
           y = centro_y[i] + 62.5 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 1.7,
                     alpha = 0.15)
  
}

# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 40 * cos(1:360),
           y = centro_y[i] + 40 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 1,
                     alpha = 0.4)
  
}


# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 20 * cos(1:360),
           y = centro_y[i] + 20 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 0.7,
                     alpha = 0.7)
  
}

# --------------------------------------------------------------------------

# Imprime grafico
p + theme_gray()

# --------------------------------------------------------------------------

# Tabela de Soluções
round(resultados)


# --------------------------------------------------------------------------
