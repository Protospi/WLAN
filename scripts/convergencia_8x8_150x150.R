#--------------------------------------------------------------------------

# Algoritimo de Convergencia

#--------------------------------------------------------------------------

# Importa pacotes
library(tidyverse)
library(Rcpp)
library(gridExtra)

#--------------------------------------------------------------------------

# Declara data frame de graficos
p <- list()

# Laco para verificar convergencia
for(i in 1:16){
  
  # Declara grid de centro dos circulos
  centro_x <- rep(seq(0, 800, length.out = i * 8), each = i * 8)  
  centro_y <- rep(seq(0, 800, length.out = i * 8), times = i * 8)
  
  # Declara data frame de pontos de busca
  centros <- tibble(x = centro_x, y = centro_y) 
  
  # Popula df
  p[[i]] <-  ggplot(centros,aes(x = x, y = y)) +
    geom_point(size = 0.01, alpha = 1/i)+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size=10))+
    ggtitle(paste0(i^2*8^2, " Pontos"))
  
}

# Arranja grid
do.call(grid.arrange, c(p))

#--------------------------------------------------------------------------

# Importa dados
wlan_completa <- read_csv("clientes.csv",
                          col_names = c("x", "y", "consumo"))

# Insere colunas de indice
wlan_id <- wlan_completa %>%
  mutate(indice = 1:nrow(wlan_completa))

# Implementacao de conta_pontos em cpp
sourceCpp('conta_pontos.cpp')

# Declara data frame de performance
performance <- tibble(intervalos = seq(8, 150, by = 1),
                      PA = rep(0,143),
                      tempo = rep(0,143))


# Laco para verificar convergencia
for(intervalo in seq(8, 150, by = 1)){
  
  # Declara banco de dados
  wlan <- wlan_id
  
  # Declara grid de centro dos circulos
  centro_x <- rep(seq(0, 800, length.out = intervalo), each = intervalo)  
  centro_y <- rep(seq(0, 800, length.out = intervalo), times = intervalo)
  
  # Declara vencedores
  vencedores <- NULL
  
  # Contador de pontos cobertos
  contador <- 0
  
  # Declara total megabites
  Mbps <- NULL
  
  # Inicia contador de tempo
  inicio <- Sys.time()
  
  # Laco para para varrear espaco
  while(contador < 475){
    
    # Numero de Pontos cobertos
    cobertura <- NULL
    
    # Declara Inicio
    mais_populoso <- 1
    
    # Laco para determinar maior contagem
    for(i in seq(2, intervalo^2, by = 1)){
      
      # Condicao para comparar circulos com maior quantidade de pontos
      if(length(conta_pontos(x = centro_x[i],
                             y = centro_y[i],
                             wlanx = wlan$x,
                             wlany = wlan$y,
                             indice = wlan$indice)) >= 
         length(conta_pontos(x = centro_x[mais_populoso],
                             y = centro_y[mais_populoso],
                             wlanx = wlan$x,
                             wlany = wlan$y,
                             indice = wlan$indice))){
        
        # Atualiza circulo mais populoso
        mais_populoso <- i
        
        # Atualiza pontos cobertos por circulo mais populoso
        cobertura <- conta_pontos(x = centro_x[mais_populoso],
                                  y = centro_y[mais_populoso],
                                  wlanx = wlan$x,
                                  wlany = wlan$y,
                                  indice = wlan$indice)
        
      }
      
    }
    
    # Atualiza total de megas
    megas <- wlan %>% 
      filter(indice %in% as.integer(cobertura)) %>%
      summarise(total = sum(consumo)) %>%
      pull(total)
    
    # Condicao verifica Mbps
    if(megas < 150){
      
      # Atualiza totais de Mbps
      Mbps <- c( Mbps, megas)
      
      # Atualiza vencedores
      vencedores <- c(vencedores, mais_populoso)
      
      # Remove pontos ja cobertos
      wlan <- wlan %>% filter(!indice %in% as.integer(cobertura) )
      
      # Atualiza contador
      contador <- contador + length(cobertura)
      
      # Verbaliza passos da funcao
      print(paste0("Indice: ", intervalo, " - ", "Contador: ", contador))
      
    }
    
  }
  
  # Finaliza contador de tempo
  fim <- Sys.time()
  
  # Atualiza tempo
  performance$tempo[intervalo-7] <- fim - inicio
  
  # popula Quantidade de Pontos de Acessos utilizados
  performance$PA[intervalo-7] <- length(vencedores)
  
}

# ------------------------------------------------------------------------

# Grafico de performance numero de pa's
ggplot(data = performance, aes(x = intervalos, y = PA))+
  geom_line(size = 0.7, color = "forestgreen") +
  ggtitle("Convergência do Nº de Pontos de Acesso",
          subtitle = "Quantidade mínima de pontos de acesso por espaço de busca que atendem os requisitos do problema.")+
  coord_cartesian(xlim = c(5, 150), ylim = c(11, 20))+
  geom_point(size = 1.5, color = "red", alpha = 0.3)+
  ylab("Nº de Pontos de Acesso")+
  xlab(expression(Espaço["(x,x)"]))+
  scale_y_continuous(breaks = seq(11, 20, 1))+
  scale_x_continuous(breaks = seq(0, 150, 10))

# ------------------------------------------------------------------------

# Grafico de performance tempo computacional
ggplot(data = performance, aes(x = intervalos, y = tempo))+
  geom_line(size = 1, color = "indianred") +
  ggtitle("Custo Computacional",
          subtitle = "Tempo em segundos por espaço de busca necessário para atingir o mínimos local.")+
  coord_cartesian(xlim = c(5, 150), ylim = c(0, 4))+
  ylab("Tempo (segundos)")+
  xlab(expression(Espaço["(x,x)"]))+
  scale_y_continuous(breaks = seq(0, 4, 0.5))+
  scale_color_brewer(palette="Reds")+
  scale_x_continuous(breaks = seq(0, 150, 10))

# ------------------------------------------------------------------------


