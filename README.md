

## Abstract

* \textcolor{blue}{This article comprises the applied problem of the Decision Theory course from the department of Systems Engineering of UFMG taught by Professor Lucas Batista. The objective of this work is to apply the optmization and decision techniques taught throughout the semester. The proposed problem aims to minimize the number of access points routers to supply the Mbps consumption demanded by clients in a controlled environment. Also meeting pre-established optimization restrictions.}

## Introdução

* \textcolor{blue}{O problema foi modelado no software R com implementação da função objetivo em C++.}
* \textcolor{blue}{Diferentes espaços de busca foram considerados com o intuito de descobrir a partir de qual densidade de espaço de busca o algoritmo convergiria para a solução de mínimo ótima.}

## Especificações do Problema

* Deseja-se instalar uma rede WLAN do tipo N 2D para atendimento de um centro de convenções com 800 × 800 metros. Para planejamento dessa rede foram estimados 500 pontos de demanda, com suas respectivas posições geográficas e consumos de largura de banda. O arquivo clientes.csv contém:
  * Coordenada x do cliente em metros.
  * Coordenada y do cliente em metros.
  * Consumo de banda do cliente em Mbps.
  
* Amostra das 10 primeiras observações:
  
```{r warning=F, message=F, error=F, echo=F}

# Carrega Pacotes
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(Rcpp)
library(knitr)

# Importa dados
wlan <- read_csv("https://raw.githubusercontent.com/Protospi/WLAN/main/dados/clientes.csv",
                 col_names = c("x", "y", "Mbps")) %>%
                 mutate(Id = 1:500) %>%
                 relocate(Id, .before = x)

# 5 primeiras observacoes da tabela
kable(head(wlan,10))

```

* Variáveis de Decisão:
  + Coordenadas dos pontos a serem instalados.
  + Ponto de Acesso que será responsável pelo atendimento de cada cliente.
  
* Restrições:
  + Ao menos 95% dos pontos de demanda devem ter suas demandas integralmente atendidas.
  + Cada ponto de acesso a ser instalado tem capacidade de 150Mbps, que não pode ser excedida.
  + Um cliente pode ser atendido por um PA se a distância entre ambos é inferior a 85 metros.
  + Cada cliente só pode ser atendido por um único PA.
  + Devido a restrições orcamentárias, podem ser instalados no máximo 100 PA's.
  
* Simplificações:
  + Os pontos de demanda e seus consumos de banda são estáticos.
  + O efeito de obstáculos no ambiente são desprezados.
  + Um ponto de acesso não causa interferência em outros.
  
***
  
## Modelagem

* \textcolor{blue}{Definição de Variáveis:}
  + \textcolor{blue}{$Pd_i \hspace{0.2cm} \leftarrow$  Pontos de Demanda $i = \{1, 2, 3, ..., 500\}$, $Pd_i \in$ $R\{x, y\}$, x, y = $\{1, ..., 800\}$}
  + \textcolor{blue}{$Pa_j \hspace{0.16cm} \leftarrow$ Pontos de Acesso $j = \{1, 2, ..., 100\}$, $Pa_j \in$ $R\{x, y\}$, x, y = $\{1, ..., 800\}$}
  + \textcolor{blue}{$\beta_i \hspace{0.43cm} \leftarrow$  Consumo de banda em Mbps dos Pd's, $\forall \hspace{0.1cm} i \in Pd_i \hspace{0.1cm}$}
  + \textcolor{blue}{$d_{i,j} \hspace{0.22cm} \leftarrow$  Distância Euclidiana entre $Pd_i$ e $Pa_j$, $\forall \hspace{0.1cm} i \in Pd_i \hspace{0.1cm}$, $\forall \hspace{0.1cm} j \in Pa_j$}
  + \textcolor{blue}{$\alpha_j \hspace{0.38cm} \leftarrow$  Ativação do $Pa_j$, $\alpha_j$ $\in \{0,1\}$ $\forall \hspace{0.1cm} j \in Pa_j$}
  + \textcolor{blue}{$\eta_{i,j} \hspace{0.24cm} \leftarrow$  Atendimento do $Pd_i$ pelo $Pa_j$, $\eta_{i,j}$ $\in \{0,1\}$ $\forall \hspace{0.1cm} i \in Pd_i \hspace{0.1cm}$, $\forall \hspace{0.1cm} j \in Pa_j$}
  
<br>

* \textcolor{blue}{Definição da Função 1:}
  + \textcolor{blue}{min  $(\sum_{j=1}^{|P_a|} \hspace{0.1cm} \alpha_j)$}
  
<br>
  
* \textcolor{blue}{Definição da Função 2:}
  + \textcolor{blue}{min  $( \sum_{i=1}^{|P_d|} \hspace{0.1cm} \sum_{j=1}^{|P_a|} \hspace{0.1cm} d_{i,j} \hspace{0.1cm} \times \hspace{0.1cm} \eta_{i,j})$}
  
<br>
  
* \textcolor{blue}{Definição das Restrições:}
  + \textcolor{blue}{$\sum_{i=1}^{|P_d|}\hspace{0.1cm} \sum_{j=1}^{|P_a|} \hspace{0.1cm} \eta_{i,j} \hspace{0.1cm} \geq \hspace{0.1cm} 475$}
  + \textcolor{blue}{$\sum_{i=1}^{|P_d|} \hspace{0.1cm} \eta_{i,j} \hspace{0.1cm}  \times \hspace{0.1cm} \beta_i \hspace{0.1cm} \le \hspace{0.1cm} 150 \hspace{0.3cm} \forall \hspace{0.1cm} j \in \alpha_j=1$}
  + \textcolor{blue}{$\sum_{j=1}^{|Pa|} \hspace{0.1cm} \eta_{i,j} \hspace{0.1cm} \le \hspace{0.1cm} 1 \hspace{0.3cm} \forall \hspace{0.1cm} i \in Pd$}
  + \textcolor{blue}{$\sum_{j=1}^{|Pa|} \hspace{0.1cm} \alpha_j \hspace{0.1cm} \le \hspace{0.1cm} 100$}
  + \textcolor{blue}{$\exists \hspace{0.1cm} \eta_{i,j} \hspace{0.1cm} \forall \hspace{0.1cm} d_{i,j} \hspace{0.1cm} \le \hspace{0.1cm} 85$}
  
***


## Análise Exploratória


* \textcolor{blue}{Para ilustrar uma visão inicial da distribuição dos clientes no espaço de 800 x 800 foi elaborado um gráfico de dispersão 2d com os dados da localização (x, y) dos pontos de demanda.}
* \textcolor{blue}{O parâmetro alpha de transparência foi alterado para 0.5 com o intuito de eliminar o problema da sobreposição de Pd's e facilitar a compreesão da densidade destes pontos.}
* \textcolor{blue}{A dimensão de cor foi adicionada considerando o gradiente das cores preta até azul para o consumo de largura de banda requisitada em Mbps por cada cliente.}


```{r warning=F, message=F, error=F, echo=F, fig.height=8 , fig.width=9, fig.align="center"}

# Desenha posicao dos pontos de demanda
wlan %>% 
  ggplot(aes(x = x, y = y, color = Mbps))+
    geom_point(alpha = 0.5)+
    ggtitle("Gráfico de Dispersão dos Pontos de Demanda",
            subtitle = "Localização dos clientes e seus respectivos consumos em Mbps")

```

* \textcolor{blue}{Existem 2 grandes clusters na dispersão da localização dos clientes em torno dos pontos (200, 200) e (600, 600). Os demais pontos apresentam distribuição esparsa na área de 800 x 800.}

* \textcolor{blue}{Na diagonal que vai de (0,0) até (800,800) os pontos de demanda são menos espalhados enquanto que na diagonal (0,800) até (800,0) os pontos estão mais dispersos.}

* \textcolor{blue}{Parece não existir padrão visual para a distribuição das demandas de Mbps.}


## Espaços de Busca 

* \textcolor{blue}{A construção de empaços de busca considerou espaços na forma de matrizes quadradas do tipo $M_{(i,j)}$ com i = {8, 9, ..., 150} e j = {8, 9, ..., 150} dentro da área de 800 x 800. Cada (i,j) representa a possível localização de um ponto de acesso.}

* \textcolor{blue}{Para ilustra a construção do espaço de buscas foram consideradas as matrizes de $M_{8,8}$ até $M_{128,128}$ porque para espaços menores o algoritimo não converge.}


![](https://raw.githubusercontent.com/Protospi/WLAN/main/imagens/espacos_discretos.png){width=70%, height=100%}


* \textcolor{blue}{Conforme a dimensão da matriz de espaço de busca aumenta é possivel visualizar o aumento de granularidade na inspeção do espaço de busca.}

* \textcolor{blue}{Os espaços de busca entre $M_{(129, 129)}$ e $M_{(150, 150)}$, que foram utilizados na simulação, não foram apresentados no gráfico de grids porque a visualização do processo de aumento na densidade de pontos já é óbvio no intervalo $M_{(8, 8)}$ até $M_{(128, 128)}$}


## Prototipagem do Algoritmo 

* \textcolor{blue}{Função Conta Pontos em C++}

```{Rcpp eval = F}
double square(double x){
  double squared = x * x;
  return squared;
}
std::vector<double> conta_pontos(x, y, wlanx, wlany, indice) {
  int n = wlany.size(); double soma; double raio = square(85); indices(0);
  for(int i = 0; i < n; i++) {
    soma = square(wlanx[i] - x) + square(wlany[i] - y);
    if( soma <= raio) {
      indices.push_back(indice[i]);}}
  return indices;
}
```

* \textcolor{blue}{Algoritimo de Convergência em R}

```{r eval = F}
wlan_completa <- read_csv("clientes.csv", col_names = c("x", "y", "Mbps"))
wlan_id <- wlan_completa %>% mutate(indice = 1:nrow(wlan_completa))
sourceCpp('conta_pontos.cpp')
performance <- tibble(intervalos = 8:50, PA = rep(0,143), tempo = rep(0,143))

for(intervalo in seq(8, 150, by = 1)){
  wlan <- wlan_id
  centro_x <- rep(seq(0, 800, length.out = intervalo), each = intervalo)  
  centro_y <- rep(seq(0, 800, length.out = intervalo), times = intervalo)
  vencedores <- NULL; contador <- 0 ;Mbps <- NULL
  inicio <- Sys.time()
  while(contador < 475){
    cobertura <- NULL
    populoso <- 1
    for(i in seq(2, intervalo^2, by = 1)){
      if(length(conta_pontos(x = centro_x[i], y = centro_y[i],
                             wlanx = wlan$x, wlany = wlan$y, indice = wlan$indice)) >= 
         length(conta_pontos(x = centro_x[populoso], y = centro_y[populoso],
                             wlanx = wlan$x, wlany = wlan$y, indice = wlan$indice))){
        populoso <- i
        cobertura <- conta_pontos(x = centro_x[populoso], y = centro_y[populoso],
                                  wlanx = wlan$x, wlany = wlan$y, indice = wlan$indice)}}
    megas <- wlan %>% 
              filter(indice %in% as.integer(cobertura)) %>%
              summarise(total = sum(Mbps)) %>%
              pull(total)
    if(megas < 150){
      Mbps <- c(Mbps, megas)
      vencedores <- c(vencedores, populoso)
      wlan <- wlan %>% filter(!indice %in% as.integer(cobertura) )
      contador <- contador + length(cobertura)}}
  fim <- Sys.time()
  performance$tempo[intervalo-7] <- fim - inicio
  performance$PA[intervalo-7] <- length(vencedores)
}
```

## Convergência

* \textcolor{blue}{O algoritmo completo utlizado para convergência considerou espaços de busca de $M_{8,8}$ até $M_{150,150}$ contabilizando o máximo de 22500 coordenadas para possíveis localizações dos pontos de acesso.}

* \textcolor{blue}{Para cada espaço de busca foi contabilizado o número de Pontos de Acesso para atingir uma solução ótima.}


![](https://raw.githubusercontent.com/Protospi/WLAN/main/imagens/convergencia.png){width=100%, height=100%}


* \textcolor{blue}{O primeiro ponto no gráfico ( $Espaço_{(x,x)}$ = 8, Nº de Pontos de Acesso = 20 ), corresponde a varredura do espaço de busca $M_{(8,8)}$ que atinge a solução ótima de mínimo local com 20 Pontos de Acesso.}


* \textcolor{blue}{Entre $M_{(8,8)}$ e $M_{(30,30)}$ o algoritimo passa pelos locais de 20 até 13, em $M_{(40,40)}$ até $M_{(53,53)}$ o algoritimo estabiliza em 13 Pa's, em $M_{(64,64)}$ o algoritimo atinge pela primeira vez o mínimo global de 12 Pa's e entre $M_{(65,65)}$ e $M_{(115,115)}$ oscila entre 12 e 13 Pa's estabilizando em 12 Pa's para espaços mais densos que $M_{(116,116)}$} 

* \textcolor{blue}{Como foram utilizados espaços de busca que começavam sempre na coordenada 1 e a maioria dos pontos de demanda não estão próximos das extremidades da área de 800 x 800, é razoável considerar que outros pontos de ínicio para os espaços de busca talvez apresentem melhor performance.} 


## Custo Computacional

* \textcolor{blue}{Para comparar as performances do algoritimo em diferentes espaços de busca foi utilizada a função Sys.time() do R para verificar o tempo computacional gasto a cada interação de varredura dos espaços de busca.}

* \textcolor{blue}{O heardware utilizado foi um processador Intel i7 com 16 GB RAM}

![](https://raw.githubusercontent.com/Protospi/WLAN/main/imagens/custo_computacional.png){width=100%, height=100%}

* \textcolor{blue}{O primeiro ponto no gráfico ($Espaço_{(x,x)}$ = 8, Tempo (segundos) = 0,2234), corresponde a varredura do espaço de busca $M_{(8,8)}$ que atinge a solução ótima de mínimo local com custo computacional de 0.2 segundos.}

* \textcolor{blue}{A curva do tempo computacional apresenta leves ocilações em torno de sua tendência que provavelmente são oriundas das particularidades do funcionamento do hardware.}

* \textcolor{blue}{Como a função de contagem de pontos foi implementada em C++, a performance apresentou resultados muito superiores se comparados aos obtidos com toda a prototipagem em R.}

* \textcolor{blue}{O crescimento do tempo computacional é visivelmente não linear e apesar que convergir para a solução ótima global apartir de 2.5 segundos, a varredura de $M_{(800,800)}$ com 640000 possíveis localizações dos pontos de acesso custo mais de um dia para o algoritimo e identificou o mesmo ótimo global. }


## Mínimo Global 

* \textcolor{blue}{A solução ótima considerou o espaço de busca $M_{(80,80)}$ totalizando 6400 coordenadas de possíveis localizações para os pontos de acesso. Essa solução foi escolhida porque 80 é o ponto intermediário de um plato com 3 soluções consecutivas sobre o mínimo global, ou seja, a primeira vez que a solução global estabiliza.}

* \textcolor{blue}{O algoritmo varreu o espaço de busca de 6400 possíveis localizações dos pontos de acesso para determinar a localização de pontos ótimos que minimizam o número de pontos de acesso e que cobrem 95 porcento da demanda sem ultrapassar o máximo de 150 Mbps para a cobertura de cada ponto de acesso.}


![](https://raw.githubusercontent.com/Protospi/WLAN/main/imagens/solucao_80x80.png){width=100%, height=100%}

* \textcolor{blue}{É possível observar que ocorre somente uma sobreposição de áreas de cobertura dos pontos de acesso no quadrante superior esquerdo do gráfico.}

* \textcolor{blue}{Alguns pontos de acesso cobrem apenas 4 pontos de demanda e poderiam estar localizados em outras regiões cobrindo o mesmo número de pontos cobertos pela solução apresentada. Tal fato indica a existência de soluções diferentes mais igualmente eficientes apontadas pelo algoritmo de convergência.}

## Tabela de Resultados

* \textcolor{blue}{Tabela de localização dos 12 Pa's na área de 800 x 800 com os respectivos consumos totais:}

```{r warning=F, message=F, error=F, echo=F}

# Declara data frame de Pontos otimos e consumo
otimo <- tibble(Pa = 1:12,
                x = c(191, 621, 181, 681, 491, 421,  91, 701, 601, 451, 721, 371),
                y = c(201, 601, 551, 371, 491, 271, 691, 691, 191,  71,  31, 661),
                `Consumo (Mbps)` = c(146.5, 146.8, 7.4, 6.7, 6.2, 4.5, 5.0, 5.4, 6.6, 3.7, 4.6, 2.1))

# Tabela de pontos otimos
kable(otimo)

```

* \textcolor{blue}{É possível observar que os dois clusters principais com um total de 146 Mbps de consumo estão muito próximos da saturação e talvez fosse adequado a colocação de mais 2 Pa's nessas regiões para garantir a integridade da entrega do sinal onde há maior concentração de pontos de demanda.}

* \textcolor{blue}{Outra possível solução seria o deslocamento de 2 Pa's com poucos pontos de consumo para as regiões de clusters de forma que a sobreposição parcial de 2 pontos de acesso sobre os clusters principais diminua o consumo dos Pa's no limiar da saturação e também cubra os pontos vizinhos aos clusters garantindo a cobertura dos mesmos 475 pontos de demanda sem aumentar o número de Pa's.}

***

## Referências

* \textcolor{blue}{Para desenvolver este trabalho foram utilizados os slides e videos fornecidos pelo Professor Lucas Batista do departamento de Engenharia Elétrica da UFMG.}

* \textcolor{blue}{Devido a questões legais de proteção aos direitos autorais e regras da UFMG, os slides e videos não puderam ser compartilhados.}




