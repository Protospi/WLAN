#include <Rcpp.h>
using namespace Rcpp;

double square(double x){
  double squared = x * x;
  return squared;
}

// [[Rcpp::export]]
std::vector<double> conta_pontos(double x, double y, DoubleVector wlanx, DoubleVector wlany, NumericVector indice) {
  
  // Declara tamanho de pontos
  int n = wlany.size();
  double soma;
  double raio = square(85);
  
  // Declara vetor de indices
  std::vector<double> indices(0);
  
  // Laco para condicao de pertencimento
  for(int i = 0; i < n; i++) {
    
    // Calcula soma de quadrados
    soma = square(wlanx[i] - x) + square(wlany[i] - y);
    
    // Condicao de pertencimento
    if( soma <= raio) {
      
      // Popula indices
      indices.push_back(indice[i]);
      
    }
    
  }
  
  // Retorno da funcao
  return indices;
}


