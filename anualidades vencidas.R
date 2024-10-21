#1Funcion para calcular el valor futuro de una anualidad venciada 
Valor_futuro_anualidad = function(anualidad, r, n){
  return(anualidad * (((1 + r)^n - 1) / r))
}



#Funcion para calcular la anualidad dada un valor futuro
anualidad_dada_futuro = function(valor_futuro, r, n){
  return(valor_futuro * r / ((1 + r)^n -1))
}



#Funcion para calcular el numero de pagos
numero_pagos = function(valor_futuro, anualidad, r){
  return(log((valor_futuro * r + anualidad) + 1) / log(1 + r))
}



#Funcion para calcular la tasa de interes 
tasa_interes = function(anualidad, valor_futuro, n){
  f = function(r){
    anualidad * ((1 + r)^n - 1)/ r - valor_futuro
  }
#usamos uniroot para encontrar la tasa
  result = uniroot(f, c(0, 1)) #intervalo de 0 a 1 para la tasa 
  return(result$root)
  }



#Funcion para calcular el valor actual de una anualidad vencida
valor_actual_anualidad = function(anualidad, r, n){
  return(anualidad * ((1 - (1 + r)^-n) / r))
}


#Funcion para caclular la anualidad dada un valor actual
anualidad_dado_actual = function(valor_actual, r, n){
  return((valor_actual * r) / (1 - (1 + r)-n))
}



#Funcion para calcular el numero de pagos dados un valor actual 
numero_pagos_actual = function(valor_actual, anualidad, r){
  return((log(anualidad) - log(anualidad - valor_actual * r)) / log(1 + r))
}



#Funcion para calular la tasa de interes dado un valor actual
tasa_interes_periodo = function(anualidad, valor_actual, n){
  f = function(r){
    anualidad * (1 - (1 + r)^-n) / r - valor_actual
  }
#usamos uniroot para encontrar la tasa 
 result = uniroot(f, c(0, 1)) #intervalo de 0 a 1 para nuestra tasa
 return(result$root)
}




