# CACLULOS DE ANUALIDADES VENCIDAS
En este repositorio contienen funciones para relizar calculos relacionados con anualidades vencidas. Estas funciones te permitiran calcular el valor futuro, el valor actual, la cantidad de la anualida, la tasa de interes por periodo y eñl numero de pagos, basandote en diferentes parametros de entrada.

# FUNCIONES DE ANUALIDADES ANTICIPADAS
Con el siguiente link puede cargar las funciones relativas con el calculo de las mismas

```{r}
https://raw.githubusercontent.com/DEX-ILOVE/Anualidades-vencidas-/refs/heads/main/anualidades%20vencidas.R
```
A continucacion se da un ejemplo con las funciones correspondientes

# VALOR FUTURO

# Datos iniciales
$Anualidad$ = 1000

$r$ = 0.05

$n$ = 10

# CALCULAMOS CON LA FORMULA CORRESPONDIENTE

Valor_futuro_anualidad = function(anualidad, r, n){
  return(anualidad * (((1 + r)^n - 1) / r))

# MOSTRAMOS EL RESULTADOS DADO

valor_futuro = Valor_futuro_anualidad(anualidad, r, n)
print(paste("valor futuro:", valor_futuro))

