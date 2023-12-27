# Como correr código
# Ativar Source on Save OU
# Selecionar todo o documento e clicar ctrl/command+Enter

 
# Criação de Calculadora que soma elementos
# Criar função multiply que multipla 2 argumentos dados 

multiply <- function(x, y){
  z <- x*y
  return(z)
}

# Criar um Calcudora de BMI
# Criação de função com nome BMI com 2 argumentos o peso e a altura em cm e calcula o Indice de Massa Corporal 

BMI <- function(peso, altura){
  bmi <- peso/(altura/100)^2
  return(bmi)
}


#Não introduzir código abaixo - Validação

sink("output_functions.txt",split = T)


if(exists("multiply")){
  print(":) Objeto multiply existe")}
if(!exists("multiply")){
  print(":( Objeto multiply não existe")
  multiply <- function(x,y){
    x <- NA
    return(x)
  }
}
if(exists("multiply")){
  if(multiply(2,2)==4){
    print(":) multiply(2,2) é igual a 4") 
  } else{
    print(":) multiply(2,2) não é igual a 4")
  }
  if(multiply(3,2)==6){
    print(":) multiply(3,2) é igual a 6") 
  } else{
    print(":( multiply(3,2) não é igual a 6")
  }
} 
if(exists("BMI")){
  print(":) Objeto BMI existe")}
if(!exists("BMI")){
  print(":( Objeto BMI não existe")
  BMI <- function(x,y){
    x <- NA
    return(x)
  }
}

if(exists("BMI")){
  if(round(BMI(80,180),2)==24.69){
    print(":) BMI(80,180) é igual a 24,69") 
  } else{
    print(":) BMI(80,180) é igual a 24,69")
  }
  if(round(BMI(40,150),2)==17.78){
    print(":) BMI(40,150) é igual a 17,78") 
  } else{
    print(":( BMI(40,150) não é igual a 17,78")
  }
} 

sink()

rm(list = ls())