# Como correr código
# Selecionar todo o documento e clicar ctrl/command+Enter

 
# Criação de vetores
# Criar um vetor com 3 elementos com nomes de utentes com o nome
# nomes_pacientes



# Criar um vetor numerico com 3 elementos com pesos de utentes e calcular média
# peso_pacientes com os valores 40, 50 , 60
# media é igual a média de 50




# Criar um factor com 3 elementos com os dados de tipo de sangue
# nome do vetor = tipos_de_sangue


#Não introduzir código abaixo - Validação

sink("output_vectors.txt",split = T)

if(exists("nomes_pacientes")){
  print(":) Vetor criado com sucesso")}
if(!exists("nomes_pacientes")){
  print(":( Vetor nomes_pacientes não existe")
  nomes_pacientes <- c()
}
if(exists("nomes_pacientes")){
  if(is.character(nomes_pacientes)){
    print(":) Vetor criado é do tipo caracter") 
  }
} 
if(!is.character(nomes_pacientes)|!exists("nomes_pacientes")){
print(":( Vetor não é do tipo caracter")} 

if(length(nomes_pacientes) == 3) {
  print(":) Vetor tem 3 elementos")
} else {
  print(":( Vetor não tem 3 elementos")
}

if(exists("peso_pacientes")){
  print(":) Vetor peso_pacientes criado com sucesso")}
if(!exists("peso_pacientes")){
  print(":( Vetor peso_pacientes não existe")
  nomes_pacientes <- c()
}
if(exists("peso_pacientes")){
  if(is.numeric(nomes_pacientes)){
    print(":) Vetor peso_pacientes criado é do tipo numerico") 
  } else {
    print(":( Vetor peso_pacientes criado é do tipo numerico")
  }
} 
if(exists("media")){
  print(":) Vetor media criado com sucesso")}
if(!exists("peso_pacientes")){
  print(":( Vetor peso_pacientes não existe")
  nomes_pacientes <- c()
}
if(exists("media")){
  print(":) Vetor media criado com sucesso")
  if(media==50){
    print(":) Média é o valor esperado") 
  } else {
    print(":( Média não é o valor esperado")
  }
}
if(!exists("media")){
  print(":( Vetor media não existe")
  print(":( Média não é o valor esperado")
  media <- c()
}
if(exists("tipos_de_sangue")){
  print(":) Vetor tipos_de_sangue criado com sucesso")}
if(!exists("tipos_de_sangue")){
  print(":( Vetor tipos_de_sangue não existe")
  tipos_de_sangue <- c()
}
if(exists("tipos_de_sangue")){
  if(is.factor(tipos_de_sangue)){
    print(":) Vetor tipos_de_sangue criado é do tipo factor") 
  } else {
    print(":( Vetor tipos_de_sangue criado é do tipo factor")
  }
} 

rm(list = ls())