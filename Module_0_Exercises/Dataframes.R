# Como correr código
# Ativar Source on Save OU
# Selecionar todo o documento e clicar ctrl/command+Enter

# Criação de dataframe
# Criar um dataframe chamado de data
# Deve ter as seguintes colunas e 5 linhas, na seguinte ordem
# ID deve ser apenas inteiros
# Nome deve ser o vetor de texto
# Idade deve ser apenas números inteiros
  # Valores dados devem ser 30, 65, 47, 53 80
# Data_da_consulta - colocar uma vetor de data

data <- data.frame(
  ID = 1:5,
  Nome = c("Alice", "Bruno", "Carla", "David", "Eva"),
  Idade = c(30, 65, 47, 53, 80),
  Data_da_consulta = seq(as.Date("2023-12-01"), by = "day", length.out = 5)
)

# Extrair os dados da coluna Idade e calcular média
# obejeto deve ter nome media_idade

media_idade <- mean(data$Idade)

# Criar uma nova base de dados anonimizada apenas com o ID e idade 
# data_anonimizada

data_anonimizada <- data[,c("ID","Idade")]

# Adicionar a coluna do género
# Genero

data$Genero <- c("M","F","M","F","M")


#Não introduzir código abaixo - Validação

sink("output_dataframes.txt",split = T)


# Check if 'data' dataframe is created
if(exists("data")) {
  print(":) DataFrame 'data' criado com sucesso")
} else {
  print(":( DataFrame 'data' não existe")
  data <- data.frame(
    ID = "A"
  )
}
if(exists("data")& is.integer(data$ID)
                 & is.character(data$Nome)
                 & (is.integer(data$Idade)| is.numeric(data$Idade))) {
  print(":) DataFrame 'data' tem as primeiras 5 colunas do tipo adequado")
} else {
  print(":( DataFrame 'data' não tem estrutura adequada")
}
# Check if average age is calculated
if(exists("media_idade")) {
  print(paste(":) Média de idade calculada:", media_idade))
} else {
  print(":( Média de idade não foi calculada")
}

# Check if anonymized dataframe is created
if(exists("data_anonimizada")) {
  print(":) DataFrame 'data_anonimizada' criado com sucesso")
} else {
  print(":( DataFrame 'data_anonimizada' não existe")
  data_anonimizada <- data.frame()
}
if(length(colnames(data_anonimizada)==2)
   & identical(sort(colnames(data_anonimizada)), c("ID", "Idade"))) {
  print(":) DataFrame 'data_anonimizada' criado com colunas pedidas")
} else {
  print(":( DataFrame 'data_anonimizada' não tem as condições")
}

if(length(colnames(data)==6)
   & "Genero"%in% colnames(data)) {
  print(":) Coluna Genero criada e na dataframe")
} else {
  print(":( Coluna Genero não criada ou não na dataframe")
}

sink()

rm(list = ls())