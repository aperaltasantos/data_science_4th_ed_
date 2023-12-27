## Includes

Reutilizar conteúdo em Quarto é possível usando o código "{{< include >}}". 

Isto permite reutilizar código de outros documentos sem precisar de copiar e colar.

```{r}
#| echo: true

# ---
# title: "Relatórios Conjuntos"
# ---
# 
# {{< include Relatório_hospitalA.qmd >}}
# {{< include Relatório_hospitalB.qmd >}}
# 
# Usar os dados e outputs do documentos atuais...
```


Params definition in Quarto and Run.r fro multiple reports

# Exercício 1

- No dataset covid_inc, calcule a média e desvio padrao da incidencia por ARS.

```{r}
mean_covid <-covid_inc %>%
  group_by(ARS) %>% 
  summarise(
    mean_Incidencia=mean(Incidencia, na.rm=TRUE),
    SD_Incidencia=sd(Incidencia, na.rm=TRUE)
  )
```

Exercicio
1. Dataset inpatient_hospital_lite
2. So queremos metricas de hospitais universitários 
3. Criar metricas de medias e SD dos doentes saidos so para o mes de janeiro
4. Hospitais superiores a media global sao codificados como zero, os outros como 1

```{r}
tipo_esp01 <-inpatient_hospital_lite |>
  select(tipo_de_especialidade)|>
  mutate(
    esp01=ifelse(tipo_de_especialidade=="Especialidade Cirurgica", 0,1)
  )
```


### Exercício
Crie a média e a mediana dos doentes seguidos e dias de internamento, por ano para os hospitais de oncologia (agrupados).