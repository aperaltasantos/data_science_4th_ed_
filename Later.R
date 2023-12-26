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
