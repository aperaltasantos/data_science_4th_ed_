
# Autor: Manuel Ribeiro
# Data : 01/10/2024
# Ultima revisao : 01/10/2024
# Descricao : Código e-book Modulo Aprendizagem automática
# email: manuel.ribeiro@tecnico.ulisboa.pt

# livrarias
library(crayon) # para palete de cores
library(dplyr) # para pipeline
library(kableExtra) # para tabelas
library(ggplot2) # visualizacao de grafico

# Ler as bibliotecas de funções para a sessão
library(tidymodels) # familia tidy
library(tidyclust) # clusters (n-supervisionada)
library(MLDataR)    # dataset
library(ISLR) # dataset nci60
library(rpart.plot) # gráficos arvores de decisao
library(kknn)       # algoritmo k-nn
library(ggplot2)    # gráficos
library(factoextra) # graficos (n-supervisionada)

# Ler o dataset HearDisease
heartd <- MLDataR::heartdisease

# dimensoes
dim(heartd)
head(heartd)
str(heartd)

# Converter variáveis categóricas em variáveis tipo factor
heartd$Sex <- factor(heartd$Sex)
heartd$FastingBS  <- factor(heartd$FastingBS, labels = c("Normal","Diabetes"))
heartd$RestingECG  <- factor(heartd$RestingECG)
heartd$Angina <- factor(heartd$Angina)
heartd$HeartDisease <- factor(heartd$HeartDisease, labels = c("N", "Y"))

# ---- análise exploratoria -----

# plot proporções y (Heart Disease, HD)
heartd %>%
  count(HeartDisease) %>%
  mutate(freq = round(n / sum(n),2)) %>%
  mutate(cum = cumsum(freq)) %>%
  ggplot(aes(x = "", y = freq, fill = HeartDisease, label = freq)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis_d(end = .5) +
  geom_text(aes(label = freq), position = position_stack(vjust = .95), colour = "white") +
  labs(fill = "Heart disease")

# plot proporções y, por sexo
heartd %>% 
  ggplot(aes(x = HeartDisease, fill = Sex)) +
  geom_bar() +
  scale_fill_viridis_d(end = .5)

# plot proporções y, MaxHR
heartd %>%
  ggplot(aes(MaxHR, fill = HeartDisease)) +
  geom_histogram(binwidth = 2) + 
  scale_fill_viridis_d(end = .5)

# ----- pre-processamento -----

# sempre que usamos uma função geradora de um output (pseudo)aleatorio, 
# podemos usar o seeding para assegurar sempre o mesmo output
set.seed(12399300)

# initial_split() cria as partições e
# arg strata estratifica a amostragem aleatória
# em cada partição
particao <- initial_split(heartd, strata = HeartDisease)

# atribuir nome/objeto aos
# datasets de treino e de teste
heartdtr <- training(particao)
heartdtt  <- testing(particao)

# ---- validacao-kfold  -----

set.seed(4800000)
k_folds <- vfold_cv(heartdtr, v = 5, strata = HeartDisease)

# ----- recipe -----

receita <- 
  recipe(HeartDisease ~ ., data = heartdtr) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())


# ---- recipe-show -----

# resultados do pré-processamento
receita %>% 
  prep(training = heartdtr) %>% 
  bake(new_data = NULL)

# ---- reg log -----

# modelo, motor, modo
log_spec <-
  logistic_reg() %>% 
  set_engine("glm") %>%
  set_mode("classification")

# ----- log-workflow (pipeline) -----

# pipeline
log_wflow <- 
  workflow() %>% 
  add_recipe(receita) %>%
  add_model(log_spec)

# log-fit
log_fit <- 
  log_wflow %>%
  fit(data = heartdtr) 

# log-predict
# predict com dados de treino
predict(log_fit, new_data = heartdtr)

# podemos obter informação adicional com augment()
broom::augment(log_fit, new_data = heartdtr)

# ver lado-a-lado as classes observadas e previstas para os dados de treino
augment(log_fit, new_data = heartdtr) %>%
  relocate(HeartDisease, .pred_class, .pred_Y, .pred_N)

# medidas de desempenho
heart_metrics <- metric_set(accuracy, specificity, sensitivity)
augment(log_fit, new_data = heartdtr) %>%
  heart_metrics(truth = HeartDisease, estimate = .pred_class)

# validacao cruzada
log_res <- fit_resamples(log_wflow, k_folds)

# tx Erro (accuracy)
log_res %>%
  collect_metrics()


# ajustamento nos dados de teste
log_finalfit <- last_fit(log_wflow, particao) 

# guardar resultado final 
log_results <- collect_metrics(log_finalfit)
log_results

# ----- knn ----

  knn_spec <-
  nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# grelha para o tuning
k_grid <- tibble(neighbors = seq(1,51, by = 2))

# knn-workflow
knn_wflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(knn_spec)

# knn-tune
knn_res <- 
  knn_wflow %>%
  tune_grid(resamples = k_folds, 
            grid = k_grid)

# top-5
show_best(knn_res, metric="accuracy", n=5)

# top-1
select_best(knn_res, metric="accuracy")

# objeto para o wkflow final
best_knn <- select_best(knn_res, metric="accuracy")

final_knn_wflow <- 
  knn_wflow %>% 
  finalize_workflow(best_knn)

# teste
knn_finalfit <- final_knn_wflow %>% 
  last_fit(split = particao)

knn_results <-  
  knn_finalfit %>% 
  collect_metrics()

knn_results

# ----- dtree ----

# dttree-spec
dtree_spec <-
  decision_tree(cost_complexity= tune()) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# dtree-workflow
dtree_wflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(dtree_spec)

# dtree grid
dtree_grid <- grid_regular(cost_complexity(), levels = 100)

# ---- dtree-fit ----

# dtree-tune
dtree_res <- 
  dtree_wflow %>% 
  tune_grid(resamples = k_folds, grid = dtree_grid)

# top-5
show_best(dtree_res, metric="accuracy")

# top-1
best_dtree <- select_best(dtree_res, metric="accuracy")

final_dtree_wflow <- 
  dtree_wflow %>% 
  finalize_workflow(best_dtree)

# ----- dtree-finalfit -----

dtree_finalfit <- final_dtree_wflow %>% 
  last_fit(split = particao)

# guardar resultados de teste
dtree_results <-  
  dtree_finalfit %>% 
  collect_metrics()

dtree_results

# dtree-plot
dtree_finalfit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)


# ---- bag-spec-wflow-fit ----

# biblioteca para bagging com tidymodels
library(baguette)

set.seed(992801000)
bag_spec <- bag_tree() %>%
  set_engine("rpart", times = 50) %>% # 50 amostras bootstrap
  set_mode("classification")
 
bag_wflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(bag_spec)

bag_fit <- bag_wflow %>% fit(heartdtr)

bag_fit

# bag teste
bag_rs <- heartdtt %>%
  bind_cols(predict(bag_fit, heartdtt)) 

bag_results <- bag_rs %>%
  accuracy(HeartDisease, .pred_class)
bag_results

# ---- rf-spec_wf_grid_tune_top

# especificacao do modelo
rf_spec <- rand_forest( mtry = tune(), trees = 500) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# workflow 
rf_wflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(rf_spec)

# grid de afinação
rf_grid <- grid_regular(mtry(range = c(1,4)), levels = 4)

# rf-tunebest
set.seed(33889922)
rf_res <- 
  rf_wflow %>% 
  tune_grid(resamples = k_folds, grid = rf_grid)

# top-5
show_best(rf_res, metric="accuracy")

# top-1
select_best(rf_res, metric="accuracy")
best_rf <- select_best(rf_res, metric="accuracy")


# rf-tunebest-plot

# grafico tunning
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(mtry, mean)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Accuracy") +
  labs(x = "# preditores")


# rf-tunebestmodel
final_rf_wflow <- 
  rf_wflow %>% 
  finalize_workflow(best_rf)


# rf-finalfit
rf_finalfit <- final_rf_wflow %>% 
           last_fit(split = particao)

# guardar resultados de teste
rf_results <-  
  rf_finalfit %>% 
  collect_metrics()

# matriz confusao dados teste
rf_finalfit %>%
    collect_predictions() %>%
    conf_mat(HeartDisease, .pred_class)

# ---- nao-supervisionada


# data
data(NCI60, package = "ISLR")
nci60 <- NCI60$data %>%
  as_tibble(.name_repair = ~ paste0("V_", .x)) %>%
  mutate(label = factor(NCI60$labs)) %>%
  relocate(label)

# pca-prcomp
nci60_pca <- nci60 %>%
  select(-label) %>%
  prcomp(scale = TRUE)

# pca-eigen
tidy(nci60_pca, matrix = "eigenvalues")


# pca-eigenperc
tidy(nci60_pca, matrix = "eigenvalues") %>%
  ggplot(aes(factor(PC), percent)) +
  geom_bar(stat="identity", aes(fill = percent)) +
  scale_x_discrete(breaks = c(1, 10, 20 ,30,40, 50, 60)) +
  scale_y_continuous(breaks = c(0, 0.05,0.1)) +
  xlab("Componentes principais") +
  ylab("Proporção da variabilidade explicada")

# pca-eigencum
tidy(nci60_pca, matrix = "eigenvalues") %>%
 ggplot(aes(factor(PC), cumulative)) +
  geom_bar(stat="identity", aes(fill = percent)) +
  scale_x_discrete(breaks = c(1, 10, 20 ,30,40, 50, 60)) +
  scale_y_continuous(breaks = c(0, .2,.4, .6,.8,1)) +
  xlab("Componentes principais") +
  ylab("Proporção da variabilidade explicada")


# pca-label
nci60_pcs <- bind_cols(
  augment(nci60_pca),
  nci60 %>% select(label)
)


# pca-palete

# n de categorias
nlevels(nci60$label)
colors <- unname(palette.colors(n = 14, palette = "Polychrome 36"))

# pca-plot12
nci60_pcs %>%
  ggplot(aes(.fittedPC1, .fittedPC2, color = label)) +
  geom_point(size =3, alpha = 0.5) +
  scale_color_manual(values = colors)


# pca-plot13
nci60_pcs %>%
  ggplot(aes(.fittedPC1, .fittedPC3, color = label)) +
   geom_point(size =3, alpha = 0.5) +
  scale_color_manual(values = colors)


# clust-recipe
nci60_rec <- recipe(~ ., data = nci60) %>%
  step_rm(label) %>%
  step_normalize(all_predictors())

# clust-wflow
nci60_wf <- workflow() %>%
  add_recipe(nci60_rec)

# clust-linkage
nci60_complete <- nci60_wf %>%
  add_model(hier_clust(linkage_method = "complete")) %>%
  fit(data = nci60)
nci60_average <- nci60_wf %>%
  add_model(hier_clust(linkage_method = "average")) %>%
  fit(data = nci60)
nci60_single <- nci60_wf %>%
  add_model(hier_clust(linkage_method = "single")) %>%
  fit(data = nci60)

# clust-dencomp

# visualizacao dendrograma
nci60_complete %>%
  extract_fit_engine() %>%
  fviz_dend(main = "Complete")
nci60_average %>% 
  extract_fit_engine() %>%
  fviz_dend(main = "Average")
nci60_single %>%
  extract_fit_engine() %>%
  fviz_dend(main = "Single")

# # visualizacao dendrograma colorido k=4
nci60_complete %>%
  extract_fit_engine() %>%
  fviz_dend(k = 4, main = "Classf. hierarquica com k=4 classes")


# com pca
nci60_pca_rec <- recipe(~ ., data = nci60) %>%
  step_rm(label) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 12)

nci60_pca_wf <- workflow() %>%
  add_recipe(nci60_pca_rec)


# pca com classificação
nci60_pca <- nci60_pca_wf %>%
  add_model(hier_clust(linkage_method = "complete")) %>%
  fit(data = nci60)

# dendrograma
nci60_pca %>%
  extract_fit_engine() %>%
  fviz_dend(k = 4, main = "Classf. hierarquica com 4 PCs")

