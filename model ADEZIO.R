
# Giuditta ADEZIO 880076 ----------------------------------------------------

library(tidyverse)

data <- read.csv("training clean.csv", header = T)

library(visdat)
skim(data)
vis_dat(data)

# rendo factor le variabili che devono esserlo
data[,c(3, 5, 6, 9, 12, 13, 15:23, 25:27)] <- 
  data.frame(lapply(data[,c(3, 5, 6, 9, 12, 13, 15:23, 25:27)], as.factor))

data$condominium_fees <- as.numeric(data$condominium_fees)
data$selling_price <- as.numeric(data$selling_price)

# ordino le variabili ordinabili e successivamente le rendo numeriche 
# in modo tale da diminuire la complessità computazionale 
data$bathrooms_number <- factor(data$bathrooms_number, 
                                levels = c("1", "2", "3", "3+"), ordered = T)

data$rooms_number <- factor(data$rooms_number, 
                            levels = c("1", "2", "3", "4", "5", "5+"), ordered = T)

data$floor <- factor(data$floor, 
                     levels = c("-1", "0", "0.5", "1", "2", "3", "4", 
                                "5", "6", "7", "8", "9"), ordered = T)

data$total_floors_in_building <- factor(data$total_floors_in_building, 
                     levels = c("1", "2", "3", "4", 
                                "5", "6", "7", "8", "9", "9+"), ordered = T)

# gli anni presenti in year_of_construction sono molto dispersi quindi 
# cerco di raggrupparli in una variabile factor
data <- data %>%
  mutate(
   year_group = case_when(
      year_of_construction <= 1900 ~ "1900",
      year_of_construction <= 1945 ~ "1901_1945",
      year_of_construction <= 1969 ~ "1946_1969",
      year_of_construction <= 1989 ~ "1970_1989",
      year_of_construction <= 2009 ~ "1990_2009",
      year_of_construction <= 2019 ~ "2010_2019",
      year_of_construction >= 2020 ~ "2020",
      TRUE ~ NA_character_
    ),
    year_group = factor(
      year_group,
      levels = c("1900", "1901_1945", "1946_1969", "1970_1989",
                 "1990_2009", "2010_2019", "2020"),
      ordered = TRUE
    )
  )

# rendo factor anche la variabile cluster 
data$cluster <- as.factor(data$cluster)


# divido il dataset in training e validation
set.seed(123)
sample <- sort(sample(1:nrow(data), round(nrow(data)*0.75), replace = F))
train <- data[sample,]
validation <- data[-sample,]


# funzione per MAE e per MSE
MAE <- function(y, y_fit){
  mean(abs(y - y_fit))
}
MSE <- function(y, y_fit){
  mean((y - y_fit)^2)
}


# -------------------------------------------------------------------------
### MODELLI ###
# -------------------------------------------------------------------------

# MEDIANA -----------------------------------------------------------------

y_hat_median <- rep(median(train$selling_price), nrow(validation)) 

round(MAE(train$selling_price, y_hat_median), 4)
round(MSE(train$selling_price, y_hat_median), 4)

## REGRESSIONE LINEARE -----------------------------------------------------

lm_mod <- lm(selling_price ~ ., data = train[,- 10])

yhat_lm <- predict(lm_mod, validation)

round(MAE(validation$selling_price, yhat_lm), 4)
summary(yhat_lm) # il minimo è negativo il che non ha senso
summary(data$selling_price) # il minimo di tutte le case è 25629  
# guardando attentamente i valori mi rendo conto che il valore minimo 
# successivo è circa 65000 quindi setto il minimo delle mie 
# previsioni a 50000

# applico una correzione 
yhat_lm <- pmax(yhat_lm, 50000)

round(MAE(validation$selling_price, yhat_lm), 4)
round(MSE(validation$selling_price, yhat_lm), 4)


## SUBSET SENZA LOG ---------------------------------------------------------

step_mod <- step(lm(selling_price ~ ., data = train[,-10]))
summary(step_mod)

yhat_step <- predict(step_mod, validation)
summary(yhat_step) # il minimo è sempre negativo

MAE(validation$selling_price, yhat_step)
MSE(validation$selling_price, yhat_step)

# applico una correzione per lo stesso motivo spiegato in precedenza
yhat_step <- pmax(yhat_lm, 50000)

MAE(validation$selling_price, yhat_step)
MSE(validation$selling_price, yhat_step)


## REGRESSIONE LINEARE CON LOG ----------------------------------------------

log_mod <- lm(log(selling_price) ~ log(square_meters) + sqrt(distance) + . 
              - square_meters - distance,
              data = train[,-10])
# il log della condominium fee non può essere eseguito in quanto la variabile 
# presenta molti zeri

yhat_log <- predict(log_mod, validation)

MAE(validation$selling_price, exp(yhat_log))
MSE(validation$selling_price, exp(yhat_log))


## REGRESSIONE LINEARE FINAL -------------------------------------------------

plot(log(data$condominium_fees[data$condominium_fees>0]), 
     log(data$selling_price[data$condominium_fees>0]))
plot((data$distance), log(data$selling_price))
hist(data$distance, col = 'slateblue')

plot(log(data$selling_price) ~ data$condominium_fees)
cor(data$square_meters, data$condominium_fees)

lm_m <- lm(log(selling_price) ~ log(square_meters) + bathrooms_number + 
             lift * as.numeric(floor) + rooms_number + condominium_fees +
             year_group + total_floors_in_building + conditions + floor + 
             energy_efficiency_class + balcony.terrace + garden + 
             distance + cluster + security_level + other_features,  
           data = train[,-10])

y_lm <- exp(predict(lm_m, validation))
length(coef(lm_m))

round(MAE(validation$selling_price, y_lm), 4)
summary(lm_m)
summary(y_lm)


# BEST SUBSET -------------------------------------------------------------

steplog_mod <- step(lm(log(selling_price) ~ log(square_meters) +  . 
                       - square_meters, 
                    data = train[,-10]))
summary(steplog_mod)
yhat_steplog <- exp(predict(steplog_mod, validation))

MAE(validation$selling_price, yhat_steplog)
MSE(validation$selling_price, yhat_steplog)


# RIDGE --------------------------------------------------------------------
library(glmnet)
formula_ridge <- log(selling_price) ~ log(square_meters) - square_meters + .

X_shrinkage <- model.matrix(formula_ridge, data = train[,-10])[, -1]
X_valid <- model.matrix(formula_ridge, data = validation[,-10])[, -1]

common_cols <- intersect(colnames(X_shrinkage), colnames(X_valid))
X_shrinkage <- X_shrinkage[, common_cols]
X_valid <- X_valid[, common_cols]

y_shrinkage <- log(train$selling_price)

# cv ridge
set.seed(123)
ridge_cv <- cv.glmnet(
  X_shrinkage, y_shrinkage,
  alpha = 0,
  nfolds = 10,
  standardize = TRUE
)

(lambda_opt <- ridge_cv$lambda.min)

y_pred_log <- predict(ridge_cv, newx = X_valid, s = lambda_opt)
y_pred <- exp(y_pred_log)

round(MAE(validation$selling_price, y_pred), 4)
round(MSE(validation$selling_price, y_pred), 4)

plot(ridge_cv)
abline(v = log(lambda_opt), col = "red", lty = 2)


## ELASTIC NET -------------------------------------------------------------
formula_enet <- (log(selling_price) ~ log(square_meters) + sqrt(distance) + . 
                - square_meters)

factor_vars <- names(Filter(is.factor, train))
for (var in factor_vars) {
  validation[[var]] = factor(validation[[var]], levels = levels(train[[var]]))
}

X_shrinkage <- model.matrix(formula_enet, data = train[,-10])[, -1]
X_valid <- model.matrix(formula_enet, data = validation[,-10])[, -1]

common_cols <- intersect(colnames(X_shrinkage), colnames(X_valid))
X_shrinkage <- X_shrinkage[, common_cols]
X_valid <- X_valid[, common_cols]

y_shrinkage <- log(train$selling_price)

set.seed(123)
enet_cv <- cv.glmnet(
  X_shrinkage, y_shrinkage,
  alpha = 0.5,
  nfolds = 10,
  standardize = TRUE
)

lambda_enet_opt <- enet_cv$lambda.min
y_pred_enet_log <- predict(enet_cv, newx = X_valid, s = lambda_enet_opt)
y_pred_enet <- exp(y_pred_enet_log)

round(MAE(validation$selling_price, y_pred_enet), 4)


## RANDOM FOREST ----------------------------------------------------------
library(ranger)
m_rf <- ranger(log(selling_price) ~ ., data = train[,-10], num.trees = 2000, 
               mtry = 10, max.depth = 30)
y_hat_rf <- exp(predict(m_rf, data = validation, 
                        type = "response")$predictions)

MAE(validation$selling_price, y_hat_rf)
MSE(validation$selling_price, y_hat_rf)


## GAM --------------------------------------------------------------------

library(mgcv)
m_gam_simple <- gam(log(selling_price) ~ s(log(square_meters)) + 
                      bathrooms_number + lift * as.numeric(floor) + 
                      rooms_number + total_floors_in_building +
                      s(condominium_fees) + conditions + floor + 
                      energy_efficiency_class + 
                      furnished + concierge.reception + balcony.terrace +
                      garden + s(distance) + cluster + security_level + 
                      year_group + other_features, data = train[,-10], 
                    family = "gaussian")
summary(m_gam_simple)
y_hat_gam_simple <- exp(c(predict(m_gam_simple, newdata = validation, 
                                  type = "response")))
round(MAE(validation$selling_price, y_hat_gam_simple),4)


# ---------------------------------------------------------------------------
### test ###
# ----------------------------------------------------------------------------
test <- read.csv("test_clean.csv")

test[,c(3, 5, 6, 9, 12:22, 24, 25)] <- 
  data.frame(lapply(test[,c(3, 5, 6, 9, 12:22, 24, 25)], as.factor))
test$condominium_fees <- as.numeric(test$condominium_fees)

test$bathrooms_number <- factor(test$bathrooms_number, 
                                levels = c("1", "2", "3", "3+"), ordered = T)

test$rooms_number <- factor(test$rooms_number, 
                            levels = c("1", "2", "3", "4", "5", "5+"), ordered = T)

test$floor <- factor(test$floor, 
                     levels = c("-1", "0", "0.5", "1", "2", "3", "4", 
                                "5", "6", "7", "8", "9"), ordered = T)


test = test %>%
  mutate(
    year_group = case_when(
      year_of_construction <= 1900 ~ "1900",
      year_of_construction <= 1945 ~ "1901_1945",
      year_of_construction <= 1969 ~ "1946_1969",
      year_of_construction <= 1989 ~ "1970_1989",
      year_of_construction <= 2009 ~ "1990_2009",
      year_of_construction <= 2019 ~ "2010_2019",
      year_of_construction >= 2020 ~ "2020",
      TRUE ~ NA_character_
    ),
    year_group = factor(
      year_group,
      levels = c("1900", "1901_1945", "1946_1969", "1970_1989",
                 "1990_2009", "2010_2019", "2020"),
      ordered = TRUE
    )
  )


# codifico le zone del train set
zone_to_cluster <- data %>%
  dplyr::select(zone, cluster) %>%
  distinct()
zone_cluster_map <- setNames(zone_to_cluster$cluster, zone_to_cluster$zone)

# creo sul test una nuova variabile con cluster corrispondente
test$cluster <- zone_cluster_map[test$zone]
table(test$cluster)


### MODELLO FINALE ----------------------------------------------------------
lm_f <- lm(log(selling_price) ~ log(square_meters) + bathrooms_number + 
             lift * as.numeric(floor) + rooms_number + total_floors_in_building + 
             car_parking + condominium_fees + conditions + floor + 
             energy_efficiency_class + 
             furnished + tavern + concierge.reception + 
             balcony.terrace + garden + distance + cluster + 
             security_level + other_features + year_group,  data = data[,-10])

y_lm <- exp(predict(lm_f, test))
length(coef(lm_f))
summary(lm_f)

sub1 <- cbind((1:length(y_lm)), round(y_lm, 2))
colnames(sub1) <- c('ID', 'prediction')
write.csv(data.frame(sub1), "sub1.csv", row.names = F)

# CV
library(caret)

mae1 <- function(data, lev = NULL, model = NULL) {
  pred <- exp(data$pred)
  obs <- exp(data$obs)
  
  mae <- mean(abs(pred - obs))
  
  out <- c(MAE1 = mae)
  return(out)
}
control <- trainControl(method = "cv", number = 10, summaryFunction = mae1)

model_cv <- train(
  log(selling_price) ~ log(square_meters) + bathrooms_number + 
    lift * as.numeric(floor) + rooms_number + total_floors_in_building + 
    car_parking + condominium_fees + conditions + floor + 
    energy_efficiency_class + furnished + tavern + concierge.reception + 
    balcony.terrace + garden + distance + cluster + 
    security_level + other_features + year_group,
  data = data[,-10], method = "lm", trControl = control, metric = "MAE1"
)

print(model_cv)

