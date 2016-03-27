# preprocessing train set
train[, "Dates"] <- parse_date_time(train[, "Dates"], "%Y-%m-%d %H:%M:%S", tz = "UTC")
train[, "year"]  <- as.numeric(format(train[, "Dates"], "%Y"))
train[, "month"] <- as.numeric(format(train[, "Dates"], "%m"))
train[, "hour"]  <- as.numeric(format(train[, "Dates"], "%H"))
train[, "Category"] <- as.factor(train[, "Category"])
train[, "PdDistrict"] <- as.factor(train[, "PdDistrict"])
train[, "DayOfWeek"]<- as.factor(train[, "DayOfWeek"])
train <- train[-which(train[, "Y"] == 90), c(2, 4, 5, 8, 9, 10, 11, 12)]

# preprocessing test set
test[, "Dates"] <- parse_date_time(test[, "Dates"], "%Y-%m-%d %H:%M:%S", tz = "UTC")
test[, "year"]  <- as.numeric(format(test[, "Dates"], "%Y"))
test[, "month"] <- as.numeric(format(test[, "Dates"], "%m"))
test[, "hour"]  <- as.numeric(format(test[, "Dates"], "%H"))
test[, "PdDistrict"] <- as.factor(test[, "PdDistrict"])
test[, "DayOfWeek"]<- as.factor(test[, "DayOfWeek"])
test <- test[, c(3, 4,6,7,8,9,10)]
test <- sparse.model.matrix(~.-1, data = test)

# construct matrices for training
X <- sparse.model.matrix(Category ~.-1, data = train)
Y <- as.numeric(train[, "Category"]) - 1
numclass <- range(Y)[2] + 1

# set the parameter
params <- list("objective" = "multi:softprob",
               "eta" = .5,
               "max_depth" = 5,
               "eval_metric" = "mlogloss",
               "num_class" = numclass)

# cross-validation
bst.cv <-  xgb.cv(params = params, data = X, label = Y, nfold = 3, nround = 50, verbose = T)

# training with gradient boost
bst <- xgboost(data = X, label = Y, params = params, nrounds = 50)

# save the model
xgb.dump(bst, with.stats = TRUE)

# apply prediction
pred <- predict(bst, newdata = test)
result <- matrix(pred, nrow = 884262, ncol = 39, byrow = T)
Id <- sample[, "Id"]
name <- colnames(sample)
name <- gsub("\\.", "/", name)
result <- data.frame(Id, result)
names(result) <- name

# export the output. 
write.csv(result, file = "solution.csv", row.names = F)
