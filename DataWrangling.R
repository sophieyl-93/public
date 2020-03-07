#### loading packages for data wragnling ####

library(dplyr)
library(tidyr)
library(data.table)
library(stats)
#### loading packages for performance evaluation ####

if (!require("caret")) {
  install.packages("caret")
  library("caret")
}
if (!require("ROCR")) {
  install.packages("ROCR")
  library("ROCR")
}

#### data wrangling - casting into correct types ####

listings <- data.table(read.delim(file = "TH_data_challenge.tsv", na.strings = c("NA", "NULL")))
# # convert response variable to binary
# listings$dim_is_requested <- factor(ifelse(listings$dim_is_requested == "true", yes = 1, no = 2))
# # convert market variable to 1,2,3
# listings$dim_market <- factor(ifelse(listings$dim_market == "Los Angeles", yes = 1, 
                              # ifelse(listings$dim_market == "Paris", yes = 2, no = 3)))
# # convert room type variable to 1,2,3
# listings$dim_room_type <- factor(ifelse(listings$dim_room_type == "Entire home/apt", yes = 1, 
#                                   ifelse(listings$dim_room_type == "Private room", yes = 2, no = 3)))
# # convert instance_bookable type variable to 1,2,3
# listings$dim_is_instant_bookable <- factor(ifelse(listings$dim_is_instant_bookable == "true", yes = 1,no = 2))

# convert cancellation policy to factor
listings$cancel_policy <- factor(listings$cancel_policy, levels = 3:9)
# convert wifi accessibility to factor
listings$dim_has_wireless_internet <- factor(listings$dim_has_wireless_internet, levels = 0:1)
# use ds_night_day_of_week as factors
listings$ds_night_day_of_week <- factor(listings$ds_night_day_of_week)


# convert dates into Date format
listings$ds <- as.Date(x = listings$ds)
listings$ds_night <- as.Date(x = listings$ds_night)

#### data wrangling - filter out negative prices in effective daily price####

filtered <- listings %>% 
  filter(m_effective_daily_price > 0) %>%
  data.table()

# check if odd ones are filtered
table(filtered$m_effective_daily_price)
length(filtered$m_effective_daily_price[filtered$m_effective_daily_price < 0])

#### data wrangling - fill out missing values in m_checkouts ####
filtered <- filtered %>% 
  filter(is.na(m_checkouts) == FALSE) %>%
  data.table()

# check if odd ones are filtered
table(filtered$m_checkouts)
sum(is.na(filtered$m_checkouts))


#### data wrangling - filter out rare variants in cancellation policy ####
#### data wrangling - fill out missing values in r_kdt_listing_views_0_6_avg_n100 ####

filtered <- filtered %>%
  filter(cancel_policy %in% c("3","4","5")) %>% # 4 apartments filtered
  filter(!is.na(r_kdt_listing_views_0_6_avg_n100)) %>% # 1 record filtered
  filter(!is.na(r_kdt_m_effective_daily_price_available_n100_p50)) %>%
  data.table()

filtered$cancel_policy <- factor(filtered$cancel_policy)
levels(filtered$cancel_policy) <- c('1', '2', '3')

# check if odd ones are filtered
table(filtered$cancel_policy)
anyNA(filtered$r_kdt_listing_views_0_6_avg_n100)
anyNA(filtered$r_kdt_m_effective_daily_price_available_n100_p50)

#### data wrangling - fill out NA in ds_checkin_gap & ds_checkout_gap ####
filtered <- filtered %>% 
  filter(is.na(ds_checkin_gap) == FALSE) %>%
  data.table()

# check if odd ones are filtered
table(filtered$ds_checkin_gap)
sum(is.na(filtered$ds_checkin_gap))
sum(is.na(filtered$ds_checkout_gap))
sum(is.na(filtered$m_minimum_nights))
sum(is.na(filtered$m_maximum_nights))



######################## Remove some variables ######################################
#### data wrangling - remove variables
names(filtered)

list.remove = c('id_listing_anon', 'id_user_anon', 'days_since_last_booking',
                'image_quality_score', 'occ_occupancy_plus_minus_7_ds_night',
                'occ_occupancy_plus_minus_14_ds_night',
                'occ_occupancy_trailing_90_ds',
                'price_booked_most_recent',
                'p2_p3_click_through_score',
                'p3_inquiry_score',
                'listing_m_listing_views_2_6_ds_night_decay')
list.kdt = c('kdt_score',
             'r_kdt_listing_views_0_6_avg_n100',
             'r_kdt_n_active_n100',
             'r_kdt_n_available_n100',
             "r_kdt_m_effective_daily_price_n100_p50",
             'r_kdt_m_effective_daily_price_available_n100_p50',
             'r_kdt_m_effective_daily_price_booked_n100_p50')

cleaned = filtered %>%
  select(-list.remove) %>%
  select(-list.kdt)

#write.csv(cleaned, 'cleanedData.csv')

############################ Split data: training 4 vs testing 1######################
set.seed(123)
test.id <- sample(1:dim(cleaned)[1], size = ceiling(dim(cleaned)[1]/5))
cleaned.train <- cleaned[-test.id,]
cleaned.test  <- cleaned[test.id,]

#write.csv(cleaned.train, 'cleanedData_train.csv')
#write.csv(cleaned.test, 'cleanedData_test.csv')

######################## PCA  ######################################
#cleaned.train <- read.csv('cleanedData_train.csv')
#cleaned.test <- read.csv('cleanedData_test.csv')

list.factor <- c('dim_market', 'dim_room_type', 'dim_is_instant_bookable', 'cancel_policy',
                 'dim_has_wireless_internet', 'ds_night_day_of_week')
list.date <- c('ds_night', 'ds')

cleaned.trian.num <- cleaned.train %>%
  select(-list.factor) %>%
  select(-list.date)

clean.train.pca = princomp(cleaned.trian.num[,-c(1,2)], cor = T)
summary(clean.train.pca)
clean.train.pca$loadings
# biplot(clean.train.pca)
#saveRDS(clean.train.pca, 'clean.train.pca.RDS')
#clean.train.pca <- readRDS('clean.train.pca.RDS')

######################## Engineer new variables ######################################

#### data wrangling - day-of-the-year effect by market with splines ####

# splines needed for this

require(splines)

# create basis splines for dates for each of the three markets

basis.degree <- 30
equidistant.ret <- list(NULL, NULL, NULL)
names(equidistant.ret) <- levels(filtered$dim_market)
for (market in levels(filtered$dim_market)) {
  x <- as.numeric(filtered[dim_market==market]$ds_night_day_of_year)
  x <- x - min(x) + 1; range(x)
  equidistant.ret[[market]] <- bs(x, degree = basis.degree, 
                                  intercept = FALSE, Boundary.knots = range(x))
}
lapply(equidistant.ret, dim)

splines.mat <- matrix(0, nrow = nrow(filtered), ncol = basis.degree * 3)
for (market.idx in 1:3) {
  market <- levels(filtered$dim_market)[market.idx]
  splines.mat[filtered$dim_market == market, 
              (market.idx-1)*basis.degree + 1:30] <- equidistant.ret[[market]]
}
#sum(splines.mat[filtered$dim_market == market,])  
#plot(splines.mat[200,], type = 'l')
#splines.mat[200,29]
colnames(splines.mat) <- paste0(rep(c("LA", "PAR", "SF"), each = basis.degree), 1:basis.degree)


#### data wrangling - Splines for kdt.views ####

boxplot(log(filtered$r_kdt_listing_views_0_6_avg_n100+0.1))

splines.vec.kdt.views <- bs(log(filtered$r_kdt_listing_views_0_6_avg_n100+0.1),
                            degree = 5, intercept = FALSE, 
                            Boundary.knots = range(log(filtered$r_kdt_listing_views_0_6_avg_n100+0.1)))


######################### Start fitting models ###########################################
### adaBoost ###
library(gbm)
set.seed(1)
var.remove = c('ds_night', 'ds', 'id_listing_anon', 'id_user_anon')
test = gbm(data = filtered, dim_is_requested ~ . -ds_night -ds -id_listing_anon - id_user_anon, 
           distribution = 'bernoulli', 
           n.trees = 100, interaction.depth = 4)
summary(test)

### mds ### FAIL too big!!!
install.packages('cluster')
library(cluster)
gower.dissimilarity.mtrx <- daisy(filtered[,-c(1,2,3,4,5)], metric = c("gower"))


### pca ###
x.pca = prcomp(filtered[,-c(1,2,3,4,5)])



install.packages('evclust')
library("evclust")

train <- matrix(sample(c("a","b","c"),12,replace=TRUE), ncol=2) # n x 2
n = dim(train)[1]
distMatrix <- matrix(runif(n^2,0,1),ncol=n) # n x n
distMatrix <- filtered

# matrix of neighbours
knn_dist(D = distMatrix, K = 3)
knn_dist(D = distMatrix, K = 1)


### knn ###
library(class)
library(fastDummies)
library(caret)
library(ROCR)
#install.packages("pROC")
#library(pROC)

cleaned.train.dummy = dummy_cols(cleaned.train, select_columns = list.factor, remove_most_frequent_dummy = TRUE) %>%
  select(-list.factor, -list.date)

cleaned.train.scale = scale(cleaned.train.dummy[,-1])
cleaned.train.scale = as.data.frame(cleaned.train.scale)
cleaned.train.scale = cleaned.train.scale %>%
  mutate(dim_is_requested = cleaned.train$dim_is_requested)

cleaned.test.dummy = dummy_cols(cleaned.test, select_columns = list.factor, remove_most_frequent_dummy = TRUE) %>%
  select(-list.factor, -list.date)

cleaned.test.scale = scale(cleaned.test.dummy[,-1])
cleaned.test.scale = as.data.frame(cleaned.test.scale)
cleaned.test.scale = cleaned.test.scale %>%
  mutate(dim_is_requested = cleaned.test$dim_is_requested)

set.seed(1)
sample.id = sample(1:dim(cleaned.trian.num)[1], size = ceiling(dim(cleaned.trian.num)[1]/5))
train.train = cleaned.train.scale[-sample.id,]
train.test = cleaned.train.scale[sample.id,]

train.train.Label = train.train %>%
  select(dim_is_requested) %>%
  .$dim_is_requested

train.test.Label = train.test %>%
  select(dim_is_requested) %>%
  .$dim_is_requested

train_error = c()
test_error = c()
k.optm = 1
for (i in c(1, 4, 5, 10, 23, 27, 40, 60, 100, 200, 380)) {
  set.seed(1)
  knn_test_pred = knn(train.train[,-34], 
                 train.test[,-34], 
                 train.train.Label, 
                 k = i)
  knn_train_pred = knn(train.train[,-34], 
                      train.train[,-34], 
                      train.train.Label, 
                      k = i)
  train_error = c(train_error, mean(knn_train_pred != train.train.Label))
  test_error = c(test_error, mean(knn_test_pred != train.test.Label))
  
  k.optm[i] = 100 * sum(train.test.Label != knn_test_pred)/NROW(train.test.Label)
  k=i  
  cat(k,'=',k.optm[i],'\n')
}

k_list = c(1, 1/4, 1/5, 1/10, 1/23, 1/27, 1/40, 1/60, 1/100, 1/200, 1/380)
error = data.frame(train_error, test_error, k_list)

#par(mfrow = c(1, 2))
error_plot = ggplot(data = error)
error_plot + geom_line(aes(x = k_list, y = train_error), color = 'blue') + 
  geom_line(aes(x = k_list, y = test_error), color = 'red') + 
  labs(x = '1/k', y = 'Error Rate') + geom_text()


plot(k.optm, type="p", xlab="K- Value",ylab="Error rate")  # to plot % accuracy wrt to k-value

# choose k = 27
train.Label = cleaned.train %>%
  select(dim_is_requested) %>%
  .$dim_is_requested


test.Label = cleaned.test %>%
  select(dim_is_requested) %>%
  .$dim_is_requested

set.seed(1)
knn_pred.test = knn(cleaned.train.scale[,-34], 
                    cleaned.test.scale[,-34], 
                    train.Label, 
                    k = 27,
                    prob = TRUE)

table(knn_pred.test, test.Label)
print(mean(knn_pred.test != test.Label))

##AUC##
prob = attr(knn_pred.test, "prob")
#roc(cleaned.test$dim_is_requested, prob)
prob = 2*ifelse(knn_pred.test == "false", 1-prob, prob) - 1

pred_knn = prediction(prob, cleaned.test$dim_is_requested)
pred_knn.auc = performance(pred_knn, "tpr", "fpr")
#plot(pred_knn.auc, avg= "threshold", colorize=T, lwd=3, main="Voilà, a ROC curve!")
AUC.knn = performance(pred_knn, "auc")
AUC.knn.value = AUC.knn@y.values[[1]]
AUC.knn.value

saveRDS(pred_knn.auc, 'perf_knn.RDS')
saveRDS(AUC.knn.value, 'AUC_knn_value.RDS')


plot(pred_knn.auc@x.values[[1]], pred_knn.auc@y.values[[1]],
     xaxs = "i", yaxs = "i", xaxt = 'n',
     lty = 2, las = 1, type = 'l',
     ylab = '', xlab = '', main = 'ROC Curve')
axis(side = 1, at = 0:5/5, labels = format(5:0/5, digits = 1))
mtext(text = "Specificity", side = 1,line = 2, at = 0.95)
mtext(text = "Sensitivity", side = 2,line = -2, at = 1.07, las = 1)
abline(0, 1, lty = 1) #add a 45 degree line
#lines(pred_knn.auc@x.values[[1]], pred_knn.auc@y.values[[1]], lty = 2, col = 1)

