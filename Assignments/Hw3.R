###------------------------------------Assignment 3------------------------------------###
###------------------------------------chb2132------------------------------------###

setwd("~/Documents/R/Assignment 3");

RNGversion(vstr = 3.6);

library(arules);
library(arulesViz);

###------------------------------------Section 3.1------------------------------------###

data(Groceries);

?Groceries;

summary(Groceries)

rules_all = apriori(Groceries, parameter = list(support = 0.01, confidence = 0.01));

summary(rules_all)

rules_all_2 = apriori(Groceries, parameter = list(support = 0.001, confidence = 0.001));

summary(rules_all_2)

rules_all_3 = apriori(Groceries, parameter = list(supp = 0.01, conf = 0.01, maxlen = 2));

summary(rules_all_3)

basket_rules_3 <- sort(rules_all_3, by = 'confidence', decreasing = TRUE);

soda_rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.01, maxlen = 2), 
                       appearance = list(default = "rhs", lhs = "soda"));

inspect(soda_rules)

yogurt_rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.01, maxlen = 2), 
                      appearance = list(default = "rhs", lhs = "yogurt"));

inspect(yogurt_rules)

yogurt_rules <- sort(yogurt_rules, by = 'support', decreasing = TRUE);

inspect(yogurt_rules)

###------------------------------------Section 3.2------------------------------------###

library(recommenderlab);

read.csv("product_ratings_data.csv");

str(read.csv("product_ratings_data.csv"));

ratings_matrix <- as(read.csv("product_ratings_data.csv"), Class = 'realRatingMatrix');
as(ratings_matrix, 'matrix');

nratings(ratings_matrix);

as(ratings_matrix, 'matrix')['u10023', c('prod_14')]

###-------------------------------------------------------------------------------###

set.seed(617);
split_617 = sample(nrow(ratings_matrix), size = 0.9*nrow(ratings_matrix));
train_617 = ratings_matrix[split_617,];
test_617 = ratings_matrix[-split_617,];

set.seed(1031);
split_1031 = sample(nrow(ratings_matrix), size = 0.9*nrow(ratings_matrix));
train_1031 = ratings_matrix[split_1031,];
test_1031 = ratings_matrix[-split_1031,];

###-------------------------------------------------------------------------------###

hist(getRatings(train), breaks="FD");

colMeans(train[,'prod_100']);

colMeans(normalize(train, method = 'center', row = TRUE)[,'prod_100']);

norm_train <- normalize(train_1031, method = 'center', row = TRUE);

similarity(normalize(train_1031, method = 'center', row = TRUE)[1:6,], method = 'cosine')

###------------------------------------Section 3.3------------------------------------###

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix

recom_ubcf = Recommender(train_1031, 
                         method='UBCF', 
                         parameter=list(method='cosine',nn=25, normalize='center'));

pred_ubcf_topN = predict(recom_ubcf,newdata=test_1031,method='topNList',n=5);

getList(pred_ubcf_topN)['u10139'];

pred_ubcf = predict(recom_ubcf,newdata=test_1031,type='ratings');

as(pred_ubcf,'matrix')['u10139',];

###--------------------------------------------------------------------------------###

recommenderRegistry$get_entries(data = 'realRatingMatrix')$UBCF_realRatingMatrix;

recom_ubcf = Recommender(train, method = 'UBCF', 
                         parameter = list(method = 'cosine', nn = '100', normalize = 'center'));

recom_ubcf

pred_ubcf_topN = predict(recom_ubcf, newdata = test, method = 'topNList', n = 5);

getList(pred_ubcf_topN)['u10139']

as(pred_ubcf, 'matrix')['u10139',]

###--------------------------------------------------------------------------------###

recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix

recom_ibcf = Recommender(train_1031, 
                         method='IBCF', 
                         parameter=list(method='cosine',nn=25, normalize='center'));

pred_ibcf_topN = predict(recom_ibcf,newdata=test_1031,method='topNList',n=5);

getList(pred_ibcf_topN)['u10139'];

pred_ibcf = predict(recom_ibcf,newdata=test_1031,type='ratings');

as(pred_ibcf,'matrix')['u10139',];

###--------------------------------------------------------------------------------###

recommenderRegistry$get_entries(data = 'realRatingMatrix')$IBCF_realRatingMatrix;

recom_ibcf = Recommender(train, method = 'IBCF', 
                         parameter = list(k = 30, method = 'cosine', normalize = 'center'));

recom_ibcf

pred_ibcf_topN = predict(recom_ibcf, newdata = test, method = 'topNList', n = 5);

getList(pred_ibcf_topN)['u10139'];
as(pred_ibcf_topN, 'matrix')['u10139',]

###--------------------------------------------------------------------------------###

set.seed(1031);
es = evaluationScheme(ratings_matrix,method='split_1031',train = 0.8, given=30);

recom_I = Recommender(getData(es,'train'), method='IBCF');

pred_ibcf = predict(recom_I, newdata = getData(es,'known'), type='ratings');

accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf, data = getData(es,'unknown'));
accuracy_ibcf;

###--------------------------------------------------------------------------------###

recom_U = Recommender(getData(es,'train'), method='UBCF');

pred_ubcf = predict(recom_U, newdata = getData(es,'known'), type='ratings');

accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf, data = getData(es,'unknown'));
accuracy_ubcf;

###--------------------------------------------------------------------------------###

recom_U_100 = Recommender(getData(es,'train'), 
                      method='UBCF', 
                      parameter = list(nn = 100));

pred_ubcf_100 = predict(recom_U_100, newdata = getData(es,'known'), type='ratings');

accuracy_ubcf_100 = calcPredictionAccuracy(x = pred_ubcf_100, data = getData(es,'unknown'));
accuracy_ubcf_100;

###--------------------------------------------------------------------------------###

recommenderRegistry$get_entries(data  = 'realRatingMatrix')$UBCF_realRatingMatrix;

recom_ubcf = Recommender(train, method = 'UBCF', 
                         parameter = list(method = 'cosine', nn = 100, normalize = 'center'));

recom_ubcf

pred_ubcf_topN = predict(recom_ubcf, newdata = test, method = 'topNList', n = 5);

getList(pred_ubcf_topN)['u10139']

recom_u2 <- Recommender(data = getData(es, 'train'),
                        method = 'UBCF',
                        parameter = list(method = 'cosine', nn = 100, normalize = 'center'));

pred_u2 <- predict(recom_u2, newdata = getData(es, 'known'), type = 'ratings');

calcPredictionAccuracy(pred_u2, data = getData(es, 'unknown'));

###--------------------------------------------------------------------------------###

recom_pop = Recommender(train_1031, method = 'POPULAR', 
                         parameter = list(k = 30, method = 'cosine'));

recom_pop

recom_pop_topN = predict(recom_pop, newdata = test_1031, method = 'topNList', n = 5);

getList(recom_pop_topN)['u10139'];
as(recom_pop, 'matrix')['u10139']

pred_pop = predict(recom_pop, newdata = getData(es, 'known'), type = 'ratings');
calcPredictionAccuracy(pred_pop, data = getData(es, 'unknown'))

###--------------------------------------------------------------------------------###

set.seed(1031);

train_rows = sample(1:nrow(ratings_matrix), size = 0.9*nrow(ratings_matrix), replace = F)
ratings_train = ratings_matrix[train_rows,]
ratings_test = ratings_matrix[-train_rows,]

rec_model = Recommender(data = ratings_train, method = "UBCF")
rec_model

n_reco = 5 # lets get 5 recommendations for each user
recommendations = predict(object = rec_model, newdata = ratings_test, n = n_reco);

sort(ratings_test, method = 'center', decreasing = T, is.na = F)
getList(recommendations)['u10139']
