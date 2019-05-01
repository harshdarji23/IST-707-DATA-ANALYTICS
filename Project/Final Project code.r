
movies<-read.csv("C://Users//Harsh Darji//Desktop//movies.csv")

head(movies)

ratings<-read.csv("C://Users//Harsh Darji//Desktop//ratingss.csv")

head(ratings)

#Libraries

library(ggplot2)

install.packages('recommenderlab')

library(recommenderlab)

library(data.table)

# Data Pre-processing

genres<-as.data.frame(movies$genres)

head(genres)

genres2<-as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE))

head(genres2)

colnames(genres2)<-c(1:10)

## we have 18 genres in total
genre_list<-c("Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror",
             "Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")

#empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix<-matrix(0,10330,18)

#set first row to genre list
genre_matrix[1,]<-genre_list

#set column names to genre list
colnames(genre_matrix)<-genre_list

#iterate through matrix

for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe

 #remove first row, which was the genre list
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)

 #convert from characters to integers

for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

head(genre_matrix2)

#Create a matrix to search for a movie by genre:

years <- as.data.frame(movies$title, stringsAsFactors=FALSE)

library(data.table)

head(years)

str(years)

years$`movies$title`<-as.character(years$`movies$title`)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

search_matrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genre_matrix2)

colnames(search_matrix) <- c("movieId", "title", "year", genre_list)

write.csv(search_matrix, "search.csv")

search_matrix <- read.csv("search.csv", stringsAsFactors=FALSE)

head(search_matrix)

# Example of search an Action movie produced in 1995:
subset(search_matrix, Action == 1 & year == 1995)$title

## Create a user profile

binaryratings <- ratings

# ratings of 4 and 5 are mapped to 1, 
# representing likes, and ratings of 3 
# and below are mapped to -1,(duslikes)

for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}


# convert binaryratings matrix to the correct format:

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}

#remove movieIds col. Rows are movieIds, cols are userIds
binaryratings2 = binaryratings2[,-1]

#Remove rows that are not rated from movies dataset

movieIds <- length(unique(movies$movieId))

ratingmovieIds <- length(unique(ratings$movieId))

movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]

rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2

genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]

rownames(genre_matrix3) <- NULL

# calculate the dot product of the genre matrix and 
# the ratings matrix and obtain the user profiles

#Calculate dot product for User Profiles

# here, 668=no of users/raters, 18=no of genres
result = matrix(0,18,668) 

for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c])) #ratings per genre
  }
}


#Convert to Binary scale

for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles

library(reshape2)

ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)

#remove userIds
ratingmat <- as.matrix(ratingmat[,-1])

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")

as.matrix(similarity_users)

image(as.matrix(similarity_users), main = "User similarity")

 #compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")

as.matrix(similarity_items)

image(as.matrix(similarity_items), main = "Item similarity")

# Exploring values of ratings:

vector_ratings <- as.vector(ratingmat@data)

# what are unique values of ratings
unique(vector_ratings) 

# what is the count of each rating value
table_ratings <- table(vector_ratings)

table_ratings

# Visualize the rating:

# rating == 0 are NA values

vector_ratings <- vector_ratings[vector_ratings != 0]

vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

# Exploring viewings of movies:

# count views for each movie
views_per_movie <- colCounts(ratingmat)

# create dataframe of views
table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie)

# sort by number of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

#Visualizing the matrix:

image(ratingmat, main = "Heatmap of the rating matrix")

image(ratingmat[1:10, 1:15], main = "Heatmap of the first rows and columns")

image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                 colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and movies")

#Normalize the data

ratingmat_norm <- normalize(ratingmat)

ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")

#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering

recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)

model_details$data

#Obtain top 10 recommendations for 1st user in dataset
recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10)

recom

#convert recommenderlab object to readable list
recom_list <- as(recom, 
                 "list")
recom_list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}

recom_result

# Evaluation:

evaluation_scheme <- evaluationScheme(ratingmat, 
                                      method="cross-validation", 
                                      k=5, given=3, 
                                      goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, 
                               method="UBCF", 
                               n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

#Explore

ratings_movies <- ratingmat[rowCounts(ratingmat) > 50,
                             colCounts(ratingmat) > 50]

ratings_movies 

min_movies <- quantile(rowCounts(ratings_movies), 0.98)

min_users <- quantile(colCounts(ratings_movies), 0.98)

image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users], 
main = "Heatmap of the top users and movies")

average_ratings_per_user <- rowMeans(ratings_movies)

qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")


### Normalizing data

ratings_movies_norm <- normalize(ratings_movies)

sum(rowMeans(ratings_movies_norm) > 0.00001)

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users], 
main = "Heatmap of the top users and movies")

### Binarizing data

#### 1st option: define a matrix equal to 1 if the movie has been watched

ratings_movies_watched <- binarize(ratings_movies, minRating = 1)
min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)

image(ratings_movies_watched[rowCounts(ratings_movies) > min_movies_binary,
                             colCounts(ratings_movies) > min_users_binary], 
main = "Heatmap of the top users and movies")

#### 2nd option: define a matrix equal to 1 if the cell has a rating above the threshold

ratings_movies_good <- binarize(ratings_movies, minRating = 3)

image(ratings_movies_good[rowCounts(ratings_movies) > min_movies_binary, 
colCounts(ratings_movies) > min_users_binary], 
main = "Heatmap of the top users and movies")

## ITEM-based Collaborative Filtering Model

## Defining training/test sets

which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratings_movies),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))

recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

## Building the recommendation model

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")

recommender_models$IBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, 
                          method = "IBCF",
                          parameter = list(k = 30))

recc_model

class(recc_model)

model_details <- getModel(recc_model)

class(model_details$sim) 

dim(model_details$sim)


n_items_top <- 20

image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")


row_sums <- rowSums(model_details$sim > 0)

table(row_sums)

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")

## Applying recommender system on the dataset:

n_recommended <- 10 

recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test, 
                          n = n_recommended)

recc_predicted

recc_user_1 <- recc_predicted@items[[1]] # recommendation for the first user

movies_user_1 <- recc_predicted@itemLabels[recc_user_1]

movies_user_2 <- movies_user_1

for (i in 1:10){
  movies_user_2[i] <- as.character(subset(movies, 
                                         movies$movieId == movies_user_1[i])$title)
}


movies_user_2

recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratings_movies)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recc_matrix[,1:4]

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)

table_top <- data.frame(as.integer(names(number_of_items_top)),
                       number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                         movies$movieId == table_top[i,1])$title)
}


colnames(table_top) <- c("Movie title", "No of items")
head(table_top)

## USER-based Collaborative Filtering Model

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")

recommender_models$UBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, method = "UBCF")

recc_model
model_details <- getModel(recc_model)

model_details$data

## Applying the recommender model on the test set

n_recommended <- 10

recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test, 
                          n = n_recommended)

recc_predicted

## Explore results

recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratings_movies)[x]) })

recc_matrix[, 1:4]

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)

table_top <- data.frame(as.integer(names(number_of_items_top)), number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                         movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie title", "No of items")
head(table_top)

## Evaluating the Recommender Systems

### Splitting the data

percentage_training <- 0.8

min(rowCounts(ratings_movies)) 
items_to_keep <- 5 #number of items to generate recommendations
rating_threshold <- 3 # threshold with the minimum rating that is considered good
n_eval <- 1 #number of times to run evaluation

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "split",
                              train = percentage_training, 
                              given = items_to_keep, 
                              goodRating = rating_threshold, 
                              k = n_eval)

eval_sets

getData(eval_sets, "train") # training set
getData(eval_sets, "known") # set with the items used to build the recommendations
getData(eval_sets, "unknown") # set with the items used to test the recommendation

qplot(rowCounts(getData(eval_sets, "unknown"))) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("unknown items by the users")

### Bootstrapping the data

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "bootstrap", 
                              train = percentage_training, 
                              given = items_to_keep,
                              goodRating = rating_threshold, 
                              k = n_eval)

table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))

qplot(n_repetitions) + 
  ggtitle("Number of repetitions in the training set")


### Using cross-validation to validate models

n_fold <- 4
eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "cross-validation",
                              k = n_fold, 
                              given = items_to_keep, 
                              goodRating = rating_threshold)

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

## Evavluating the ratings

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "cross-validation",
                              k = n_fold, 
                              given = items_to_keep, 
                              goodRating = rating_threshold)

model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, 
                                parameter = model_parameters)

items_to_recommend <- 10

eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings")

qplot(rowCounts(eval_prediction)) + 
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = TRUE)

head(eval_accuracy)

qplot(eval_accuracy[, "RMSE"]) + 
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = FALSE) 

eval_accuracy

## Evaluating the recommendations

results <- evaluate(x = eval_sets, 
                    method = model_to_evaluate, 
                    n = seq(10, 100, 10))

head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

plot(results, annotate = TRUE, main = "ROC curve")

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")

## Comparing models

models_to_evaluate <- list(
IBCF_cos = list(name = "IBCF", 
                param = list(method = "cosine")),
IBCF_cor = list(name = "IBCF", 
                param = list(method = "pearson")),
UBCF_cos = list(name = "UBCF", 
                param = list(method = "cosine")),
UBCF_cor = list(name = "UBCF", 
                param = list(method = "pearson")),
random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

sapply(list_results, class) == "evaluationResults"

avg_matrices <- lapply(list_results, avg)
head(avg_matrices$IBCF_cos[, 5:8])

## Identifying the most suitable model

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

## Optimizing a numeric parameter

vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF",
       param = list(method = "cosine", k = k))
})

names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")




