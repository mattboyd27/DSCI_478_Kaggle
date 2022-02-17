library(tidyverse)
library(class)
library(caret)
df = read.csv("~/DSCI 478/Kaggle Project/digit-recognizer/train.csv")
test = read.csv("~/DSCI 478/Kaggle Project/digit-recognizer/test.csv")

# Visualize different numbers by row
df %>%
  slice(15) %>%
  select(starts_with("pixel")) %>%
  pivot_longer(starts_with("pixel")) %>%
  mutate(x = (row_number() - 1) %% 28,
         y = -((row_number() - 1) %/% 28)) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_tile()

# Training data count of different labels
df %>%
  group_by(label) %>%
  summarize(n = n()) %>%
  ggplot() +
  geom_col(aes(x = label, y = n))


## KNN
vector = createDataPartition(df$label, p = 0.8, list = F) %>% as.numeric()
train1 = df[vector,]
test2 = df[-vector,]

response = train1 %>% select(label) %>% as.matrix()

test1 = test2  %>% select(-label) %>% as.matrix()

train1 = train1 %>% select(-label) %>% as.matrix()


knn_model = knn(train1[1:100,], test1[1:100,], response[1:100], k=2)


knn_df = data.frame()
folds = sample(1:5, nrow(df), replace = T)
for(i in 1:5) {
  message(paste("Fold = ", i))
  train = df %>%
    filter(folds != i) %>%
    select(-label) %>%
    as.matrix()
  
  test = df %>%
    filter(folds == i) %>%
    select(-label) %>%
    as.matrix()
  
  train_response = df %>%
    filter(folds != i) %>%
    select(label) %>%
    as.matrix()
  
  test_response = df %>%
    filter(folds == i) %>%
    select(label) %>%
    pull()
  
  for(k in seq(1, 21, 10)) {
    message(paste("Neighbors = ", k))
    
    knn_model = knn(train, test, train_response, k = k)
    
    accuracy = mean(knn_model == test_response)
    print(accuracy)
    
    knn_df = knn_df %>%
      bind_rows(data.frame(k = k, accuracy = accuracy, fold = i))
    
  }
}

knn_df %>%
  group_by(k) %>%
  summarize(accuracy = mean(accuracy)) %>%
  arrange(desc(accuracy))



  
