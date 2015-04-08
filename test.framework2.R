positive = read.csv('Tyrosine/positive_train2.csv')
negative = read.csv('Tyrosine/negative_train2.csv')

ppm = positive[,1] # First column are the variable names
train = t(cbind(positive[,-1],negative[,-1])) # combine them except the first column
train = as.data.frame(train) # Now convert it to a data frame
train$label = 0 # Now add in a label column initially set to all 0's
train$label[1:94] = 1 # Now set the first 94 samples to 1 to indicate they are positive
train$label = factor(train$label)

colnames(train) = c(ppm,'label') # Now correct the column names
rownames(train) = 1:nrow(train) # Now add the row names

#### This only needs to run once ####
# One way to handle the data is to divide into 4 regions
n.regions = 0
regions.start = c()
regions.end = c()
dist.cutoff = 0.01
for (i in 2:length(ppm)) {
  if (n.regions == 0 && ppm[i-1]-ppm[i] > dist.cutoff) {
    n.regions = n.regions + 1
    regions.start[n.regions] = i
  } else if (ppm[i-1]-ppm[i] > dist.cutoff) {
    regions.end[n.regions] = i-1  
    n.regions = n.regions + 1
    regions.start[n.regions] = i
  }
}
regions.end[n.regions] = ncol(train)-1
####

engineer_features = F
if (engineer_features == T) {
  #### This needs to be run every time you want to create new features ####
  # Now compute the features that we can use for prediction
  # This is only necessary if you want to use features for each region. 
  # Please adapt this in new ways. These features do not contain enough detail.
  nFeatures = 3*n.regions
  features = matrix(nrow = nrow(train), ncol = nFeatures)
  cnames = c()
  for (i in 1:nrow(train)) {
    # Max of each region
    for (j in 1:n.regions) {
      features[i,j] = max(unlist(train[i,regions.start[j]:regions.end[j]]))
      if (i == 1) {
        cnames[j] = paste('max',j,sep="")
      }
    }
    # Min of each region
    shift_over = n.regions
    for (j in 1:n.regions) {
      features[i,j+shift_over] = min(unlist(train[i,regions.start[j]:regions.end[j]]))
      if (i == 1) {
        cnames[j+shift_over] = paste('min',j,sep="")
      }
    }
    # Mean of each region
    shift_over = shift_over + n.regions
    for (j in 1:n.regions) {
      features[i,j+shift_over] = mean(unlist(train[i,regions.start[j]:regions.end[j]]))
      if (i == 1) {
        cnames[j+shift_over] = paste('mean',j,sep="")
      }
    }
  }
  features = as.data.frame(features)
  colnames(features) = cnames
  features$label = train$label
  ####
} else {
  features = train
}  

# k-fold cross-validation loop
library(rpart)
k = 3
positive_accuracy = 0
negative_accuracy = 0
train_accuracy = 0
accuracy = 0
random.order = sample(1:nrow(features),nrow(features))
features = features[random.order,]
size = round(nrow(features)/k)
for (i in 1:k) {
  start = 1 + size*(i-1)
  #end = min(nrow(train),size + size*(i-1))
  if (i == k) {
    end = nrow(features)
  } else {
    end = size + size*(i-1)
  }
  test.set = features[start:end,]
  train.set = features[-c(start:end),]
  fit <- rpart(label ~ ., data=train.set)
  predictions = predict(fit,test.set,type='vector')-1 # They use 1 to indicate class 1 which is actually 0
  correct_predictions = test.set$label
  accuracy = accuracy + length(which(predictions == correct_predictions))/length(predictions)
  if (length(which(correct_predictions==1)) > 0) {
    positive_accuracy = positive_accuracy + length(which(predictions == correct_predictions & correct_predictions == 1))/length(which(correct_predictions==1))
  }
  negative_accuracy = negative_accuracy + length(which(predictions == correct_predictions & correct_predictions == 0))/length(which(correct_predictions==0))

  predictions = predict(fit,train.set,type='vector')-1 # They use 1 to indicate class 1 which is actually 0
  correct_predictions = train.set$label
  train_accuracy = train_accuracy + length(which(predictions == correct_predictions))/length(predictions)
}
accuracy = accuracy / k
train_accuracy = train_accuracy / k
positive_accuracy = positive_accuracy / k
negative_accuracy = negative_accuracy / k
print('The training accuracy is')
print(train_accuracy)
print('The accuracy is')
print(accuracy)
print('The negative accuracy is')
print(negative_accuracy)
print('The positive accuracy is')
print(positive_accuracy)

