#loading necessary libraries

library(neuralnet)
library(readr)

#preparing an array of column names
names <- NULL
for(i in 1:784){
  names[i] <- paste("pixel",i, sep = "." )
}

# creating empty data frame with column  names
train_df  <- as.data.frame(setNames(replicate(784,numeric(0), simplify = F), names))

# set working directory and picking up files 
setwd(dir = "/home/jaspreet/Documents/Neural Network/Images/train/")
file_list <- list.files()

#preparing array of file names 
file_list <- paste(1:49000, "png", sep = ".")

# developing pixel_data_frame
for (file in file_list){
  temp <- readbitmap::read.bitmap(file_list[nrow(train_df) +1])
  a <- as.data.frame(t(as.vector(temp[, , 1])))
  train_df[nrow(train_df) + 1,] <- as.vector(a)
}

# Collumn bind
head_train <- head(train , 40815)
train_df <- cbind(train_df,head_train[,2])

# removing row names 
rownames(train_df) <-  NULL

#exporting it to excel
write_excel_csv(x = train_df, path = "/home/jaspreet/Desktop/train_df", col_names = TRUE )

#fetching required value from data frame
train_df <- train_df[,1:785]

# Neural Network :)
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial

# developing formula
f <- as.formula(paste("label ~", paste(names[!names %in% "y"], collapse = " + ")))

ANN <- neuralnet(formula = f ,train_df, hidden=10, threshold=0.01)
print(ANN)

#################################################################################
#Now developing a test data frame 

# creating empty data frame with column  names
test_df  <- as.data.frame(setNames(replicate(784,numeric(0), simplify = F), names))

# set working directory and picking up files 
setwd(dir = "/home/jaspreet/Documents/Neural Network/Images/test/")
file_list <- list.files()

#sorting files 
file_list <- paste(49000:70000, "png", sep = ".")

# developing train_data_frame
for (file in file_list){
  temp <- readbitmap::read.bitmap(file_list[nrow(test_df) +1])
  a <- as.data.frame(t(as.vector(temp[, , 1])))
  test_df[nrow(test_df) + 1,] <- as.vector(a)
}

# removing row names 
rownames(test_df) <-  NULL

write_excel_csv(x = test_df, path = "/home/jaspreet/Desktop/test_df", col_names = TRUE )
#write.csv(x = pixel_df, file = "/home/jaspreet/Desktop/pixel_df")

#################################################################################
#Test the neural network on some training data

ANN.results <- compute(ANN, test_df) #Run them through the neural network
write_excel_csv(x = as.data.frame(ANN.results) , path = "/home/jaspreet/Desktop/result_df")
ANN.results$net.result
final_result <- abs(round(ANN.results$net.result))

################################################################################
#preparing output
file_list <- as.data.frame(file_list)
submission <- cbind(as.data.frame(file_list), final_result)
write_csv(x = as.data.frame(file_list), path = "/home/jaspreet/Desktop/file_list")
write_csv(x = as.data.frame(final_result), path = "/home/jaspreet/Desktop/final_result_df")
#################################################################################

#Lets see what properties net.result has
ls(ANN.results)

#print output 
print(ANN.results$neurons)
print(net.results$ANN.result)








