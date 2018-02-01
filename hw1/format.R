
library(knitr)

# create an empty dataframe to store the output table 
data <- data.frame(matrix(nrow = 10, ncol = 5))

# create a list for all the text files in the working directory
list.filename <- list.files(pattern = ".txt$")

# create an empty list to store files
list.data <- list()

n <- length(list.filename)

# create a loop to read in all the texts 
for(i in 1:n){
  list.data[[i]] <- read.table(list.filename[i])
}

# add the names of data to the list, so that can call the file directly
names(list.filename) <- list.filename

# write in the second column
data[,2] <- c("PrimeAvg","SampAvg","PrimeAvg","SampAvg","PrimeAvg","SampAvg",
              "PrimeAvg","SampAvg","PrimeAvg","SampAvg")

# input sample size 
for(i in 1:10){
  # using index to specify the sample size
  index <- as.integer((i+1)/2)
  if(i %% 2 == 1){
    data[i,1] <- index*100
    
  }
}


# select file with same sample size 
fileindex <- seq(1,15,3)

# input data into table 
for(i in fileindex) {
  for(j in 0:2){
    temp <- list.data[[i+j]]
    data[(i+2)/3*2-1,j+3] <- temp[1]
    data[(i+2)/3*2,j+3] <- temp[2]
  }
}

# change "gaussian" col position 
output <- cbind(data[,-3],data[,3])

# rename the columns
colname <- c("n","Method","t1","t5","Gaussian")
colnames(output) <- colname

# output data in table format
kable(output,"markdown")