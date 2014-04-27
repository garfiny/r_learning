library(plyr)

#get subjects
load_subjects <- function(setname) {
  file <- sprintf("original_data/%s/subject_%s.txt", setname, setname)
  subjects <- read.csv(file, header=F)
  subjects$V1
}

#get activities
load_activities <- function(setname) {
  data_file <- sprintf("original_data/%s/y_%s.txt", setname, setname)
  activities <- read.csv(data_file, header=F)
  names(activities) <- c("activity_id")
  labels <- read.csv("original_data/activity_labels.txt", header=F)
  l <- strsplit(as.character(labels$V1), " ")
  l <- t(data.frame(l))
  colnames(l) <- c("activity_id", "activity")
  activities <- join(activities, data.frame(l), by = "activity_id")
  activities$activity
}

load_data <- function(setname) {
  #get features
  features <- read.csv("original_data/features.txt", header=F, sep=" ")
  features <- as.character(features[,2])
  feature_indices <- grep("-mean[()-]|-std[()-]", features)
  #get data x
  data_file <- sprintf("original_data/%s/X_%s.txt", setname, setname)
  x <- read.csv(data_file, header=F)
  dat <- as.character(x$V1)
  dat <- strsplit(dat, "[ ]+")
  dat <- data.frame(dat)
  dat <- dat[-1,]
  transformed <- t(dat)
  rownames(transformed) <- 1:nrow(transformed)
  #filter unused columns
  dat <- as.data.frame(transformed[,feature_indices])
  names(dat) <- features[feature_indices]
  dat
}

merge_data <- function() {
  testset <- cbind(subject = load_subjects("test"), activity = load_activities("test"), load_data("test"))
  trainset <- cbind(subject = load_subjects("train"), activity = load_activities("train"), load_data("train"))
  testlist <- split(testset, as.factor(testset$subject))
  trainlist <- split(trainset, as.factor(trainset$subject))
  
  l <- list()
  for(n in names(trainlist)) l[[n]] <- trainlist[[n]]
  for(n in names(testlist)) l[[n]] <- testlist[[n]]
  l
}

calculate_means <- function(data) {
  l <- list()
  result <- data.frame()
  for(n in names(data)) {
    activitygroup <- split(data[[n]], as.factor(data[[n]]$activity))
    for(activityname in names(activitygroup)) {
      result <- rbind(result, mean_by_group(activitygroup[[activityname]]))
    }
  }
  names(result) <- sapply(names(result), function(n) rename_column(n))
  result
}

mean_by_group <- function(group) {
  r <- sapply(group[, 3:ncol(group)], function(item) { mean(as.numeric(as.character(item))) })
  df <- data.frame(group[1,1], group[1,2])
  df <- cbind(df, t(data.frame(r)))
  names(df) <- names(group)
  df
}

rename_column <- function(colname) {
  colname <- gsub("^t", "Time Domain - ", colname)
  colname <- gsub("^f", "FFT - ", colname)
  colname <- gsub("Acc", " Acceleration", colname)
  colname <- gsub("(Gyro|Jerk)", " \\1", colname)
  colname <- gsub("Mag", " Magnitude", colname)
  colname <- gsub("-mean\\(\\)", " Mean", colname)
  colname <- gsub("-std\\(\\)", " Standard Deviation", colname)
  gsub("-([X|Y|Z]$)", " (\\1)", colname)
}

write_to_file <- function(data, path) {
  write.csv(data, path, row.names = F)
}
