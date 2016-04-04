#' SVM(Support Vector Machine) Fire Classifier
#'
#' Script for building and training the SVM ibutton fire classification model.
#'
#' @details Will require trainingHhs.Rda to create svm_fire_classifier.Rda
#' @export

# **Funksie gaan verskillend wees vir die 3 van ons**

# Date created: 26 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
#
# ---------------------------------------- #
# start clean
svm_fire_classifier <- function(){
rm(list = ls())
# ---------------------------------------- #
if (!exists("dropdir")) {dropdir <- "C:/Users/Willem/Desktop/Nova Packages/NovaFire/novaFire/"}
novafunctdir <- paste(dropdir, "R/", sep = "")
novadatadir <- paste(dropdir, "data/", sep = "")
# ---------------------------------------- #
# load necessary libraries
if (!require(e1071)){
        install.packages("e1071")
        if (!require(e1071)){
                stop("Load package e1071 manually")
        }
}

if (!require(caret)){
        install.packages("caret")
        if (!require(caret)){
                stop("Load package caret manually")
        }
}

if (!require(novaUtils)){
        devtools::install_github("NovaInstitute/Rpackages/novaUtils")
        if (!require(novaUtils)){
                stop("Load package novaUtils manually")
        }
}


# ---------------------------------------- #
# source locally required code
source(paste(novafunctdir, "EOPhhObj.R", sep = ""))
library(novaUtils)
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# load the data to train our dragon with
load(paste(novadatadir, "trainingHhs.Rda", sep = ""))

# ---------------------------------------- #
# construct a single training data frame
ibuttonDfs <- lapply(X = trainingHhs, FUN = function(thh) {
  dfIbutton <- thh@ibuttonData$data_df
  dfIbutton$stand_number <- thh@address@standNumber
  return(dfIbutton)
})
trainingData <- do.call("rbind", ibuttonDfs[1:7])
testData <- do.call("rbind", ibuttonDfs[8:9])
rm(ibuttonDfs, trainingHhs)
allData <- rbind(trainingData, testData)
rm(trainingData, testData)

# remove unnecessary fields
dropCols <- c("w_id", "w_shortname", "w_read_date",
             "c_id", "c_shortname", "c_read_date", "mon_per", "log_ignition_time")
allData <- remove.cols(df = allData, colNames = dropCols)

# ---------------------------------------- #
# change NA in log_fire_type to "none"
allData[which(is.na(allData$log_fire_type)), "log_fire_type"] <- "none"
allData$run_length <- NA

# ---------------------------------------- #
# sit 'n run-length in om refills te help probeer voorspel
standsplits <- split(x = allData, f = allData$stand_number)
standsplits <- lapply(X = standsplits, FUN = function(sdf) {

  runlength <- 0L
  for (r in 1:nrow(sdf)) {
    if (sdf[r, "log_fire_type"] == "no") {runlength <- 0}
    if (sdf[r, "log_fire_type"] %in% c("new", "refill")) {runlength <- runlength + 1}
    sdf[r, "run_length"] <- runlength
  }

  return(sdf)
})
allData <- do.call("rbind", standsplits)
rm(standsplits)

# ---------------------------------------- #
# sit lag terme in vir die eerste model wat yes/no vir "fire" voorspel
allData$lag_c <- lag(allData$c)
allData$lag_c2 <- lag(allData$c, 2)
allData$lag_c3 <- lag(allData$c, 3)
allData$lag_w <- lag(allData$w)
allData$lag_w2 <- lag(allData$w, 2)
allData$lag_w3 <- lag(allData$w, 3)
allData$lag_sdif <- lag(allData$sdif)
allData$lag_sdif2 <- lag(allData$sdif, 2)
allData$lag_sdif3 <- lag(allData$sdif, 3)
allData$lag_rle <- lag(allData$run_length)

# ---------------------------------------- #
# verdeel die data in 'n training set en 'n test set
standsplits <- split(x = allData, f = allData$stand_number)

trainingData <- standsplits[1:7]
trainingData <- do.call("rbind", trainingData)
testData <- standsplits[8:9]
testData <- do.call("rbind", testData)

# ---------------------------------------- #
# ---------------------------------------- #
# TRAIN DIE MODEL VIR DIE FIRE YES/NO
X <- as.matrix(trainingData[,c("c", "w", "sdif",
                               "lag_c", "lag_c2", "lag_c3",
                               "lag_w", "lag_w2",
                               "lag_sdif", "lag_sdif2", "lag_sdif3")])
Y <- as.vector(as.factor(trainingData[,"fire"]))
model_fire <- svm(X, Y, type = "C-classification")

antw <- testData[ ,"fire", drop = FALSE]
pred <- predict(model_fire,
                newdata = testData[, c("c", "w", "sdif",
                                       "lag_c", "lag_c2", "lag_c3",
                                       "lag_w", "lag_w2",
                                       "lag_sdif", "lag_sdif2", "lag_sdif3")])
prop.table(table(as.character(pred), testData$fire), 1)

# ---------------------------------------- #
# ---------------------------------------- #
# now train our dragon with all the data we've got
trainingData <- allData

# ---------------------------------------- #
# ---------------------------------------- #
archive(fileName = "svm_fire_classifier.Rda",
        currentDir = novadatadir, verbose = TRUE)
save(model_fire, file = paste(novadatadir, "svm_fire_classifier.Rda", sep=""))



# sal graag area under the curve (auc) wil uitwerk en plot

# ---------------------------------------- #
# ---------------------------------------- #
# # TRAIN DIE MODEL VIR DIE FIRE_TYPE NEW/REFILL/NO
# trainingData$fire <- gsub(pattern = "yes", replacement = 1, x = trainingData$fire, fixed = TRUE)
# trainingData$fire <- gsub(pattern = "no", replacement = 0, x = trainingData$fire, fixed = TRUE)
# trainingData$fire <- as.numeric(trainingData$fire)
#
# X <- as.matrix(trainingData[, c("c", "w", "sdif",
#                                 "lag_c",
#                                 "lag_w",
#                                 "lag_sdif", "lag_sdif2",
#                                 "fire", "run_length", "lag_rle")])
# Y <- as.vector(as.factor(trainingData[, "log_fire_type"]))
# model_newOrRefill <- svm(X, Y, type = "C-classification")
#
# testData$fire <- gsub(pattern = "yes", replacement = 1, x = testData$fire, fixed = TRUE)
# testData$fire <- gsub(pattern = "no", replacement = 0, x = testData$fire, fixed = TRUE)
# testData$fire <- as.numeric(testData$fire)
#
# antw <- testData[ ,"log_fire_type", drop = FALSE]
# pred <- predict(model_newOrRefill,
#                 newdata = testData[, c("c", "w", "sdif",
#                                 "lag_c",
#                                 "lag_w",
#                                 "lag_sdif", "lag_sdif2",
#                                 "fire", "run_length", "lag_rle")])
# prop.table(table(as.character(pred), testData$log_fire_type), 1)

# ---------------------------------------- #
# ---------------------------------------- #

}
