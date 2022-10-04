library(arules)
library(tokenizers) 
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2) 
#install.packages("syuzhet") ## sentiment analysis library(syuzhet)
library(stringr) 
library(arulesViz) ## load last
library(stopwords)


arm_data = read.csv("/Users/gauthamiaithal/Local_Documents/MS DS/Fall22/ML/ML Assignment 1/Code/cleaned_arm_data_for_R.csv")
arm_data <- subset (arm_data, select = -X)
transFile = "/Users/gauthamiaithal/Desktop/YT_titles.csv"
Trans = file(transFile)
## Tokenizing words in the titles of Youtube videos
Tokens <- tokenizers::tokenize_words(arm_data$title[1], stopwords = stopwords::stopwords("en"),
                                    lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, 
                                    simplify = TRUE)

## writing squished tokens in the file
cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = "," )
close(Trans)

## Appending remaining list of extracted tokens to the file
Trans = file(transFile, open= "a")
for(i in 2:nrow(arm_data)){
  Tokens <- tokenize_words(arm_data$title[i], stopwords = stopwords::stopwords("en"),
                                       lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, 
                                       simplify = TRUE)
  cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = "," )
  
}
close(Trans)

## Reading in the transactions and inspecting them
TitleTrans = read.transactions(transFile, rm.duplicates = FALSE, format = "basket", sep = ",")
inspect(TitleTrans)
sample_Trans = sample(TitleTrans, 50)
summary(sample_Trans)
inspect(sample_Trans)


## Reading the transaction data into a dataframe
DF = read.csv(transFile, header = FALSE, sep=",")
head(DF)


## Specifically removing words

##converting all column sto char
DF = DF %>%
  mutate_all(as.character())
(str(DF))

## Cleaning with grepl
myDF = NULL
for(i in 1:ncol(DF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", DF[[i]]))
  myDF = cbind(myDF, MyList)
}

## For all true replace with blank

DF[myDF] <- ""
(DF)


## More cleaning
myDF = NULL
myDF2 = NULL
myDF3 = NULL

for(i in 1:ncol(DF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", DF[[i]]))
  
  MyList2 = c()
  MyList2 = c(MyList, grepl("[A-Z]{4,}", DF[[i]]))
  
  MyList3 = c()
  MyList3 = c(MyList, grepl("[A-Z]{12,}", DF[[i]]))
  
  
  myDF = cbind(myDF, MyList)
  myDF2 = cbind(myDF, MyList2)
  myDF3 = cbind(myDF, MyList3)
  
}

DF[myDF] <- ""
DF[!myDF2] <- ""
DF[myDF] <- ""
(DF)

##Storing DF in a csv
write.csv(DF,"/Users/gauthamiaithal/Desktop/Cleaned_YT_titles.csv", row.names = FALSE)
## Let start with association rule mining
## Reading in the transactions and inspecting them
transFileNew = "/Users/gauthamiaithal/Desktop/Cleaned_YT_titles.csv"
TitleTransNew = read.transactions(transFileNew, rm.duplicates = FALSE, format = "basket", sep = ",")
inspect(TitleTrans)
sample_Trans_new = sample(TitleTransNew, 50)
summary(sample_Trans)
inspect(sample_Trans_new)

YT_title_rules = arules::apriori(TitleTransNew,
                                 parameter = list(support= 0.0001, confidence= 0.0001, minlen=2, maxlen=6))

inspect(YT_title_rules[1:30])

## Support visualization
sortedRules_sup = sort(YT_title_rules, by="support", decreasing = TRUE)
inspect(sortedRules_sup[1:20])
plot(sortedRules_sup[1:25], method = "graph", engine = 'interactive', shading = "confidence")

## Confidence Visualization
sortedRules_conf = sort(YT_title_rules, by="confidence", decreasing = TRUE)
inspect(sortedRules_conf[1:20])
plot(sortedRules_conf[1:25], method = "graph", engine = 'interactive', shading = "support")

## Lift Visualization
sortedRules_lift = sort(YT_title_rules, by="lift", decreasing = TRUE)
inspect(sortedRules_lift[1:20])
plot(sortedRules_lift[1:25], method = "graph", engine = 'interactive', shading = "confidence")



