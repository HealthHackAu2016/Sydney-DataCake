#require("plyr")
require("xml2")
require("XML")
library(httr) 
#library(readr)
#library(rjson) 
#library(dplyr)
library(jsonlite)
#library(XML2R)


#read it
ids <- read.csv("~/Downloads/Publication.csv")
id <- as.data.frame(unique(ids$PMID))
pages <- list(id)
data <- list()
# authors <- list()
# types <- list()
# keys <- list()
# i=2

nrow(id)
#for(i in 1:nrow(id)){
for(i in 3080:3090) {
  mydata <- content(POST(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=abstract&id=", id[as.numeric(i), ])))
  
  message("Retrieving page ", i)
  xml <-  xmlTreeParse(mydata)
  xml <- xmlParse(mydata)
  title <- as.data.frame(t(xmlSApply(xml["//*/ArticleTitle"],xmlValue)), stringsAsFactors=FALSE)
  abstract <- as.data.frame(t(xmlSApply(xml["//*/Abstract"],xmlValue)), stringsAsFactors=FALSE)
  date_created <- as.data.frame(t(xmlSApply(xml["//*/DateCreated"],xmlValue)), stringsAsFactors=FALSE)
  author <- as.data.frame(t(xmlSApply(xml["//*/ForeName"],xmlValue)), stringsAsFactors=FALSE)
  author2 <- as.data.frame(t(xmlSApply(xml["//*/LastName"],xmlValue)), stringsAsFactors=FALSE)
  type <- as.data.frame(t(xmlSApply(xml["//*/PublicationType"],xmlValue)), stringsAsFactors=FALSE)
  #id <- as.data.frame(t(xmlSApply(xml['//*/ArticleId"'],xmlValue)), stringsAsFactors=FALSE)
  keywords <- as.data.frame(t(xmlSApply(xml["//*/DescriptorName"],xmlValue)), stringsAsFactors=TRUE)
 
   one <- cbind(id[i, ], title, abstract, date_created)
  names(one) <- c("ID", "Title", "Abstract", "Date Created")
  

  #titles[[i+1]] <- one  
 # two <- cbind(id[i, ], author)
  two2 <- NULL
  names <- as.data.frame(t(paste(author, author2, sep= " ")))
  if (ncol(names) == 0) {
    two2$Authors <- data.frame(matrix("NA"))
  }
  if (ncol(names) == 1) {
    two2$Authors <- apply(names , 1 , paste , collapse = " " )
  }
  if (ncol(names) > 1) {
    two2$Authors <- apply(names[ , 1:ncol(names)] , 1 , paste , collapse = ", " )
  }
  two <- as.data.frame(two2)
 # authors[[i+1]] <- two
  #three <- cbind(id[i, ], type)
  three2 <- NULL
  if (ncol(type) == 0) {
    two2$Types <- data.frame(matrix("NA"))
  }
  
  if (ncol(type) == 1) {
    three2$Types <-  apply(type, 1 , paste , collapse = " " )
  }
  if (ncol(type) > 1) {
  three2$Types <- apply(type[ , 1:ncol(type)] , 1 , paste , collapse = ", " )
    }
  three <- as.data.frame(three2)
 # types[[i+1]] <- three
  #four <- cbind(id[i, ], keywords)
  four2 <- NULL
  if (ncol(keywords) == 0) {
    two2$Keywords <- data.frame(matrix("NA"))
  }
  if (ncol(keywords) == 1) {
    four2$Keywords <- apply(keywords, 1 , paste , collapse = " " )
  }
  if (ncol(keywords) > 1) {
    four2$Keywords <- apply( keywords[ , 1:ncol(keywords)] , 1 , paste , collapse = ", " )
  }
  four <- as.data.frame(four2)
 
   merge <- cbind(one, two, three, four)
  data[[i+1]] <- merge  
  rm(xml, one, two, three, four, two2, three2, four2, title, abstract, date_created, author, type, keywords, mydata)
}


article <- rbind.pages(data)
write.csv(article, "~/Downloads/Article.csv")

