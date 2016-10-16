#load Libraries
library(xml2)
library(XML)
library(httr)
library(jsonlite)

#read the Publication file
#from http://www.disgenet.org/ds/DisGeNET/results/befree_results_only_version_4.0.tar.gz
ids <- read.csv("~/Downloads/Publication.csv")
id <- as.data.frame(unique(ids$PMID))
#Get the unique article ID's
pages <- list(id)
#Create an empty dataset to store data
data <- list()
articles <- nrow(id)
#Start Scrapping information from the articles
for(i in 1:nrow(id)){
#Get the XML
  mydata <- content(POST(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=abstract&id=", id[as.numeric(i), ])))
  #Print article
  message("Retrieving article ", i, " out of ", articles)
  #Deal with the XML
  xml <-  xmlTreeParse(mydata)
  xml <- xmlParse(mydata)
  title <- as.data.frame(t(xmlSApply(xml["//*/ArticleTitle"],xmlValue)), stringsAsFactors=FALSE)
  abstract <- as.data.frame(t(xmlSApply(xml["//*/Abstract"],xmlValue)), stringsAsFactors=FALSE)
  date_created <- as.data.frame(t(xmlSApply(xml["//*/DateCreated"],xmlValue)), stringsAsFactors=FALSE)
  author <- as.data.frame(t(xmlSApply(xml["//*/ForeName"],xmlValue)), stringsAsFactors=FALSE)
  author2 <- as.data.frame(t(xmlSApply(xml["//*/LastName"],xmlValue)), stringsAsFactors=FALSE)
  type <- as.data.frame(t(xmlSApply(xml["//*/PublicationType"],xmlValue)), stringsAsFactors=FALSE)
  keywords <- as.data.frame(t(xmlSApply(xml["//*/DescriptorName"],xmlValue)), stringsAsFactors=TRUE)

  one <- cbind(id[i, ], title, abstract, date_created)
  names(one) <- c("ID", "Title", "Abstract", "Date Created")

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
  #Final merge of the fields
  merge <- cbind(one, two, three, four)
  data[[i+1]] <- merge
  #Remove rubbish
  rm(xml, one, two, three, four, two2, three2, four2, title, abstract, date_created, author, type, keywords, mydata)
}

#Bind pages
article <- rbind.pages(data)
#Write the final file
write.csv(article, "~/Downloads/Article.csv")
