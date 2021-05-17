library(ggplot2)

full=FALSE

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../data/analysis-years.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N/A")
data<-data[1:54,]

data <- data.frame(data$Venue, data$Venue.full, data$Publisher, data$Year)
names(data) <- c("venue", "venue-full", "publisher", "year")

data <- data.frame(
  lapply(
    data,
    function(variables){
      if (is.character(variables)){
        return(toupper(variables))
      } else{
        return(variables)
      }
    }),
  stringsAsFactors = FALSE)


aggr <- data.frame(matrix(ncol = 4))
colnames(aggr)<-c("venue", "venue-full", "count", "years")

for(index in 1:nrow(data)){
  v = data[index,]
  i = which(aggr$venue == v$venue)
  if(length(i) == 0){
    print(paste("Adding row for ", index))
    aggr <- rbind(aggr, c(v$venue, v$venue.full, as.numeric(1), as.character(v$year)))
  }else{
    print(paste("Updating with ", i))
    aggr[i,3] <- as.numeric(aggr[i,3])+1
    aggr[i,4] <- paste(aggr[i,4], as.character(v$year), sep = ", ")
  }
}

aggr <- aggr[-1,]
aggr$count <- as.numeric(aggr$count)

aggr <- aggr[order(aggr$count, decreasing = TRUE),]
aggr$ratio <- 0
for(i in 1:nrow(aggr)){
  a = aggr[i, ]
  aggr[i,]$ratio <- round(a$count/sum(aggr$count), 4)
}


sink("venues.txt")
for(i in 1:nrow(aggr)){
  a=aggr[i,]
  cat(c(paste(a$venue, a$`venue-full`, a$count, a$years, a$ratio, sep = "\t"), "\n"))
}
sink()