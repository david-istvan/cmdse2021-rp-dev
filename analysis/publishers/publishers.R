setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../data/analysis-years.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N/A")
data<-data[1:54,]
data <- data[as.numeric(data$Year) > 2015, ]
data <- data[as.numeric(data$Year) < 2021, ]

data<-data[,c(1,6)]

publishers = data.frame(unique(data[,2]))
publishers[,2]<-0

for(i in 1:nrow(publishers)){
  p <- publishers[i,1]
  count = 0
  for(j in 1:nrow(data)){
    if(p ==  data[j,2]){
      count <- count+1
    }
  }
  publishers[i,2] <- count
}

publishers[,3]<-0
colnames(publishers) <- c("Publisher", "Count", "%")

for(i in 1:nrow(publishers)){
  publishers[i, 3] <- publishers[i,2]/sum(publishers[,2])
}

publishers <- publishers[
  with(publishers, order(Count, decreasing=TRUE)),
]

write.table(publishers, "publishers.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)