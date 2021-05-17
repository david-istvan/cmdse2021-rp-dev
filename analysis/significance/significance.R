setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../data/analysis-years.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N/A")
data<-data[1:54,]
data <- data[as.numeric(data$Year) < 2021, ]

data<-data[,c(1,5)]
data[,2] <- as.numeric(data[,2])

years = data.frame(unique(data[,2]))
years[,2]<-0

for(i in 1:nrow(years)){
  y <- years[i,1]
  count = 0
  for(j in 1:nrow(data)){
    if(y == data[j,2]){
      count <- count+1
    }
  }
  years[i,2] <- count
}

#########DATA FROM THE ORIGINAL STUDY#########
od <- list(c(2003, 3), c(2004, 7), c(2005, 5), c(2006, 7), c(2007, 6), c(2008, 12),
        c(2009, 11), c(2010, 9), c(2011, 6), c(2012, 2), c(2013, 16), c(2014, 10))

for(d in od){
  years<-rbind(years, d)
}

colnames(years) <- c("Year", "Count")

#########ADD THE OVERLAPPING YEAR c(2015, 9)#########
i <- years$Year == 2015
years$Count[i] <- 9+3


years <- years[
  with(years, order(Year, decreasing=FALSE)),
]
row.names(years) <- 1:nrow(years)

m1 <- mean(years[1:10,2])
print(m1)
m2 <- mean(years[11:18,2])
print(m2)
w <- wilcox.test(years[1:10,2], years[11:18,2], exact = FALSE)
print(w)

boxplot(years[1:10,2], years[11:18,2])

sink("significance.txt")
cat(c("m1=", as.character(m1), "\n"))
cat(c("m2=", as.character(m2), "\n"))
cat(c("p=", as.character(w$p.value), "\n"))
sink()