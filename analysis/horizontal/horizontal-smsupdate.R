library(splitstackshape)
library(plyr)
library(ggplot2)
library(reshape2)

#remove non-analyzable columns
indicesToBeRemoved <- c(4, 5, 6, 7, 9, 10, 11, 14, 28, 59, 60, 61, 62, 63, 65, 69, 70, 71, 73, 74, 77)

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../data/analysis.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "AAAAAAAA")
data <- data[1:29,1:76]
data <- data[,-indicesToBeRemoved]
startIndex <- 6
stopIndex <- ncol(data)

# TODO: remove this line after SerVCS has been reviewed
#data <- data[-5,]
#row.names(data) <- 1:nrow(data)

ignoredIndexNames <- c("Modelig.framework", "UML.diagrams", "Language.s..custo..mization.support",
                       "Roles", "Approach.specific.stakeholders", "Collaborating.parties",
                       "UML.diagrams","Publication.type", "Year", "References", "Citations..Google.scholar.")

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

dataSource <- data

# separator used in categorical variables with multiple values
separator <- ', '

############# DO NOT CHANGE ANYTHING BELOW THIS LINE ############# 

df <- dataSource

index <- 1
generateCouples <- function() {
  indexes <- c(startIndex:stopIndex)
  var1 <- c()
  var2 <- c()
  for(i in indexes) {
      if(!(names(data)[i] %in% ignoredIndexNames) && (i <= length(names(df)))) {
        var1el <- names(df)[i]
        indexes2 <- c(i:stopIndex)
        for(j in indexes2) {
          if(!(names(data)[j] %in% ignoredIndexNames) && (j <= length(names(df)))) {
            var2el <- names(df)[j]
            if(var1el != var2el) {
              var1 <- append(var1, var1el)
              var2 <- append(var2, var2el)
            }
          }
        }
      }
  }
  result <- data.frame(var1, var2)
  return(result)
} 

result <- generateCouples()
resultLength <- nrow(result)

topScale <- nrow(dataSource)


# PREPARE DATA SOURCE FOR PLOTS
dt2 <- cSplit(df, splitCols=names(df), sep=separator, direction="long", drop=TRUE)
dt2 <- dt2[!duplicated(dt2), ]

ind <- apply(dt2, 1, function(x) all(is.na(x)))
dt2 <- dt2[ !ind, ]

index = 1
for(i in 1:nrow(dt2)) {
  row <- dt2[i,]
  if(is.na(row$ClusterID)) {
    dt2[[i,1]] <- dt2[[i-1,1]]
  } else {
    index <- i
  }
}

createPlot <- function(plotName, var1, var2) {
  tbl <- table(as.factor(dt2[[as.character(var1)]]), as.factor(dt2[[as.character(var2)]]))
  resultDf <- as.data.frame.matrix(tbl)
  
  for(x in 1:nrow(tbl)) {
    xName <- rownames(tbl)[x]
    for(y in 1:ncol(tbl)) {
      yName <- colnames(tbl)[y]
      tbl[x,y] <- 0
      for(i in 1:nrow(data)) {
        row <- data[i,]
        if(grepl(xName, row[[as.character(var1)]]) && grepl(yName, row[[as.character(var2)]])) {
          #grepl("VAL", "VAL_EXP")
          #if(yName=="VAL"){
            #print("x")
            #print(xName)
            #print("y")
            #print(yName)
            #print(row[[as.character(var1)]])
            #print(row[[as.character(var2)]])
          #}
          tbl[x,y] <- tbl[x,y] + 1
        }
      }
    }
  }
  
  plot <- ggplot(melt(as.factor(tbl)), aes(as.factor(Var2), as.factor(Var1))) +
    geom_tile(data=melt(tbl), aes(fill=as.integer(value)), color="grey") +
    geom_text(data=melt(tbl), aes(label=value), size=4, color="black") +
    theme_bw() + 
    scale_fill_gradient2(low="white", high="red", mid="orange",name="Frequency", limit=c(0,max(tbl)), midpoint = max(tbl)/2) +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1)) +
    coord_equal() + labs(x=var2, y=var1) +
    ggtitle(plotName)

  print(plot)
}



# PRINT INTO FILE
outputFile <- "horizontal.pdf"
pdf(outputFile, width=10, height=10)
par(mar=c(2, 2, 2, 2))
par(mfrow=c(1, 1))
par(las=1)

for(i in 1:resultLength) {
  plotName <- paste(result[i,]$var1, "_____", result[i,]$var2, sep="")
  print(paste(i, "/", resultLength, " - ", plotName))
  createPlot(plotName, result[i,]$var1, result[i,]$var2)
}

dev.off()