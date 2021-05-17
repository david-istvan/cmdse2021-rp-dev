library(ggplot2)
library(stringr)
library(plyr)
library(grid)
library(gridExtra)
library(Rmisc)
library(pals)

#remove non-analyzable columns
indicesToBeRemoved <- c(4, 5, 6, 7, 9, 10, 11, 14, 28, 59, 60, 61, 62, 63, 65, 69, 70, 71, 73, 74, 77)

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../data/latest/analysis.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "AAAAAAAA")
data <- data[1:29,1:76]

data <- data[,-indicesToBeRemoved]
startIndex <- 6
stopIndex <- ncol(data)

# TODO: remove this line after SerVCS has been reviewed
#data <- data[-5,]
#row.names(data) <- 1:nrow(data)

#ignoredIndexNames <- c("Modeling.framework", "UML.diagrams", "Language.s..custo..mization.support",
#                       "Roles", "Approach.specific.stakeholders")

ignoredIndexNames <- c()

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

gatherValues <- function(index){
  if(!(names(data)[i] %in% ignoredIndexNames) && !is.numeric(data[,index])){
    df <- data.frame(cat = data[,index])
    x<-data.frame(values=unique(str_trim(unlist(strsplit(df$cat, ",")))))
    
    for(i in 1:nrow(x)){
      v <- x[i,1]
      count = 0
      for(j in 1:nrow(df)){
        if(v %in% str_trim(unlist(strsplit(df[j,1], ",")))){
          count <- count+1
        }
      }
      x[i,2] <- count
    }
    
    #rename col 2
    names(x)[2]<-"count"
    
    #remove factors with count = 0
    x <- x[x$count > 0, ]
    
    #pretty print label names and values
    for(i in 1:nrow(x)){
      x[i,1] <- paste(x[i,1], paste("(", x[i,2], " - ", round(x[i,2]/29, 4)*100, '%', ")", sep=""))
    }
    
    #order by count DESC
    x <- x[
      with(x, order(count, decreasing=TRUE)),
    ]
    x$values <- factor(x$values, levels = x$values[order(x$count, decreasing = TRUE)])
    
    #factorize
    #x$values <- as.factor(x$values)
    
    
    return(x)
  }
}


longParameterNames = c("Collaboration.types")
manyParameters = c("Communication.means...builtin", "Communication.means.links","Workspace.awareness")

# plotting
plot <- function(index){
  x <- gatherValues(index)
  if(names(data)[index] %in% longParameterNames){
    guideFill = guide_legend(title="", ncol=2, reverse = FALSE)
  }else if(names(data)[index] %in% manyParameters){
    guideFill = guide_legend(title="", ncol=4, reverse = FALSE)
  }else{
    guideFill = guide_legend(title="", ncol=3, reverse = FALSE)
  }
  p <- ggplot(x, aes(x = '', y=count, fill=values))  +
    theme_void() +
    ggtitle(names(data)[index]) +
    geom_bar(stat="identity", position = position_fill(reverse = TRUE), width = 0.6) +
    theme(legend.position="bottom", legend.title = element_text(size=8)) +
    theme(axis.text.x = element_text(colour="black"),
          axis.title.x = element_text(colour="black"),
          plot.title = element_text(size=8),
          legend.key.size = unit(0.2, 'cm'),
          legend.text=element_text(size=8)) +
    guides(fill=guideFill) +
    ylab("") +
    scale_fill_manual(values=as.vector(glasbey(27))) +
    coord_flip()
}


outputFile <- "descriptive.pdf"
pdf(outputFile, paper="a4", width=10, height=15)
#l <- layout(matrix(c(1,2,3,4,5,6),ncol=1), widths=c(4,4,4), heights=c(1,1,1,1,1,1), TRUE)

plots <- c()
plotIndex = 1
for(i in startIndex:stopIndex){
  if(!(names(data)[i] %in% ignoredIndexNames) && !is.numeric(data[,i])){
    plotName <- paste(names(data)[i])
    print(paste("Printing plot for ", plotName, "."))
    plots[[plotIndex]]<-plot(i)
    plotIndex <- plotIndex+1
  }
  if(plotIndex %% 8 == 0){
    multiplot(plotlist = plots)
    plots <- c()
    plotIndex = 1
  }
}
if(!is.null(plots)){
  multiplot(plotlist = plots)
}

dev.off()
