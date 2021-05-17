library(ggplot2)

full=TRUE

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../data/analysis-years.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N/A")
data<-data[1:54,]

Var1ColumnName <- "Year"
Var2ColumnName <- "Publication.type"
fileName <- "stacked_year.pdf"

Var2ColumnOldValues <- c("workshop paper", "conference paper", "journal paper")
Var2ColumnNewValues <- c("W", "C", "J")

#######################################################################

countOccurrences <- function(el1, el2) {
  sum <- 0
  for(i in 1:length(Elements1)) {
    if((Elements1[i] == el1) && (Elements2[i] == el2)) {
      sum <- sum + 1
    }
  }
  return(sum)
}

Elements1 <- as.character(data[[Var1ColumnName]])
Elements1 <- Elements1[!is.na(Elements1)]
#Var1 <- unique(Elements1)
Var1 <- c(min(data[[Var1ColumnName]]):max(data[[Var1ColumnName]]))#c("2006","2007","2008","2009","2010", "2011","2012","2013","2014", "2015")


Elements2 <- as.character(data[[Var2ColumnName]])
Elements2 <- Elements2[!is.na(Elements2)]
length(Elements2) <- length(Elements1)

for(i in 1:length(Elements2)) {
  for(j in 1:length(Var2ColumnOldValues)) {
    if(Elements2[i] == Var2ColumnOldValues[j]) {
      Elements2[i] <- Var2ColumnNewValues[j]
    }
  }
}

Var2 <- unique(Elements2)


#count <- rep(1, 7*4)
counts <- vector("integer", length(Var1) * length(Var2))

grid <- expand.grid(Var1, Var2)
grid$count<-counts


grid <- data.frame(matrix(ncol = 3))
colnames(grid)<-c("Var1", "Var2", "count")
for(i in 1:(length(Var1))){
  for(j in 1:(length(Var2))){
    grid <- rbind(grid, c(Var1[i], Var2[j], 0))
  }
}
grid <- grid[-1,]

#grid <- data.frame(Var1, Var2, counts)
grid <- grid[ order(grid[,1], grid[,2]), ]

index <- 0
for(i in 0:(length(Var1) - 1)) {
  index <- 3 * i
  for(j in 1:length(Var2)) {
    if(!is.na(grid[index + j, 1])) {
      grid[index + j, 3] <- countOccurrences(grid[index + j, 1], grid[index + j, 2])
    }
  }
}

if(full){
  od <- list(c(2003, 3), c(2004, 7), c(2005, 5), c(2006, 7), c(2007, 6), c(2008, 12),
             c(2009, 11), c(2010, 9), c(2011, 6), c(2012, 2), c(2013, 16), c(2014, 10),
             c(2015, 9))
  for(d in od){
    grid <- rbind(grid, c(d[1],"O", d[2]))
  }
}

# we remove all the rows with 0 value as count
grid <- grid[grid$count > 0, ]
grid <- grid[grid$Var1 < 2021, ]



if(full){
  pubTypes = c("J", "C", "W", "O")
}else{
  pubTypes = c("J", "C", "W")
}
grid$Var2 <- factor(grid$Var2, levels = pubTypes)

if(full){
  Var1 <- 2003:2020
}
grid <- grid[
  with(grid, order(Var1,Var2)),
]

row.names(grid) <- 1:nrow(grid)
grid$count<-as.numeric(grid$count)

library(dplyr)
library(forcats)
grid <- grid %>%
  arrange(Var1, fct_rev(Var2))

g<-grid

grid <- grid %>%
  group_by(Var1) %>%
  mutate(label_y = cumsum(count) - 0.5*count)

totals <- grid %>%
  group_by(Var1) %>%
  summarize(total = sum(count))


grid$sum <- ave(grid$count, grid$Var1, FUN=sum)

if(full){
  Var2<-c(Var2, "O")
  colorPalette=c("#999999", "#fe53bb", "#ffd700", "#00a5e3")
  breaks_1=c('O', 'W', 'C', 'J')
  labels_1 = c("Original Study", "Workshop", "Conference", "Journal")
}else{
  colorPalette=c("#fe53bb", "#ffd700", "#00a5e3")
  breaks_1=c('W', 'C', 'J')
  labels_1 = c("Workshop", "Conference", "Journal")
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

paste("'",substrRight(grid$Var1[1], 2), sep="")

#for (i in 0:nrow(grid)){
#  grid[i,1] <- paste("'",substrRight(grid[i,1], 2), sep="")
#}


library(wesanderson)
# Stacked
p<- ggplot(grid, aes(fill=Var2, y=count, x=Var1)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + ylab("Number of publications") +
  theme(axis.text.x = element_text(size=13, angle=90, hjust=0.5, vjust=0.5, colour="black"),
        axis.text.y = element_text(size=13, colour="black"),
        #axis.text.y = element_text(colour="black"),
        axis.title.x = element_text(vjust=-3.5, size=9),
        axis.title.y = element_text(size=13),
        legend.title=element_blank(),
        #legend.position = "bottom",
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        #legend.margin = margin(14, 6, 6, 26),
        legend.box.margin=margin(c(0,0,0,12)),
        legend.text=element_text(size=13, colour="black"),
        legend.background = element_rect(fill = "#EEEEEE",
                                        colour = "black",
                                        size = 0.3, linetype = "dashed"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 1, linetype = "solid"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black")
        #axis.title.y = element_text(size=15)
  ) + 
  scale_y_continuous(breaks=seq(0,16,2)) +
  guides(fill=guide_legend(ncol=2, title="")) +
  #geom_text(aes(label = count)) +
  geom_text(aes(y = label_y, label = count), colour = "#222222") +
  geom_text(
    aes(label = stat(y), group = Var1),
    stat = 'summary', fun = sum, vjust = -0.5, fontface = "bold"
  ) +
  #scale_fill_discrete(name = "Var2", labels = c("Journal", "Conference", "Workshop")) +
  #scale_fill_grey(start=0.0, end=0.8) +
  scale_fill_manual(name = "Var2",
                    #values=c("#999999", wes_palette(n=3, name="FantasticFox1")),
                    values=colorPalette,
                    breaks=breaks_1,
                    labels = labels_1)

if(full){
  w=6.5
}else{
  w=4
}
pdf(fileName, width=w, height=4.75)

print(p)
ggsave(fileName, p)

dev.off()
