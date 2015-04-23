library(stringr)
library(ggplot2)
library(RColorBrewer)
library(plyr)

checkouts_graph <- function(col.cl, top, fct, fct_name){
  # Remove col.clumns with no checkouts data
  col.cl <- col.cl[complete.cases(col.cl$tot.chkout), ] 
  
  # Remove if over 1000 checkouts because that's certainly a clerical error
  #q <- which(col.cl$tot.chkout > 1000)
  #if(length(q) != 0) col.cl <- col.cl[-q, ]
  
  # Sort title by number of file visits and eliminate titles for which there are no (NA) visits
  col.cl$title <- make.unique(col.cl$title)  # Make title names unique
  col.cl$title.ordered <- with(col.cl, reorder(col.cl$title, col.cl$tot.chkout))
  col.cl <- arrange(col.cl, desc(title.ordered))
  #col.cl <- col.cl[with(col.cl, order(title.ordered, decreasing = TRUE)), ]
  
  
  # If "top" is passed, reorder the df according to visits and take the top first through the value passed to top
  if(!missing(top)){
    col.cl <- col.cl[1:top, ]
  }
  
  col.cl$title.ordered <- as.character(str_extract_all(col.cl$title.ordered, "^[^\\/]*"))
  col.cl$title.ordered <- str_wrap(col.cl$title.ordered, width = 30)
  col.cl$title.ordered <- make.unique(col.cl$title.ordered)  # Make title names unique
  #col.cl$title.ordered <- factor(n
  #            #, labels = n
  #            , ordered = TRUE
  #            , levels = n)
  
  # Couldn't get that to work, so run it again.
  col.cl$title.ordered <- with(col.cl, reorder(col.cl$title.ordered, col.cl$tot.chkout))
  col.cl <- arrange(col.cl, desc(title.ordered))
    
  getPalette = colorRampPalette(brewer.pal(9, "Reds"))
  col.cl$fct <- col.cl[, fct]
  colorRamp <- length(unique(col.cl$fct))
  
  graph <- ggplot(data=col.cl) +
    aes(title.ordered, tot.chkout) +
    geom_bar(stat="identity",
             aes(fill=as.factor(fct))) +
    #theme(text = element_text(size=20)) +
    coord_flip() +
    scale_fill_manual(values = getPalette(colorRamp), na.value = "black",
                      name=fct_name) +    #scale_fill_brewer(palette = "Spectral") +
    geom_text(aes(title.ordered, tot.chkout, label=tot.chkout), hjust = -0.2) + #set numeric labels
    #ggtitle(label=paste("Total Checkouts Per Item")) +
    #scale_y_continuous(expand = c(0.1, 0)) +
    ylab("Checkouts") +
    xlab("Item") #+
    #theme_bw()
  print(graph)
}

checkouts_graph_md <- function(col.cl, top, fct, fct_name){
  # Remove col.clumns with no checkouts data
  col.cl <- col.cl[complete.cases(col.cl$tot.chkout), ] 
  
  # Remove if over 1000 checkouts because that's certainly a clerical error
  #q <- which(col.cl$tot.chkout > 1000)
  #if(length(q) != 0) col.cl <- col.cl[-q, ]
  
  # Sort title by number of file visits and eliminate titles for which there are no (NA) visits
  col.cl$title <- make.unique(col.cl$title)  # Make title names unique
  col.cl$title.ordered <- with(col.cl, reorder(col.cl$title, col.cl$tot.chkout))
  col.cl <- arrange(col.cl, desc(title.ordered))
  #col.cl <- col.cl[with(col.cl, order(title.ordered, decreasing = TRUE)), ]
  
  
  # If "top" is passed, reorder the df according to visits and take the top first through the value passed to top
  if(!missing(top)){
    col.cl <- col.cl[1:top, ]
  }
  
  col.cl$title.ordered <- as.character(str_extract_all(col.cl$title.ordered, "^[^\\/]*"))
  col.cl$title.ordered <- str_wrap(col.cl$title.ordered, width = 30)
  col.cl$title.ordered <- make.unique(col.cl$title.ordered)  # Make title names unique
  #col.cl$title.ordered <- factor(n
  #            #, labels = n
  #            , ordered = TRUE
  #            , levels = n)
  
  # Couldn't get that to work, so run it again.
  col.cl$title.ordered <- with(col.cl, reorder(col.cl$title.ordered, col.cl$tot.chkout))
  col.cl <- arrange(col.cl, desc(title.ordered))
  
  getPalette = colorRampPalette(brewer.pal(9, "Blues"))
  col.cl$fct <- col.cl[, fct]
  colorRamp <- length(unique(col.cl$fct))
  
  graph <- ggplot(data=col.cl
                  , aes(title.ordered, tot.chkout, fill = as.factor(fct))
                  , width = 0.5) +
    geom_bar(stat="identity") +
    geom_bar(stat = "identity", colour = "black", show_guide = FALSE) + #  hack to hide the legend slashes
    #theme(text = element_text(size=20)) +
    coord_flip() +
    scale_fill_manual(values = getPalette(colorRamp), na.value = "black",
                      name=fct_name) +    #scale_fill_brewer(palette = "Spectral") +
    geom_text(aes(title.ordered, tot.chkout, label=tot.chkout)
              , hjust = -0.2
              , size = 3) + #set numeric labels
    scale_y_continuous(limits = c(0, 45)) +
    theme(axis.text.y = element_text(size=6, colour = "black")
          , axis.text.x = element_text(size = 6, colour = "black")
          , legend.key = element_rect(fill = 'black')) +
    #ggtitle(label=paste("Total Checkouts Per Item")) +
    #scale_y_continuous(expand = c(0.1, 0)) +
    ylab("Checkouts") +
    xlab("Item") #+
  #theme_bw()
  print(graph)
}


dewey_checkouts <- function(col.cl){
  col.cl$cls <- paste0(col.cl$cls, "00")
  
  graph <- ggplot(data=col.cl
                  , aes(cls, total, fill = total)) +
    geom_bar(stat="identity") +
    geom_bar(stat = "identity", colour = "black", show_guide = FALSE) + #  hack to hide the legend slashes
    scale_fill_gradient2(#low = "black"
                         #, mid = "blue"
                         #, high = "red"
                         midpoint = length(unique(col.cl$cls))/5
                         , space = "Lab") +
    geom_text(aes(cls, total, label=total), vjust = -0.2, size = 3) + #set numeric labels
    #theme(text = element_text(size=20)) +
    #ggtitle(label=paste("Total Checkouts Per Item")) +
    #scale_y_continuous(expand = c(0.1, 0)) +
    ylab("Number of Checkouts") +
    xlab("Dewey Classification") 
  print(graph)
}
