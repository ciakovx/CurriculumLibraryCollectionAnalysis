fullLC <- function(df){
  a <- which(colnames(df) == "call...biblio.")
  b <- which(colnames(df) == "call...item.")
  df$LC <- ifelse(!is.na(df[ ,a])
                  , as.character(df[ ,a])
                  , as.character(df[ ,b])) # Merges the two different uri fields based on NA values
  df <- df[, c(-a, -b)]
  return(df)
}

fullLC2 <- function(df){  # swap biblio with item
  a <- which(colnames(df) == "call...item.")
  b <- which(colnames(df) == "call...biblio.")
  df$LC <- ifelse(!is.na(df[ ,a])
                  , as.character(df[ ,a])
                  , as.character(df[ ,b])) # Merges the two different uri fields based on NA values
  df <- df[, c(-a, -b)]
  return(df)
}

plot_LC <- function(df, lab){
  graph <- ggplot(data=df
                  , aes(cls, Freq, fill = Freq)) +
    geom_bar(stat="identity") +
    geom_bar(stat = "identity", colour = "black", show_guide = FALSE) + #  hack to hide the legend slashes
    ylab("Number of Items") +
    xlab(lab) +
    scale_fill_gradient2(low = "black"
                         , mid = "blue"
                         , high = "red"
                         , midpoint = length(unique(df$cls))/5
                         , space = "Lab") +
    geom_text(aes(cls, Freq, label=Freq), vjust = -0.2, size = 3) #set numeric labels
    #theme(axis.text.x = element_text(angle=-90
    #                                 , vjust = .3))
    #scale_x_continuous(breaks = c(min(col.cl$x008.date.one):max(col.cl$x008.date.one)+1)
    #                   , limits = c(min(col.cl$x008.date.one), max(col.cl$x008.date.one)+1))
  print(graph)
}
