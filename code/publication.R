library(scales)
library(ggplot2)

pub_distr <- function(col.cl){
  col.cl <- col.cl[complete.cases(col.cl$x008.date.one), ]
  if(any(which(col.cl$x008.date.one < 1800))) {col.cl <- col.cl[-which(col.cl$x008.date.one < 1800), ]}
  if(any(which(col.cl$x008.date.one > 2014))) {col.cl <- col.cl[-which(col.cl$x008.date.one > 2014), ]}
  brk <- as.character(sort(unique(col.cl$x008.date.one)))
  mn <- min(col.cl$x008.date.one)
  mx <- max(col.cl$x008.date.one)
  graph <- ggplot(data=col.cl) +
    aes(x008.date.one) +
    geom_histogram(stat="bin", binwidth = 1, aes(fill = ..count..)) +
    ylab("Number of Items") +
    xlab("Year of Publication") +
    scale_fill_gradient2(low = "black"
                         , mid = "blue"
                         , high = "red"
                         , midpoint = length(unique(col.cl$x008.date.one))/5
                         #, guide = "legend"
                         , space = "Lab") +
    geom_text(stat = "bin"
              , binwidth = 1
              , aes(x008.date.one, label=..count..)
              , vjust = -0.4
              , size = 2
              , na.rm = TRUE) + #set numeric labels
    theme(axis.text.x = element_text(angle=-90
                                     , vjust = -1.3
                                     , color = "black"
                                     , size = 6)
          , axis.text.y = element_text(size=6
                                     , colour = "black")) +
    scale_x_continuous(breaks = mn:mx
                      , limits = c(mn, mx+1))
  print(graph)
}

