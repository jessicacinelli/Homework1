#BOXPLOT
library(ggplot2)


ggplot_box_legend <- function(family = "sans"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  # Lettura del file interarrivals.txt
  interarrivals <- read.table("C:/Users/jessi/Desktop/Università/Magistrale/II ANNO/Data Science/Homeworks/Homework1/ffdatools/tuples-bglsep_1-120/interarrivals.txt")
  mean_val <- mean(interarrivals$V1)
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", "50th percentile (median)", "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile (median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(interarrivals$V1)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", list(size = 3,  hjust = 0, family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", list(size = 3,  hjust = 0, family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +     
    stat_boxplot(data = interarrivals, aes(x = "", y=V1),geom ='errorbar', width = 0.3) + 
    geom_boxplot(data = interarrivals,aes(x = "", y=V1), width = 0.3, fill = "lightgrey") +
    geom_text(aes(x = 1, y = 40000, label = ""), hjust = 0.5) +    
    geom_text(aes(x = 1.17, y = 40000,label = ""), fontface = "bold", vjust = 0.4) +     
    theme_minimal(base_size = 5, base_family = family) + 
    geom_point(aes( x="", y=mean_val), col="red", size=2 ) +
    geom_text(aes(x = c(1.33), y = mean_val, label = "Mean"), hjust = 0.5) +
    geom_segment(aes(x = 2.3, xend = 2.3,                      
                     y = ggplot_output[["25th percentile"]], yend = ggplot_output[["75th percentile"]])) +     
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]], yend = ggplot_output[["25th percentile"]])) +     
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["75th percentile"]], yend = ggplot_output[["75th percentile"]])) + 
    geom_text(aes(x = 2.4, 
                  y = (ggplot_output[["50th percentile (median)"]]+0.2)),label = "Interquartile range", fontface = "bold",vjust = 0.4) +  
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]], ggplot_output[["lower_whisker"]]), 
                  label = c("Largest value within 1.5 times interquartile range above\n75th percentile","Smallest value within 1.5 times interquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +  
    geom_text(aes(x = c(1.17),y = 45000,label = "Outliers"),vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = 1.17, 
                  y = ggplot_output[["lower_dots"]], 
                  label = ""), 
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]], label = names(ggplot_output[["quartiles"]])),vjust = c(0.4,0.85,0.4), fill = "white", label.size = 0) +
    ylab("") +
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks.x  = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_line(color = "black"),
          axis.ticks.y= element_line(color="black"),
          axis.ticks.length.y = unit(.25, "cm"),
          axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"), size=10),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
   
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(0 , 80000)) +
  
    scale_y_continuous(breaks = seq(0, 80000, by=10000), limits=c(0,80000))
   
  
  return(explain_plot) 
  
}

ggplot_box_legend()

