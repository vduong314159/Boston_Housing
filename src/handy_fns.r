library(ggplot2); library(GGally); library(repr)


heatmap <- function(melted_data, xx, yy, heatspots) {    
    options(repr.plot.width = length(unique(melted_data[[xx]])), 
            repr.plot.height = length(unique(melted_data[[yy]])))
    
    plt <- ggplot(melted_data, aes_string(x = xx, y = yy, fill = heatspots)) +
                    geom_tile(color = 'white') +
                    geom_text(aes_string(label = paste('round(as.numeric(', heatspots, '), digits = 2)')),
                                         size = 7, color = 'white')                        
    return(plt)
}