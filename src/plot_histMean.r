library(ggplot2); library(GGally); library(purrr) # purrr gives us map(x, y, fn)

plot_histMean <- function(df, feature, c = 'black') {    
    df <- Filter(is.numeric, df)
    meann <- round(mean(df[[feature]]), digits = 2)
    
    gg <- ggplot(data = df) + 
            # bins = 30 so we don't get a message suggesting to play w/ bins
            geom_histogram(aes_string(x = feature), bins = 30, fill = c, alpha = 0.3) 
            
    
    x_center <- max(df[[feature]]) + min(df[[feature]]); x_center <- x_center /2
    y_center <- max(ggplot_build(gg)[['data']][[1]][['count']]); y_center <- y_center/2
    
    gg <- gg + 
            geom_vline(xintercept = meann) +
            annotate(geom = 'text', label = meann, 
                     x = x_center, y = y_center, 
                     col = c, size = 4, 
                     fontface = 'bold')
    return(gg)
}

rep_list <- function(df, n) {
    list_ <- list()
    for (i in seq(1,n)) {list_[[i]] <- df}
    return(list_)
}

plot_histMean_df <- function(df) {
    df <- Filter(is.numeric, df)
    xx <- rep_list(df, length(colnames(df)))
    yy <- colnames(df)
    map2(xx, yy, plot_histMean)
}