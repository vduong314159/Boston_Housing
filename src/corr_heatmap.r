library(ggplot2); library(GGally); library(repr); library(reshape2)

get_lower_tri <- function(sq_matrix) {
    sq_matrix[upper.tri(sq_matrix)] <- NA
    return(sq_matrix)
}

set_width_height <- function(length) {
    options(repr.plot.width = length,
            repr.plot.height = length)
}

legend_in_upper_tri <- function(hmap) {
    hmap <- hmap + theme(legend.justification = c(1,0),
                       legend.position=c(0.3, 0.7),
                       panel.background = element_blank())
    return(hmap)
}

order_cormat <- function(cormat) {
    # make it so that tiles closer in value are closer to eachother
    # blues in heatmap cluster together
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat_ordered <- cormat[hc$order, hc$order]
    return(cormat_ordered)
}

corr_heatmap <- function(data) {
    data <- Filter(is.numeric, data)
    corr_mat <- round(cor(data), digits = 2)
    corr_mat_ordered <- order_cormat(corr_mat)
    lower_tri <- get_lower_tri(corr_mat_ordered)
    melted_data <- melt(lower_tri, na.rm = TRUE)
    
    l <- length(colnames(data))
    set_width_height(l)
    
    plt <- ggplot(melted_data, aes(x = Var1, y =Var2 , fill =value)) +
                    geom_tile(color = 'white') +
                    geom_text(aes(label = round(value, digits = 2)),
                                  size = 7, color = 'black') +
                    theme(text = element_text(size = l*2), axis.text.x = element_text(angle = 45),
                          axis.title = element_blank()) +
                    scale_fill_gradient2(low = 'blue', high = 'darkred', mid = 'white',
                                         midpoint = 0, name = 'Pearson\nCorrelation')
    
    plt <- legend_in_upper_tri(plt)
    
    return(plt)
}