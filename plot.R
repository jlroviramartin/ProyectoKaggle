plotDataXY <- function(dataset, varName) {
    tmp <- dataset %>%
        mutate(var = dataset[[ varName ]]) %>%
        select(var, loc_x, loc_y)
    
    p1 <- ggplot(tmp, aes(loc_x, loc_y, color = var)) +
        geom_point(size = 1, alpha = 0.8) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_blank(),
              axis.title.x    = element_blank(),
              plot.title      = element_blank())
    
    p2 <- ggplot(tmp, aes(x = fct_infreq(var))) +
        geom_bar(aes(fill = var)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_text(angle = 45, hjust = 1),
              axis.title.x    = element_blank(),
              plot.title      = element_text(hjust = 0.5))
    
    grid.arrange(p1, p2, layout_matrix = cbind(c(1, 2)))
}

plotDataAngleLen <- function(dataset, varName) {
    tmp <- dataset %>%
        mutate(var = dataset[[ varName ]]) %>%
        select(var, shot_angle, shot_distance)
    
    p1 <- ggplot(tmp, aes(shot_angle, shot_distance, color = var)) +
        geom_point(size = 1, alpha = 0.8) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_blank(),
              axis.title.x    = element_blank(),
              plot.title      = element_blank())
    
    p2 <- ggplot(tmp, aes(x = fct_infreq(var))) +
        geom_bar(aes(fill = var))
    
    grid.arrange(p1, p2, layout_matrix = cbind(c(1, 2)))
}

plotDataAngleLen_v2 <- function(dataset, varName) {
    tmp <- dataset %>%
        mutate(var = dataset[[ varName ]]) %>%
        select(var, shot_angle, shot_distance)
    
    p1 <- tmp %>%
        ggplot(aes(shot_angle, shot_distance, color = var)) +
        geom_point(size = 1, alpha = 0.8) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_blank(),
              axis.title.x    = element_blank(),
              plot.title      = element_blank())
    
    tmp2 <- dataset %>%
        mutate(var = dataset[[ varName ]],
               value = as.numeric(as.character(shot_made_flag))) %>%
        group_by(var) %>%
        summarise(avg = mean(value), n = n(), sd = sd(value), se = sd / sqrt(n))
    
    p2 <- tmp2 %>%
        ggplot(aes(x = reorder(var, -n) )) +
        geom_bar(aes(weight = avg, fill = avg), width = 0.2) +
        #geom_errorbar(aes(ymin = min, ymax  = max, width = 0.1)) +
        scale_fill_gradient(low = "red", high = "green", limits = c(0, 1)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_blank(),
              axis.title.x    = element_blank(),
              plot.title      = element_blank())
    
    p3 <- tmp2 %>%
        ggplot(aes(x = reorder(var, -n) )) +
        geom_bar(aes(weight = n, fill = avg), width = 0.2)  +
        scale_fill_gradient(low = "red", high = "green", limits = c(0, 1)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x     = element_text(angle = 45, hjust = 1),
              plot.title      = element_text(hjust = 0.5))
    
    grid.arrange(p1, p2, p3, ncol = 1)
}

MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
    n  = nrow(as.matrix(datos))

    vector = rep(FALSE, n)
    vector[indices_de_Outliers] = TRUE

    colores = rep("black", n)
    colores[ vector ] = "red"

    ggplot(datos, colors = colores)
}




