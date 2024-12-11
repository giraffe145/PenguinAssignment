#function to plot scatter graphs
plot_scatter <- function(data, 
                         x_column, 
                         y_column, 
                         x_label, 
                         y_label, 
                         colour_mapping,
                         caption,
                         point_size = 2,
                         point_alpha = 0.7) {
  data <- data %>%
    drop_na({{ y_column }}) #removing na values
  # Now make the scatter plot
  ggplot(data = data, 
         aes(
           x = {{ x_column }}, #Use {{ }} for x and y columns
           y = {{ y_column }},
           colour = species,#making the colour separated by species
           shape = species)) +  
    geom_point(
      size = point_size,
      alpha = point_alpha) +
    scale_color_manual(
      values = colour_mapping) +  #Different colours for different species
    scale_shape_manual(
      values = c(15, 16, 17)  #Different shape points for species
    ) + 
    labs(
      x = x_label, 
      y = y_label,
      caption = caption) #adding labels and 
}

#functions to save images
#first as a png
save_plot_png <- function(scatterplot, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  print(scatterplot)
  dev.off()
}

#also as an svg
save_plot_svg <- function(scatterplot, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  print(scatterplot)
  dev.off()
}

#function to plot ancova results
plot_ancova_results <- function(data, 
                                x_column, 
                                y_column, 
                                x_label, 
                                y_label, 
                                colour_mapping,
                                caption,
                                point_size = 2,
                                point_alpha = 0.7) {
  # Now make the scatter plot
  ggplot(data = data, 
         aes(
           x = {{ x_column }}, #Use {{ }} for x and y columns
           y = {{ y_column }}, 
           colour = species,
           shape = species)) +  
    geom_point(
      size = point_size,
      alpha = point_alpha) +
    scale_color_manual(
      values = colour_mapping) +  #Different colours for different species
    scale_fill_manual(
      values = CI_colours) + 
    scale_shape_manual(
      values = c(15, 16, 17)  #different shape points for species
    ) + 
    geom_smooth(method = "lm", aes(fill = species), se = TRUE) +
    labs(
      x = x_label, 
      y = y_label,
      caption = caption) + #adding labels and caption
    theme_bw()
}