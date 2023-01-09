






sector_return_plot_fun <- function(valuation_date = NULL, 
                                   location = 1,
                                   index_mapping, 
                                   idx_history){
  
  valuation_date <<- valuation_date
  index_mapping <<- index_mapping
  idx_history <<- idx_history
  
  # Error handling for date == NULL
  if(is.null(date)){
    valuation_date <- aikia::val_date()
  }
  
  
  location_idx <- index_mapping %>% 
    dplyr::filter(country %in% ifelse(location==1, "US", c("EU","DE")))
  
  
  idx_returns <- idx_history %>% 
    dplyr::filter(ticker_yh %in% (location_idx %>% 
                                    dplyr::pull(ticker_yh)),
                  date == valuation_date) %>% 
    dplyr::select(date,ticker_yh, dtd_return, ytd_return, wtd_return, qtd_return) %>% 
    dplyr::left_join(index_mapping[,c("ticker_yh","supersector",'name')] , by= 'ticker_yh')
    
  
  day_plot <- ggplot2::ggplot(idx_returns)+
    ggplot2::geom_col(ggplot2::aes(name, dtd_return, fill = dtd_return))+
    ggplot2::facet_grid(supersector ~ 1, scale = "free_y", switch = "y", space = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_gradient2(high = "darkgreen",
                                  mid = scales::alpha('#484B42',0.5),
                                  low = "#E6272F",
                                  guide = 'none')+
    ggplot2::coord_flip() +
    ggplot2::labs(x = 'Index Sectors', y = 'Day-to-Day Return') +
    aikia::theme_aikia() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, colour = "black", face = "bold"),
                   strip.text.x = ggplot2::element_text(colour = "white"),
                   strip.background.x = ggplot2::element_rect(fill = "white"),
                   strip.background.y = ggplot2::element_rect(fill = "white"))
  
  
  week_plot <- ggplot2::ggplot(idx_returns)+
    ggplot2::geom_col(ggplot2::aes(name, wtd_return, fill = dtd_return))+
    ggplot2::facet_grid(supersector ~ 1, scale = "free_y", switch = "y", space = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_gradient2(high = "darkgreen",
                                  mid = scales::alpha('#484B42',0.5),
                                  low = "#E6272F",
                                  guide = 'none')+
    ggplot2::coord_flip() +
    ggplot2::labs(x = 'Index Sectors', y = 'Week-to-Day Return') +
    aikia::theme_aikia() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, colour = "black", face = "bold"),
                   strip.text.x = ggplot2::element_text(colour = "white"),
                   strip.background.x = ggplot2::element_rect(fill = "white"),
                   strip.background.y = ggplot2::element_rect(fill = "white"))
  
  quarter_plot <- ggplot2::ggplot(idx_returns)+
    ggplot2::geom_col(ggplot2::aes(name, qtd_return, fill = dtd_return))+
    ggplot2::facet_grid(supersector ~ 1, scale = "free_y", switch = "y", space = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_gradient2(high = "darkgreen",
                                  mid = scales::alpha('#484B42',0.5),
                                  low = "#E6272F",
                                  guide = 'none')+
    ggplot2::coord_flip() +
    ggplot2::labs(x = 'Index Sectors', y = 'Quarter-to-Day Return') +
    aikia::theme_aikia() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, colour = "black", face = "bold"),
                   strip.text.x = ggplot2::element_text(colour = "white"),
                   strip.background.x = ggplot2::element_rect(fill = "white"),
                   strip.background.y = ggplot2::element_rect(fill = "white"))
  
  
  year_plot <- ggplot2::ggplot(idx_returns)+
    ggplot2::geom_col(ggplot2::aes(name, ytd_return, fill = dtd_return))+
    ggplot2::facet_grid(supersector ~ 1, scale = "free_y", switch = "y", space = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_gradient2(high = "darkgreen",
                                  mid = alpha('#484B42',0.5),
                                  low = "#E6272F",
                                  guide = 'none')+
    ggplot2::coord_flip() +
    ggplot2::labs(x = 'Index Sectors', y = 'Year-to-Day Return') +
    aikia::theme_aikia() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, colour = "black", face = "bold"),
                   strip.text.x = ggplot2::element_text(colour = "white"),
                   strip.background.x = ggplot2::element_rect(fill = "white"),
                   strip.background.y = ggplot2::element_rect(fill = "white"))
  
   
  index_return_plots <- list(day_plot = day_plot,
                             week_plot = week_plot,
                             quarter_plot = quarter_plot,
                             year_plot = year_plot)
}

















