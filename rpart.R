###################################################################################################
######################### Script to make plots ####################################################
###################################################################################################

#' iterate_ri_rpart from Julian Gebauer modified by Sandra K.
#' Function to draw reflim lines to iterate for each RI
#'
#' @param df Dataset
#' @param g ggplot
#' @param subset
#' @param ri_color Color for ggplot
#' @param xlog Logarithmic x-axis
iterate_ri_rpart <- function(df, g, subset, ri_color, xlog = FALSE){
  if (nrow(subset) != 0){
    for (i in 1:nrow(subset)){
      x <- subset[i,]
      reflim_results <- df %>% filter(leaf %in% x$leaf) %>% pull(VALUE) %>%
        reflim(plot.it = FALSE, n.min = 30)
      if (xlog) {
        g <- g + geom_rect(alpha = 0, colour = ri_color, xmin = log(x$min_age), xmax = log(x$max_age),
                           ymin = reflim_results$limits['lower.lim'],
                           ymax = reflim_results$limits['upper.lim'])
      } else{
        g <- g + geom_rect(alpha = 0, colour = ri_color, xmin = x$min_age, xmax = x$max_age,
                           ymin = reflim_results$limits['lower.lim'],
                           ymax = reflim_results$limits['upper.lim'])
      }
    }
  }
  return(g)
}


#' draw_ri_rpart from Julian Gebauer modified by Sandra K.
#'
#' @param df Dataset
#' @param g ggplot
#' @param sex
#' @param is_unisex
#' @param xlog Logarithmic x-axis
draw_ri_rpart <- function(df, g, sex, is_unisex = FALSE, xlog = FALSE){
  #define color of reference interval box
  ri_color <- case_when(sex == 'M' & is_unisex == FALSE ~ 'cornflowerblue',
                        sex == 'F' & is_unisex == FALSE ~ 'indianred',
                        sex %in% c('M','F') & is_unisex == TRUE ~ 'black'
  )
  
  leafs_unisex_group <- df %>%  group_by(leaf) %>%  summarise(n_sex = n_distinct(SEX)) %>%
    filter(n_sex == 2) %>% pull(leaf)
  
  if (is_unisex == TRUE){
    #get the groups which are unisex
    
    #create subset of unisex_groups
    subset <- df %>% filter(leaf %in% leafs_unisex_group) %>%group_by(leaf) %>% summarise( n = n(), min_age = min(AGE_DAYS), max_age = max(AGE_DAYS)) %>%
      arrange(min_age, max_age)
    #draw ri
    g <- iterate_ri_rpart(df, g, subset, ri_color, xlog)
  }
  else if (is_unisex == FALSE){
    subset <- df %>% group_by(leaf, SEX) %>% summarise( n = n(), min_age = min(AGE_DAYS), max_age = max(AGE_DAYS)) %>%
      arrange(min_age, max_age) %>% filter(!leaf %in% leafs_unisex_group & SEX == sex) %>% ungroup()
    #draw ri
    if (nrow(subset)>0){
      g <- iterate_ri_rpart(df, g, subset, ri_color, xlog)
    }
  }
  return(g)
}