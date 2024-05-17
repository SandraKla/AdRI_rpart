###################################################################################################
######################### Script to make plots ####################################################
###################################################################################################

#' Make decision trees function by Sandra K.
#'
#' @param df Dataset
#' @param tree_cp Complexity hyperparameter for rpart
#' @param tree_minsplit minsplit hyperparameter for rpart
make_rpart <- function(df, tree_cp, tree_minsplit) {
    
    if (tree_minsplit == 0 || tree_minsplit == "" || is.na(tree_minsplit)) {
        tree_minsplit <- 20
    } else{
        tree_minsplit <- as.numeric(tree_minsplit)
    }
    
    if (tree_cp == 0 || tree_cp == "" || is.na(tree_cp)) {
        tree_cp <- 0.01
    } else{
        tree_cp <- as.numeric(tree_cp)
    }
    
    sex_age_rpart <-
        rpart(
            VALUE ~ AGE_DAYS + SEX,
            data = df,
            control = rpart.control(cp = tree_cp, minsplit = tree_minsplit)
        )
    return(sex_age_rpart)
}

#' Function to draw reflim lines to iterate for each RI
#' iterate_ri_rpart from Julian Gebauer modified by Sandra K.
#' 
#' @param df Dataset
#' @param g ggplot
#' @param subset Subset of the dataset
#' @param ri_color Color for ggplot
#' @param xlog Logarithmic x-axis
#' @param reflim_n.min n.min parameter for reflimR
iterate_ri_rpart <- function(df, g, subset, ri_color, xlog = FALSE, reflim_n.min = 40) {
        
    if (reflim_n.min <= 39 || is.na(reflim_n.min) || reflim_n.min == "") {
        reflim_n.min = 40
    }
    
        if (nrow(subset) != 0) {
            for (i in 1:nrow(subset)) {
                x <- subset[i,]
                
                tryCatch({
                    reflim_results <-
                        df %>% filter(leaf %in% x$leaf) %>% pull(VALUE) %>%
                        reflim(n.min = reflim_n.min, plot.it = FALSE)
                    
                    reflim_results_sex <- df %>% filter(leaf %in% x$leaf)
                    reflim_results_sex <- reflim_results_sex$SEX %>% unique()
                    
                    if ("F" %in% reflim_results_sex &&
                        "M" %in% reflim_results_sex) {
                        reflim_results_sex <- "MF"
                    }
                    
                    if (xlog) {
                        g <-
                            g + geom_rect(
                                alpha = 0,
                                colour = ri_color,
                                xmin = log(x$min_age),
                                xmax = log(x$max_age),
                                ymin = reflim_results$limits['lower.lim'],
                                ymax = reflim_results$limits['upper.lim']
                            )
                    } else{
                        g <-
                            g + geom_rect(
                                alpha = 0,
                                colour = ri_color,
                                xmin = x$min_age,
                                xmax = x$max_age,
                                ymin = reflim_results$limits['lower.lim'],
                                ymax = reflim_results$limits['upper.lim']
                            )
                    }
                    
                    ri_results <-
                        rbind(
                            ri_results,
                            c(
                                x$min_age,
                                x$max_age,
                                reflim_results_sex,
                                reflim_results$limits['lower.lim'],
                                reflim_results$limits['upper.lim']
                            )
                        )
                },
                # use quantile if reflim is not working
                
                error = function(e) {
                    quantile_results <- df %>% filter(leaf %in% x$leaf) %>% pull(VALUE)
                    
                    quantile_results_sex <- df %>% filter(leaf %in% x$leaf)
                    quantile_results_sex <- quantile_results_sex$SEX %>% unique()
                    
                    if ("F" %in% quantile_results_sex &&
                        "M" %in% quantile_results_sex) {
                        quantile_results_sex <- "MF"
                    }
                    
                    if (xlog) {
                        g <-
                            g + geom_rect(
                                alpha = 0,
                                colour = ri_color,
                                xmin = log(x$min_age),
                                xmax = log(x$max_age),
                                ymin = as.numeric(quantile(quantile_results, 0.025)),
                                ymax = as.numeric(quantile(quantile_results, 0.975))
                            )
                    } else{
                        g <-
                            g + geom_rect(
                                alpha = 0,
                                colour = ri_color,
                                xmin = x$min_age,
                                xmax = x$max_age,
                                ymin = as.numeric(quantile(quantile_results, 0.025)),
                                ymax = as.numeric(quantile(quantile_results, 0.975))
                            )
                    }
                    
                    ri_results <-
                        rbind(
                            ri_results,
                            c(
                                x$min_age,
                                x$max_age,
                                reflim_results_sex,
                                as.numeric(quantile(quantile_results, 0.025)),
                                as.numeric(quantile(quantile_results, 0.975))
                            )
                        )
                })
            }
        }
        ri_results <<- ri_results
        return(g)
    }

#' draw_ri_rpart from Julian Gebauer modified by Sandra K.
#'
#' @param df Dataset
#' @param g ggplot
#' @param sex If dataset is for female or male
#' @param is_unisex If dataset is for female and male
#' @param xlog Logarithmic x-axis
#' @param reflim_n.min n.min parameter for reflimR
draw_ri_rpart <- function(df, g, sex, is_unisex = FALSE, xlog = FALSE, reflim_n.min) {
    
        #define color of reference interval box
        ri_color <-
            case_when(
                sex == 'M' & is_unisex == FALSE ~ 'cornflowerblue',
                sex == 'F' &
                    is_unisex == FALSE ~ 'indianred',
                sex %in% c('M', 'F') &
                    is_unisex == TRUE ~ 'black'
            )
        
        leafs_unisex_group <-
            df %>%  group_by(leaf) %>%  summarise(n_sex = n_distinct(SEX)) %>% filter(n_sex == 2) %>% pull(leaf)
        
        if (is_unisex == TRUE) {
            #get the groups which are unisex
            
            #create subset of unisex_groups
            subset <-
                df %>% filter(leaf %in% leafs_unisex_group) %>% group_by(leaf) %>% summarise(
                    n = n(),
                    min_age = min(AGE_DAYS),
                    max_age = max(AGE_DAYS)
                ) %>%
                arrange(min_age, max_age)
            #draw ri
            g <- iterate_ri_rpart(df, g, subset, ri_color, xlog, reflim_n.min)
        }
        
        else if (is_unisex == FALSE) {
            subset <-
                df %>% group_by(leaf, SEX) %>% summarise(
                    n = n(),
                    min_age = min(AGE_DAYS),
                    max_age = max(AGE_DAYS),
                    .groups = 'drop'
                ) %>%
                arrange(min_age, max_age) %>% filter(!leaf %in% leafs_unisex_group & SEX == sex) %>% ungroup()
            #draw ri
            if (nrow(subset) > 0) {
                g <- iterate_ri_rpart(df, g, subset, ri_color, xlog, reflim_n.min)
            }
        }
        return(g)
    }