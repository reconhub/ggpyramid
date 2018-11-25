# Age pyramid function
ggpyramid <- 
  function(data, age, sex, males = regex("1|^m", ignore_case = TRUE), females = regex("2|^f", ignore_case = TRUE), x_breaks, make_age_cat = TRUE) {
    
    age <- enquo(age)
    sex <- enquo(sex)
    
    age_cat <- function(x, lower = 0, upper, by = 10, sep = "-", above.char = "+") {
      
      labs <- 
        c(paste(seq(lower, upper - by, by = by),
                seq(lower + by - 1, upper - 1, by = by),
                sep = sep),
          paste(upper, above.char, sep = ""))
      
      cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
          right = FALSE, labels = labs)
      
    }
    
    if(make_age_cat %in% TRUE) {
      
      data <- 
        data %>% 
        mutate(!!age := !!age %>% age_cat(by = 5, upper = 70))
      
    }
    
    y_axis_extent <-
      data %>% 
      count(!!age, !!sex) %>% 
      top_n(1, wt = n) %>% 
      pull(n) %>% 
      plyr::round_any(x_breaks, f = ceiling) %>% 
      seq(-., ., x_breaks)
    
    data <- data %>% mutate(!!sex := as.character(!!sex))
    
    data <- 
      data %>%
      group_by(!!sex) %>% 
      count(!!age) %>% 
      mutate(
        Sex = 
          !!sex %>% 
          str_replace_all(males, "Male") %>% 
          str_replace_all(females, "Female") %>% 
          fct_relevel("Male", "Female")
      ) %>% 
      drop_na()
    
    ggplot(data) +
      aes(x = !!age, fill = Sex) +
      geom_col(data = data %>% filter(Sex %in% "Male"), aes(y = n), alpha = .8) + 
      geom_col(data = data %>% filter(Sex %in% "Female"), aes(y = -n), alpha = .8) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(
        breaks = y_axis_extent, 
        limits = c(min(y_axis_extent), max(y_axis_extent)), 
        labels = abs(y_axis_extent)
      ) +
      coord_flip() +
      theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
      labs(
        x = "Age group",
        y = "Number of people"
      ) 
    
  }