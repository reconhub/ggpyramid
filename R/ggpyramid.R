#' Age pyramid using ggplot2
#'
#' This is where you put your description. Bla bla bla. Lines should not be more
#' than 80 characters.
#'
#' @author Jonathan Polonsky \email{polonskyj@@who.int}.
#'
#' @export
#'
#' @import tidyverse
#'
#' @import dplyr
#'
#' @param data A `data.frame`.
#'
#' @param age ...
#'
#' @param sex ...
#'
#' @param males ...
#'
#' @param females ...
#'
#' @param x_breaks ...
#'
#' @param make_age_cat ...
#'
#' @examples
#' age <- rgeom(1000, .1)
#' sex <- sample(c("male", "female"), 1000, replace = TRUE)
#' df <- data.frame(age, sex)
#' ggpyramid(df, age = age, sex = sex, x_breaks = 5)

ggpyramid <- function(data, age, sex,
                      males = stringr::regex("1|^m", ignore_case = TRUE),
                      females = stringr::regex("2|^f", ignore_case = TRUE),
                      x_breaks, make_age_cat = TRUE) {
  
  age <- rlang::enquo(age)
  sex <- rlang::enquo(sex)
  
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
      dplyr::mutate(!!age := !!age %>% age_cat(by = 5, upper = 70))
    
  }
  
  y_axis_extent <-
    data %>% 
    dplyr::count(!!age, !!sex) %>% 
    dplyr::top_n(1, wt = n) %>% 
    dplyr::pull(n) %>% 
    plyr::round_any(x_breaks, f = ceiling) %>% 
    seq(-., ., x_breaks)
  
  data <- data %>% dplyr::mutate(!!sex := as.character(!!sex))
  
  data <- 
    data %>%
    dplyr::group_by(!!sex) %>% 
    dplyr::count(!!age) %>% 
    dplyr::mutate(
      Sex = 
        !!sex %>% 
          stringr::str_replace_all(males, "Male") %>% 
          stringr::str_replace_all(females, "Female") %>% 
          forcats::fct_relevel("Male", "Female")
    ) %>% 
    tidyr::drop_na()
  
  ggplot(data) +
    aes(x = !!age, fill = Sex) +
    geom_col(data = data %>% dplyr::filter(Sex %in% "Male"), aes(y = n), alpha = .8) + 
    geom_col(data = data %>% dplyr::filter(Sex %in% "Female"), aes(y = -n), alpha = .8) +
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
