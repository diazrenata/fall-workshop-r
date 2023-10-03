library(dplyr)


#### Functions ####


my_function <- function(island_to_use) {
  
  
  if(island_to_use == "Biscoe") {
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv"
  } else if (island_to_use == "Dream"){
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Dream.csv"
  } else if (island_to_use == "Torgersen") {
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv"
  } else {
    stop("The island_to_use doesn't exist!")
  }
  
  island_dat <- read.csv(data_url)


  island_dat <- island_dat |>
    na.omit()


  island_dat_means <- island_dat |>
    group_by(species, sex) |>
    summarize(across(ends_with("mm") | ends_with("g"), mean))

  island_dat_means_imperial <- island_dat_means |>
    mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in"),
           across(ends_with("g"), ~ . * 0.002204623, .names = "{.col}_lb"))

  island_dat_means_imperial <- island_dat_means_imperial |>
    rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
    rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))

  island_dat_means_imperial <- island_dat_means_imperial |>
    select(c(where(is.character) |
               ends_with("in")))


  return(island_dat_means_imperial)

}

#### Analysis #### 

biscoe_output <- my_function()
biscoe_output <- my_function(island_to_use = "Biscoe")
torgersen_output <- my_function(island_to_use = "Torgersen")
dream_output <- my_function(island_to_use = "Dream")


#### For loop ####

# Run my_function() on each island (Biscoe, Torgersen, Dream)

islands_we_want <- unique(my_data$island)
cleaned_data <- list()

for(i in 1:3) {

  cleaned_data[[i]] <- my_function(islands_we_want[i])
  
}

names(cleaned_data) <- islands_we_want

cleaned_data_df <- bind_rows(cleaned_data, .id = "island")
