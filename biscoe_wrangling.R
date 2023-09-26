library(dplyr)

biscoe_dat <- read.csv("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv")

head(biscoe_dat)

biscoe_dat <- biscoe_dat |>
  na.omit()

biscoe_dat_means <- biscoe_dat |>
  group_by(species, sex) |>
  summarize(across(ends_with("mm") | ends_with("g"), mean))

biscoe_dat_means_imperial <- biscoe_dat_means |>
  mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in"),
         across(ends_with("g"), ~ . * 0.002204623, .names = "{.col}_lb"))

biscoe_dat_means_imperial <- biscoe_dat_means_imperial |>
  rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
  rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))

biscoe_dat_means_imperial <- biscoe_dat_means_imperial |> 
  select(c(where(is.character) |
         ends_with("in")))

#### Functions


my_function <- function(data_url = "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv") {
  
  
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

function_output <- my_function()
torgersen_output <- my_function("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv")


