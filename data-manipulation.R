library("dplyr")
library("tidyr")
library("palmerpenguins")

install.packages("dplyr") # dataframe manipulation
install.packages("tidyr") # "reshaping" data
install.packages("palmerpenguins")

penguins_data <- penguins

class(penguins_data)

head(penguins_data)

str(penguins_data)

unique(penguins_data$species)

years_of_sampling <- paste("Year: ", penguins_data$year)

mean(years_of_sampling)

str(penguins_data)

island_year <- select(penguins_data, island, year)

str(island_year)

torgersen_penguins <- filter(penguins_data, island == "Torgersen")

torgersen_penguins_only_sex_and_species <- select(torgersen_penguins, sex, species)

str(torgersen_penguins_only_sex_and_species)

torgerson_penguins_one_chunk <- filter(penguins_data, island == "Torgersen") |>
  select(sex, species)

torgerson_penguins_one_chunk

torgersen_penguins <- torgersen_penguins |>
  mutate(rounded_bill_length = round(bill_length_mm)) |>
  select(species, sex, rounded_bill_length)

torgersen_penguins

torgersen_penguins_summary <- torgersen_penguins |>
  group_by(species, sex) |>
  summarize(mean_bill_length = mean(rounded_bill_length, na.rm = TRUE))

torgersen_penguins_summary

penguin_counts <- penguins_data |>
  group_by(species, sex, island) %>%
  summarize(n = dplyr::n())

penguins_wide <- penguin_counts |>
  pivot_wider(names_from = island, values_from = n, values_fill = 0)

penguins_long_again <- penguins_wide |>
  pivot_longer(-c(species, sex), values_to = "count")
