# This script processes data from the California Department of Finance
# To run it, download the following files and store them in the data folder
# P1A_State_Total.xlsx
# P1B_State_Age.xlsx
# Available at: https://dof.ca.gov/Forecasting/Demographics/projections/

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(janitor)

# Path to the app folder
folder_path <- "./"
# Path to the CA DoF data in the app folder
data_path <- paste0(folder_path, "data/")

# Print sheets in excel files
# Total Population
excel_sheets(paste0(data_path, "/P1A_State_Total.xlsx"))
# State Population by Age
excel_sheets(paste0(data_path, "/P1B_State_Age.xlsx"))


total_pop <- read_excel(
    paste0(data_path, "/P1A_State_Total.xlsx"),
    sheet = "Total Population",
    # Only reads cells with needed data
    range = "B3:AP4",
    # Turns years into valid column names through the janitor package
    .name_repair = make_clean_names
    ) |>
    pivot_longer(
        cols = everything(),
        names_to = "year",
        values_to = "population"
    ) |>
    # Now that year is a column, we can strip the "x" applied to year values by the janitor package
    mutate(
        year = str_remove(year, "x"),
        year = as.numeric(year),
        fig_type = if_else(
            year %in% c(2020:2022),
            "estimate",
            "projection"
        )
    ) %>%
    # There is a gap in the trend line between estimate and projection,
    # so I duplicate the 2022 row with fig_type set to "projection"
    add_row(
        year = 2022, 
        fig_type = "projection",
        population = .$population[.$year == 2022 & .$fig_type == "estimate"]
        )

# The CA DoF provides a separate file for population by age
pop_by_age <- read_excel(
    paste0(data_path, "/P1B_State_Age.xlsx"),
    sheet = "State Population by Age",
    range = "A3:AP105",
    .name_repair = make_clean_names
    ) |>
    # The first row contains the population totals for each year,
    # which we already have in total_pop
    slice(-1) |>
    rename(age = population) |>
    pivot_longer(
        cols = 2:last_col(),
        names_to = "year",
        values_to = "population"
    ) |>
    mutate(
        year = str_remove(year, "x"),
        year = as.numeric(year),
        fig_type = if_else(
            year %in% c(2020:2022),
            "estimate",
            "projection"
        )
    )

# I repeat the step duplicating 2022 rows for each group,
# setting fig_type to "projection" to avoid gaps in trend lines
unique_ages <- unique(pop_by_age$age)

for (age in unique_ages) {
  pop_by_age <- pop_by_age |>
    add_row(
      year = 2022, 
      fig_type = "projection",
      age = age,
      population = pop_by_age$population[pop_by_age$year == 2022 & pop_by_age$fig_type == "estimate" & pop_by_age$age == age]
    )
}

pop_data <- pop_by_age |>
  # We are interested in the following age groups
  mutate(
    age_group = case_when(
      age %in% c(18:25) ~ "18-25",
      age < 18 ~ "Juvenile",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  ) |>
  # Removing ages that we are not interested in
  drop_na(age_group) |>
  # Counting population projections in each year
  # by age group
  summarize(
    population = sum(population),
    .by = c(year, age_group, fig_type)
  ) |>
  bind_rows(
    total_pop |>
        mutate(age_group = "Total")
  ) |>
  # Converting population to millions for readability
  mutate(population = population / 1000000)

# Saving processed data to the app folder
saveRDS(pop_data, paste0(data_path, "/pop_data.rds"))
