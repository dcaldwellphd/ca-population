## This script processes data from the California Department of Finance
## To run it, download the following files and store them in the data folder
## E4_2000-2010_Report_Final_EOC_000.xlsx
## E-4_2010-2020-Internet-Version.xlsx
## P1A_State_Total.xlsx
## Intercensal_2000-2010_Total_Age-Race.xlsx
## P1B_State_Age.xlsx
## Available: https://dof.ca.gov/forecasting/demographics/

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(janitor)

## Path to the app folder
folder_path <- "./"
## Path to the CA DoF data in the app folder
data_path <- paste0(folder_path, "data/")

# Cleaning population totals

## 2000-2009 data
dof_00_09_total <- read_excel(
  paste0(data_path, "E4_2000-2010_Report_Final_EOC_000.xlsx"),
  sheet = "Table 1 County State",
  range = "A4:M65",
  .name_repair = make_clean_names
  ) |>
  # Keep only the total population row
  slice(n()) |>
  # Remove the first column, which contains the county name,
  # and the last two columns, which contain 2010 estimates
  select(-c(1, last_col(), (last_col()-1))) |>
  # Rename columns as "x2000", "x2001", ..., "x2009"
  rename_with(~str_c("x", 2000:2009)) |>
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "population"
  ) |>
  mutate(
    year = as.numeric(str_remove(year, "x")),
    fig_type = "estimate"
    )

## 2010-2019 data
dof_10_19_total <- read_excel(
  paste0(data_path, "E-4_2010-2020-Internet-Version.xlsx"),
  sheet = "Table 1 County State",
  range = "A2:L61",
  .name_repair = make_clean_names
  ) |>
  # Keep only the total population row
  slice(n()) |>
  # Remove the first column, which contains county names,
  # and the last column, which contains the 2020 estimate
  select(-c(1, last_col())) |>
  # Rename columns as "x2010", "x2011", ..., "x2019"
  rename_with(~str_c("x", 2010:2019)) |>
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "population"
  ) |>
  mutate(
    year = as.numeric(str_remove(year, "x")),
    fig_type = "estimate"
    )

# 2020-2060 data
dof_20_60_total <- read_excel(
    paste0(data_path, "P1A_State_Total.xlsx"),
    sheet = "Total Population",
    range = "B3:AP4",
    .name_repair = make_clean_names
    ) |>
    pivot_longer(
        cols = everything(),
        names_to = "year",
        values_to = "population"
    ) |>
    # Now that year is a column, we can strip the "x" applied to year values by the janitor package
    mutate(
        year = as.numeric(str_remove(year, "x")),
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

 ## One data frame with population totals for each year
 pop_total <- bind_rows(
    dof_00_09_total,
    dof_10_19_total,
    dof_20_60_total
    )

# Cleaning population by age

## 2000-2010 data
dof_00_10_age <- read_excel(
  paste0(data_path, "Intercensal_2000-2010_Total_Age-Race.xlsx"),
  sheet = "Total",
  range = "A1:R41714",
  .name_repair = make_clean_names
  ) |>
  filter(county_name == "California") |>
  # columns starting "county_" no longer needed
  # gender column adds no information
  # 7th column is an extra population estimate for 2000
  select(-c(contains("county_"), race_code, gender, 7)) |>
  # Rename columns as "x2010", "x2011", ..., "x2020"
  rename_with(~str_c("x", 2000:2010), .cols = 3:last_col()) |>
  pivot_longer(
    cols = 3:last_col(),
    names_to = "year",
    values_to = "population"
  ) |>
  mutate(
    year = as.numeric(str_remove(year, "x")),
    fig_type = "estimate"
    ) |>
  # We are interested in the following age groups
  mutate(
    age_group = case_when(
      age %in% c(18:25) ~ "18-25",
      age < 18 ~ "Juvenile",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  ) |>
  # Removing other ages not in these cats
  drop_na(age_group) |>
  # Counting population estimates in each year
  # by age group
  summarize(
    population = sum(population),
    .by = c(year, age_group, fig_type)
  )

## 2020-2060 data
dof_20_60_age <- read_excel(
    paste0(data_path, "P1B_State_Age.xlsx"),
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
        year = as.numeric(str_remove(year, "x")),
        fig_type = if_else(
            year %in% c(2020:2022),
            "estimate",
            "projection"
        )
    )

## I repeat the step duplicating 2022 rows for each group,
## setting fig_type to "projection" to avoid gaps in trend lines
unique_ages <- unique(dof_20_60_age$age)

for (age in unique_ages) {
  dof_20_60_age <- dof_20_60_age |>
    add_row(
      year = 2022, 
      fig_type = "projection",
      age = age,
      population = dof_20_60_age$population[dof_20_60_age$year == 2022 & dof_20_60_age$fig_type == "estimate" & dof_20_60_age$age == age]
    )
}

dof_20_60_age <- dof_20_60_age |>
  mutate(
    age_group = case_when(
      age %in% c(18:25) ~ "18-25",
      age < 18 ~ "Juvenile",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  ) |>
  drop_na(age_group) |>
  # Counting population projections in each year
  # by age group
  summarize(
    population = sum(population),
    .by = c(year, age_group, fig_type)
  ) 

## One data frame with population by age group for each year  
pop_by_age <- bind_rows(
  dof_00_10_age,
  dof_20_60_age
  )

# All data
pop_data <- pop_total |>
  mutate(age_group = "Total") |>
  bind_rows(pop_by_age) |>
  # Converting population to millions for readability
  mutate(population = population / 1000000)

# Saving processed data to the app folder
saveRDS(pop_data, paste0(data_path, "/pop_data.rds"))
