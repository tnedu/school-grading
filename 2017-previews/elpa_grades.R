library(tidyverse)

elpa <- haven::read_dta("K:/ORP_accountability/data/2017_ELPA/school_level_elpa_JW.dta") %>%
    filter(!subgroup %in% c("Non-Economically Disadvantaged", "Non-Students with Disabilities", "Unidentified")) %>% 
    transmute(system, school = schoolnumber, pct_met_growth_standard,
        subgroup = case_when(
            subgroup == "English Language Learners" ~ "English Learners",
            subgroup == "Native Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup
        ),
        grade_elpa = case_when(
            valid_tests < 10 ~ NA_character_,
            pct_met_growth_standard >= 60 ~ "A",
            pct_met_growth_standard >= 50 ~ "B",
            pct_met_growth_standard >= 40 ~ "C",
            pct_met_growth_standard >= 25 ~ "D",
            pct_met_growth_standard < 25 ~ "F"
        )
    )

write_csv(elpa, path = "data/elpa_grades.csv", na = "")
