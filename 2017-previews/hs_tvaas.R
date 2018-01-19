library(tidyverse)

tvaas <- readxl::read_excel("K:/ORP_accountability/data/2017_tvaas/Updated School Subject Level 2016-17.xlsx") %>%
    janitor::clean_names() %>%
    filter(test == "EOC", subject != "US History") %>%
    group_by(district, district_number, school, school_number) %>%
    mutate(n_students = sum(number_of_students, na.rm = TRUE)) %>%
    ungroup() %>%
# Numerator is average of component EOC TVAAS estimates, weighted by number of test takers in each subject
# Denominator is square root of squared standard errors, weighted by squared number of test takers in each subject
    mutate(partial_numerator = number_of_students/n_students * growth_measure,
        partial_denom = (number_of_students/n_students)^2 * standard_error^2) %>%
    group_by(district, district_number, school, school_number) %>%
    mutate(numerator = sum(partial_numerator),
        denom = sqrt(sum(partial_denom))) %>%
    ungroup() %>%
    mutate(new_index = numerator/denom) %>%
    transmute(system = district_number, system_name = district, school_name = school, school = school_number,
        test, year, n_students, estimate = numerator, standard_error = denom, index = new_index,
        level = case_when(
            index < -2 ~ "Level 1",
            index < -1 ~ "Level 2",
            index < 1 ~ "Level 3",
            index < 2 ~ "Level 4",
            index >= 2 ~ "Level 5"
        )
    ) %>%
    distinct()

write_csv(tvaas, "data/new_school_composites.csv")
