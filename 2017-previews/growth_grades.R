library(readxl)
library(tidyverse)

# Recalculate TVAAS to exclude US History
hs_TVAAS <- read_excel("K:/ORP_accountability/data/2017_tvaas/Updated School Subject Level 2016-17.xlsx") %>%
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
    transmute(system = as.integer(district_number), system_name = district, school_name = school, school = as.integer(school_number),
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

write_csv(hs_TVAAS, "data/tvaas_hs_composites.csv", na = "")

# Grades for preview
pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool)

tvaas_k8 <- read_excel("K:/ORP_accountability/data/2017_tvaas/SAS-NIET School-Wide Indexes-E1E2M2-Suppressions.xlsx") %>%
    transmute(system = as.integer(`District Number`), school = as.integer(`School Number`),
        index = `TCAP/EOC: School-Wide: Composite`) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    filter(pool == "K8")

grades <- hs_TVAAS %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    filter(pool == "HS") %>%
    transmute(system, school, index, pool) %>%
    bind_rows(tvaas_k8) %>%
    transmute(system, school,
        grade_growth = case_when(
            index >= 2 ~ "A",
            index >= 1 ~ "B",
            index >= -1 ~ "C",
            index >= -2 ~ "D",
            index < -2  ~ "F"
        )
    )

write_csv(grades, "data/growth_grades.csv", na = "")
