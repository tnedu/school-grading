library(acct)
library(tidyverse)

subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners with T1/T2", "Students with Disabilities", "Super Subgroup")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("N:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool) %>%
    filter(!is.na(pool))

school_base <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools, by = c("system", "school"))

participation_rate_subgroup <- school_base %>%
    filter(year == 2017,
        subgroup %in% subgroups, grade %in% as.character(3:12),
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12)) %>%
    mutate(grade = as.numeric(grade),
        subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup)) %>%
# Aggregate enrolled/tested across grades
    group_by(system, school, pool, subject, subgroup) %>%
    summarise_at(c("enrolled", "tested"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("enrolled", "tested"), funs(if_else(enrolled < 30, 0, .))) %>%
    group_by(system, school, pool, subgroup) %>%
# Aggregate enrolled/tested across subjects
    summarise_at(c("enrolled", "tested"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school, subgroup, enrolled, tested,
        participation_rate = if_else(enrolled != 0, round5(100 * tested/enrolled), NA_real_),
        meet_95_participation = as.integer(participation_rate >= 95))

write_csv(participation_rate_subgroup, "data/participation_rate_subgroup.csv", na = "")

participation_rate_school <- participation_rate_subgroup %>%
    group_by(system, school) %>%
    summarise(meet_all_participation = min(meet_95_participation, na.rm = TRUE))

write_csv(participation_rate_school, "data/participation_rate_school.csv", na = "")
