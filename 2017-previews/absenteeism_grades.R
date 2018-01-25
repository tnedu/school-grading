library(acct)
library(tidyverse)

instructional_days <- readxl::read_excel("K:/Research_Transfers/2018_01_Jan_16Absenteeism/data/Instructional_Days_SY_2015_16.xlsx") %>%
    transmute(year = 2017, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

demographic <- read_delim("K:/Research and Policy/ORP_Data/Student_Information/Student_Demographics/Raw_Files/Student_Demographics_2016.txt", delim = "\t") %>%
    janitor::clean_names() %>%
    transmute(student_key,
        ED = directcertecondis == "Y",
        SWD = p01 == "Y" | p02 == "Y" | p04 == "Y" | p05 == "Y" | p06 == "Y" | p07 == "Y" | p08 == "Y" | p09 == "Y" | p10 == "Y" |
            p11 == "Y" | p12 == "Y" | p13 == "Y" | p14 == "Y" | p15 == "Y" | p17 == "Y" | p18 == "Y",
        EL = englishlearner == "Y" | transitional1 == "Y" | transitional2 == "Y")

attendance <- read_csv("K:/ORP_accountability/projects/Alex/school-grading/2017-previews/data/absenteeism_student_level.csv") %>%
    janitor::clean_names() %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade,
        student_key = as.integer(student_key),
        BHN = ethnic_origin %in% c("B", "H", "N"),
        Black = ethnic_origin == "B", Hispanic = ethnic_origin == "H", Native = ethnic_origin == "N",
        HPI = ethnic_origin == "P", Asian = ethnic_origin == "A", White = ethnic_origin == "W",
        begin_date, end_date, isp_days,
        count_total = if_else(is.na(cnt_total), 0L, as.integer(cnt_total))) %>%
# For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
# For students with same system, school, student ID, enrollment dates, instructional program days,
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
# For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
# Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
# Collapse multiple enrollments at the same school
    rename(n_absences = count_total) %>%
    group_by(system, school, grade, student_key, BHN, Black, Hispanic, Native, HPI, Asian, White) %>%
    summarise_at(c("n_absences", "isp_days"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        grade = case_when(
            grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K-8",
            grade %in% c("09", "10", "11", "12") ~ "9-12"
        ),
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        All = 1L
    ) %>%
    left_join(demographic, by = "student_key")

school_CA <- tibble()

for (s in c("All", "BHN", "ED", "SWD", "EL", "Black", "Hispanic", "Native", "HPI", "Asian", "White")) {
    
# School all grades
    school_CA <- attendance %>%
    # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
    # Drop students enrolled less than 50% of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        group_by(system, system_name, school, school_name) %>%
        summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)) %>%
        ungroup() %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(school_CA, .)
    
}

absenteeism_targets <- school_CA %>%
    transmute(system, school,
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup
        ),
        pct_CA_prior = pct_chronically_absent,
        AMO_target = amo_reduction(n_students, pct_chronically_absent),
        AMO_target_4 = amo_reduction(n_students, pct_chronically_absent, double = TRUE))

write_csv(absenteeism_targets, "data/absenteeism_targets.csv", na = "")

absenteeism_grades <- read_csv("K:/ORP_accountability/data/2017_chronic_absenteeism/school_chronic_absenteeism.csv") %>%
    select(system, school, subgroup, n_students, pct_CA = pct_chronically_absent) %>%
    left_join(absenteeism_targets, by = c("system", "school", "subgroup")) %>%
    mutate(lower_bound_ci = ci_lower_bound(n_students, pct_CA),
        grade_absenteeism_abs = case_when(
            n_students < 30 ~ NA_character_,
            pct_CA <= 8 ~ "A",
            pct_CA <= 12 ~ "B",
            pct_CA <= 17 ~ "C",
            pct_CA <= 24 ~ "D",
            pct_CA > 24 ~ "F"
        ),
        grade_absenteeism_reduction = case_when(
            n_students < 30 ~ NA_character_,
            pct_CA <= AMO_target_4 ~ "A",
            pct_CA < AMO_target ~ "B",
            lower_bound_ci <= AMO_target ~ "C",
            lower_bound_ci < pct_CA_prior ~ "D",
            lower_bound_ci >= pct_CA_prior ~ "F"
        ),
        grade_absenteeism = pmin(grade_absenteeism_abs, grade_absenteeism_reduction)
    ) %>%
    select(system, school, subgroup, grade_absenteeism_abs, grade_absenteeism_reduction, grade_absenteeism)

write_csv(absenteeism_grades, "data/absenteeism_grades.csv", na = "")
