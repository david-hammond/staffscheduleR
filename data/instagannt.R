# library(lubridate)
# library(data.tree)
# library(dm)
# library(ganttrify)
#remotes::install_github("giocomai/ganttrify")
library(tidyverse)
library(dm)
# library(lpSolve)
# library(scales)
library(readxl)
library(programmeManageR)
library(pbapply)
source("./data/timecamp.R")
fname = "../../../Downloads/exportgantt.xlsx"
y = read_excel(fname, range = cell_cols(c(1:7, 9)))
names(y) = tolower(y[1,])
y = y[-1,]
pos = y$level == 1 & y$start == "-"
y$bucket = NA
y$bucket[pos] = y$name[pos]
y = y %>% fill(bucket, .direction = "down")
y = y[!pos,]
pos = y$level == 1
y$project = NA
y$project[pos] = y$name[pos]
y = y %>% fill(project, .direction = "down")
y = y[!pos,]
pos = y$level == 2
y$phase = NA
y$phase[pos] = y$name[pos]
y = y %>% fill(phase, .direction = "down")
y = y[!pos,]
y = y %>% relocate(id, bucket, project, phase) %>%
  select(-level)
y = y %>% filter(!is.na(user))
y = y %>% mutate(progress = parse_number(progress)/100)
y = y %>% mutate(start = mdy(start))
y = y %>% mutate(due = mdy(due))
y = y %>% rename(completed = `completion date`) %>% mutate(completed = mdy(completed))
y = y %>% separate_rows(user, sep = ',') %>%
  mutate(staff = trimws(user)) %>%
  group_by(id) %>%
  mutate(total_days = as.numeric(`estimated hours`)/n()) %>%
  ungroup()
y$constant_load = ifelse(y$bucket == 'Management', TRUE, FALSE)
y$total_days[y$constant_load] = y$total_days[y$constant_load]*interval(y$start[y$constant_load], y$due[y$constant_load])/weeks(1)
y$progress[y$constant_load] = 1-(as.numeric((y$due[y$constant_load]-today()))/
                                   as.numeric((y$due[y$constant_load] - y$start[y$constant_load])))
y = y %>% mutate(days_left = (1-progress)*total_days)
y = y %>%
  select(-user, -`estimated hours`) %>%
  rename(activity = name, task_id = id)

timesheet = timecamp %>% filter(date >= min(y$start)) %>%
  mutate(week = floor_date(date, "week")) %>%
  mutate(days_spent = time_spent/8.5) %>%
  group_by(project, user_name, week) %>%
  summarise(days_spent = sum(days_spent)) %>%
  ungroup() %>%
  separate(user_name, c("staff", "guff"), sep = " ") %>%
  select(project, week, staff, days_spent) %>%
  filter(project %in% y$project)

projects = y %>% decompose_table(project_id, bucket, project)
tasks = projects$child_table
projects = projects$parent_table
timesheet = timesheet %>% left_join(projects)
timesheet = timesheet %>% select(-project, -bucket)
project_spend = timesheet %>% left_join(staff) %>% left_join(charge_out) %>%
  mutate(spent = charge_out*days_spent) %>%
  group_by(project_id, week) %>%
  summarise(spent = sum(spent)) %>%
  ungroup()
projects$client = NA
projects$contract_amount = NA
projects$account_manager = NA
projects$project_manager = NA
projects$staff_budget = NA
projects$additional_costs = NA
projects$admin_fee = 0.1*(projects$additional_costs + projects$staff_budget)
projects$total_budget = projects$additional_costs + projects$staff_budget + projects$admin_fee
fname = "../programmeManageR/data/p4p plan.xlsx"
staff = readxl::read_excel(fname, sheet = "staff")
charge_out = readxl::read_excel(fname, sheet = "daily_rates")
datamodel = dm(staff, charge_out, projects, tasks, timesheet, project_spend) %>%
  dm_add_pk(charge_out, role) %>%
  dm_add_fk(staff, role, charge_out) %>%
  dm_add_pk(projects, project_id) %>%
  dm_add_fk(tasks, project_id, projects) %>%
  dm_add_pk(staff, staff) %>%
  dm_add_fk(timesheet, staff, staff) %>%
  dm_add_fk(timesheet, project_id, projects) %>%
  dm_add_fk(project_spend, project_id, projects)
datamodel %>%
  dm_draw()
datamodel %>% dm_flatten_to_tbl(timesheet, .recursive = T) %>%
  mutate(spent = charge_out*days_spent) %>%
  group_by(project_id, week) %>%
  summarise(spent = sum(spent)) %>%
  ungroup()
#want a planned alue = burndown
#want a actual cost per week
#want an earned value based on progress
