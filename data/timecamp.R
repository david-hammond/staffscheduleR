library(httr)
library(jsonlite)
library(lubridate)
library(bizdays)
library(ggrepel)
library(tidyverse)
#library(priceR)
library(patchwork)

folder = ""

bizdays.options$get("default.calendar")
all_time = function(){
  tmp = floor_date(floor_date(today(), "week"), "week", 1)
  tmp2 = as.Date("2018-01-01")
  tmp = paste0("/from/", as.character(tmp2), "/to/", as.character(tmp + 6))
  return(tmp)
}

find_last_week = function(){
  tmp = data.frame(from = floor_date(floor_date(today(), "week"), "week", 1))
  tmp$to = tmp$from + 6
  return(tmp)
}

get_research_staff = function(){
  url = "https://app.timecamp.com/third_party/api/users/format/json/api_token/"
  token = "18cefd9473580f2b67dbeedf7d"
  url = paste0(url, token)
  x = GET(url)
  x = fromJSON(rawToChar(x$content))

  url = "https://app.timecamp.com/third_party/api/group/713495/user/format/json/api_token/"
  token = "18cefd9473580f2b67dbeedf7d"
  url = paste0(url, token)
  y = GET(url)
  y = fromJSON(rawToChar(y$content))
  x = x %>% mutate(research = user_id %in% y$user_id) %>%
    mutate(research = ifelse(grepl("dhammond", email), TRUE, research))
  x = x %>% dplyr::filter(research)
  return(x)
}

get_timecamp = function(){
  url = "https://app.timecamp.com/third_party/api/entries/format/json/api_token/"
  token = "18cefd9473580f2b67dbeedf7d"
  dates = all_time()
  url = paste0(url, token, dates)
  url = paste0(url, "/include_project/true")
  url = paste0(url, "/active_only/false")
  url = paste0(url, "/with_subtask/true")
  url = paste0(url, "/round_duration/true")
  url = paste0(url, "/opt_fields/breadcrumbs")
  x = GET(url)
  x = fromJSON(rawToChar(x$content))
  x$start = as.POSIXct(paste(x$date, x$start_time))
  x$end = as.POSIXct(paste(x$date, x$end_time))
  x$time_spent = as.numeric(difftime(x$end, x$start,  units = "hours"))
  x$date = as.Date(x$date)
  x = x %>% mutate(fy = as.character(fy::date2fy(date)))  %>%
    select(task_id, fy, breadcrumps, name, date, user_name, time_spent, start, end)


  x = x %>% dplyr::select(task_id, fy, project = name, user_name, date, time_spent, start, endtime = end) %>%
    mutate(task_id = as.numeric(task_id)) %>% dplyr::filter(user_name %in% get_research_staff()$display_name)
  return(x)
}

get_budgets = function(){
  url = "https://app.timecamp.com/third_party/api/tasks/format/json/api_token/"
  token = "18cefd9473580f2b67dbeedf7d"
  url = paste0(url, token)
  x = fromJSON(url)
  get_budget = function(y){
    tmp = data.frame(task_id = y$task_id, value = y$note)
    return(tmp)
  }
  x = lapply(x, get_budget) %>% bind_rows() %>%
    dplyr::filter(value != "") %>% mutate(task_id = as.character(task_id))
  return(x)
}

get_attendance = function(last_week){
  attendance = last_week %>% group_by(user_name, date) %>%
    summarise(logged_in = strftime(min(start), "%H:%M"),
              logged_out = strftime(max(endtime), "%H:%M"),
              logged_time = round(as.numeric(difftime(max(endtime), min(start),
                                                      units = "hours")),2)) %>%
    ungroup() %>%
    mutate(day = as.character(wday(date, label = T))) %>%
    select(user_name, date, day, logged_in, logged_out, logged_time)
  return(attendance)
}


timecamp = get_timecamp()
