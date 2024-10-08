#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate left_join group_by ungroup filter summarise relocate distinct
#' @importFrom scales dollar
#' @importFrom readr parse_number
#' @importFrom rlang .data
#' @importFrom data.tree as.Node SetFormat FormatPercent FormatFixedDecimal
#' @importFrom dm decompose_table dm dm_add_pk dm_add_fk dm_flatten_to_tbl dm_draw copy_dm_to
#' @importFrom lpSolve lp
#' @importFrom lubridate today floor_date
#' @importFrom ggplot2 ggplot labs geom_tile theme_light theme geom_text scale_color_brewer scale_fill_gradientn
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'

schedule = function(data){

  data_model = data %>%
    arrange(constant_load) %>%
    mutate(project = factor(project, unique(project), ordered = T)) %>%
    # group_by(project, phase, activity, days_left, constant_load) %>%
    # summarise(start = min(start),
    #           due = max(due),
    #           days_left = sum(days_left)) %>%
    # ungroup() %>%
    arrange(start) %>%
    decompose_table(id, project, phase, activity, days_left, constant_load)

  schedule = gantt_matrix(data_model$child_table)
  schedule = schedule %>% left_join(data_model$parent_table %>% select(id, days_left, constant_load), by = "id") %>%
    relocate(id, constant_load, days_left)
  schedule = create_gantt_matrix(schedule)
  optsoln = schedule

  schedule$flat_gantt = schedule$gantt %>% gather(week, loading, -c(id)) %>%
    mutate(week = as.Date(week)) %>%
    #filter(loading > 0) %>%
    left_join(data_model$parent_table %>% select(id, constant_load, project, phase), by = "id") %>%
    select(-id) %>%
    relocate(project, phase, constant_load) %>%
    arrange(constant_load, project)

  schedule$loading_table = schedule$flat_gantt
  schedule$loading_chart = loading_plot(schedule)
  p = data %>% select(-phase) %>%
    rename(phase = project) %>%
    group_by(phase, activity) %>%
    summarise(start = min(start),
              due = max(due),
              progress = sum(progress*total_days)/sum(total_days)) %>% ungroup() %>%
    mutate(activity = make.unique(activity)) %>%

    mutate(phase = factor(phase, unique(phase), ordered = T)) %>%
    select(phase, activity, start, due, progress) %>%
    arrange(phase, start)
  schedule$gantt = get_gantt(p)
  return(schedule)
}
gantt_matrix = function(data){
  require(lubridate)
  date_created = today()
  planning_horizon = 12
  expanded_data <- data %>%
    rowwise() %>%
    mutate(week_sequence = list(seq.Date(floor_date(as.Date(start), "week"), ceiling_date(as.Date(due), "week") - 1, by = "week"))) %>%
    unnest(week_sequence) %>%
    select(id, week_sequence) %>%
    distinct() %>%
    group_by(id, week_sequence) %>%
    summarize(active = 1) %>%
    ungroup()
  all_weeks <- seq.Date(floor_date(date_created, "week"),
                        floor_date(date_created + months(planning_horizon), "week"), by = "week")

  complete_data <- expanded_data %>%
    complete(id, week_sequence = all_weeks, fill = list(active = 0))


  gantt_matrix <- complete_data %>%
    spread(week_sequence, active, fill = 0)
  return(gantt_matrix)
}

extend_matrix = function(mat) {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)
  new_cols <- num_rows * num_cols

  new_mat <- matrix(0, nrow = num_rows, ncol = new_cols)

  for (i in 1:num_rows) {
    start_col <- (i - 1) * num_cols + 1
    end_col <- start_col + num_cols - 1
    new_mat[i, start_col:end_col] <- mat[i, ]
  }

  return(new_mat)
}

# Function to bind two matrices row-wise, with padding if necessary
rbind_with_padding = function(mat1, mat2) {
  num_cols_mat1 <- ncol(mat1)
  num_cols_mat2 <- ncol(mat2)

  if (num_cols_mat2 < num_cols_mat1) {
    padding <- matrix(0, nrow = nrow(mat2), ncol = num_cols_mat1 - num_cols_mat2)
    mat2 <- cbind(mat2, padding)
  }

  combined_mat <- rbind(mat1, mat2)
  return(combined_mat)
}


create_gantt_matrix = function(schedule) {
  timeframes = schedule %>% select(-c(1:3)) %>% as.matrix()
  row_sums = schedule$days_left
  # Replace NA values with 0
  timeframes[is.na(timeframes)] <- 0

  # Get the number of tasks and weeks from the timeframes matrix
  tasks <- nrow(timeframes)
  weeks <- ncol(timeframes)

  # Set column sums and right-hand side constraints
  col_sums <- rep(5, weeks)
  rhs <- c(col_sums, row_sums)

  # Calculate the number of variables
  raw_vars <- tasks * weeks
  total_vars <- raw_vars + 2 * weeks

  # Set direction of constraints
  dir <- rep("=", length(rhs))

  # Create the constraint matrix A
  A <- NULL
  for (i in 1:tasks) {
    A <- cbind(A, diag(weeks))
  }
  A <- cbind(A, diag(weeks), -diag(weeks))

  # Extend the timeframes matrix and convert to a data frame
  extended_timeframes <- extend_matrix(as.matrix(timeframes))

  # Bind the extended timeframes matrix to the constraint matrix A
  A <- rbind_with_padding(A, extended_timeframes)
  A = A %>% cbind(0)

  # Add auxiliary Variable

  aux = diag(weeks) %>% cbind(-1)
  aux = matrix(0, nrow = nrow(aux), ncol = ncol(A) - ncol(aux)) %>% cbind(aux)


  A = A %>% rbind(aux)
  rhs = c(rhs, rep(0, weeks))
  dir = c(dir, rep("<=", weeks))

  #If non flexcible scheduling
  pos = which(schedule$constant_load)
  if(length(pos)>=1){
    tmp = schedule
    for (i in 1:nrow(tmp)){
      if(tmp$constant_load[i]){
        tmp[i,-c(1:3)] = t(rep(tmp$days_left[i]/sum(tmp[i,-c(1:3)]), ncol(tmp[i,-c(1:3)])))
      }else{
        tmp[i,-c(1:3)] = t(rep(0, ncol(tmp[i,-c(1:3)])))
      }
    }
    aux = as.data.frame(diag(as.vector(t(tmp[,-c(1:3)]))))
    auxrhs = apply(aux, 1, max)
    aux = aux %>% filter(auxrhs != 0)
    aux[aux>0] = 1
    auxrhs = auxrhs[auxrhs>0]
    aux = aux %>% cbind(matrix(0, nrow = nrow(aux), ncol = ncol(A) - ncol(aux)))
    A = A %>% rbind(as.matrix(aux))
    rhs = c(rhs, auxrhs)
    dir = c(dir, rep("<=", length(auxrhs)))
  }


  # Define the objective coefficients
  #objective_coef <- c(rep(1, ncol(A)-1), 10000) seq(1, 2, length.out = weeks)
  objective_coef <- c(rep(seq(1, 20, length.out = weeks), tasks),
                      rep(2, (total_vars - raw_vars) / 2),
                      rep(1, (total_vars - raw_vars) / 2),
                      10000)


  # Solve the linear program
  result <- lp(direction = "min",
               objective.in = objective_coef,
               const.mat = A,
               const.dir = dir,
               const.rhs = rhs)

  # Extract results
  solution <- round(result$solution, 1)

  # Create the Gantt matrix
  result$gantt_matrix <- as.data.frame(matrix(solution[1:raw_vars], nrow = tasks, byrow = TRUE))
  names(result$gantt_matrix) = names(schedule %>% select(-c(1:3)))
  result$gantt_matrix <- result$gantt_matrix %>%
    mutate(id = schedule$id) %>%
    relocate(id)

  return(result)
}

get_colour_pallette = function(data){
  require(RColorBrewer)
  cols = data.frame(project = levels(data$project),
                    palette = RColorBrewer::brewer.pal(8, "Dark2")[1:length(unique(data$project))])
  cols$palette = ifelse(cols$project == "Leave", RColorBrewer::brewer.pal(3, "Set1")[1], cols$palette )
  cols$palette = ifelse(cols$project == "Management", RColorBrewer::brewer.pal(3, "Set1")[2], cols$palette)
  return(cols$palette)
}

loading_plot = function(data){
  data = data$flat_gantt
  data$project = as.character(data$project)
  data = data %>% group_by(project) %>% mutate(mgmt = max(constant_load)) %>%
    ungroup() %>% arrange(mgmt, project) %>%
    mutate(project = factor(project, c(setdiff( unique(data$project),c("Leave", "Management")),
                                       intersect(unique(data$project),c("Leave", "Management"))), ordered = T))
  pal = get_colour_pallette(data)
  ggplot(data, aes(x = week, y = loading, fill = project)) +
    geom_hline(yintercept = 5, linetype = "dotted", color = "red") +
    geom_vline(xintercept = today(), linetype = "dashed", color = "blue") +
    geom_bar(stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = pal) +
    labs(                         x = "",
                                  y = "Days Assigned",
                                  fill = "") +
    theme_minimal() + ylim(c(0, max(data$loading)+1)) +
    theme(legend.position = c(0.9,0.8))
}

get_gantt = function(y){
  require(plan)
  g <- new("gantt")
  y = y %>% arrange((start))
  y = split(y, factor(y$phase, unique(y$phase), ordered = T))
  for (i in y){
    g <- ganttAddTask(g, as.character(i$phase[1]))
    for(j in 1:nrow(i)){
      g <- ganttAddTask(g, i$activity[j], as.character(i$start[j]),  as.character(i$due[j]),
                        done = 100*i$progress[j])
    }

  }
  font <- ifelse(is.na(g[["start"]]), 2, 1)
  return(g)
}
