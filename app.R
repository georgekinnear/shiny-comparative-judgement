library(tidyverse)
library(shiny)
library(shinyjs)
library(pool)
library(yaml)
library(DT)

# Connection info is stored in dbconfig.yml (not in public repo) for security
dbconfig <- yaml::read_yaml("dbconfig.yml")
pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = dbconfig$dbname,
  host = dbconfig$host,
  username = dbconfig$username,
  password = dbconfig$password
)
onStop(function() {
  poolClose(pool)
})

#
# Define the various judging groups
#
studies <- pool %>% 
  tbl("studies") %>% 
  collect()
# tibble::tribble(
#   ~study,                                                                        ~judging_prompt, ~judging_method, ~target_judges, ~judgements_per_judge,
#   "vehicle_pairs",                                      "Which is the most typical example of a vehicle?",        "binary",            20L,                  100L,
#   "vehicle_slider",                                      "Which is the most typical example of a vehicle?",        "slider",            20L,                  100L,
#   "violation_pairs", "A sign says No Vehicles in the Park. Which example would be the the worst violation?",        "binary",            20L,                  100L,
#   "violation_slider", "A sign says No Vehicles in the Park. Which example would be the the worst violation?",        "slider",            20L,                  100L,
#   "nuisance_pairs",                   "Which of the two examples would be the biggest nuisance in a park?",        "binary",            20L,                  100L,
#   "nuisance_slider",                   "Which of the two examples would be the biggest nuisance in a park?",        "slider",            20L,                  100L
# )

scripts <- read_yaml("vehicles.yml") %>%
  purrr::map(as_tibble_row) %>%
  enframe(name = NULL) %>%
  unnest(cols = c("value")) %>% 
  rename_with(~ str_replace(., "-", "_")) %>%
  rename(markdown = html)

# add item for the attention check
scripts <- scripts %>% 
  add_row(
    item_num = 0,
    item_name = "attention_check",
    markdown = "This is an attention check, please pick this item"
  ) %>% 
  mutate(html = purrr::map(markdown, ~ markdown::markdownToHTML(
    text = .,
    fragment.only = TRUE
  )))

assign_to_study <- function() {
  print(study_status %>% mutate(judge_slots = target_judges - num_judges))
  # allocate to one of the study conditions, weighted by current progress
  study_status %>%
    # identify the number of judges needed by each condition to meet its target
    mutate(judge_slots = target_judges - num_judges) %>%
    # pick the condition with the most open slots
    slice_max(judge_slots, n = 1, with_ties = FALSE) %>% 
    #pull(study_id)
    unlist()
}

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  
  tags$head(
    # Custom CSS
    tags$style(HTML("
      /* body {padding-top: 50px}  for the boostrap nav */
      /*ul.nav-pills {margin-top: 5px}*/
      @media (max-width: 768px) { .navbar-nav {float: left; margin: 5px; } }
      /*.navbar-nav {float: left; margin: 5px; }*/
      .navbar-text {float:left; margin-left:15px; }
      .navbar-right {float:right; margin-right:15px; }
      div#pageContent { margin-bottom: 2em; }
      #demographics .shiny-input-container { width: auto; clear: both; }
      #demographics .shiny-options-group { display: block; float: left; }
      #demographics .control-label { float: left; width: 12em; text-align: right; margin-right: 1em; }
      div.item_panel { padding: 1em 2em; border: 1px solid #ccc; box-shadow: 3px 4px 15px 0px #0000002b; overflow: auto;}
      div.item_content {margin-top: 1em; }
      #chooseLeft_comment-label, #chooseRight_comment-label { color: #999; font-weight: normal; font-style: italic; margin-top: 1em; }
/* this is needed so that longer text in the item choice buttons wraps onto new lines */
.btn { white-space:normal !important; }

#judging_comment-label { color: #999; font-weight: normal; font-style: italic; margin-top: 1em; }
.comparison-image { width: 100%; }
.cj_slider {
  -webkit-appearance: none;
  width: 100%;
  height: 15px;
  border-radius: 5px;  
  background: #d3d3d3;
  outline: none;
  opacity: 0.7;
  -webkit-transition: .2s;
  transition: opacity .2s;
}

.cj_slider::-webkit-slider-thumb {
  -webkit-appearance: none;
  appearance: none;
  width: 25px;
  height: 25px;
  border-radius: 50%; 
  background: #003399;
  cursor: pointer;
}

.cj_slider::-moz-range-thumb {
  width: 25px;
  height: 25px;
  border-radius: 50%;
  background: #003399;
  cursor: pointer;
}
/* hide the shiny slider features we don't want */
.irs-min, .irs-max, .irs-single, .irs-bar { display:none !important; }
#choice_slider-label {display:none !important;}
.slider_left {
  text-align: left;
  width: 90%;
  float: left;
  border-left: 2px solid black;
  padding: 1em;
  background: linear-gradient(90deg, rgba(0,0,0,0.1) 0%, rgba(255,255,255,0) 100%);
}
.slider_right {
  text-align: right;
  width: 90%;
  float: right;
  border-right: 2px solid black;
  padding: 1em;
  background: linear-gradient(-90deg, rgba(0,0,0,0.1) 0%, rgba(255,255,255,0) 100%);
}

.design-comment { color: #ccc; font-style: italic;}
    "))
  ),
  
  # Placeholder for page content - the server will update this as needed
  uiOutput("pageContent")
)



server <- function(input, output, session) {
  
  # These will be global variables within each session
  assigned_study <- NULL
  session_info <- NULL
  prolific_id <- NULL
  judge_id <- NULL
  judging_method <- NULL
  starting_pair <- NULL # this will be set to 1 by default, but when resuming a session it will record where to start
  
  #
  # Check on judging progress
  #
  all_existing_judgements <<- pool %>% 
    tbl("judgements") %>% 
    select(-contains("comment")) %>% 
    collect() %>% 
    semi_join(studies, by = "study")
  
  judges <<- pool %>% 
    tbl("judges") %>% 
    collect() %>% 
    left_join(
      all_existing_judgements %>% 
        group_by(judge_id) %>% 
        summarise(
          num_judgements = n(),
          time_spent_s = sum(time_taken, na.rm = TRUE)/1000
        ),
      by = "judge_id"
    ) %>% 
    left_join(
      pool %>% tbl("comments") %>% 
        collect(),
      by = "judge_id"
    ) %>% 
    left_join(
      # attention checks
      all_existing_judgements %>% 
        group_by(judge_id) %>% 
        filter(left == 0 | right == 0) %>% 
        mutate(
          attention_check_result = case_when(
            won == 0 ~ TRUE,
            left == 0 & score < 0 ~ TRUE,
            right == 0 & score > 0 ~ TRUE,
            TRUE ~ FALSE,
          ),
        ) %>% 
        summarise(
          attention_checks = n(),
          attention_checks_passed = sum(attention_check_result),
          attention_checks_failed = sum(!attention_check_result)
        ),
      by = "judge_id"
    )
  
  study_progress <<- studies %>% 
    full_join(
      judges %>% 
        select(judge_id, prolific_id, study_id, num_judgements, attention_checks_passed),
      by = c("study" = "study_id")
    )
  study_status <<- studies %>% 
    left_join(
      study_progress %>%
        group_by(study) %>%
        summarise(
          num_judges = n_distinct(judge_id),
          num_judges_completed = sum(num_judgements == 100, na.rm = TRUE),
          num_judgements = sum(num_judgements, na.rm = TRUE)
        ),
      by = "study"
    ) %>% 
    mutate(across(starts_with("num_"), ~replace_na(.x, 0L)))
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['PROLIFIC_PID']])) {
      prolific_id <<- query[['PROLIFIC_PID']]
      # Check if this user already exists in the DB: if so, pick up from where they left off
      session_info <<- pool %>% tbl("judges") %>%
        filter(prolific_id == !!prolific_id) %>%
        arrange(-judge_id) %>%
        collect() %>%
        slice(1)
      
      if(nrow(session_info) > 0) {
        # User has already consented - find out how many judgements they completed, and pick up where they left off
        num_existing_judgements <- pool %>% tbl("judgements") %>%
          filter(judge_id == !!session_info$judge_id) %>% 
          collect() %>% 
          nrow()
        starting_pair <<- num_existing_judgements + 1
      } else {
        # ID is not recognised, so proceed as a new user
        starting_pair <<- 1
      }
    } else if (!is.null(query[['ADMIN_USER']])) {
      # TODO - admin dashboard
      output$pageContent <- renderUI({
        tagList(
          navbarPage("CJ Dashboard",
                     tabPanel("Summary", 
                              fluidRow(tableOutput("judge_tally")),
                              fluidRow(tableOutput("summary_table"))
                     ),
                     tabPanel("Participants", 
                              fluidRow(column(12, downloadButton("download_judges", "Download judges.csv"))),
                              fluidRow(dataTableOutput("participants_table"))
                     ),
                     tabPanel("Judgements",
                              fluidRow(column(12, downloadButton("download_judgements", "Download judgements.csv"))),
                              fluidRow(dataTableOutput("judgements_table"))
                     )
          ),
          tags$div(class = "clearfix")
        )
      })
    } else {
      prolific_id <<- "None"
      # Quit with a warning
      output$pageContent <- renderUI({
        tagList(
          h3("Participant ID not found"),
          p("The PROLIFIC_PID is missing from the URL."),
          p("Please return to Prolific and try again."),
        )
      })
    }
  })
    
  #
  # Page 0 - consent form
  #
  output$pageContent <- renderUI({
    tagList(
      includeMarkdown("step0-participant-info.md"),
      fluidRow(
        column(4, offset = 4, actionButton("consentButton", "I consent", class = "btn-success btn-lg btn-block", icon = icon("check")))
      )
    )
  })
 
  
  #
  # Page 1 - judging instructions
  #
  observeEvent(input$consentButton, {
    
    if(starting_pair > 1) {
      # They are resuming a previous session; session_info will already have been recreated
      print("here")
      print(starting_pair)
      print(session_info)
      judge_id <<- session_info$judge_id
      assigned_study <<- studies %>% filter(study == !!session_info$study_id)
      judging_method <<- assigned_study[["judging_method"]]
      print(paste("Returning judge with ID:", judge_id))
    } else {
      # They are a new user
    
      # Now they have consented, assign them to a condition
      assigned_study <<- assign_to_study()
      
      # Create session_info and synch with the judges table in the database
      ## 1. Write session info to the database
      session_info <<- tibble(
        shiny_info = session$token,
        shiny_timestamp = as.character(Sys.time()),
        study_id = assigned_study[["study"]],
        prolific_id = prolific_id,
      )
      dbWriteTable(pool,
                   "judges",
                   session_info,
                   row.names = FALSE,
                   append = TRUE)
      
      ## 2. Update session_info to include the autoincremented judge_id produced by the database
      session_info <<- pool %>% tbl("judges") %>%
        filter(shiny_info == !!session_info$shiny_info) %>%
        arrange(-judge_id) %>%
        collect() %>%
        slice(1)
      
      ## 3. Pick out the judge_id and judging_method for ease of reference later on
      judge_id <<- session_info$judge_id
      judging_method <<- assigned_study[["judging_method"]]
      print(judge_id)
      
    }
    
    # update the page content
    output$pageContent <- renderUI({
      tagList(
        h3("Instructions"),
        markdown::markdownToHTML(text = read_file(paste0("judging-instructions-", judging_method, ".md")),
                                 fragment.only = TRUE) %>% 
          str_replace("\\[JUDGING PROMPT\\]", assigned_study[["judging_prompt"]]) %>% HTML() %>% withMathJax(),
        fluidRow(
          column(4, offset = 4, actionButton("startComparing", "Start comparing", class = "btn-success btn-lg btn-block", icon = icon("check")))
        )
      )
    })
  })
  
  
  #
  # Judging
  #
  
  make_pairs <- function(pairs_to_make = 20) {
    
    # 1. Gather data on which judgements have been made already in this study group

    # all comparisons from this study
    judgement_data <- pool %>% 
      tbl("judgements") %>% 
      filter(study == !!session_info$study_id) %>% 
      select(-contains("comment")) %>% 
      collect() %>% 
      mutate(across(c("left", "right"), as.integer))

    # count the number of comparisons for each pair
    pairs_judged <- judgement_data %>%
      rowwise() %>%
      mutate(pair = paste(sort(c(left, right)), collapse = '_')) %>% 
      select(pair, judge_id) %>% 
      group_by(pair) %>% 
      tally() %>% 
      arrange(-n) %>% 
      separate(pair, c("s1", "s2"), "_") %>% 
      mutate(across(c("s1", "s2"), as.integer))
    
    # systematically list all pairs, and add the counts for each
    # (filter for item_num > 0 to exclude the attention check item at this point)
    all_pairs_status <-
      crossing(scripts %>% filter(item_num > 0) %>% select(s1 = item_num),
               scripts %>% filter(item_num > 0) %>% select(s2 = item_num)) %>%
      filter(s1 < s2) %>%
      left_join(pairs_judged, by = c("s1", "s2")) %>%
      mutate(n = replace_na(n, 0))
    
    # 2. Select pairs from among the least judged so far
    
    pairs_to_judge <- tibble()
    
    while(nrow(pairs_to_judge) < pairs_to_make) {
      
      if(nrow(pairs_to_judge) == 0) {
        # start with the least judged pairs
        new_pairs_to_judge <- all_pairs_status %>% 
          filter(n == min(n)) %>% 
          slice_sample(n = pairs_to_make)
      } else {
        # but if we have selected some pairs already, remove them from
        # consideration (using anti_join) and look at the next-least-judged pairs
        new_pairs_to_judge <- all_pairs_status %>% 
          anti_join(pairs_to_judge, by = c("s1", "s2", "n")) %>% 
          filter(n == min(n)) %>% 
          slice_sample(n = pairs_to_make)
      }
      
      pairs_to_judge <- bind_rows(pairs_to_judge,
                                  new_pairs_to_judge)
    }
    
    return(pairs_to_judge %>%
             # trim to the desired number of pairs
             slice_head(n = pairs_to_make) %>% 
             # shuffle the scripts into left and right
             mutate(
               x = sample(c(0,1), size = pairs_to_make, replace = TRUE),
               left = ifelse(x==0, s1, s2),
               right = ifelse(x==0, s2, s1)
             ) %>% 
             select(left, right) %>%
             mutate(pair_num = row_number(), .before = 1)
    )
  }
  next_pair = function(old_pair_num) {
    # print("next_pair")
    # print(old_pair_num)
    # print(nrow(pairs))
    # move on to the next pair
    pair_to_return = old_pair_num + 1
    
    # if we've reached the end, add 20 more pairs to the list
    if(pair_to_return > nrow(pairs)) {
      pairs <<- pairs %>% bind_rows(make_pairs(pairs_to_make = 20) %>% mutate(pair_num = pair_num + old_pair_num))
      pair$pairs_available <- nrow(pairs)
      # print(pairs)
    }
    pairs %>% 
      filter(pair_num == pair_to_return) %>%
      head(1)
  }
  
  # initialise empty data structures, to be used when judging begins
  pair <- reactiveValues(
    pair_num = 0,
    pairs_available = 0,
    left = 1000,
    right = 1001
  )
  pairs <- tibble()
  
  observeEvent(input$startComparing, {
    
    # pairs <<- make_pairs(pairs_to_make = 100)
    pairs <<- make_pairs(pairs_to_make = 95) %>% 
      # add attention checks after pairs 15, 32, 50, 75, 90
      add_row(left = 0, right = sample(c(1:25))[1], .after = 15) %>% 
      add_row(right = 0, left = sample(c(1:25))[1], .after = 32) %>%
      add_row(left = 0, right = sample(c(1:25))[1], .after = 50) %>% 
      add_row(right = 0, left = sample(c(1:25))[1], .after = 75) %>%
      add_row(right = 0, left = sample(c(1:25))[1], .after = 90) %>%
      mutate(pair_num = row_number())
    print(pairs)
    pair$pairs_available <- nrow(pairs)
  
    first_pair = pairs %>% filter(pair_num == starting_pair)
    pair$pair_num <- first_pair$pair_num
    pair$left <- first_pair$left
    pair$right <- first_pair$right

    # print(pair)
    # print("OK")

    # update the page content
    output$pageContent <- renderUI({
      tagList(
        htmlOutput("judging_progress"),
        h3(assigned_study[["judging_prompt"]]),
        fluidRow(
          column(12, htmlOutput("binary"))
        ),
        fluidRow(
          column(12, htmlOutput("slider"))
        ),
        fluidRow(
          column(8, offset = 2, htmlOutput("comments"))
        )
      )
    })
    
    pair$start_time = Sys.time()
    print("Judging initialised")
    # print(pair)
  })
  
  vehicle_name <- function(item_id) {
    scripts %>% filter(item_num == item_id) %>% pull(markdown) %>% as.character()
  }

  output$comments <- renderUI({
    if(pair$pair_num > 0) {
      textAreaInput("judging_comment", label = "Comments (optional)", width = "100%", height = "4em")
    }
  })
  
  output$binary <- renderUI({
    if(judging_method == "binary") {
      fluidRow(
        column(6, actionButton(
          "chooseLeft",
          label = vehicle_name(pair$left),
          class = "btn-block btn-primary"
        )),
        column(6, actionButton(
          "chooseRight",
          label = vehicle_name(pair$right),
          class = "btn-block btn-primary"
        ))
      )
    }
  })
  
  output$slider <- renderUI({
    if(judging_method == "slider") {
      tagList(
        fluidRow(
          column(3, p(vehicle_name(pair$left), class = "slider_left"), style = "display: flex"),
          column(6, sliderInput("choice_slider", "",
                                min = -10, max = 10, value = 0, ticks = FALSE, width = "100%")
          ),
          #column(6, tags$input(id = "choice_slider", type = "range", min = "-10", max = "10", class = "cj_slider")),
          column(3, p(vehicle_name(pair$right), class = "slider_right"))
        ),
        fluidRow(
          column(4, offset = 4, p(actionButton("submit_slider", "Submit decision", class = "btn-primary"), style = "text-align: center;"))
        )
      )
    }
  })
  observe({
    if (is.null(input$choice_slider) || input$choice_slider == "0") {
      shinyjs::disable("submit_slider")
      shinyjs::html("submit_slider", html = "Please move the slider to indicate the strength of your decision")
    } else {
      shinyjs::enable("submit_slider")
      shinyjs::html("submit_slider", html = "Submit decision")
    }
  })
  
  output$judging_progress <- renderPrint({
    pc <- round((pair$pair_num -1) / 100 * 100)
    pc <- min(pc, 100)
    # https://getbootstrap.com/docs/3.4/components/#progress
    div(
      class = "progress",
      div(
        class = ifelse(pc < 100, "progress-bar", "progress-bar progress-bar-success"),
        role = "progressbar",
        `aria-valuenow` = pc,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = str_glue("min-width: 1em; width: {pc}%;"),
        pair$pair_num - 1
      )
    )
  })
  
  update_pair <- function() {
    new_pair <- next_pair(pair$pair_num)
    # print(new_pair)
    pair$pair_num <- new_pair$pair_num
    pair$left <- new_pair$left
    pair$right <- new_pair$right
    pair$start_time <- Sys.time()
  }
  
  elapsed_time <- function() {
    start_time <- pair$start_time
    current_time <- Sys.time()
    return( as.integer((current_time - start_time) * 1000) )
  }
  
  record_judgement_binary <- function(pair, winner = "left", loser = "right") {
    print(paste(pair$left, pair$right, "winner:", winner))
    time_taken = elapsed_time()
    
    winning_item = ifelse(winner == "left", pair$left, pair$right)
    losing_item = ifelse(loser == "left", pair$left, pair$right)
    
    dbWriteTable(
      pool,
      "judgements",
      tibble(
        study = session_info$study_id,
        judge_id = session_info$judge_id,
        left = pair$left,
        right = pair$right,
        won = winning_item,
        lost = losing_item,
        time_taken = time_taken,
        comment = input$judging_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
  }
  record_judgement_slider <- function(pair, score = 0) {
    print(paste(pair$left, pair$right, "score:", score))
    time_taken = elapsed_time()
    
    dbWriteTable(
      pool,
      "judgements",
      tibble(
        study = session_info$study_id,
        judge_id = session_info$judge_id,
        left = pair$left,
        right = pair$right,
        score = score,
        time_taken = time_taken,
        comment = input$judging_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
  }
  observeEvent(input$chooseLeft, {
    record_judgement_binary(pair, winner = "left", loser = "right")
    update_pair()
  })
  observeEvent(input$chooseRight, {
    record_judgement_binary(pair, winner = "right", loser = "left")
    update_pair()
  })
  observeEvent(input$submit_slider, {
    record_judgement_slider(pair, score = input$choice_slider)
    update_pair()
  })
  
  # Give a message when they reach the required number of comparisons
  # TODO - make this return them to the special Prolific landing page that will mark them as completed
  observe({
    if(!exists("pair")) return()
    if (pair$pair_num <= 100) return()
    
    # update the page content
    output$pageContent <- renderUI({
      tagList(
        htmlOutput("judging_progress"),
        h3("Thank you!"),
        p("You have now completed the comparisons needed for this survey."),
        p("Thank you for taking part."),
        p("If you have any comments about the judging process, please leave them here:"),
        fluidRow(
          column(8, offset = 2, htmlOutput("final_comments"))
        ),
        fluidRow(
          column(4, offset = 4, actionButton("saveFinalComments", "Save and return to Prolific", class = "btn-success btn-lg btn-block", icon = icon("check")))
        )
      )
    })
  })
  output$final_comments <- renderUI({
    if(pair$pair_num > 0) {
      textAreaInput("final_comment", label = "Comments (optional)", width = "100%", height = "6em")
    }
  })
  observeEvent(input$saveFinalComments, {
    dbWriteTable(
      pool,
      "comments",
      tibble(
        judge_id = session_info$judge_id,
        final_comments = input$final_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
    prolific_completion_url <- "https://app.prolific.co/submissions/complete?cc=64DD2AF9"
    output$pageContent <- renderUI({
      tagList(
        p("Saved", style = "text-align:center"),
        p("Redirecting to Prolific", style = "text-align:center"),
        p(prolific_completion_url, style = "text-align:center"),
        tags$script(paste0('window.location.replace("',prolific_completion_url,'");'))
      )
    })
  })

  #
  # Admin dashboard
  #
  output$judge_tally <- renderTable({
    study_status %>%
      arrange(study) %>% 
      separate(study, into = c("prompt", "method")) %>% 
      select(-judging_method, -judgements_per_judge)
  })
  output$summary_table <- renderTable({
    judges %>%
      select(study_id, judge_id, prolific_id, num_judgements, attention_checks_passed, time_spent_s) %>% 
      arrange(study_id, -num_judgements)
  })
  
  output$participants_table <- renderDT(
    judges %>% select(-shiny_info) %>% arrange(-judge_id),
    filter = "top",
    options = list(pageLength = 20),
    rownames = FALSE
  )
  # Downloadable csv of judges dataset ----
  output$download_judges <- downloadHandler(
    filename = "judges.csv",
    content = function(file) {
      write_csv(judges, file, na = "")
    }
  )
  
  output$judgements_table <- renderDT(
    all_existing_judgements,
    filter = "top",
    options = list(pageLength = 20),
    rownames = FALSE
  )
  # Downloadable csv of judges dataset ----
  output$download_judgements <- downloadHandler(
    filename = "judgements.csv",
    content = function(file) {
      write_csv(all_existing_judgements, file, na = "")
    }
  )
  
}

shinyApp(ui, server)
