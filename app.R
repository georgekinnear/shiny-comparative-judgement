library(tidyverse)
library(shiny)
library(shinyjs)
library(sortable)
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
# studies <- pool %>% 
#   tbl("studies") %>% 
#   collect()
studies <- tibble::tribble(
  ~study,                                                                        ~judging_prompt, ~judging_method, ~target_judges, ~judgements_per_judge,
  "regular_cj",                                      "Which is the better item?",        "binary",            20L,                  30L,
  "chained_cj",                                      "Which is the better item?",        "binary",            20L,                  30L,
)

scripts <- read_yaml("items-to-be-judged.yml") %>%
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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
/* custom CSS can go here, but also in the external www/styles.css file */
    "))
  ),
  
  # Placeholder for page content - the server will update this as needed
  uiOutput("pageContent")
  uiOutput("pageContent"),
  uiOutput("debugging")
)



server <- function(input, output, session) {
  
  # These will be global variables within each session
  assigned_study <- NULL
  session_info <- NULL
  judge_id <- NULL
  judge_code <- NULL
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
    # left_join(
    #   pool %>% tbl("comments") %>% 
    #     collect(),
    #   by = "judge_id"
    # ) %>% 
    left_join(
      # attention checks
      all_existing_judgements %>% 
        group_by(judge_id) %>% 
        filter(left == 0 | right == 0) %>% 
        mutate(
          attention_check_result = case_when(
            won == 0 ~ TRUE,
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
        select(judge_id, judge_code, study_id, num_judgements, attention_checks_passed),
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
    
    if (isTruthy(query[['JUDGE']])) {
      judge_code <<- query[['JUDGE']]
      # Check if this user already exists in the DB: if so, pick up from where they left off
      session_info <<- pool %>% tbl("judges") %>%
        filter(shiny_info == !!judge_code) %>%
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
    } else if (isTruthy(query[['ADMIN_USER']]=="AdminPassword123")) {
      # admin dashboard
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
      # TODO - make this assign them a code
      # judge_code <<- "None"
      # # Quit with a warning
      # output$pageContent <- renderUI({
      #   tagList(
      #     h3("Judge code not found"),
      #     p("The JUDGE is missing from the URL."),
      #   )
      # })
      starting_pair <<- 1
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
        study_id = assigned_study[["study"]]
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
  
  make_tuple <- function() {
    
    # 1. Gather data on which judgements have been made already in this study group
    
    # all comparisons from this study
    judgement_data <- pool %>% 
      tbl("rankings") %>% 
      #filter(study == !!session_info$study_id) %>% 
      select(ranking_id, items_presented) %>% 
      collect() %>% 
      separate_rows(items_presented, sep = ",")
    
    # count the number of comparisons for each pair
    pairs_judged <- judgement_data %>% 
      left_join(judgement_data, by = "ranking_id") %>% 
      rename(left = items_presented.x, right = items_presented.y) %>% 
      # strip out the spurious self-comparisons
      filter(left != right) %>%
      # keep only the pairs in order e.g. 1_2 not 2_1
      rowwise() %>% 
      # put the pairs in order, with item IDs as integers
      mutate(across(c("left", "right"), as.integer)) %>% 
      mutate(pair = paste(sort(c(left, right)), collapse = '_')) %>% 
      select(pair, ranking_id) %>% 
      distinct() %>% 
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
    
    # 2. Select tuples that include pairs from among the least judged so far
    
    # (i) pick one of the least judged pairs, (A,B)
    base_pair = all_pairs_status %>% 
      filter(n == min(n)) %>% 
      slice_sample(n = 1)
    A <- base_pair["s1"] %>% deframe()
    B <- base_pair["s2"] %>% deframe()
    
    # (ii) pick novel pairs involving A and B that are among the least judged
    C <- all_pairs_status %>% 
      filter(s1 == A | s2 == A) %>% 
      filter(s1 != B, s2 != B) %>% 
      filter(n == min(n)) %>% 
      slice_sample(n = 1) %>% 
      mutate(c = if_else(s1 == A, s2, s1)) %>% 
      select(c) %>% 
      deframe()
    D <- all_pairs_status %>% 
      filter(s1 == B | s2 == B) %>% 
      filter(s1 != A, s2 != A, s1 != C, s2 != C) %>% 
      filter(n == min(n)) %>% 
      slice_sample(n = 1) %>% 
      mutate(d = if_else(s1 == B, s2, s1)) %>% 
      select(d) %>% 
      deframe()
    
    # (iii) pick a further novel pair, being one of the least judged pairs that have
    #       one of A,B,C,D paired with another item
    E <- all_pairs_status %>% 
      filter((s1 %in% c(A,B,C,D) & !s2 %in% c(A,B,C,D)) | (!s1 %in% c(A,B,C,D) & s2 %in% c(A,B,C,D))) %>% 
      filter(n == min(n)) %>% 
      slice_sample(n = 1) %>% 
      mutate(e = if_else(s1 %in% c(A,B,C,D), s2, s1)) %>% 
      select(e) %>% 
      deframe()
    
    return(c(A,B,C,D,E))
    
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
  # 
  #  Demo of the sortable interface
  # 
  observeEvent(input$startComparing_DEMO, {
    tuple <- make_tuple()
    print(tuple)
    
    output$pageContent <- renderUI({
      tagList(
        h3("Ranking"),
        p(tuple %>% paste0(collapse = ", ")),
        # rank_list(
        #   text = "You can drag, drop and re-order these items:",
        #   labels = tuple,
        #   input_id = "ranking"
        # ),
        div(id = "items_to_be_ranked",
          div(class = "item_to_rank", id = "ranking1", `data-rank-id` = tuple[1], item_name(tuple[1])),
          div(class = "item_to_rank", id = "ranking2", `data-rank-id` = tuple[2], item_name(tuple[2])),
          div(class = "item_to_rank", id = "ranking3", `data-rank-id` = tuple[3], item_name(tuple[3])),
          div(class = "item_to_rank", id = "ranking4", `data-rank-id` = tuple[4], item_name(tuple[4])),
          div(class = "item_to_rank", id = "ranking5", `data-rank-id` = tuple[5], item_name(tuple[5]))
        ),
        sortable_js(
          css_id = "items_to_be_ranked",
          options = sortable_options(
            onSort = sortable_js_capture_input(input_id = "ranked_items")
          )
        ),
        fluidRow(
          column(4, offset = 4, p(actionButton("submit_ranking", "Submit decision", class = "btn-primary"), style = "text-align: center;"))
        )
      )
    })
  })
  
  observeEvent(input$submit_ranking, {
    print(input$ranked_items)
    output$pageContent <- renderUI({
      tagList(
        h3("Result"),
        p(input$ranked_items %>% paste0(collapse = ", "))
      )
    })
  })
  
  observeEvent(input$startComparing, { # TODO - delete the _REAL to make this function as normal
    
    # TODO - instead of hard-coded 30, get it to work using assigned_study[["judgements_per_judge"]]
    pairs <<- make_pairs(pairs_to_make = 30) %>% 
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
  
  item_name <- function(item_id) {
    scripts %>% filter(item_num == item_id) %>% pull(markdown) %>% as.character() %>% HTML() %>% withMathJax()
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
          label = item_name(pair$left),
          class = "btn-block btn-primary"
        )),
        column(6, actionButton(
          "chooseRight",
          label = item_name(pair$right),
          class = "btn-block btn-primary"
        ))
      )
    }
  })
  
  output$slider <- renderUI({
    if(judging_method == "slider") {
      tagList(
        fluidRow(
          column(3, p(item_name(pair$left), class = "slider_left"), style = "display: flex"),
          column(6, sliderInput("choice_slider", "",
                                min = -10, max = 10, value = 0, ticks = FALSE, width = "100%")
          ),
          #column(6, tags$input(id = "choice_slider", type = "range", min = "-10", max = "10", class = "cj_slider")),
          column(3, p(item_name(pair$right), class = "slider_right"))
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
    # TODO - replace the hard-coded 30
    pc <- round((pair$pair_num -1) / 30 * 100)
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
    prolific_completion_url <- "https://app.prolific.co/submissions/complete?cc=XXXXX"
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
      select(study_id, judge_id, num_judgements, attention_checks_passed, time_spent_s) %>% 
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
