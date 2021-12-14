library(tidyverse)
library(shiny)
library(shinyjs)
library(sortable) # for the ranking
library(shinyWidgets) # for the slider
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
# TODO - expand this to describe the set of groups that we want in the study
studies <- tibble::tribble(
  ~study,    ~judging_prompt,             ~target_judges,
  "cj_rank", "Which is the best summary of the proof?", 8L,
  "rank_cj", "Which is the best summary of the proof?", 8L,
  "cj_ccj",  "Which is the best summary of the proof?", 8L,
  "ccj_cj",  "Which is the best summary of the proof?", 8L
)

method_labels <- c(
  "cj" = "Comparative judgement",
  "ccj" = "Comparative judgement (chained pairs)",
  "rank" = "Ranking"
)

# Define the pages that each group will be shown in order
# - for each group, provide a list of strings, where the content of the string
#   matches the name of a page to show
study_pages <- list(
  "cj_rank" = c("instructions_proof",
                "instructions_cj",
                paste0("cj", c(1:15)),
                "evaluate_cj",
                "instructions_rank",
                paste0("rank", c(1:5)),
                "evaluate_rank",
                "evaluation",
                "thanks"
              ),
  "rank_cj" = c("instructions_proof",
                "instructions_rank",
                paste0("rank", c(1:5)),
                "evaluate_rank",
                "instructions_cj",
                paste0("cj", c(1:15)),
                "evaluate_cj",
                "evaluation",
                "thanks"
  ),
  "cj_ccj" = c("instructions_proof",
               "instructions_cj",
               paste0("cj", c(1:15)),
               "evaluate_cj",
               "instructions_ccj",
               paste0("ccj", c(1:15)),
               "evaluate_ccj",
               "evaluation",
               "thanks"),
  "ccj_cj" = c("instructions_proof",
               "instructions_ccj",
               paste0("ccj", c(1:15)),
               "evaluate_ccj",
               "instructions_cj",
               paste0("cj", c(1:15)),
               "evaluate_cj",
               "evaluation",
               "thanks")
)

scripts <- read_yaml("items-to-be-judged.yml") %>%
  purrr::map(as_tibble_row) %>%
  enframe(name = NULL) %>%
  unnest(cols = c("value")) %>% 
  rename_with(~ str_replace(., "-", "_")) %>%
  rename(markdown = html)


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
  uiOutput("overall_progress"),
  uiOutput("pageContent"),
  uiOutput("debugging")
)



server <- function(input, output, session) {
  
  # This file contains helper functions that implement the approach to choosing pairs/tuples to compare
  # (local = TRUE is needed to ensure the functions have the right scope, e.g. access to the pool database connection)
  source("cj_functions.R", local = TRUE)
  
  # These will be global variables within each session
  assigned_study <- NULL
  pages_to_show <- NULL
  session_info <- NULL
  judge_id <- NULL
  judge_code <- NULL
  current_page <- NULL # TODO - perhaps redundant, use page_to_show reactive?
  
  page_to_show <- reactiveValues(
    page = "step0-participant-info"
  )
  
  #
  # Check on judging progress
  #
  all_existing_judgements <<- pool %>% 
    tbl("decisions") %>% 
    select(-contains("comment")) %>% 
    collect()
  
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
    )
  
  study_progress <<- studies %>% 
    full_join(
      judges %>% 
        select(judge_id, shiny_info, study_id, num_judgements),
      by = c("study" = "study_id")
    )
  study_status <<- studies %>% 
    left_join(
      study_progress %>%
        group_by(study) %>%
        summarise(
          num_judges = n_distinct(judge_id),
          # TODO - this is not correct, but it may be hard to do if the numbers differ by study based on study_pages
          num_judges_completed = sum(num_judgements == 100, na.rm = TRUE),
          num_judgements = sum(num_judgements, na.rm = TRUE)
        ),
      by = "study"
    ) %>% 
    mutate(across(starts_with("num_"), ~replace_na(.x, 0L)))
  
  observe({
    # Check the URL for parameters e.g. ?judge=123
    # This is used to give each judge a unique URL that they can reload, or return to later
    query <- parseQueryString(session$clientData$url_search)

    if (isTruthy(query[['JUDGE']])) {
      
      if(isTruthy(judge_code) && query[['JUDGE']] == judge_code) {
        # this is the same judge as the current session, so nothing needs to be done
        print("DEBUG: Continuing judge, page reload")
        return()
      }
      
      judge_code <<- query[['JUDGE']]
      print(judge_code)
      # Check if this user already exists in the DB: if so, pick up from where they left off
      session_info <<- pool %>% tbl("judges") %>%
        filter(shiny_info == !!judge_code) %>%
        collect() %>%
        arrange(-judge_id) %>%
        slice(1)
      
      if(nrow(session_info) > 0) {
        # Pick up where this user left off
        
        # Save information about the judging session to global variables for easy reference
        judge_id <<- session_info$judge_id
        assigned_study <<- study_status %>% filter(study == !!session_info$study_id)
        pages_to_show <<- study_pages[[session_info$study_id]]
        print(pages_to_show)
        
        # User has already consented - find out where they got to, and pick up where they left off
        current_judge_existing_judgements <- pool %>% tbl("decisions") %>%
          filter(judge_id == !!session_info$judge_id) %>% 
          collect()
        
        # Take the full list of study_pages and subtract any that have already been completed
        # TODO - perhaps worry about deleting any superfluous pages, e.g. instructions_cj if there are no more cj judging pages left
        remaining_pages <<- setdiff(pages_to_show,
                                   current_judge_existing_judgements %>% select(step) %>% deframe())
        page_to_show$page <- remaining_pages[[1]]
        page_to_show$remaining_pages <- remaining_pages
        print(page_to_show$page)
      } else {
        # ID is not recognised
        output$pageContent <- renderUI({
          # TODO - get this to be the link to the survey homepage
          redirect_to_url <- "LINK GOES HERE"
          tagList(
            p("Judge ID not recognised", style = "text-align:center"),
            p("Please try restarting the survey:", style = "text-align:center"),
            p(redirect_to_url, style = "text-align:center")
            #tags$script(paste0('window.location.replace("',redirect_to_url,'");'))
          )
        })
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
      # When there is no JUDGE id in the URL, show the consent form
      # This is the default page_to_show anyway, as defined above:
      # page_to_show$page <- "step0-participant-info"
    }
  })

  #
  # Participant has consented - send them to their unique URL
  #
  observeEvent(input$consentButton, {
    
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
    
    redirect_to_url <- paste0("?JUDGE=", session_info$shiny_info)
    
    print(paste0("Redirecting judge ", session_info$judge_id, " to ", redirect_to_url))
    output$pageContent <- renderUI({
      tagList(
        p("Thank you!", style = "text-align:center"),
        p("Redirecting to the next step:", style = "text-align:center"),
        p(redirect_to_url, style = "text-align:center"),
        tags$script(paste0('window.location.replace("',redirect_to_url,'");'))
      )
    })
    
  })
  
  # Helper function to move the user to the next page
  advance_page <- function() {
    remaining_pages <<- remaining_pages[-1]
    print("Moving to page:")
    print(remaining_pages[[1]])
    page_to_show$page <<- remaining_pages[[1]]
    page_to_show$remaining_pages <<- remaining_pages
  }
  
  elapsed_time <- function() {
    start_time <- page_to_show$start_time
    current_time <- Sys.time()
    return( as.integer((current_time - start_time) * 1000) )
  }
  
  # Box for comments used across all judging pages 
  output$comments <- renderUI({
    if (str_starts(page_to_show$page, "cj") | str_starts(page_to_show$page, "ccj") | str_starts(page_to_show$page, "rank")) {
      textAreaInput("judging_comment", label = "Comments (optional)", width = "100%", height = "4em")
    }
  })
  
  
  #
  # Information pages
  #
  observe({
    # Step 0: Participant information sheet
    if (page_to_show$page == "step0-participant-info") {
      
      all_scripts_html = scripts %>%
        mutate(html_out = purrr::map(markdown, ~ markdown::markdownToHTML(
          text = .,
          fragment.only = TRUE
        ))) %>% 
        mutate(html_out = paste0("<h2>Script ", item_num, "</h2>", html_out))
      
      output$pageContent <- renderUI({
        tagList(
          includeMarkdown("step0-participant-info.md"),
          fluidRow(
            column(4, offset = 4, actionButton("consentButton", "I consent", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )#,
          #h1("For debugging purposes, these are the scripts:"),
          #paste(all_scripts_html$html_out, collapse = "") %>% HTML() %>% withMathJax()
        )
      })
    }
    # Instruction pages
    if (page_to_show$page == "instructions_proof") {
      output$pageContent <- renderUI({
        tagList(
          markdown::markdownToHTML(text = read_file(paste0("PAGE_", page_to_show$page, ".md")),
                                   fragment.only = TRUE) %>% HTML() %>% withMathJax(),
          fluidRow(
            column(4, offset = 4, actionButton(paste0("completed_", page_to_show$page), "Continue", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
    if (page_to_show$page %in% c("instructions_cj","instructions_ccj","instructions_rank")) {
      output$pageContent <- renderUI({
        tagList(
          #h3("Instructions"),
          markdown::markdownToHTML(text = read_file(paste0("PAGE_", page_to_show$page, ".md")),
                                   fragment.only = TRUE) %>% 
            str_replace("\\[JUDGING PROMPT\\]", assigned_study[["judging_prompt"]]) %>% HTML() %>% withMathJax(),
          fluidRow(
            column(4, offset = 4, actionButton(paste0("completed_", page_to_show$page), "Start comparing", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
  })
    
  #
  # Participant feedback
  #
  observe({
    if (page_to_show$page %in% c("evaluate_cj", "evaluate_ccj", "evaluate_rank")) {
      output$pageContent <- renderUI({
        tagList(
          h3("Your opinions about this method"),
          p(""),
          sliderTextInput(
            inputId = "method_rating",
            label = "How easy did you find it to make your decisions?",
            grid = TRUE,
            #force_edges = TRUE,
            choices = c("Very difficult", "Difficult", "Neutral", "Easy", "Very easy"),
            selected = "Neutral",
            width = "400px"
          ),
          textAreaInput("method_comment", label = "What difficulties (if any) did you encounter?", width = "100%", height = "6em"),
          fluidRow(
            column(4, offset = 4, actionButton("completed_method_eval", "Submit", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
    if (page_to_show$page == "evaluation") {
      methods_list <- paste0("Method ", c(1:2), ": ", method_labels[str_split(session_info$study_id, "_")[[1]]])
      output$pageContent <- renderUI({
        tagList(
          h3("Your opinions"),
          p("Which of the two methods did you prefer, and why?"),
          radioButtons(
            inputId = "eval_rating",
            label = "",
            choices = methods_list,
            selected = character(0)
          ),
          textAreaInput("eval_comment", label = "", width = "100%", height = "6em"),
          fluidRow(
            column(4, offset = 4, actionButton(paste0("completed_", page_to_show$page), "Submit", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
    if (page_to_show$page == "thanks") {
      output$pageContent <- renderUI({
        tagList(
          h3("Thank you!"),
          markdown::markdownToHTML(text = read_file(paste0("PAGE_", page_to_show$page, ".md")),
                                   fragment.only = TRUE) %>% HTML() %>% withMathJax()
        )
      })
    }
  })
  observeEvent(input$completed_method_eval, {
    # save their answers to the database
    dbWriteTable(
      pool,
      "decisions",
      tibble(
        judge_id = session_info$judge_id,
        step = page_to_show$page,
        decision = input$method_rating,
        time_taken = 0,
        comment = input$method_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
    advance_page()
  })
  observeEvent(input$completed_evaluation, {
    validate(
      need(input$eval_rating, 'Please select which method you preferred.')
    )
    # save their answers to the database
    dbWriteTable(
      pool,
      "decisions",
      tibble(
        judge_id = session_info$judge_id,
        step = page_to_show$page,
        decision = input$eval_rating,
        time_taken = 0,
        comment = input$eval_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
    advance_page()
  })
  
  observeEvent(input$completed_instructions_proof, {
    advance_page()
  })
  #
  # Traditional comparative judgement
  #
  
  # Once the judge has read the instructions, generate the full list of pairs to be judged
  observeEvent(input$completed_instructions_cj, {
    
    # find out how many pairs are needed by looking at the list of remaining pages
    cj_pages <- tibble(page = remaining_pages) %>%
      filter(str_starts(page, "cj")) %>% 
      mutate(pair_num = row_number())
    num_pairs_to_make <- cj_pages %>% nrow()
    # TODO - worry about if this is ever 0?
    
    # use the make_cj_pairs function from cj_functions.R to produce suitable pairs in this study
    pairs_to_judge <<- make_cj_pairs(pairs_to_make = num_pairs_to_make,
                                     restrict_to_study_id = session_info$study_id) %>% 
      left_join(cj_pages, by = "pair_num")
    print(pairs_to_judge)
    print("Judging initialised")
    
    advance_page()
  })
  
  #copy for completed_instructions_ccj
  observeEvent(input$completed_instructions_ccj, {
    
    # find out how many pairs are needed by looking at the list of remaining pages
    ccj_pages <- tibble(page = remaining_pages) %>%
      filter(str_starts(page, "ccj")) %>% 
      mutate(pair_num = row_number())
    num_pairs_to_make <- ccj_pages %>% nrow()
    # TODO - worry about if this is ever 0?
    
    # use the make_ccj_pairs function from cj_functions.R to produce suitable pairs in this study
    pairs_to_judge <<- make_ccj_pairs(pairs_to_make = num_pairs_to_make,
                                     restrict_to_study_id = session_info$study_id) %>% 
      left_join(ccj_pages, by = "pair_num")
    print(pairs_to_judge)
    print("Judging initialised")
    
    advance_page()
  })
  
  
  # Set up the page for paired comparison
  observe({
    if (str_starts(page_to_show$page, "cj") | str_starts(page_to_show$page, "ccj")) {
      page_to_show$start_time <- Sys.time()
      print(paste0("Now showing page: ", page_to_show$page))
      pair <<- pairs_to_judge %>% filter(page == page_to_show$page)
      output$pageContent <- renderUI({
        tagList(
          h3(assigned_study[["judging_prompt"]]),
          fluidRow(
            column(6, render_item_panel("chooseLeft", pair$left)),
            column(6, render_item_panel("chooseRight", pair$right))
          ),
          fluidRow(
            column(8, offset = 2, htmlOutput("comments"))
          )
        )
      })
    }
  })
  
  render_item_panel <- function(button_id, item_id) {
    tagList(
      div(class = "item_panel",
          fluidRow(
            actionButton(button_id, "Choose this one", class = "btn-block btn-primary")
          ),
          div(class = "item_content", item_content(item_id))
      )
    )
  }
  item_content <- function(item_id) {
    the_item <- scripts %>% filter(item_num == item_id)
    if(str_length(the_item$markdown %>% as.character()) > 0) {
      return(the_item$markdown %>% as.character() %>% HTML() %>% withMathJax())
    } else {
      return(img(src = the_item$img_src, class = "comparison-image"))
    }
  }
  record_judgement_binary <- function(pair, winner = "left", loser = "right") {
    print(paste(pair$left, pair$right, "winner:", winner))
    time_taken = elapsed_time()
    
    winning_item = ifelse(winner == "left", pair$left, pair$right)
    losing_item = ifelse(loser == "left", pair$left, pair$right)
    
    dbWriteTable(
      pool,
      "decisions",
      tibble(
        judge_id = session_info$judge_id,
        step = page_to_show$page,
        decision = str_glue("{pair$left},{pair$right} -> {winning_item},{losing_item}"),
        time_taken = time_taken,
        comment = input$judging_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
  }
  observeEvent(input$chooseLeft, {
    record_judgement_binary(pair, winner = "left", loser = "right")
    advance_page()
  })
  observeEvent(input$chooseRight, {
    record_judgement_binary(pair, winner = "right", loser = "left")
    advance_page()
  })
  
  
  
  #
  # Rank ordering
  #
  
  # When the judge has read the instructions, there is nothing to do but send them to the next page
  observeEvent(input$completed_instructions_rank, {
    print("Ranking initialised")
    advance_page()
  })
  
  # Set up the page for rank ordering - this generates a new tuple to rank when the page is loaded
  observe({
    if (str_starts(page_to_show$page, "rank")) {
      print(paste0("Now showing page: ", page_to_show$page))
      
      # Generate the tuple to rank
      items_to_rank <<- make_tuple(restrict_to_study_id = session_info$study_id)
      print(items_to_rank)
      
      # Record the start time for this decision
      page_to_show$start_time <- Sys.time()
      
      output$pageContent <- renderUI({
        tagList(
          h3(assigned_study[["judging_prompt"]]),
          h4("BEST", align = "center"),
          # The "items_to..." id/class values are used to style the items using CSS
          # The `data-rank-id` is used by sortable_js to keep track of the item id's after the judge has moved them around
          div(id = "items_to_be_ranked",
              div(class = "item_to_rank", id = "ranking1", `data-rank-id` = items_to_rank[1], item_content(items_to_rank[1])),
              div(class = "item_to_rank", id = "ranking2", `data-rank-id` = items_to_rank[2], item_content(items_to_rank[2])),
              div(class = "item_to_rank", id = "ranking3", `data-rank-id` = items_to_rank[3], item_content(items_to_rank[3])),
              div(class = "item_to_rank", id = "ranking4", `data-rank-id` = items_to_rank[4], item_content(items_to_rank[4])),
              div(class = "item_to_rank", id = "ranking5", `data-rank-id` = items_to_rank[5], item_content(items_to_rank[5]))
          ),
          h4("WORST", align = "center"),
          sortable_js(
            css_id = "items_to_be_ranked",
            options = sortable_options(
              # Set the ranked_items input value to the original unsorted order
              onLoad = sortable_js_capture_input(input_id = "ranked_items"),
              # Update ranked_items whenever the user re-sorts the list
              onSort = sortable_js_capture_input(input_id = "ranked_items")
            )
          ),
          fluidRow(
            column(8, offset = 2, htmlOutput("comments"))
          ),
          fluidRow(
            column(4, offset = 4, p(actionButton("submit_ranking", "Submit decision", class = "btn-primary"), style = "text-align: center;"))
          )
        )
      })
    }
  })
  
  observeEvent(input$submit_ranking, {
    record_judgement_tuple(items_to_rank, input$ranked_items)
    advance_page()
  })
  record_judgement_tuple <- function(items_presented, items_ranked) {
    time_taken = elapsed_time()
    
    dbWriteTable(
      pool,
      "decisions",
      tibble(
        judge_id = session_info$judge_id,
        step = page_to_show$page,
        decision = str_glue("{paste0(items_presented, collapse = ',')} -> {paste0(items_ranked, collapse = ',')}"),
        time_taken = time_taken,
        comment = input$judging_comment
      ),
      row.names = FALSE,
      append = TRUE
    )
  }
  
  
  
  
  output$overall_progress <- renderPrint({
    if(!isTruthy(pages_to_show)) {
      pc <- 0
    } else {
      num_pages_total <- length(pages_to_show)
      # remaining_pages includes the current page, so subtract 1 to get the number of subsequent pages
      num_pages_left <- length(page_to_show$remaining_pages) - 1
      num_pages_completed <- num_pages_total - num_pages_left
      
      pc <- round(num_pages_completed / num_pages_total * 100)
      pc <- min(pc, 100)
    }

    # https://getbootstrap.com/docs/3.4/components/#progress
    div(
      class = "progress",
      div(
        class = ifelse(pc < 100, "progress-bar", "progress-bar progress-bar-success"),
        role = "progressbar",
        `aria-valuenow` = pc,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = str_glue("min-width: 1em; width: {pc}%;")
      )
    )
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