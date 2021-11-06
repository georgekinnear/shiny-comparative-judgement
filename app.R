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
  ~study,    ~judging_prompt,             ~target_judges,
  "cj_rank", "Which is the better item?", 20L,
  "rank_cj", "Which is the better item?", 20L,
)

study_pages <- list(
  "cj_rank" = c("instructions_cj",
                paste0("cj", c(1:15)),
                "instructions_rank",
                paste0("rank", c(1:5)),
                "evaluation",
                "thanks"
              ),
  "rank_cj" = c("instructions_rank",
                paste0("rank", c(1:5)),
                "instructions_cj",
                paste0("cj", c(1:15)),
                "evaluation",
                "thanks"
  )
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
  
  # print(session)
  
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
    print("Checking the URL")
    query <- parseQueryString(isolate(session$clientData$url_search))
    
    if (isTruthy(query[['JUDGE']])) {
      
      judge_code <<- query[['JUDGE']]
      print(judge_code)
      # Check if this user already exists in the DB: if so, pick up from where they left off
      session_info <<- pool %>% tbl("judges") %>%
        filter(shiny_info == !!judge_code) %>%
        collect() %>%
        arrange(-judge_id) %>%
        slice(1)
      
      if(nrow(session_info) > 0) {
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
      page_to_show$page <- "step0-participant-info"
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
    
    redirect_to_url <- paste0("../?JUDGE=", session_info$shiny_info)
    
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
  
  observe({
    # Step 0: Participant information sheet
    if (page_to_show$page == "step0-participant-info") {
      output$pageContent <- renderUI({
        tagList(
          includeMarkdown("step0-participant-info.md"),
          fluidRow(
            column(4, offset = 4, actionButton("consentButton", "I consent", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
    if (page_to_show$page %in% c("instructions_cj", "instructions_rank")) {
      output$pageContent <- renderUI({
        tagList(
          h3("Instructions"),
          markdown::markdownToHTML(text = read_file(paste0("PAGE_", page_to_show$page, ".md")),
                                   fragment.only = TRUE) %>% 
            str_replace("\\[JUDGING PROMPT\\]", assigned_study[["judging_prompt"]]) %>% HTML() %>% withMathJax(),
          fluidRow(
            column(4, offset = 4, actionButton(paste0("completed_", page_to_show$page), "Start comparing", class = "btn-success btn-lg btn-block", icon = icon("check")))
          )
        )
      })
    }
    if (str_starts(page_to_show$page, "cj")) {
      print(paste0("Now showing page", page_to_show$page))
      pair <- pairs %>% filter(page == page_to_show$page)
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
  
  advance_page <- function() {
    print("Advancing page - current page list is")
    print(remaining_pages)
    remaining_pages <<- remaining_pages[-1]
    print(remaining_pages)
    print("Moving to page:")
    print(remaining_pages[[1]])
    page_to_show$page <<- remaining_pages[[1]]
  }
  
  observeEvent(input$completed_instructions_cj, {
    
    # find out how many pairs are needed by looking at the list of remaining pages
    cj_pages <- tibble(page = remaining_pages) %>%
      filter(str_starts(page, "cj")) %>% 
      mutate(pair_num = row_number())
    num_pairs_to_make <- cj_pages %>% nrow()
    # TODO - worry about if this is ever 0?
    
    # use the make_cj_pairs function from cj_functions.R to produce suitable pairs in this study
    pairs <<- make_cj_pairs(pairs_to_make = num_pairs_to_make,
                            restrict_to_study_id = session_info$study_id) %>% 
      left_join(cj_pages)
    print(pairs)
    print("Judging initialised")

    advance_page()
  })

  #
  # Judging
  #
  
  # initialise empty data structures, to be used when judging begins
  pair <- reactiveValues(
    pair_num = 0,
    pairs_available = 0,
    left = 1000,
    right = 1001
  )
  pairs <- tibble()
  
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
  
  render_item_panel <- function(button_id, item_id) {
    tagList(
      div(class = "item_panel",
          fluidRow(
            actionButton(button_id, "Choose this one", class = "btn-block btn-primary")
          ),
          div(class = "item_content", display_item(item_id))
      )
    )
  }
  display_item <- function(item_id) {
    the_item <- scripts %>% filter(item_num == item_id)
    if(str_length(the_item$html %>% as.character()) > 0) {
      return(the_item$html %>% as.character() %>% HTML() %>% withMathJax())
    } else {
      return(img(src = the_item$img_src, class = "comparison-image"))
    }
  }
  
  item_name <- function(item_id) {
    scripts %>% filter(item_num == item_id) %>% pull(markdown) %>% as.character() %>% HTML() %>% withMathJax()
  }

  output$comments <- renderUI({
    if(pair$pair_num > 0) {
      textAreaInput("judging_comment", label = "Comments (optional)", width = "100%", height = "4em")
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
