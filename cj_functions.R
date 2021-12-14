make_cj_pairs <- function(pairs_to_make = 20, restrict_to_study_id = NULL) {
  
  # 1. Gather data on which judgements have been made already in this study group
  
  # all comparisons from this study
  judgement_data <- pool %>% 
    tbl("decisions") %>% 
    select(-contains("comment")) %>% 
    collect() %>% 
    filter(str_starts(step, "cj")) %>% 
    separate(decision, into = c("pair_shown", "winner_loser"), sep = " -> ") %>% 
    separate(pair_shown, into = c("left", "right"), sep = ",") %>% 
    separate(winner_loser, into = c("won", "lost"), sep = ",") %>% 
    mutate(across(c("left", "right", "won", "lost"), as.integer))
  
  if(length(restrict_to_study_id) > 0) {
    judgement_data <- judgement_data %>%
      left_join(
        pool %>% tbl("judges") %>% 
          select(judge_id, study_id) %>% 
          collect(),
        by = "judge_id"
      ) %>% 
      filter(study_id == !!restrict_to_study_id)
  }
  
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

make_ccj_pairs <- function(pairs_to_make = 20, restrict_to_study_id = NULL) {
  
  # 1. Gather data on which judgements have been made already in this study group
  
  # all comparisons from this study
  judgement_data <- pool %>% 
    tbl("decisions") %>% 
    select(-contains("comment")) %>% 
    collect() %>% 
    filter(str_starts(step, "ccj")) %>% 
    separate(decision, into = c("pair_shown", "winner_loser"), sep = " -> ") %>% 
    separate(pair_shown, into = c("left", "right"), sep = ",") %>% 
    separate(winner_loser, into = c("won", "lost"), sep = ",") %>% 
    mutate(across(c("left", "right", "won", "lost"), as.integer))
  
  if(length(restrict_to_study_id) > 0) {
    judgement_data <- judgement_data %>%
      left_join(
        pool %>% tbl("judges") %>% 
          select(judge_id, study_id) %>% 
          collect(),
        by = "judge_id"
      ) %>% 
      filter(study_id == !!restrict_to_study_id)
  }
  
  # count the number of comparisons for each pair
  pairs_judged <- judgement_data %>%
    rowwise() %>%
    mutate(pair = paste(sort(c(won, lost)), collapse = '_')) %>% 
    select(pair, judge_id) %>%
    group_by(pair) %>% 
    tally() %>% 
    arrange(-n) %>% 
    separate(pair, c("s1", "s2"), "_") %>% 
    mutate(across(c("s1", "s2"), as.integer))
  
  # systematically list all pairs, and add the counts for each
  all_pairs_status <-
    crossing(scripts %>% select(s1 = item_num),
             scripts %>% select(s2 = item_num)) %>%
    filter(s1 < s2) %>%
    left_join(pairs_judged, by = c("s1", "s2")) %>%
    mutate(n = replace_na(n, 0))
  
  # 2. Select pairs from among the least judged so far
  
  pairs_to_judge <- tibble()
  
  while(nrow(pairs_to_judge) < pairs_to_make) {
    
    if(nrow(pairs_to_judge) == 0) {
      # start with the least judged pairs
      new_pair_to_judge <- all_pairs_status %>% 
        filter(n == min(n)) %>% 
        slice_sample(n = 1)
    } else {
      # but if we have selected some pairs already, remove them from
      # consideration (using anti_join) and look at the next-least-judged pairs
      last_rh_script <- tail(pairs_to_judge,1)$s2
      new_pair_to_judge <- all_pairs_status %>% 
        # remove pairs that have already been shown to this judge
        anti_join(
          bind_rows(
            pairs_to_judge %>% select(s1, s2),
            pairs_to_judge %>% select(s1 = s2, s2 = s1)
          ),
          by = c("s1", "s2")
        ) %>% 
        # restrict to pairs that involve the last RH script
        filter(s1 == last_rh_script | s2 == last_rh_script) %>%
        filter(n == min(n)) %>% 
        slice_sample(n = 1)
      # if last_rh_script is on the right, swap order
      if(last_rh_script == new_pair_to_judge$s2){
        new_pair_to_judge$s2 <- new_pair_to_judge$s1
        new_pair_to_judge$s1 <- last_rh_script
      }
    }
    
    pairs_to_judge <- bind_rows(pairs_to_judge,
                                new_pair_to_judge)
  }
  
  return(pairs_to_judge %>%
           # trim to the desired number of pairs
           slice_head(n = pairs_to_make) %>% 
           select(left = s1, right = s2) %>%
           mutate(pair_num = row_number(), .before = 1)
  )
}

make_tuple <- function(restrict_to_study_id = NULL) {
  
  # 1. Gather data on which judgements have been made already in this study group
  
  # all comparisons from this study
  judgement_data <- pool %>% 
    tbl("rankings") %>% 
    #filter(study == !!session_info$study_id) %>% 
    select(ranking_id, items_presented) %>% 
    collect() %>% 
    separate_rows(items_presented, sep = ",")
  
  
  # all comparisons from this study
  judgement_data <- pool %>% 
    tbl("decisions") %>% 
    select(-contains("comment")) %>% 
    collect() %>% 
    filter(str_starts(step, "rank")) %>% 
    separate(decision, into = c("items_presented", "items_ranked"), sep = " -> ") %>% 
    separate_rows(items_presented, sep = ",")
  
  if(length(restrict_to_study_id) > 0) {
    judgement_data <- judgement_data %>%
      left_join(
        pool %>% tbl("judges") %>% 
          select(judge_id, study_id) %>% 
          collect(),
        by = "judge_id"
      ) %>% 
      filter(study_id == !!restrict_to_study_id)
  }
  
  # count the number of comparisons for each pair
  pairs_judged <- judgement_data %>% 
    left_join(judgement_data, by = "decision_id") %>% 
    rename(left = items_presented.x, right = items_presented.y) %>% 
    # strip out the spurious self-comparisons
    filter(left != right) %>%
    # keep only the pairs in order e.g. 1_2 not 2_1
    rowwise() %>% 
    # put the pairs in order, with item IDs as integers
    mutate(across(c("left", "right"), as.integer)) %>% 
    mutate(pair = paste(sort(c(left, right)), collapse = '_')) %>% 
    select(pair, decision_id) %>% 
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