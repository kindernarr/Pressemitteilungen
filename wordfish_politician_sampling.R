{
  rm(list=ls(all=TRUE))
  require(quanteda)
  require(quanteda.textmodels)
  library(tidyverse)
  library(stringi)
  library(lubridate)
  library(magrittr)
  setwd("D:/Users/namentlich/Desktop/Pressemitteilungen perfect/all politicians")
  party_color <- c("cdu" = "black", "csu" = "dark grey", "spd" = "red", "afd" = "#660099", "fdp" = "#FFFF33", "dielinke" = "#993366", "gruene" = "dark green")
  subcorpus_shape <- c("cducsu_pressemitteilungen" = 13, "cducsu_texteundinterviews" = 14, "csu-landesgruppe" = 0, 
                       "spdfraktion" = 0, 
                       "afd" = 1, "afdkompakt" = 2, "afdbundestag" = 3, 
                       "fdp" = 4, "fdpbt" = 5, 
                       "die-linke" = 6, "linksfraktion_imwortlaut" = 7, "linksfraktion_nachricht" = 8, "linksfraktion_periodika" = 9, "linksfraktion_pressemitteilung" = 10, "linksfraktion_rede" = 11, 
                       "gruene-bundestag" = 12)
  partys <- list()
  partys$AFD <- c("afd", "afdkompakt", "afdbundestag")
  partys$DIELINKE <- c("die-linke", "linksfraktion_imwortlaut", "linksfraktion_nachricht", "linksfraktion_periodika", "linksfraktion_pressemitteilung", "linksfraktion_rede")
  partys$FDP <- c("fdp", "fdpbt")
  partys$CDUCSU <- c("cducsu_pressemitteilungen", "cducsu_texteundinterviews", "csu-landesgruppe")
  partys$SPD <- c("spdfraktion")
  partys$GRUENE <- c("gruene-bundestag")
}

{
  timestamp2 <- function(x, my_timestamp){
    paste0(str_replace_all(as.character(my_timestamp),':','：'), ' ', x)
  }
  
  write_tsv2 <- function(x, fname, my_timestamp){
    output_format <- str_c("{", colnames(x), "}", sep = "", collapse = "\t")
    output_name <- timestamp2(fname, my_timestamp)
    print(output_name)
    x %>% 
      str_glue_data(output_format) %>% 
      stri_write_lines(output_name)
    print(output_format)
  }
  
  top_m_politician <- function(x, m) {
    x %>% 
      na.omit() %>%
      group_by(SUBCORPUS, POLITICIAN) %>% 
      count() %>% 
      mutate(n = n + runif(length(n))) %>%
      ungroup() %>% 
      group_by(SUBCORPUS) %>%
      arrange(desc(n), .by_group = TRUE) %>%
      top_n(m, n) %>% 
      ungroup()
  }
  
  find_everybody_text <- function(x, y, politician_document_sample){
    str_c(subset(politician_document_sample, SUBCORPUS == x & POLITICIAN == y)$TEXT, collapse = ' ')
  }
  
  sample_dfmat <- function(sample_volume, my_timestamp){
    politician_document_sample <- politician_document %>% 
      group_by(SUBCORPUS, POLITICIAN) %>% 
      sample_n(sample_volume, replace = T)
    politicians_top_n_sample_text <- politician_document_sample %>% 
      select(SUBCORPUS, POLITICIAN) %>% 
      unique() %>%
      mutate(TEXT = find_everybody_text(SUBCORPUS, POLITICIAN, politician_document_sample)) %>% 
      ungroup()
    politicians_top_n_sample_text$DOCUMENT <- paste(politicians_top_n_sample_text$SUBCORPUS, politicians_top_n_sample_text$POLITICIAN, sep = '@')
    politicians_top_n_sample_text %>%
      select(DOCUMENT, TEXT) %>% 
      corpus(docid_field = "DOCUMENT",
             text_field = "TEXT") %>% 
      corpus(docvars = data.frame(SUBCORPUS = as.character(politicians_top_n_sample_text$SUBCORPUS))) %>% 
      dfm(remove_punct = TRUE)
  }
  
  wfm_sparse_mt <- function(x){
    quanteda_options(threads = RcppParallel::defaultNumThreads() - 1)
    document_length <- length(subcorpus_in_use) * 5
    textmodel_wordfish(x, dir = c(1,document_length))
  }
  
  save_tmod_wf <- function(tmod_wf, my_timestamp){
    tibble(docs = tmod_wf$docs, theta = tmod_wf$theta, se.theta = tmod_wf$se.theta, alpha = tmod_wf$alpha) %>% 
      arrange(desc(theta)) %>% 
      write_tsv2('tmod_wf_document_arguments.txt', my_timestamp)
    tibble(features = tmod_wf$features, beta = tmod_wf$beta, psi = tmod_wf$psi) %>% 
      arrange(desc(abs(beta))) %>% 
      write_tsv2('tmod_wf_term_arguments.txt', my_timestamp)
  }
}

{
  subcorpus_combinations <- t(combn(c("AFD", "DIELINKE", "FDP", "CDUCSU", "SPD", "GRUENE"), 4))
  pseudotexts <- read_tsv("corpus_pseudotexts.txt", col_names = c('URL', 'TEXT'), col_types = cols_only(URL = col_character(),TEXT = col_character()), quote = '【')
  politicians0 <- read_tsv("all_politicians.txt", col_names = c('PARTY', 'SUBCORPUS', 'URL', 'POLITICIAN'), col_types = cols_only(PARTY = col_character(), SUBCORPUS = col_character(), URL = col_character(), POLITICIAN = col_character()), quote = '【')
}

for (combination_num in 1:dim(subcorpus_combinations)[1])
{
  {
    
    politicians <- politicians0 %>% filter(SUBCORPUS %in% unlist(partys[(subcorpus_combinations[combination_num,])]))
    (subcorpus_in_use <- politicians$SUBCORPUS %>% unique())
    politicians_top_n <- top_m_politician(politicians, 5)
    politician_document <- politicians %>% 
      inner_join(politicians_top_n) %>% 
      inner_join(pseudotexts) %>%
      select(SUBCORPUS, POLITICIAN, TEXT)
  }
  
  {
    docs_result <- tibble()
    colnames(docs_result) <- c("MY_TIMESTAMP", "DOCUMENT", "THETA", "SE.THETA", "ALPHA")
    terms_result <- tibble()
    colnames(terms_result) <- c("MY_TIMESTAMP", "TERM", "BETA", "PSI")
    sample_volume <- 20
    drawplot <- T
    epoche <- 10
    epoche0 <- epoche
  }
  
  repeat{
    tryCatch({
      my_timestamp <- now()
      if (epoche == 0){
        break
      }
      dfmat <- sample_dfmat(sample_volume, my_timestamp)
      dfmat2 <- dfm_trim(dfmat, min_termfreq = 5)
      tmod_wf <- wfm_sparse_mt(dfmat2)
      if (drawplot){
        textplot_scale1d(tmod_wf, groups = dfmat2$SUBCORPUS)
        print(png_name <- timestamp2("tmod_wf.png", my_timestamp))
        ggsave(png_name, device = "png", dpi = "retina")
      }
      attr(tmod_wf, "my_timestamp") <- my_timestamp
      attr(tmod_wf, "subcorpus_in_use") <- subcorpus_in_use
      docs_summary_tmof_wf <- tibble(MY_TIMESTAMP = as.character(my_timestamp), DOCUMENT = attr(tmod_wf$x, "docvars", T)$docid_, THETA = tmod_wf$theta, SE.THETA = tmod_wf$se.theta, ALPHA = tmod_wf$alpha)
      terms_summary_tmof_wf <- tibble(MY_TIMESTAMP = as.character(my_timestamp), TERM = tmod_wf$features, BETA = tmod_wf$beta, PSI = tmod_wf$psi)
      docs_result <- rbind(docs_result, docs_summary_tmof_wf)
      terms_result <- rbind(terms_result, terms_summary_tmof_wf)
      save_tmod_wf(tmod_wf, my_timestamp)
      epoche <- epoche - 1
    }, warning = function(w) {
      print("warning")
    }, error = function(e) {
      print("error")
    })
  }
  
  {
    docs_result_analysis <- politicians %>% 
      select(-URL) %>%
      unique() %>% 
      mutate(DOCUMENT = paste(SUBCORPUS, POLITICIAN, sep = '@')) %>% 
      inner_join(docs_result, by = "DOCUMENT")
    docs_result_analysis$NUM <- as.integer(factor(docs_result_analysis$MY_TIMESTAMP))
    
    (docs_jitter <- docs_result_analysis %>% 
        ggplot(aes(NUM, THETA, color = PARTY, shape = SUBCORPUS)) +
        geom_jitter() +
        scale_x_discrete("num") +
        scale_y_continuous("theta") + 
        scale_color_manual(values = party_color) +
        scale_shape_manual(values = subcorpus_shape))
    timestamp2(paste0(paste(docs_jitter$data$PARTY %>% unique(), collapse = "+"), epoche0, " docs_jitter.png"), my_timestamp) %>%
      print() %>% # print()具有值传递的功能
      ggsave(plot = docs_jitter, device = "png", dpi = "retina")
    
    (docs_pointrange <- docs_result_analysis %>% 
        group_by(NUM, SUBCORPUS) %>% 
        mutate(M_THETA = mean(THETA)) %>% 
        mutate(M_SE.THETA = mean(SE.THETA)) %>%
        select(NUM, SUBCORPUS, PARTY, M_THETA, M_SE.THETA) %>% 
        unique() %>% 
        ungroup() %>% 
        ggplot(aes(NUM, M_THETA, ymin = M_THETA - M_SE.THETA, ymax = M_THETA + M_SE.THETA, color = PARTY, shape = SUBCORPUS)) +
        geom_pointrange() +
        scale_x_discrete("num") +
        scale_y_continuous("theta") + 
        scale_color_manual(values = party_color) +
        scale_shape_manual(values = subcorpus_shape) +
        guides(guide_legend(legend.title = NULL)))
    timestamp2(paste0(paste(docs_jitter$data$PARTY %>% unique(), collapse = "+"), epoche0, " docs_pointrange.png"), my_timestamp) %>%
      print() %>%
      ggsave(plot = docs_pointrange, device = "png", dpi = "retina")
    
    wordfishdata <- list(my_timestamp, subcorpus_in_use, epoche0, docs_result, terms_result, docs_jitter, docs_pointrange)
    save(wordfishdata, file = timestamp2(paste0(paste(docs_jitter$data$PARTY %>% unique(), collapse = "+"), epoche0, " wordfishdata.RData"), my_timestamp))
  }
}
