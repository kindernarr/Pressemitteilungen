{rm(list=ls(all=TRUE))
  require(quanteda)
  require(quanteda.textmodels)
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(reticulate)
  setwd("D:/Users/namentlich/Desktop/Pressemitteilungen perfect/all politicians")
  wfm_sparse_mt <- function(x){
    quanteda_options(threads = RcppParallel::defaultNumThreads() - 1)
    textmodel_wordfish(x, dir = c(ndoc(x)-1, ndoc(x)))
  }
  timestamp2 <- function(x, my_timestamp){
    paste0(str_replace_all(as.character(my_timestamp),':','：'), ' ', x)
  }

  party_number <- c("dielinke" = 1, "spd" = 2, "cdu" = 3, "csu" = 3, "gruene" = 4, "fdp" = 5, "afd" = 6) %>% 
    as_tibble_row() %>% 
    pivot_longer(everything(), names_to = "PARTY", values_to = "NUMBER")
  subcorpus_party_number <- c("afd" = "afd",
                              "afdkompakt" = "afd",
                              "afdbundestag" = "afd",
                              "die-linke" = "dielinke",
                              "linksfraktion_imwortlaut" = "dielinke",
                              "linksfraktion_nachricht" = "dielinke",
                              "linksfraktion_periodika" = "dielinke",
                              "linksfraktion_pressemitteilung" = "dielinke",
                              "linksfraktion_rede" = "dielinke",
                              "fdp" = "fdp",
                              "fdpbt" = "fdp",
                              "cducsu_pressemitteilungen" = "cdu",
                              "cducsu_texteundinterviews" = "cdu",
                              "csu-landesgruppe" = "csu",
                              "spdfraktion" = "spd",
                              "gruene-bundestag" = "gruene") %>% 
    as_tibble_row() %>% 
    pivot_longer(everything(), names_to = "SUBCORPUS", values_to = "PARTY") %>% 
    inner_join(party_number, by = "PARTY")}

{
  load("whole_dfm.RData")
  whole_dfm_tibble <- whole_dfm %>% attr("docvars") %>% tibble() %>% mutate(ONE = 1)
  subcorpus_info <- whole_dfm_tibble %>% group_by(PARTY_NUM) %>% 
    summarise(WORDCOUNT = sum(WORDCOUNT), ARTICLES = sum(ONE)) %>% 
    mutate(AVER_LEN = WORDCOUNT / ARTICLES) # 平均一篇的长度
  politicians0 <- read_tsv("all_politicians.txt", col_names = c('PARTY', 'SUBCORPUS', 'URL', 'POLITICIAN'), col_types = cols_only(PARTY = col_character(), SUBCORPUS = col_character(), URL = col_character(), POLITICIAN = col_character()), quote = '【')
  big_tibble <- whole_dfm_tibble %>% inner_join(politicians0) %>% mutate(DOCUMENT = paste0(SUBCORPUS, '@', POLITICIAN))
  politician_tibble <- big_tibble %>% group_by(SUBCORPUS, POLITICIAN, DOCUMENT) %>% summarise(WORDCOUNT = sum(WORDCOUNT)) %>% filter(WORDCOUNT > 5000)
}

compare_two <- function(document1, document2){
  # document1 <- "fdp@Oliver Luksic"
  # document2 <- "fdp@Christian Lindner"
  attributes(whole_dfm)$docvars$CHOOSE <- 0
  docnum_ineachparty <- 1:6 %>% 
    lapply(function(i){
      whole_dfm_tibble %>% 
        filter(PARTY_NUM == i) %>% 
        .$NUM
    })
  1:length(docnum_ineachparty) %>% lapply(function(i){
    attributes(whole_dfm)$docvars$CHOOSE[docnum_ineachparty[[i]]] <<- i
  })
  document1_docnum <- big_tibble %>% filter(DOCUMENT == document1) %>% .$NUM
  document2_docnum <- big_tibble %>% filter(DOCUMENT == document2) %>% .$NUM
  if (length(document1_docnum) > length(document2_docnum)){
    attributes(whole_dfm)$docvars$CHOOSE[document1_docnum] <- 7
    attributes(whole_dfm)$docvars$CHOOSE[document2_docnum] <- 8
  } else {
    attributes(whole_dfm)$docvars$CHOOSE[document2_docnum] <- 8
    attributes(whole_dfm)$docvars$CHOOSE[document1_docnum] <- 7
  }
  this_dfm <- whole_dfm %>% dfm_subset(CHOOSE != 0) %>% dfm_group("CHOOSE") %>% dfm_trim(min_docfreq = 8)
  tmod_wf <- this_dfm %>% wfm_sparse_mt()
  document1_theta <- tmod_wf$theta[[7]]
  document2_theta <- tmod_wf$theta[[8]]
  arrangement <- tibble(
    PARTY = c("DIELINKE", "SPD", "CDUCSU", "GRUENE", "FDP", "AFD"),
    THETA = tmod_wf$theta[1:6],
    LEFTRIGHT = 1:6
  ) %>% arrange(THETA)
  if (sum(arrangement$LEFTRIGHT[1:3]) > sum(arrangement$LEFTRIGHT[4:6])){
    document1_theta <- -document1_theta
    document2_theta <- -document2_theta
  }
  if (document1_theta < document2_theta){
    T
  } else {
    F
  }
}

compare_two("afd@Paul Hampel", "afd@Alexander Gauland")

whoisleft <- function(doc1, doc2){
  tryCatch({compare_two(doc1, doc2)}, warning = function(w) {
    print(w)
    print(doc1)
    print(doc2)
    return(T)
  }, error = function(e) {
    print(e)
    print(doc1)
    print(doc2)
    return(T)
  })
}

whoisleft("cducsu_texteundinterviews@Siegfried Kauder", "afd@Alexander Gauland")

source_python("D:/Users/namentlich/Desktop/Pressemitteilungen perfect/all politicians/insertionSort_python.py")
documentlist_to_sort <- function(party){
  politician_tibble %>% arrange(desc(WORDCOUNT)) %>% inner_join(subcorpus_party_number) %>% arrange(NUMBER) %>% filter(PARTY %in% party) %>% .$DOCUMENT
}
(party_politician <- documentlist_to_sort(c("fdp")))

{
  timestart <- Sys.time()
  qs_result <- insertionSort(party_politician)
  qs_result %>% save(file = timestamp2("qs_result.RData", now()))
  runningtime <- Sys.time() - timestart
}

# > qs_result
# [1] "fdpbt@Michael Theurer"             "fdpbt@Katja Suding"               
# [3] "fdpbt@Alexander Graf Lambsdorff"   "fdpbt@Christian Lindner"          
# [5] "fdpbt@Stephan Thomae"              "fdp@Nicola Beer"                  
# [7] "fdp@Michael Theurer"               "fdp@Alexander Graf Lambsdorff"    
# [9] "fdp@Linda Teuteberg"               "fdpbt@Marco Buschmann"            
# [11] "fdp@Volker Wissing"                "fdp@Christian Lindner"            
# [13] "fdp@Hans-Ulrich Rülke"             "fdp@Marco Buschmann"              
# [15] "fdp@Marie-Agnes Strack-Zimmermann" "fdp@Hermann Otto Solms"           
# [17] "fdp@Christian Dürr"                "fdp@Frank Sitta"                  
# [19] "fdp@Katja Suding"                  "fdp@Florian Toncar"               
# [21] "fdp@Johannes Vogel"                "fdp@Stefan Ruppert"               
# [23] "fdp@Oliver Luksic"                 "fdp@Wolfgang Kubicki" 
