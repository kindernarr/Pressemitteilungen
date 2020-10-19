{rm(list=ls(all=TRUE))
  library(tidyverse)
  library(magrittr)
  library(e1071)
  setwd("D:/Users/namentlich/Desktop/运算结果/使用总词频在10以上的词，3个党一起")
  filelist <- list.files(pattern = "tmod_wf_document_arguments.txt$")
  partys = list()
  partys$AFD <- c("afd", "afdkompakt", "afdbundestag")
  partys$DIELINKE <- c("die-linke", "linksfraktion_imwortlaut", "linksfraktion_nachricht", "linksfraktion_periodika", "linksfraktion_pressemitteilung", "linksfraktion_rede")
  partys$FDP <- c("fdp", "fdpbt")
  partys$CDUCSU <- c("cducsu_pressemitteilungen", "cducsu_texteundinterviews", "csu-landesgruppe")
  partys$SPD <- c("spdfraktion")
  partys$GRUENE <- c("gruene-bundestag")
  subcorpus_party <- bind_rows(lapply(names(partys), function(x){
    y <- as_tibble(partys[x])
    colnames(y) <- "SUBCORPUS"
    y$PARTY <- x
    return(y)
  }))
  find_party <- function(x){
    subcorpus_party %>% inner_join(tibble(SUBCORPUS = x), by = "SUBCORPUS") %>% .$PARTY
  }
  toPattern <- function(text){
    paste(unlist(strsplit(text,NULL)), collapse="(.*?)")
  }
  revPattern <- function(text){
    paste(rev(unlist(strsplit(text,NULL))), collapse="(.*?)")
  }
  possible_order <- as_tibble(apply(permutations(6), c(1, 2), function(x){names(partys)[[x]]}))
  party_to_num <- c("AFD" = 1, "DIELINKE" = 2, "FDP" = 3, "CDUCSU" = 4, "SPD" = 5, "GRUENE" = 6) %>% 
    as_tibble_row() %>% 
    pivot_longer(everything(), names_to = "PARTY", values_to = "NUMBER")
  possible_order$string <- lapply(1:dim(possible_order)[[1]], function(i){
    possible_order[i,] %>% 
      pivot_longer(everything(), names_to = "ORDER", values_to = "PARTY") %>% 
      inner_join(party_to_num, by = "PARTY") %>% 
      .$NUMBER %>% 
      str_c(collapse = "")
  }) %>% unlist()
}

{
  docs_arguments <- bind_rows(lapply(filelist, function(x){
    read_tsv(x, 
             col_names = c('DOCUMENT', 'THETA', 'SE.THETA', 'ALPHA'), 
             col_types = cols_only(DOCUMENT = col_character(), THETA = col_double(), SE.THETA = col_double(), ALPHA = col_double()), 
             quote = '【') %>% 
      mutate(FILENAME = x) %>% 
      select(FILENAME, everything())
  }))
  docs_arguments$SUBCORPUS <- docs_arguments$DOCUMENT
  docs_arguments %<>% inner_join(subcorpus_party)
  docs_theta <- docs_arguments %>% 
    select(FILENAME, PARTY, SUBCORPUS, THETA) %>% 
    group_by(FILENAME, PARTY, SUBCORPUS) %>% 
    summarise(THETA.MEDIAN = median(THETA)) %>% 
    ungroup()
}

{order_relation <- bind_rows(lapply(filelist, function(fname){
  thisfile <- docs_theta %>% 
    filter(FILENAME == fname)
  each_judgements <- character()
  THETA_MEDIAN <- thisfile$THETA.MEDIAN
  number_of_subcorpus <- length(THETA_MEDIAN)
  for (i in 1:length(THETA_MEDIAN)){
    for (j in 1:i){
      if (j == i) {next}
      if (THETA_MEDIAN[i] > THETA_MEDIAN[j]){
        each_judgements <- c(each_judgements, thisfile$SUBCORPUS[i], thisfile$SUBCORPUS[j])
      } else{
        each_judgements <- c(each_judgements, thisfile$SUBCORPUS[j], thisfile$SUBCORPUS[i])
      }
    }
  }
  each_judgements %<>% 
    matrix(ncol = 2, byrow=T) %>% 
    as_tibble(.name_repair = "minimal")
  colnames(each_judgements) <- c("middle", "smaller")
  each_judgements2 <- each_judgements
  colnames(each_judgements) <- c("bigger", "middle")
  each_judgements %<>% 
    inner_join(each_judgements2, by = "middle") %>%
    mutate(FILENAME = fname)
  each_judgements$bigger_party <- sapply(each_judgements$bigger, find_party)
  each_judgements$middle_party <- sapply(each_judgements$middle, find_party)
  each_judgements$smaller_party <- sapply(each_judgements$smaller, find_party)
  each_judgements %>% 
    filter(bigger_party != middle_party & bigger_party != smaller_party & smaller_party != middle_party)
}))
  order_relation$string <- lapply(1:dim(order_relation)[[1]], function(i){
    x <- order_relation[i,]
    y <- as_tibble_col(c(x$bigger_party, x$middle_party, x$smaller_party), column_name = "PARTY") %>% 
      inner_join(party_to_num, by = "PARTY") %>% 
      .$NUMBER %>% 
      str_c(collapse = "")
  }) %>% unlist()}

{weighted_relation <- order_relation %>% 
    group_by(FILENAME, string) %>% 
    count() %>% 
    ungroup()
  score_index <- grep("2(.*?)1", possible_order$string)
  possible_order <- possible_order[score_index,]
  
  possible_order %<>% mutate(score = 0)
  for (fname in filelist){
    onefile <- weighted_relation %>% 
      filter(FILENAME == fname)
    onefile$regex_pattern <- lapply(onefile$string, toPattern) %>% unlist()
    onefile$reversed_pattern <- lapply(onefile$string, revPattern) %>% unlist()
    for (i in 1:dim(onefile)[[1]]){
      one_order <- onefile[i,]
      score_index <- grep(one_order$regex_pattern, possible_order$string)
      possible_order[score_index,]$score <- possible_order[score_index,]$score + one_order$n
      score_index <- grep(one_order$reversed_pattern, possible_order$string)
      possible_order[score_index,]$score <- possible_order[score_index,]$score + one_order$n
    }
    possible_order %<>% 
      arrange(desc(score))
  }}

getwd()
head(possible_order, 1)

# 不含政客的抽样运行结果：

# D:/Users/namentlich/Desktop/运算结果/不限制词频，3个党一起
# DIELINKE SPD   CDUCSU GRUENE FDP   AFD   254631  4716

# D:/Users/namentlich/Desktop/运算结果/词频在5以上，3个党一起
# DIELINKE SPD   CDUCSU GRUENE FDP   AFD   254631 10140

# D:/Users/namentlich/Desktop/运算结果/词频在5以上，4个党一起
# DIELINKE SPD   CDUCSU GRUENE FDP   AFD   254631 31524

# D:/Users/namentlich/Desktop/运算结果/词频在5以上，6个党一起
# GRUENE DIELINKE SPD   CDUCSU FDP   AFD   625431 10638

# D:/Users/namentlich/Desktop/运算结果/使用仅仅出现过一次的词，3个党一起
# FDP   DIELINKE CDUCSU SPD   GRUENE AFD   324561  2286

# D:/Users/namentlich/Desktop/运算结果/使用总词频在5以下的词，3个党一起
# DIELINKE GRUENE SPD   CDUCSU FDP   AFD   265431  3103

# D:/Users/namentlich/Desktop/运算结果/使用总词频在10以上的词，3个党一起
# DIELINKE SPD   CDUCSU GRUENE FDP   AFD   254631  3764

# 含有政客的排序：

# D:/Users/namentlich/Desktop/运算结果/政客，不分组，6个党一起上
# DIELINKE SPD    CDUCSU AFD    FDP    GRUENE 254136 10134

# D:/Users/namentlich/Desktop/运算结果/政客，词频在5以上，4个党一组
# DIELINKE SPD    CDUCSU AFD    FDP    GRUENE 254136  5534

# D:/Users/namentlich/Desktop/运算结果/政客，词频在5以上，3个党一组
# DIELINKE SPD    CDUCSU FDP    AFD    GRUENE 254316  1933

# D:/Users/namentlich/Desktop/运算结果/政客，不限制词频，3个党一组
# DIELINKE SPD   CDUCSU FDP   AFD   GRUENE 254316  1851
