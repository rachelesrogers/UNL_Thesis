library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyverse)

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(fuzzyjoin)
library(purrr)



transcript_cleaning <- function(transcript, breaks="blanklines"){
  # Code from Dr. Vanderplas' 850 class
  poem <- tibble(lines = transcript) %>%
    # This looks for a letter + a space (of any sort, so an end-line counts) or
    # punctuation (last word of a line ends with e.g. a period or comma)
    mutate(n_words = str_count(lines, "([A-z][[:space:][:punct:]])"))
  
  poem$lines <- gsub("/"," ",  poem$lines)
  poem$lines <- gsub("<.*?>", "", poem$lines)
  poem$lines <- poem$lines  %>% str_replace("<center>", "")  %>% str_replace("---","") # %>% 

  if (breaks == "quote"){
    poem <- poem %>% mutate(stanza=cumsum(grepl("^\"[A-z]", lines)))
  } else if (breaks == "blanklines"){
    poem <- poem %>% mutate(stanza = cumsum(n_words == 0) + 1)
  }
  
  poem <- poem %>%
    # Now we can get rid of lines with 0 words
    filter(n_words > 0) %>%
    mutate(overall_line = 1:n()) %>%
    # We can group by stanza and count the lines within each stanza
    group_by(stanza) %>%
    mutate(stanza_line = 1:n())
  
  poem_words <- poem %>%
    mutate(words = str_split(lines, "[[:space:]]", simplify = F)) %>%
    unnest(c(words)) %>%
    # Require words to have some non-space character
    filter(nchar(str_trim(words)) > 0) %>%
    # For each line, group by line number and calculate the word number
    group_by(overall_line) %>%
    filter(!(words %in% c("<br", "><br", ">"))) %>%
    mutate(word_num = 1:n())
  
  # Removing html break marks
  poem_words$words<-gsub("</br>","", poem_words$words)
  poem_words$words<-gsub( "<br/>","", poem_words$words)
  
  # counting the number of characters
  poem_words$word_length<-nchar(poem_words$words)
  
  # Assigning coordinates to be used for plotting
  for (i in 1:length(poem_words$n_words)){
    if (poem_words$word_num[i] == 1){
      poem_words$x_coord[i] = 1
    }
    else {
      poem_words$x_coord[i] = poem_words$x_coord[i-1]+poem_words$word_length[i-1]*2
    }
  }
  poem_words$to_merge<- tolower(poem_words$words)
  poem_words$to_merge<-removePunctuation(poem_words$to_merge)
  
  group_exp <- poem_words %>%
    group_by(stanza, to_merge) %>%
    summarize(stanza_freq=n()) %>%
    ungroup()
  
  poem_words <- right_join(poem_words, group_exp)
  
  
  return(poem_words)
}

# Used for word comparisons, not collocations
comment_frequency <- function(comment_doc, pagenumber){
  page<- comment_doc[comment_doc$page_count==pagenumber,]
  
  #Cleaning document
  
  page_corpus<-Corpus(VectorSource(page$notes))
  
  page_clean<-tm_map(page_corpus,removePunctuation)
  page_clean<-tm_map(page_clean,stripWhitespace)
  
  page_tdm <- as.matrix(TermDocumentMatrix(page_clean))
  freq_page <- data.frame(to_merge=rownames(page_tdm),
                          Freq=rowSums(page_tdm),
                          row.names=NULL) 
  return(freq_page)
}

#Used for word comparisons
plot_transcript <- function(transcript_doc, frequency_doc, stanza_num, exclusions, past_freq, relative_freq){
  
  transcript_stanza<-subset(transcript_doc, stanza==stanza_num)
  transcript_stanza<-left_join(transcript_stanza, frequency_doc)
  
  transcript_stanza[is.na(transcript_stanza$Freq),]$Freq <- 0
  xlimit<-max(transcript_stanza$x_coord)+5
  
  transcript_stanza$frequency<- transcript_stanza$Freq
  
  if( !missing(past_freq)){
    prev_freq<-past_freq %>% rename(previous_freq = Freq)
    
    transcript_stanza<-left_join(transcript_stanza, prev_freq)
    transcript_stanza[is.na(transcript_stanza$previous_freq),]$previous_freq <- 0
    
    transcript_stanza$subtracted_freq <- transcript_stanza$Freq-transcript_stanza$previous_freq
    
    transcript_stanza$frequency <- transcript_stanza$subtracted_freq
  }
  
  if( !missing(exclusions)){
    
    transcript_stanza[(transcript_stanza$to_merge %in% exclusions),]$frequency<-0
  }
  
  if (!missing(relative_freq)){
    transcript_stanza$frequency <- transcript_stanza$frequency/transcript_stanza$stanza_freq
  }
  
  
  p <- ggplot(transcript_stanza, aes(x=x_coord, y=stanza_line, label=words))+
    geom_text(hjust="left", size=5, aes(alpha=frequency, color=frequency))+
    scale_y_reverse()+
    xlim(c(1, xlimit))+
    theme_bw()+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position="bottom") + 
    scale_color_gradient(low="grey", high="darkblue")
  
  p_obj <- ggplot_build(p)
  list(plot = p, build = p_obj, freq=transcript_stanza %>% select(words, frequency))
}

# used for word comparisons
colored_text <- function(plot_object){
  
  page_df<-plot_object$build$data[[1]]
  
  page_df$ID <- seq.int(nrow(page_df))
  
  page_df$cleancolor <- gsub("#","",page_df$colour)
  
  page_df$rgb <- NA
  page_df$color_def <- NA
  page_df$word_assign <- NA
  
  #### CSS Color Assign ###
  for (i in 1:length(page_df$colour)){
    page_df$rgb[i] <- paste(as.vector(col2rgb(page_df$colour[i])), collapse = ", ")
    if (page_df$label[i] %in% c("Q:","A:","Court:","Defense:","Prosecution:")){
      page_df$word_assign[i] <- paste("<br/>"," <span style=\"color: ",page_df$colour[i], "\">",page_df$label[i],"</span>", sep="")
    }
    else{
      page_df$word_assign[i] <- paste(" <span style=\"color: ",page_df$colour[i], "\">",page_df$label[i],"</span>", sep="")
    }
  }
  
  low_freq<-plot_object$freq[which.min(plot_object$freq$frequency),]
  colnames(low_freq) <- c("line","label", "frequency", "x","y")
  low_freq$y <- -low_freq$y
  low_combined<- inner_join(low_freq,plot_object$build$data[[1]], by=c("label","x","y"))
  
  high_freq<-plot_object$freq[which.max(plot_object$freq$frequency),]
  colnames(high_freq) <- c("line","label", "frequency", "x","y")
  high_freq$y <- -high_freq$y
  high_combined<- inner_join(high_freq,plot_object$build$data[[1]], by=c("label","x","y"))
  
  page_gradient <- paste("<div>",low_combined$frequency[1],"<div style=\"
    height: 20px;
    width: 200px;
    display: inline-block;
    background: linear-gradient(45deg,",low_combined$colour[1],",", high_combined$colour[1],");\">","</div>",high_combined$frequency[1],"</div>")
  
  complete_wordassign<-paste(as.vector(page_df$word_assign), collapse="")
  
  paste(page_gradient, complete_wordassign)
  
}


#### Collocation Functions ############


token_transcript <- function(filelocation){
  description_txt<-read.table(filelocation)
  description_df <- as.data.frame(cbind(seq(1:dim(description_txt)[1]), description_txt))
  
  colnames(description_df) <- c("docid", "text")
  description_df <- map_df(description_df, ~ gsub("<.*?>", "", .x)) #removing all html expressions
  description_df <- map_df(description_df, ~ gsub("\\\\n", "", .x)) #removing line breaks
  
  ###### Attempting to Remove Question Prompts ############
  description_df <- map_df(description_df, ~ gsub("Q:", "", .x)) #removing word indicators
  description_df <- map_df(description_df, ~ gsub("A:", "", .x)) #removing word indicators
  description_df <- map_df(description_df, ~ gsub("Court:", "", .x)) #removing word indicators
  description_df <- map_df(description_df, ~ gsub("Defense:", "", .x)) #removing word indicators
  description_df <- map_df(description_df, ~ gsub("Prosecution:", "", .x)) #removing word indicators
  description_df <- map_df(description_df, ~ gsub("\"Q:", "", .x)) #removing word indicators
  #########################################################
  
  # description_df <- map_df(description_df, ~ gsub("---Test-fired bullets admitted into evidence---", "", .x))
  
  corpus_descript <- corpus(description_df) #creating a corpus
  
  toks_des <- tokens(corpus_descript, remove_punct = TRUE) #tokenizing transcript
  return(toks_des)
}

token_comments <- function(comment_document, page_number){
  pg2 <- comment_document %>% subset(page_count == page_number) #finding comments for current page
  
  ######## Attempt to remove prompts ##########
  pg2$page_notes <- gsub("Q:", "", pg2$page_notes)
  pg2$page_notes <- gsub("A:", "", pg2$page_notes)
  pg2$page_notes <- gsub("Court:", "", pg2$page_notes)
  pg2$page_notes <- gsub("Defense:", "", pg2$page_notes)
  pg2$page_notes <- gsub("Prosecution:", "", pg2$page_notes)
  pg2$page_notes <- gsub("\"Q:", "", pg2$page_notes)
  ##############################################
  
  ############### Removing -'s ######################
  pg2$page_notes <- gsub("-", "", pg2$page_notes)
  
  # pg2$page_notes <- gsub("---Test-fired bullets admitted into evidence---","",pg2$page_notes)
  
  pg2_df <- data.frame(docid = cbind(seq(1:dim(pg2)[1])), text=tolower(pg2$page_notes)) #lowercasing text
  #pg2_df <- map_df(pg2_df, ~ gsub("-","",.x))
 
  corpus_pg2 <- corpus(pg2_df)
  
  toks_pg2 <- tokens(corpus_pg2, remove_punct = TRUE)
  return(toks_pg2)
}

collocate_comments <- function(transcript_token, page_number, note_token){
  #Creating ngrams of length 5
  descript_ngrams <- tokens_ngrams(transcript_token[page_number], n = 5L, skip = 0L, concatenator = " ")
  descript_ngram_df <- data.frame(tolower(unlist(descript_ngrams)))
  rel_freq <-as.data.frame(table(descript_ngram_df)) #calculating frequency of ngrams
  descript_ngram_df <- left_join(descript_ngram_df, rel_freq) #binding frequency to collocations
  names(descript_ngram_df) <- c("collocation", "transcript_freq")
  
  # numbering words in the collocation
  descript_ngram_df <-data.frame(collocation = descript_ngram_df$collocation, 
                                 transcript_freq = descript_ngram_df$transcript_freq, 
                                 word_1 = seq(from = 1, to = dim(descript_ngram_df)[1]),
                                 word_2 = seq(from = 2, to = (dim(descript_ngram_df)[1]+1)),
                                 word_3 = seq(from = 3, to = (dim(descript_ngram_df)[1]+2)),
                                 word_4 = seq(from = 4, to = (dim(descript_ngram_df)[1]+3)),
                                 word_5 = seq(from = 5, to = (dim(descript_ngram_df)[1]+4)))
  descript_ngram_df$first_word <- word(descript_ngram_df$collocation,1)
  #descript_ngram_df$collocation <- map_df(descript_ngram_df$collocation, ~ gsub("-","",.x))
  
  #getting collocations from notes
  col_descript <- note_token %>% textstat_collocations(min_count = 1, size=5) 
  #col_descript$collocation <- tolower(col_descript$collocation)
  
  col_merged_descript <- left_join(descript_ngram_df, col_descript)
  
  #replacing na's with 0's
  col_merged_descript$count <- replace(col_merged_descript$count,is.na(col_merged_descript$count),0)
  
  col_descript_long <- col_merged_descript %>%  pivot_longer(cols = 3:7, 
                                                             names_to = "col_number", 
                                                             names_prefix = "word_",
                                                             values_to = "word_number"
  )
  #calculating relative frequency based on number of times colloactions occur
  col_descript_long$rel_freq <- col_descript_long$count/col_descript_long$transcript_freq
  
  descript_tomerge <- col_descript_long %>% select(rel_freq, col_number, word_number) %>%
    pivot_wider(names_from = col_number, values_from = rel_freq, names_prefix = "col_")
  
  add_word<-descript_ngram_df %>% select(word_1, first_word, collocation) %>% rename("word_number"="word_1")
  
  descript_tomerge <- left_join(descript_tomerge, add_word)
  descript_tomerge<-descript_tomerge %>% rename("to_merge"="first_word")
  
  descript_tomerge[dim(descript_tomerge)[1]-3,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,2)
  descript_tomerge[dim(descript_tomerge)[1]-2,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,3)
  descript_tomerge[dim(descript_tomerge)[1]-1,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,4)
  descript_tomerge[dim(descript_tomerge)[1],]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,5)
  
  return(descript_tomerge)
  
}

#Fuzzy matching
collocate_comments_fuzzy <- function(transcript_token, page_number, note_token){
  #Same as previous notes
  descript_ngrams <- tokens_ngrams(transcript_token[page_number], n = 5L, skip = 0L, concatenator = " ")
  descript_ngram_df <- data.frame(tolower(unlist(descript_ngrams)))
  rel_freq <-as.data.frame(table(descript_ngram_df))
  descript_ngram_df <- left_join(descript_ngram_df, rel_freq)
  names(descript_ngram_df) <- c("collocation", "transcript_freq")
  descript_ngram_df <-data.frame(collocation = descript_ngram_df$collocation, 
                                 transcript_freq = descript_ngram_df$transcript_freq, 
                                 word_1 = seq(from = 1, to = dim(descript_ngram_df)[1]),
                                 word_2 = seq(from = 2, to = (dim(descript_ngram_df)[1]+1)),
                                 word_3 = seq(from = 3, to = (dim(descript_ngram_df)[1]+2)),
                                 word_4 = seq(from = 4, to = (dim(descript_ngram_df)[1]+3)),
                                 word_5 = seq(from = 5, to = (dim(descript_ngram_df)[1]+4)))
  descript_ngram_df$first_word <- word(descript_ngram_df$collocation,1)
  
  #descript_ngram_df$collocation <- map_df(descript_ngram_df$collocation, ~ gsub("-","",.x))
  
  col_descript <- note_token %>% textstat_collocations(min_count = 1, size=5)
  #col_descript$collocation <- tolower(col_descript$collocation)
  
  col_merged_descript <- left_join(descript_ngram_df, col_descript)
  col_merged_descript$count <- replace(col_merged_descript$count,is.na(col_merged_descript$count),0)
  
  ###Fuzzy Matching
  
  # Finding collocations that do not directly match the transcript
  mismatches <- anti_join(col_descript, descript_ngram_df)
  
  fuzzy_matches <-stringdist_join(descript_ngram_df, mismatches, 
                                  by='collocation', #match based on collocation
                                  mode='right', #use right join
                                  method = "lv", #use levenshtein distance metric
                                  max_dist=99, 
                                  distance_col='dist')%>%
    group_by(collocation.y) %>%
    slice_min(order_by=dist, n=1) #finding the closest match
  #counting the number of closest matches per collocation
  close_freq<-as.data.frame(table(fuzzy_matches$collocation.y))
  close_freq <- close_freq %>% rename("collocation.y"="Var1", "close_freq"="Freq")
  
  fuzzy_matches <- left_join(fuzzy_matches, close_freq)
  
  #Fuzzy matches weight
  fuzzy_matches$weighted_count <- fuzzy_matches$count/((fuzzy_matches$dist+0.25)*fuzzy_matches$close_freq)
  
  #Counting up the number of fuzzy matches per transcript collocation
  fuzzy_col <-fuzzy_matches %>% group_by(collocation.x) %>%
    summarise (fuzzy_count = sum(weighted_count))
  
  fuzzy_col <- fuzzy_col %>% rename("collocation"="collocation.x")
  
  col_merged_fuzzy <- left_join(col_merged_descript, fuzzy_col)
  col_merged_fuzzy$fuzzy_count <- replace(col_merged_fuzzy$fuzzy_count, is.na(col_merged_fuzzy$fuzzy_count),0)
  #Counting up fuzzy and non-fuzzy matches
  col_merged_fuzzy$final_count <- col_merged_fuzzy$count+col_merged_fuzzy$fuzzy_count
  
  col_descript_long <- col_merged_fuzzy %>%  pivot_longer(cols = 3:7, 
                                                          names_to = "col_number", 
                                                          names_prefix = "word_",
                                                          values_to = "word_number"
  )
  col_descript_long$rel_freq <- col_descript_long$final_count/col_descript_long$transcript_freq
  
  descript_tomerge <- col_descript_long %>% select(rel_freq, col_number, word_number) %>%
    pivot_wider(names_from = col_number, values_from = rel_freq, names_prefix = "col_")
  
  add_word<-descript_ngram_df %>% select(word_1, first_word, collocation) %>% rename("word_number"="word_1")
  
  descript_tomerge <- left_join(descript_tomerge, add_word)
  descript_tomerge<-descript_tomerge %>% rename("to_merge"="first_word")
  
  descript_tomerge[dim(descript_tomerge)[1]-3,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,2)
  descript_tomerge[dim(descript_tomerge)[1]-2,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,3)
  descript_tomerge[dim(descript_tomerge)[1]-1,]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,4)
  descript_tomerge[dim(descript_tomerge)[1],]$to_merge <- 
    word(descript_tomerge[dim(descript_tomerge)[1]-4,]$collocation,5)
  
  return(descript_tomerge)
  
}

#Calculating frequency by word
transcript_frequency <- function(filelocation, page_number, collocate_object){
  descript_words <- transcript_cleaning(readLines(filelocation), breaks="quote")
  
  descript_words_tomerge <- descript_words[descript_words$stanza== page_number,]
  
  descript_words_tomerge[descript_words_tomerge$words %in% c("-"," ","Q:","A:","Court:","Defense:","Prosecution:","\"Q:"), ]$to_merge<-""
  #descript_words_tomerge[descript_words_tomerge$lines=="Test-fired bullets admitted into evidence---", ]$to_merge<-""
  
  descript_words_tomerge_filtered <- descript_words_tomerge # %>% filter(!(words %in% c("-"," ","Q:","A:","Court:","Defense:","Prosecution:","\"Q:")))
  
  descript_words_tomerge_filtered$word_number<-NA
  descript_words_tomerge_filtered[descript_words_tomerge_filtered$to_merge!="",]$word_number <- 
    seq(from=1, to=dim(descript_words_tomerge_filtered[descript_words_tomerge_filtered$to_merge!="",])[1])
  
  collocate_object$to_merge <- gsub("'","",collocate_object$to_merge)
  collocate_object$to_merge <- gsub("\\.","",collocate_object$to_merge)
  collocate_object$to_merge <- gsub("-","",collocate_object$to_merge)
  
  merged <- left_join(descript_words_tomerge_filtered, collocate_object, by=c("word_number","to_merge"))
  
  merged$Freq <- rowSums(merged[,c("col_1","col_2","col_3","col_4","col_5")], na.rm=TRUE)/rowSums(!is.na(merged[,c("col_1","col_2","col_3","col_4","col_5")]))
  
  merged_final<- left_join(descript_words_tomerge, merged)
  
  return(merged_final)
}


collocate_plot <- function(frequency_doc,n_scenario=1){
  frequency_doc[is.na(frequency_doc$Freq),]$Freq <- 0
  xlimit<-max(frequency_doc$x_coord)+5
  
  #normalizing by number of scenarios
  frequency_doc$frequency<- frequency_doc$Freq/n_scenario
  
  #Using ggplot to establish gradient
  p <- ggplot(frequency_doc, aes(x=x_coord, y=stanza_line, label=words))+
    geom_text(hjust="left", size=5, aes(alpha=frequency, color=frequency))+
    scale_y_reverse()+
    xlim(c(1, xlimit))+
    theme_bw()+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position="bottom") + 
    scale_color_gradient(low="#f8ff1b", high="#f251fc")
  
  p_obj <- ggplot_build(p)
  
  plot_vars<- list(plot = p, build = p_obj, freq=frequency_doc %>% select(words, frequency, x_coord, stanza_line))
  return(plot_vars)
}

#Highlighting text
highlighted_text <- function(plot_object, descript){
  
  page_df<-plot_object$build$data[[1]]
  
  page_df$stanza_line <- -page_df$y
  
  page_df$ID <- seq.int(nrow(page_df))
  
  page_df$cleancolor <- gsub("#","",page_df$colour)
  
  page_df$rgb <- NA
  page_df$color_def <- NA
  page_df$word_assign <- NA
  
  #### CSS Color Assign ###
  #Get color from ggplot object, and paste css code together to print colors
  for (i in 1:length(page_df$colour)){
    page_df$rgb[i] <- paste(as.vector(col2rgb(page_df$colour[i])), collapse = ", ")
    if (page_df$label[i] =="\"Q:"){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }else if (i==1){
      page_df$word_assign[i] <- paste("<br/>","<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background-color: ",page_df$colour[i], "\">",page_df$label[i],"&nbsp;","</div>", sep="")
    } else if (page_df$label[i] %in% c("Q:","A:","Court:","Defense:","Prosecution:")){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px \">",page_df$label[i],"&nbsp;","</div>", sep="")
    } else if (page_df$label[i+1] %in% c("Q:","A:","Court:","Defense:","Prosecution:") & page_df$x[i]==1){
      page_df$word_assign[i] <- paste("<br/> <div style=\"display: inline-block; padding:0px;
  margin-left:-5px \">",page_df$label[i],"&nbsp;","</div>", sep="")
    } else if (page_df$label[i-1] %in% c("Q:","A:","Court:","Defense:","Prosecution:")){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background: linear-gradient(to right,",page_df$colour[i-2],",",page_df$colour[i],") \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }else if (page_df$label[i] =="-"){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background: linear-gradient(to right,",page_df$colour[i-1],",",page_df$colour[i+1],") \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }else if (page_df$label[i-1] =="-"){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background: linear-gradient(to right,",page_df$colour[i],",",page_df$colour[i],") \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }else if (page_df$label[i-1] =="\"Q:"){
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background: linear-gradient(to right,",page_df$colour[i],",",page_df$colour[i],") \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }else{
      page_df$word_assign[i] <- paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px; background: linear-gradient(to right,",page_df$colour[i-1],",",page_df$colour[i],") \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }
    if (page_df$y[i] %in% page_df[str_detect(page_df$label, "---"),]$y){
       page_df$word_assign[i] <-  paste("<div style=\"display: inline-block; padding:0px;
  margin-left:-5px \">",page_df$label[i],"&nbsp;","</div>", sep="")
    }
    if (page_df$y[i] != -1 & page_df$x[i] == 1 & (page_df$y[i] %in% page_df[str_detect(page_df$label, "---") ,]$y | page_df$label[i] %in% c("Q:","A:","Court:","Defense:","Prosecution:"))){
      page_df$word_assign[i] <- paste("<br/>", page_df$word_assign[i])
    }
  }
  
  #Creating the gradient legend box
  low_freq<-plot_object$freq[which.min(plot_object$freq$frequency),]
  colnames(low_freq) <- c("line","label", "frequency", "x","y")
  low_freq$y <- -low_freq$y
  low_combined<- inner_join(low_freq,plot_object$build$data[[1]], by=c("label","x","y"))
  
  high_freq<-plot_object$freq[which.max(plot_object$freq$frequency),]
  colnames(high_freq) <- c("line","label", "frequency", "x","y")
  high_freq$y <- -high_freq$y
  high_combined<- inner_join(high_freq,plot_object$build$data[[1]], by=c("label","x","y"))
  
  page_gradient <- paste("<div>",round(low_combined$frequency[1],2),"<div style=\"
    height: 20px;
    width: 200px;
    display: inline-block;
    background: linear-gradient(45deg,",low_combined$colour[1],",", high_combined$colour[1],");\">","</div>",round(high_combined$frequency[1],2),"</div>")
  
  complete_wordassign<-paste(as.vector(page_df$word_assign), collapse="")
  
  paste(page_gradient, complete_wordassign)
  
}
