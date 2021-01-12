
Check.scholar = function(scholar) {
library(tidytext)
library(lubridate)
library(ggplot2)
library(rvest)
library(tidyverse)
library(knitr)   
library(RColorBrewer)
library(ggwordcloud)
library(patchwork)
library(gridExtra)

scholar=as.character(paste0(scholar,"&pagesize=1000"))  


  
keywords= scholar %>% 
  read_html(scholar) %>%
  html_nodes(".gsc_prf_inta") %>% 
  html_text()
  

name= scholar %>% 
  read_html(scholar) %>% 
  html_node("#gsc_prf_in") %>%
  html_text()

Institution = scholar %>% 
  read_html(scholar) %>% 
  html_node(".gsc_prf_il") %>% html_text()


tables= scholar %>% 
  read_html(scholar) %>%
    html_table(fill=TRUE)  

scores = data.frame(tables[[1]])
pubs=data.frame(tables[[2]])
pubs=pubs[-1,] 

names(pubs)=c("Title", "Times.Cited", "Year")

names(scores) = c("Metrics", "All scores", "Scores since 2015")






#Word Clouds %%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
word.cloud= scholar %>% 
  read_html(scholar) %>%
  html_nodes("a.gsc_a_at") %>%
  html_text()

Titles=data.frame(pubs=word.cloud)
Titles

Titles= Titles %>% 
  mutate(pubs=as.character(pubs)) %>%
  unnest_tokens(word, pubs, 
                token="words", to_lower=TRUE) %>%
  anti_join(stop_words, by="word") %>%
  count(word, sort=TRUE) %>%
  mutate(cuttoff = mean(n)) %>%
  filter(n >= cuttoff)

p1= pubs %>% 
  mutate(Journals = str_extract(pubs$Title, pattern="\\w+[:space:](?=\\d)")
  ) %>%
  group_by(Year) %>%
  add_tally(name="num_pubs") %>%
  ggplot(aes(x=Year, y=as.numeric(Times.Cited), fill=num_pubs)) + 
  geom_col()  + 
  labs(y="Times cited per year", x="Year", title = name, 
       subtitle = Institution, fill="Number of publications") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



cat("Discipline Keywords:", ifelse(keywords>0, keywords, "Null"), sep=",")


p2= ggplot(Titles, 
       aes(label=word, size=n, color=n)) + 
  geom_text_wordcloud(rm_outside = TRUE, min.freq=mean(Titles$n)) +
  labs(Title="Wordcloud of publication titles") +
  scale_color_gradient(low = "deepskyblue4", high = "deepskyblue") +
  scale_size_area(max_size = 20)

scores = scores %>%
  tableGrob(theme = ttheme_minimal(), rows = NULL)


layout = (p1 /scores ) | p2

return(layout)

} 

