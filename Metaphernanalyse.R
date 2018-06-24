# Zeitungsscrape Gesundheit + Algorithmen

library(tm)

library(rvest)
library(stringr)

library(tidytext)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stopwords)
library(xlsx)

# (nicht alle packages werden verwendet) #

###########################################################################


#### test ####
# 1. specify URL
url <- "http://www.sueddeutsche.de/digital/kuenstliche-intelligenz-eine-maschine-gegen-die-depression-1.3431873"

# 2. download static HTML behind the URL and parse it into an XML file
url_parsed <- read_html(url)

# 3. extract specific nodes with CSS (or XPath)
heading_nodes <- html_nodes(url_parsed, css = "h2")
text_nodes <- html_nodes(url_parsed, css = "#article-body")

# 4. extract content from nodes
heading <- html_text(headings_nodes)
heading <- str_replace_all(headings, "\\n|\\t|\\r", "") %>% str_trim()
text <- html_text(text_nodes)
text <- str_replace_all(text, "\\n|\\t|\\r", "") %>% str_trim()
heading
text

SZ <- data.frame(heading, text)



#### Function ####

Scraper <- function(urls, df.name, heading.css, text.css){
  df <- data.frame(stringsAsFactors = F)
  for (i in 1:length(urls)) {
    url <- urls[[i]]
    url_parsed <- read_html(url)
    heading_nodes <- html_nodes(url_parsed, css = heading.css)
    text_nodes <- html_nodes(url_parsed, css = text.css)
    heading <- html_text(heading_nodes)
    heading <- str_replace_all(heading, "\\n|\\t|\\r", "") %>% str_trim()
    text <- html_text(text_nodes)
    text <- str_replace_all(text, "\\n|\\t|\\r", "") %>% str_trim()
    df <- rbind(df,c(heading, text), stringsAsFactors = F)
    print(heading)
  }
  colnames(df) <- c("header", "text")
  assign(df.name, df, envir = .GlobalEnv) 
}


# Stopwords 

stopworte <- get_stopwords("de")

scrape_stopwords <- tibble(word = c("typeof", "undefined", "if", "sde.init.initiqadtile",  "adcontroller", "adcontroller.render", "full", "iqadtile4", "iqadtile8", "sde", "sde.init", "catch", "false", "monitor_articleteaser", "true","try", "window.performance.mark", "ad", "desktop","mobile", "document.getelementbyid", "elem", "window.console.info", "adcontroller.version", "article", "banner", "data","div","document.createelement", "elem.classname","elem.id","elem.setattribute", "function", "on", "parentnode.appendchild", "tile", "type", "var", "window.console", "window.zeit.ismobileview", "iqadtile3", "dass"))

  
  

###### SZ ######
SZ_urls <- list("http://www.sueddeutsche.de/digital/kuenstliche-intelligenz-eine-maschine-gegen-die-depression-1.3431873", "http://www.sueddeutsche.de/gesundheit/neuroinformatik-alarm-am-handgelenk-1.3512859", "http://www.sueddeutsche.de/gesundheit/nanotechnologie-mini-maschinen-im-leib-1.2695064", "http://www.sueddeutsche.de/news/gesundheit/gesundheit-wenn-das-pflaster-die-wunde-ueberwacht-dpa.urn-newsml-dpa-com-20090101-171113-99-845744", "http://www.sueddeutsche.de/news/gesundheit/gesundheit---potsdam-mit-app-gegen-rueckenschmerzen-neues-programm-der-aok-dpa.urn-newsml-dpa-com-20090101-180314-99-476136", "http://www.sueddeutsche.de/news/gesundheit/gesundheit-umfrage-viele-patienten-offen-fuer-robo-docs-dpa.urn-newsml-dpa-com-20090101-170509-99-374982")

SZ <- data.frame(stringsAsFactors = F)
colnames(SZ) <- c("header", "text")

Scraper(SZ_urls, "SZ", heading.css = "h2", text.css = ".body")

SZ_tidy <- unnest_tokens(SZ, word, text)
SZ_tidy <- SZ_tidy %>% anti_join(stopworte)
SZ_tidy <- SZ_tidy %>% anti_join(scrape_stopwords)
SZ_wordcount <- SZ_tidy %>% count(word, sort = TRUE)




##### Die Zeit #####
zeit_urls <- list("https://www.zeit.de/digital/mobil/2014-07/runtastic-orbit-fitness-tracker-test/komplettansicht",
"https://www.zeit.de/2018/01/digitalisierung-gehirn-koerper-krankheiten-kriminalitaet/komplettansicht",
"https://www.zeit.de/kultur/2017-02/automatisierung-pflege-roboter-prekariat-soziale-spaltung/komplettansicht",
"https://www.zeit.de/wissen/gesundheit/2017-08/psychotherapie-handytherapie-depression-psychiatrie-ferntherapie/komplettansicht",
"https://www.zeit.de/zeit-wissen/2015/02/kuenstliche-intelligenz-cognitive-computing-cogs/komplettansicht",
"https://www.zeit.de/zeit-wissen/2010/06/Psychotherapie-Internet-Selbsthilfe/komplettansicht")

Scraper(zeit_urls, "zeit", heading.css = ".article-heading__title", text.css = ".article-page")

zeit_tidy <- unnest_tokens(zeit, word, text)
zeit_tidy <- subset(zeit_tidy, select=c(header,word))
zeit_tidy <- zeit_tidy %>% anti_join(stopworte)
zeit_tidy <- zeit_tidy %>% anti_join(scrape_stopwords)
zeit_wordcount <- zeit_tidy %>% count(word, sort = TRUE)



#### TAZ ####
taz_urls <- list(
"https://www.taz.de/Roboter-als-Hilfskraft-in-der-Pflege/!5504806/",      
"https://www.taz.de/Archiv-Suche/!5495833&s=digital%2Bpflege/",             
"https://www.taz.de/Archiv-Suche/!5497928&s=Künstliche%2BIntelligenz%2Bpflege/",                   
"https://www.taz.de/Archiv-Suche/!5509062&s=digital%2Bpflege/",             "https://www.taz.de/Archiv-Suche/!5493197&s=therapie%2BKI/")

Scraper(taz_urls, "taz", heading.css = ".sectbody", text.css = ".sectbody")
taz$header[1] # = Text und header gemeinsam in header

taz2 <- data.frame(taz$header, stringsAsFactors = F)


taz_tidy <- unnest_tokens(taz2, word, taz.header)
#zeit_tidy <- subset(zeit_tidy, select=c(header,word))
taz_tidy <- taz_tidy %>% anti_join(stopworte)
taz_tidy <- taz_tidy %>% anti_join(scrape_stopwords)
taz_wordcount <- taz_tidy %>% count(word, sort = TRUE)

########
save(SZ,zeit,taz,file="zeitungen_text.RDa")

save(SZ_tidy, SZ_wordcount,
      zeit_tidy, zeit_wordcount,
      taz_tidy, taz_wordcount,
     file = "zeitungen_tidy.RDa")

write.xlsx(SZ_wordcount, file = "SZ_Wordcount.xlsx")
write.xlsx(zeit_wordcount, file = "zeit_Wordcount.xlsx")
write.xlsx(taz_wordcount, file = "taz_Wordcount.xlsx")



######################################################
# Comparative Word Frequencies #

frequency <- bind_rows(mutate(SZ_tidy, outlet = "Süddeutsche Zeitung"),
                       mutate(zeit_tidy, outlet = "Die Zeit"), 
                       mutate(taz_tidy, outlet = "TAZ")) %>% 
  mutate(word = str_extract(word, "[a-z'].*")) %>%
  count(outlet, word) %>%
  group_by(outlet) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(outlet, proportion) 



plot_SZ_zeit <-   ggplot(frequency, aes(`Süddeutsche Zeitung`, `Die Zeit`)) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red")



plot_SZ_taz <-   ggplot(frequency, aes(`Süddeutsche Zeitung`, `TAZ`)) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red")
