listOfPackages <- c('tm','stringr','stringi','textclean','textstem', 
                    'ggplot2', 'tidyverse','tidytext', 'dplyr', 'data.table', 
                    'bit64', 'base', 'tidyr', 'wordcloud', 'stats', 'corrplot',
                    'igraph', 'ggraph', 'purrr', 'textmineR','micropan','scales',
                    'qdap','pacman','maps','plotrix','ggmap','readr','gdata',
                    'mapproj',)
for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}


library('data.table')
library('ggplot2')
library('dplyr')
library('bit64')
library('micropan')
library('scales')
library('qdap')
library('pacman')
library('maps')
library(plotrix)
library(ggmap)
library(tm)
library('stringi')
library(readr)
library('gdata')
library('mapproj')
library(textstem)
library(textclean)
library(stats)
library(corrplot)
library(ggraph)
library(plotly)

#TWEET CLEAN of STOPWORDS################################################################################

load("./drive/My Drive/TFM/tte.Rdata")

stop = c('i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', "you're", 
         "you've", "you'll", "you'd", 'your', 'yours', 'yourself', 'yourselves', 'he', 
         'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', "it's", 
         'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 
         'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 'are', 'was', 
         'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 
         'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 
         'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 
         'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 
         'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 
         'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 
         'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don', "don't", 'should', "should've", 
         'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 
         'doesn', "doesn't", 'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', 
         "mightn't", 'mustn', "mustn't", 'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 
         'weren', "weren't", 'won', "won't", 'wouldn', "wouldn't",'would', 'u')

clean <- function(x){
  x <-replace_contraction(x, contraction.key = lexicon::key_contractions,
                          ignore.case = T)
  x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)
  x <- gsub("@\\w+", "", x)
  x <- gsub("\\d+\\w*\\d*", "", x)
  x <- gsub("#\\w+", "", x)
  x <- gsub("[^\x01-\x7F]", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <-tolower(x)
  x <-removePunctuation(x)
  x <-removeNumbers(x)
  x <-removeWords(x,stop)
  x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")
  return(x)
}

tt_lower = clean(tt)
lower= which(!(tte_lower==""))
length(lower)

#Romove words climate and change
stop<- c('climate', 'change')
df <-removeWords(df,stop)

#Preprocess FOR SENtIMeNt ANALYSIS 

#hashtags, urls, caracteres especiales, puntuación, etc)
clean <- function(x){
  x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)
  x <- gsub("@\\w+", "", x)
  x <- gsub("\\d+\\w*\\d*", "", x)
  x <- gsub("#\\w+", "", x)
  x <- gsub("[^\x01-\x7F]", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <-tolower(x)
  x <-removePunctuation(x)
  x <-removeNumbers(x)
  x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")
  return(x)
}

saclean= clean(df)
####LANGUAGE#####################################################################################
pacman::p_load("cld2")
lang1 = cld2::detect_language(text = dt$text, plain_text = FALSE)

lang1= as.data.frame(lang1)
tab_lan= as.data.frame(table(lang1))
tab_lan <-tab_lan %>%
  mutate(prop = percent(Freq / sum(Freq)))
tab_lan <- tab_lan[order(-tab_lan$Freq),]

#see what other languages you have 
lang_other=lang1[lang1!= "en"] #1052938

ggplot(tab_lan) + 
  geom_bar(aes(x= lang1, y= Freq), stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  xlab("Language")+ ylab("Frequency of language")

#filter by only english language and remmove Duplicated TEXT#####################################
dim(df[!duplicated(df$text),])[1] #8424028
n=which(!duplicated(df$text) & (df$lang1 == 'en') & (df$tweet_type != 'retweet')) #7834406
df2= df[n,]

#COUNTING THE CHARACTERS IN EACH TWEET###########################################################
wt= strsplit(df$txt, " ")
w_ch= nchar(wt)
ggplot(w_count)+
  geom_histogram(aes(x=w_count), binwidth=1, fill= "navyblue")+
  labs(x= "Number of words") + theme_minimal()
tab_w= table(w_count)
tab_w= as.data.frame(tab_w)
tab_w= tab_w[order(-tab_w$Freq),]
head(tab_w)

#obtain hashtags ###############################################################################
hashtr= str_extract_all(txt, "#\\w+")
tmp_data_frame <- data.frame() 
for (i in 1:length(hshtr)) { 
  tmp <- map_dfr(hshtr[[i]], data.frame) 
  tmp_data_frame <- rbind(tmp_data_frame, tmp) 
} 

#Hashtag 
htx <- data.frame(txt = hash$hash,
                      stringsAsFactors = FALSE)
htx_count = htx %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  count(word, sort = TRUE)

htx_count %>%
  slice(1:30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Climate Change Tweet",
       x = "", y = "")
#ANALYSIS OF DATE##################################################################################
#DATE _______________________________________________
date= rbind(date0, date1, date2, date3)
date$date <- as.Date(date$parsed_created_at, format = "%Y-%m-%d")
date$year <- format(date$date, "%Y")
date$month <- format(date$date, "%m")

date <- as.data.frame(tweet2$date)

ggplot(tweet2) + 
  geom_bar(aes(x=date), stat="count", fill="steelblue")+
  xlab('Tweets throughout time')+ ylab('')+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(NULL, date_labels = "%b %y") + theme_bw() 

ggplot(date) + 
  geom_bar(aes(x=year, fill=month), stat="count")+
  theme(axis.text.x=element_text(angle=60, hjust=1))

table_month= as.data.frame(table(date$month))
table_date <- table_date %>% 
  mutate(prop = Freq / sum(table_date$Freq) *100)
monthh= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
ggplot(table_month, aes(x = "", y = Freq, fill = month)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = month), position = position_stack(vjust = 0.5))+
  #geom_text(aes(y = Freq,label = percent(prop/100)), position = position_stack(vjust = 0.5))+
  theme_void()+
  scale_fill_brewer(palette="Paired")+
  labs(fill = "Month")

#MAPING OF COuNTRIES 
#making it heterogeneous
location = parse_character(loc$user_location, locale=locale(encoding="UTF-8"))
location= stringi::stri_trans_general(location, "latin-ascii")
location= gsub("[^a-zA-Z]+", " ", location)
location= tolower(location)
location= as.data.frame(location)
tab_loc= as.data.frame(table(location))
tab_loc= tab_loc[-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23),]

cc=location

tab_cc= as.data.frame(table(cc))
tab_cc<- tab_cc[order(-tab_cc$Freq),]

usa <- cc$location[(cc$location %like% " us")]
tab_usa= as.data.frame(table(usa))
tab_usa<- tab_usa[order(-tab_usa$Freq),]
c= tab_usa$usa[tab_usa$Freq > 1000]
for (i in 1:length(c)){
  cc$location[grep(c[i], cc$location)] <- "united states of america"
}


aus <- cc$location[(cc$location %like% "austral")]
tab_aus= as.data.frame(table(aus))
tab_aus<- tab_aus[order(-tab_aus$Freq),]
c= tab_aus$aus[tab_aus$Freq > 1000]
for (i in 1:length(c)){
  cc$location[grep(c[i], cc$location)] <- "australia"
}

lon <- cc$location[(cc$location %like% "london")]
tab_lon= as.data.frame(table(lon))
tab_lon<- tab_lon[order(-tab_lon$Freq),]
c= tab_lon$lon[tab_lon$Freq > 990]
for (i in 1:length(c)){
  cc$location[grep(c[i], cc$location)] <- "london"
}

wash <- cc$location[(cc$location %like% "washington")]
tab_wash= as.data.frame(table(wash))
tab_wash<- tab_wash[order(-tab_wash$Freq),]
c= tab_wash$wash[tab_wash$Freq > 336]
for (i in 1:length(c)){
  cc$location[grep(c[i], cc$location)] <- "washington dc"
}

an <- cc$location[(cc$location %like% "los angeles")]
tab_an= as.data.frame(table(an))
tab_an<- tab_an[order(-tab_an$Freq),]
c= tab_an$an[tab_an$Freq > 310]
for (i in 3:length(c)){
  cc$location[(cc$location %like% c[i])] <- "los angeles"
}

ny <- cc$location[(cc$location %like% "new york")]
tab_ny= as.data.frame(table(ny))
tab_ny<- tab_ny[order(-tab_ny$Freq),]
c= tab_ny$ny[tab_ny$Freq > 389]
for (i in 3:length(c)){
  cc$location[(cc$location %like% c[i])] <- "new york"
}

chi <- cc$location[(cc$location %like% "chicago il")]
tab_chi= as.data.frame(table(chi))
tab_chi<- tab_chi[order(-tab_chi$Freq),]
c= tab_chi$chi[tab_chi$Freq > 40]
for (i in 2:length(c)){
  cc$location[(cc$location %like% c[i])] <- "chicago"
}

tab_cc2<- tab_cc[tab_cc$Freq > 100,]
p=which(tab_cc2$cc %like% "usa")


uk <- sum(tab_cc2$Freq[(tab_cc2$cc %like% " uk")])
uk1 <- sum(tab_cc2$Freq[(tab_cc2$cc %like% "united kingdom")])
tab_cc2$Freq[11] = uk+ uk1
tab_cc2= tab_cc2[-c(which(tab_cc2$cc %like% " uk"), which(tab_cc2$cc %like% "united kingdom")),]

tab_cc2<- tab_cc2[order(-tab_cc2$Freq),]
colnames(tab_cc2) <- c("name", "Freq")
world.cities$country.etc= tolower(world.cities$country.etc) 
WorldData <- map_data('world')

tab_cc2$name= as.character(tab_cc2$name) 
tab_cc2$num <- 1:nrow(tab_cc2) 
tab_cc2$obs[(tab_cc2$name %like% "espana")]= 'spain'
tab_cc2$obs[(tab_cc2$name %like% "italia")][-c(28,20,13,3)]='italy'
tab_cc2$obs[(tab_cc2$name %like% "brasil")]='brazil'
tab_cc2$obs[(tab_cc2$name %like% "belgique")]='belgium'
tab_cc2$obs[(tab_cc2$name %like% "deutschland")]='germany'
tab_cc2$obs[(tab_cc2$name %like% "nederland")]='netherlands'
tab_cc2$obs[(tab_cc2$name %like% "new zealand")]='new zealand'
tab_cc2$obs[(tab_cc2$name %like% "u s a")]='usa'
tab_cc2$obs[(tab_cc2$name %like% "brooklyn")]='usa'
tab_cc2$obs[(tab_cc2$name %like% "new delhi")][-7]='india'
tab_cc2$obs[(tab_cc2$name %like% "sf ")][-c(10,11,15)]='usa'
tab_cc2$obs[(tab_cc2$name %like% "yorkshire")]='uk'
tab_cc2$obs[(tab_cc2$name %like% "new mexico")]='usa'
tab_cc2$obs[(tab_cc2$name %like% "silicon")]='usa'
tab_cc2$obs[(tab_cc2$name %like% "socal")][-c(7,8)]='usa'
tab_cc2$obs[(tab_cc2$name %like% "hong kong")][-c(2,4,6)]='china'
tab_cc2$obs[(tab_cc2$name %like% "mumbai")][-c(5,14,15)]='india'
tab_cc2$obs[(tab_cc2$name %like% "emirates")]= 'united arab emirates'
tab_cc2$obs[(tab_cc2$name %like% "catalunya")]='spain'
tab_cc2$obs[(tab_cc2$name %like% "ivoire")][-3]='ivory coast'
tab_cc2$obs[(tab_cc2$name %like% "south africa")][-25]='south africa'
tab_cc2$obs[(tab_cc2$name %like% "rhode island")]='usa'
tab_cc2$obs[(tab_cc2$name %like% "west coast")][-9]= 'usa'
tab_cc2$obs[(tab_cc2$name %like% "chicagoland")]='usa'

tab_cc2$obs[1392]='usa'
tab_cc2$obs[8510]= "uk"
tab_cc2$obs[488]= "usa"
tab_cc2$obs[c(382,458, 1535,3119, 11308, 11395, 14122)]= "italy"
tab_cc2$obs[c(495,1957,2305,5494, 6864, 7275, 7636,8130,9441,11618,12174, 14708)]= 'usa'
tab_cc2$obs[58]= "canada"
tab_cc2$obs[c(14101,12549, 7153, 3139, 788)]='uk'
tab_cc2$obs[c(433, 6043, 8716,2872)]='usa'
tab_cc2$obs[c(579, 5466, 7013, 10656, 12079, 13509)]='usa'


table= merge(x = tab_cc2, y = world.cities , by = "name", all.x = TRUE)
sum(is.na(table$country.etc))
table$Freq= as.numeric(table$Freq)
tab_cc2$name= as.character(tab_cc2$name) 

data(world.cities)

ciudades = world.cities[order(world.cities[,1], -world.cities[,3]), ]
ciudades = ciudades[!duplicated(ciudades[,'name']),]
ciudades = ciudades[,1:2]

ciudades[,1] = tolower(ciudades[,1])
ciudades[,2] = tolower(ciudades[,2])

ciudades = ciudades[-(which(ciudades[,1] == 'mexico')),]
ciudades = ciudades[-(which(ciudades[,2] == 'jersey')),]



for(i in 1:nrow(tab_cc2)){
  if(tab_cc2[i,1] %in% ciudades[,1]){
    tab_cc2[i,3]= ciudades[(tab_cc2[i,1] == ciudades[,1]),2]}
  else{
    tab_cc2[i,3] = 'NA'
  }
}

ciudades_USA = ciudades[ciudades[,2] == 'usa',1]

stops_es =  stopwords(kind = "es")
stops_en =  stopwords(kind = "en")


ciudades_USA = setdiff(ciudades_USA,stops_es)
ciudades_USA = setdiff(ciudades_USA,stops_en)

estados = tolower(datasets::state.name)
abb = tolower(datasets::state.abb)

abb_out = c('or','la','ok','oh','in','id','me','mi')
abb = setdiff(abb,abb_out)


problematic = c('usa','nyc','america','jersey','us','carolina','francisco')

out = union(abb,problematic)
out = union(out,estados)

locations_USA = union(ciudades_USA,out)


for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% locations_USA){
      tab_cc2[i,3] = 'usa'
      break}
  }}

#paises
for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% ciudades[,2]){
      tab_cc2[i,3] = A[[1]][j]
      break}
  }}


#UK

ciudades_UK = ciudades[ciudades[,2] == 'uk',1]

stops_es =  stopwords(kind = "es")
stops_en =  stopwords(kind = "en")


ciudades_UK = setdiff(ciudades_UK,stops_es)
ciudades_UK = setdiff(ciudades_UK,stops_en)

countries_UK = c('wales','england','scotland')

locations_UK = union(ciudades_UK,countries_UK)
problematic_UK = c('york')
locations_UK = setdiff(locations_UK,problematic_UK)

for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% locations_UK){
      tab_cc2[i,3] = 'uk'
      break}
  }}

#AUSTRALIA

ciudades_aus = ciudades[ciudades[,2] == 'australia',1]


ciudades_aus = setdiff(ciudades_aus,stops_es)
ciudades_aus = setdiff(ciudades_aus,stops_en)



for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% ciudades_aus){
      tab_cc2[i,3] = 'australia'
      break}
  }}

#Canada

ciudades_can = ciudades[ciudades[,2] == 'canada',1]


ciudades_can = setdiff(ciudades_can,stops_es)
ciudades_can = setdiff(ciudades_can,stops_en)



for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% ciudades_can){
      tab_cc2[i,3] = 'canada'
      break}
  }}
#Para las frencuencias y los datos para el mapa final
frecuencias = data.frame(region = character(0),value = numeric(0))

regiones = unique(tab_cc2[,3])
for(i in regiones){
  frecuencias[i,1] = i
  otro = tab_cc2[tab_cc2$obs == i,]
  otro[,2] = as.numeric(otro[,2])
  frecuencias[i,2] = sum(otro[,2])
}

frecuencias$value = log(frecuencias$value)
frecuencias$value = 10^(frecuencias$value)
WorldData$region <- tolower(WorldData$region)
p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = frecuencias, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_viridis_b() +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Count", title="World Map", x="", y="") +
  theme_bw()
p 

#MAPA DE AMERICA #################################################################################

for(i in 1:nrow(tab_cc2)){
  if(tab_cc2[i,1] %in% ciudades[,1]){
    tab_cc2[i,3]= ciudades[(tab_cc2[i,1] == ciudades[,1]),2]}
  else{
    tab_cc2[i,3] = 'NA'
  }
}


ciudades_USA = ciudades[ciudades[,2] == 'usa',1]

stops_es =  stopwords(kind = "es")
stops_en =  stopwords(kind = "en")


ciudades_USA = setdiff(ciudades_USA,stops_es)
ciudades_USA = setdiff(ciudades_USA,stops_en)

estados = tolower(datasets::state.name)
abb = tolower(datasets::state.abb)

abb_out = c('or','la','ok','oh','in','id','me','mi')
abb = setdiff(abb,abb_out)


problematic = c('usa','nyc','america','jersey','us','carolina','francisco')

out = union(abb,problematic)
out = union(out,estados)

locations_USA = union(ciudades_USA,out)


for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% locations_USA){
      tab_cc2[i,3] = 'usa'
      break}
  }}

#paises
for(i in 1:nrow(tab_cc2)){
  A = strsplit(tab_cc2[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% ciudades[,2]){
      tab_cc2[i,3] = A[[1]][j]
      break}
  }}

#US

tab_cc2_USA = tab_cc2[tab_cc2$obs =='usa',]
datasets::state.name
datasets::state.abb
states_rel = data.frame(tolower(datasets::state.abb),tolower(datasets::state.name))

states = union(datasets::state.name,datasets::state.abb)
tolower(states)

for(i in 1:nrow(tab_cc2_USA)){
  A = strsplit(tab_cc2_USA[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% tolower(states)){
      tab_cc2_USA[i,3] = A[[1]][j]
      break}
  }}

for(i in 1:nrow(tab_cc2_USA)){
  if(tab_cc2_USA[i,3] %in% states_rel[,1]){
    tab_cc2_USA[i,3]= states_rel[(tab_cc2_USA[i,3] == states_rel[,1]),2]}
}

tab_cc2_USA

#rest
usa_cities = maps::us.cities[,1:2]
usa_cities$name = tolower(usa_cities$name)
usa_cities$country.etc = tolower(usa_cities$country.etc)
usa_cities

words_st = c('las','port','north','rio','city','new','saint','san','valley','hill','south','new','los','waterloo')
st_words = union(unique(usa_cities$country.etc),words_st)
x  = usa_cities$name
x  =  removeWords(x,st_words)
st_words
usa_cities$name = x 

usa_cities$name = str_squish(usa_cities$name)

usa_cities = usa_cities[-(which(usa_cities[,1] == 'ontario')),]



tab_cc2_rest = tab_cc2_USA[tab_cc2_USA$obs == 'usa',]

for(i in 1:nrow(tab_cc2_rest)){
  A = strsplit(tab_cc2_rest[i,1],split = ' ')
  chars = length(A[[1]])
  for(j in 1:chars){
    if(A[[1]][j] %in% usa_cities$name){
      st = usa_cities[usa_cities$name %in% A[[1]][j],2]
      if(length(st) > 1){ 
        st = st[1]}
      tab_cc2_rest[i,3] = st
      break}
  }}


for(i in 1:nrow(tab_cc2_USA)){
  if(tab_cc2_rest[i,3] %in% states_rel[,1]){
    tab_cc2_rest[i,3]= states_rel[(tab_cc2_rest[i,3] == states_rel[,1]),2]}
}

usa_data = rbind(tab_cc2_USA[tab_cc2_USA$obs != 'usa',], tab_cc2_rest[tab_cc2_rest$obs != 'usa',])

europ= which(usa_data2$name %like% "european")
usa_data2=usa_data2[-europ,]
not= which(usa_data2$name %like% "manhattan"& usa_data2$obs=='kansas')
usa_data2$obs[not]='new york'
usa_data2$obs[(usa_data2$name %like% "manhattan ks")]='kansas'
not= which(usa_data2$name %like% 'upper west side manhattan')
usa_data2$obs[usa_data2$name %like% 'north carolina'& usa_data2$obs=='usa']='north carolina'
usa_data2[usa_data2$name %like% 'north carolina'& usa_data2$obs=='usa',]



frecuencias_US = data.frame(region = character(0),value = numeric(0))

regiones_US = unique(usa_data2[,3])
for(i in regiones_US){
  frecuencias_US[i,1] = i
  otro = usa_data2[usa_data2$obs == i,]
  otro[,2] = as.numeric(otro[,2])
  frecuencias_US[i,2] = sum(otro[,2])
}

frecuencias_US$value = log(frecuencias_US$value)
frecuencias_US<- frecuencias_US[order(-frecuencias_US$value),]

UsaData <- map_data('state') %>% fortify

UsaData$region = tolower(UsaData$region)


p <- ggplot() +
  geom_map(data = UsaData, map = UsaData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = frecuencias_US, map=UsaData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0) +
  scale_fill_viridis_c()+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Count", title="USA Map", x="", y="") +
  theme_bw()
p


frecuencias_US %>%
  slice(2:30) %>%
  ggplot(aes(x= reorder(region,-value), y=value))+
  geom_bar(width = 1, stat = "identity", fill = "lightgreen")+
  labs(x='USA States', y= 'Frequency')+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

#CREATE AUTHOR POOLED DF###########################################################################
new_data = data.frame()
tweet = data[1,1]
id = data[1,2]

for(i in 2:nrow(data)){
  if(id == data[i,2]){
    tweet = paste(tweet,data[i,1], sep = " ")
  }
  else{
    new_data2= data.frame(id,tweet)
    new_data= rbind(new_data, new_data2)
    id = data[i,2]
    tweet = data[i,1]
  }}
new_data2= data.frame(id,tweet)
new_data3= rbind(new_data, new_data2)
colnames(new_data3) = c("id",'tweet')
