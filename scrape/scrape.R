# Get working directory
getwd()

# set working directory
setwd("C:/Users/hongn/Documents/GitHub/Data_Analysis/scrape")

# install.packages("rvest")
library('rvest')

# specify url to be scraped
url = 'http://www.imdb.com/search/title?count=1000&groups=oscar_best_picture_winners&sort=year,desc&view=advanced&ref_=nv_ch_osc_2'

##### SCRAPE THE DATA #####
# read HTML code from website 
webpage_HTML = read_html(url)

box_data = html_nodes(webpage_HTML,".lister-item.mode-advanced")

name_data = box_data %>% html_nodes(".lister-item-header") %>% html_nodes("a") %>% html_text()

year_tmp = box_data %>% html_nodes(".lister-item-header") %>% html_nodes("span.lister-item-year.text-muted.unbold") %>% html_text()
year_data = as.numeric(gsub("[^0-9\\.]()","",year_tmp))

# runtime data:
runtime_tmp = box_data %>% html_nodes(".text-muted") %>% html_nodes(".runtime") %>% html_text()
runtime_data = vector()
for (i in 1:length(runtime_tmp)) {
  runtime_data[i] = as.numeric(substr(runtime_tmp[i],1,3))
}

# certificate data:
certificate_data = box_data %>% html_nodes(".text-muted") %>% html_nodes(".certificate") %>% html_text()

# genre data:
genre_data = vector()
genre_tmp = box_data %>% html_nodes(".text-muted") %>% html_nodes(".genre") %>% html_text()
for (i in 0:length(genre_tmp)){
  genre_data[i] = trimws(genre_tmp[i])
}

# votes data:
votes_data = vector()
gross_data = vector()
votes_tmp = box_data %>% html_nodes(".sort-num_votes-visible") %>% html_text()
for (i in 1:length(votes_tmp)) {
  tmp = strsplit(votes_tmp[i],"\n")
  votes = tmp[[1]][3]
  gross = tmp[[1]][5]
  
  votes_data[i] = gsub("[^0-9\\.]","",votes)
  gross_data[i] = gsub("[^0-9\\.]","",gross)
}

# Some data missing from the gross data
# gross_data = gross_data[!is.na(gross_data)]

votes_data = as.numeric(votes_data)
gross_data = as.numeric(gross_data)

# star data:
star_data = as.numeric(box_data %>% html_nodes(".ratings-bar") %>% html_nodes("strong") %>% html_text())

# metascore data: (incmplete)
metascore_tmp = box_data %>% html_nodes(".ratings-bar") %>% html_nodes(".inline-block.ratings-metascore") %>% html_text()
metascore_data = vector()
for (i in 1:length(metascore_tmp)) {
  metascore_data[i] = as.numeric(trimws(strsplit(metascore_tmp[i],"\n")[[1]][2]))
}

##### COMBINE DATA INTO A DATA FRAME #####
oscars_df = data.frame(name_data, year_data, runtime_data, certificate_data, genre_data, votes_data, gross_data, star_data)

##### EXPORT THE DATA #####
write.csv(oscars_df, "oscars_data.csv")