# Get working directory
getwd()

# set working directory
setwd("C:/Users/hongn/Documents/GitHub/Data_Analysis/scrape")

# install.packages("rvest")
library('rvest')

# specify url to be scraped
url = 'http://www.imdb.com/search/title?count=1000&groups=oscar_best_picture_winners&sort=year,desc&view=advanced&ref_=nv_ch_osc_2'

# read HTML code from website
webpage_HTML = read_html(url)

box_data = html_nodes(webpage_HTML,".lister-item.mode-advanced")

# runtime data:
runtime_tmp = box_data %>% html_nodes(".text-muted") %>% html_nodes(".runtime") %>% html_text()
runtime_data = vector()
for (i in 0:length(runtime_tmp)) {
  runtime_data[i] = substr(tmp[i],1,3)
}

# certificate data:
certificate_data = box_data %>% html_nodes(".text-muted") %>% html_nodes(".certificate") %>% html_text()

# genre data:
gen_data = vector()
genre_tmp = box_data %>% html_nodes(".text-muted") %>% html_nodes(".genre") %>% html_text()
for (i in 0:length(genre_tmp)){
  genre_data[i] = trimws(genre_tmp[i])
}

# votes data: (incomplete)
votes_data = box_data %>% html_nodes(".sort-num_votes-visible") %>% html_text()

# star data:
star_data = box_data %>% html_nodes(".ratings-bar") %>% html_nodes("strong") %>% html_text()

# metascore data: (incmplete)
metascore_data = box_data %>% html_nodes(".ratings-bar") %>% html_nodes(".inline-block.ratings-metascore") %>% html_text()
