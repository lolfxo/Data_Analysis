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



# runtime data
runtime_data_html <- html_nodes(webpage_HTML,'.runtime')