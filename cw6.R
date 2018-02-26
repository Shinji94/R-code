
#' author: 32026312
#' title: Lab # 6
#' date: 11th Nov 2017
#' ---

library(rvest)
library(readr)
library(ggvis)
library(shiny)
library(purrr)
library(stringr)

str = 'http://www.zillow.com/homes/for_sale/Manhattan-New-York-NY/12530_rid/globalrelevanceex_sort/40.894829,-73.795167,40.664754,-74.151879_rect/11_zm/1_p/'
page <- read_html("http://www.zillow.com/homes/for_sale/Manhattan-New-York-NY/12530_rid/globalrelevanceex_sort/40.894829,-73.795167,40.664754,-74.151879_rect/11_zm/1_p/")

houses <- page %>% # extract html details from the photo cards
  html_nodes(".photo-cards li article")
address <- houses %>%
  html_node(".zsg-photo-card-address") %>%
  html_text() %>%
  gsub(',','',.) %>%
  gsub('\\s','_',.)
price <- houses %>%
  html_node(".zsg-photo-card-price") %>%
  html_text() %>%
  readr::parse_number()
params <- houses %>%
  html_node(".zsg-photo-card-info") %>%
  html_text() %>%
  strsplit("\u00b7")
beds <- params %>%
  purrr::map_chr(1) %>%
  gsub('\\D','',.)
baths <- params %>%
  purrr::map_chr(2) %>%
  readr::parse_number()
house_area <- params %>%
  purrr::map_chr(3) %>%
  gsub('\\D','',.)
ny_housing <- data.frame(address,price,beds,baths,house_area)
ny_housing$beds = as.numeric(ny_housing$beds)


head(ny_housing)
summary(ny_housing)#there is no 'sudio' under beds
attach(ny_housing)
#take searching 'R studio' in google scholar for example
#scary  the html raw text of page 1 to page 10 and store it in A LIST

###########note that this is only possible code##########################
#after few attempt of request,got an error :Error in open.connection(x, "rb") : HTTP error 503.
#because of server turn down the request of Web Crawl
# so after we read the html,we could do some pocessing of it


str = 'http://www.zillow.com/homes/for_sale/Manhattan-New-York-NY/12530_rid/globalrelevanceex_sort/40.894829,-73.795167,40.664754,-74.151879_rect/11_zm/1_p/'
for (i in 1:10){
  if (i == 1){
    addrs[i] = str
    page[1] = read_html(addrs[1])
    houses[i] <- page[i] %>% # extract html details from the photo cards
      html_nodes(".photo-cards li article")
    address[i] <- houses[i] %>%
      html_node(".zsg-photo-card-address") %>%
      html_text() %>%
      gsub(',','',.) %>%
      gsub('\\s','_',.)
    price[i] <- houses[i] %>%
      html_node(".zsg-photo-card-price") %>%
      html_text() %>%
      readr::parse_number()
    params[i] <- houses[i] %>%
      html_node(".zsg-photo-card-info") %>%
      html_text() %>%
      strsplit("\u00b7")
    beds[i] <- params[i] %>%
      purrr::map_chr(1) %>%
      gsub('\\D','',.)
    baths[i] <- params[i] %>%
      purrr::map_chr(2) %>%
      readr::parse_number()
    house_area[i] <- params[i] %>%
      purrr::map_chr(3) %>%
      gsub('\\D','',.)
}  else{
  addrs[i] = addrs[i-1]
  substr(addr[i], start=148, stop=148) = as.character(i)
  page[i] = read_html(addrs[i])

  houses[i] <- page[i] %>% # extract html details from the photo cards
    html_nodes(".photo-cards li article")
  address[i] <- houses[i] %>%
    html_node(".zsg-photo-card-address") %>%
    html_text() %>%
    gsub(',','',.) %>%
    gsub('\\s','_',.)
  price[i] <- houses[i] %>%
    html_node(".zsg-photo-card-price") %>%
    html_text() %>%
    readr::parse_number()
  params[i] <- houses[i] %>%
    html_node(".zsg-photo-card-info") %>%
    html_text() %>%
    strsplit("\u00b7")
  beds[i] <- params[i] %>%
    purrr::map_chr(1) %>%
    gsub('\\D','',.)
  baths[i] <- params[i] %>%
    purrr::map_chr(2) %>%
    readr::parse_number()
  house_area[i] <- params[i] %>%
    purrr::map_chr(3) %>%
    gsub('\\D','',.)
  }
}




# next sudo
#Create an interactive ggvis plot to explore one or more variables.
# As I¡¯ve already mentioned, location is a very important factor.
# The longitude and latitude for each property is availble (hint: 
#         look at houses[[1]]). Can you extract this data and plot the
# search results either on a grid or map and highlight the expensive
# parts of the city?

