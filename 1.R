#' ---
#' author: 32026312
#' title: Lab # 6
#' date: 11th Nov 2017
#' ---



library(rvest)
library(readr)
library(ggvis)
library(shiny)


page <-
  read_html("https://scholar.google.com/citations?user=o-NMg5QAAAAJ&hl=en")
overview <- page %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()
citations <- page %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table() %>%
  .[-1,2] %>%
  readr::parse_number()
head(overview)
head(citations)

Coauthors = page %>%
   html_nodes(css=".gsc_rsb_a_desc a") %>%
   html_text()
Coauthors = as.data.frame(Coauthors)
names(Coauthors)='Coauthors'
head(Coauthors)


mtcars %>% # call data set
  ggvis(~hp, ~mpg, fill := "red", stroke := "black") %>%
  layer_points() %>% # first layer is the points of a scatter plot
  layer_model_predictions(model="lm", se=TRUE)
# second layer is a simple linear model,
# SE=TRUE displays confidence bans around the predictions


#piecewise linear
mtcars %>%
  ggvis(~wt, ~mpg, fill = ~factor(cyl)) %>%
  layer_points() %>%
  group_by(cyl) %>% # tells next layer to group by cylinder factor
  layer_model_predictions(model = "lm", se=TRUE)

mtcars %>%
  ggvis(~wt, ~mpg, fill := "purple", stroke := "black") %>%
  # smoothing layer with input selection for span
  layer_smooths(span = input_slider(0.5, 1, value = 1)) %>%
  #point layer with input selection for size
  layer_points(size := input_slider(100, 1000, value = 100))

mtcars %>%
  ggvis(~wt) %>%
  layer_densities(
    # input slider to select bandwidth of smoother
    adjust = input_slider(.1, 2, value = 1, step = .1,
                          label = "Bandwidth adjustment"),
    kernel = input_select( #input slider to select kernel smoother
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )
        
%>% 