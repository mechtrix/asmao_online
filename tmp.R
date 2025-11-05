library(httr)
library(jsonlite)

foodbank <- httr::GET("https://www.givefood.org.uk/api/2/foodbanks/")

foodbankcontent <- httr::content(foodbank, as = "text")

foodbankJSON <- jsonlite::fromJSON(foodbankcontent)
