# Install RSelenium if required. You will need phantomjs in your path or follow instructions
# in package vignettes
# devtools::install_github("ropensci/RSelenium")

library(RSelenium)
library(tidyverse)

# to enter on login screen
login <- "louisa_h_smith@g.harvard.edu"
secret <- rstudioapi::askForPassword()


login_url <- "https://www.bluecrossma.com/wps/portal/login/"

# open chrome browser to page
rD <- rsDriver(browser = "chrome", chromever = "71.0.3578.33")
remDr <- rD[["client"]]
remDr$navigate(login_url)

# find the username and password elements, and send values
username <- remDr$findElement(
  using = "xpath", '//*[@id="useridin"]'
)
username$sendKeysToElement(list(login))

password <- remDr$findElement(
  using = "xpath", '//*[@id="passwordin"]'
)
password$sendKeysToElement(list(secret, key = "enter"))


### below is old, doesn't work, but maybe has some helpful hints

# find which link to click on (of the headers)
webElems <- remDr$findElements(using = "css selector", "li.large-3")
resHeaders <- unlist(lapply(webElems, function(x) {
  x$getElementText()
}))
webElem <- webElems[[which(
  resHeaders == "Review My Claims\nReview my paid and/or pending claims."
)]]
webElem$clickElement()
Sys.sleep(2)

# sort by claim period, get from entire year
elem2 <- remDr$findElement(using = "id", "claimPeriod")
elem2$sendKeysToElement(list("y", key = "enter"))
updateBut <- remDr$findElement(using = "id", "filterClaimsBtn")
updateBut$clickElement()
Sys.sleep(10)

# function to toggle all the boxes to expand
# then extract the data from the correct cells
data_page <- function(remDr) {
  toggs <- remDr$findElements(using = "css selector", "td.dojoxGridCell")
  Sys.sleep(1)

  toggvals <- unlist(lapply(toggs, function(x) {
    x$getElementText()
  }))
  Sys.sleep(3)

  date <- toggvals[seq(2, length(toggvals), by = 9)]
  type <- toggvals[seq(6, length(toggvals), by = 9)]

  goodtoggs <- toggs[which(toggvals == "+")]

  lapply(goodtoggs, function(X) {
    X$clickElement()
    Sys.sleep(2)
  })

  totals <- remDr$findElements(using = "css selector", "tr.total")

  total_vals <- unlist(lapply(totals, function(x) {
    x$getElementText()
  }))
  Sys.sleep(1)

  keep <- tibble(date, type, total_vals)
}

# get the data from the expanded page for each of the 8 pages
# then go to the next
dat <- list()
for (i in 1:8) {
  dat[[i]] <- data_page(remDr)
  nextBut <- remDr$findElement(using = "id", "next")
  nextBut$clickElement()
  Sys.sleep(5)
}

claims <- reduce(dat, bind_rows)
