library(dplyr)
library(rvest)
library(stringi)
library(stringr)
library(xml2)

### HTML -----------------------------------------------------------------------
# Path to scrips_main.html
main_page <- read_html(
  "__DATAandDOCS_Steven_Benaquist/scrapsFromTheLoft/scrips_main.html"
)
# Get the list of titles on the scrips_main page
main_titles <- main_page %>%
  html_nodes("#post-19174 a") %>%
  html_text() %>%
  # Convert to title case
  str_to_title() %>%
  # Remove the 5 password-protected transcripts
  str_subset("Protected", negate = TRUE) %>%
  # Remove the 6 Italian translations and 2 Spanish translations
  str_subset("Italiano|Italiana|Completa", negate = TRUE) %>%
  # Remove redundant words
  str_remove_all(pattern = "Full Transcript|Transcript") %>%
  # Remove whitespace from beginning and end of each title
  str_trim() %>%
  # Replace non-standard characters
  str_replace_all(pattern = fixed("\U2013"), replacement = "-") %>%
  str_replace_all(pattern = fixed("\U2019"), replacement = "'") %>%
  str_replace_all(pattern = fixed("\U2026"), replacement = "...")

### COMEDIAN NAMES -------------------------------------------------------------
# Get first 2 words of titles (most start w/names and most people have 2 names)
first2words <- word(main_titles, start = 1, end = 2)

# Select the words in the titles that are the names
start_word <- rep(1, length(first2words))
end_word <- rep(2, length(first2words))
# Roy Wood Jr., Jimmy O. Yang, Roy Wood Jr., W. Kamau Bell, Big Jay Oakerson
end_word[c(24, 35, 106, 111, 290)] <- 3
# Hasan Minhaj
start_word[82] <- 11
end_word[82] <- 12
# Gina Yashere
start_word[96] <- 3
end_word[96] <- 4
# John Leguizamo, George Carlin
start_word[c(123, 267)] <- 5
end_word[c(123, 267)] <- 6
# Tom Segura, Patton Oswalt, Dave Attell, Daniel Tosh
start_word[c(137, 184, 261, 265)] <- 4
end_word[c(137, 184, 261, 265)] <- 5

# Vector of comedian names
comedians <- main_titles %>%
  word(start = start_word, end = end_word) %>%
  gsub(pattern = "'s", replacement = "") %>%
  str_remove_all("[^ A-Za-z',.-]")
# Manually fix the rest
comedians[4] <- "Larry the Cable Guy"
comedians[c(14, 172)] <- "Michael McIntyre"
comedians[39] <- "T.J. Miller"
comedians[c(42, 143:144, 146, 312, 316, 321:322, 329:330, 349)] <- "Louis C.K."
comedians[48:49] <- "Chris D'Elia"
comedians[99] <- "Cedric the Entertainer"
comedians[121] <- "Gabriel Iglesias"
comedians[136] <- "George Carlin"
comedians[148] <- "Dave Chappelle"
comedians[c(149, 206)] <- "D.L. Hughley"
comedians[250] <- "Bill Maher"
comedians[334] <- "Patrice O'Neal"

names_dat <- data.frame(comedian = comedians)

### SHOW TITLES ----------------------------------------------------------------
# Remove comedian names from the titles
shows <- main_titles %>%
  str_remove(regex(str_flatten(comedians, "|"), ignore_case = TRUE)) %>%
  str_trim()
# Remove some unnecessary punctuation
for (i in seq_along(shows)) {
  if (str_starts(shows[i], fixed(":"))) {
    shows[i] <- str_remove(shows[i], pattern = fixed(":"))
  }
  if (str_starts(shows[i], fixed("-"))) {
    shows[i] <- str_remove(shows[i], pattern = fixed("-"))
  }
  if (str_ends(shows[i], fixed("-"))) {
    shows[i] <- stri_reverse(
      str_remove(stri_reverse(shows[i]), pattern = fixed("-"))
    )
  }
  if (str_ends(shows[i], fixed("[]"))) {
    shows[i] <- stri_reverse(
      str_remove(stri_reverse(shows[i]), pattern = fixed("]["))
    )
  }
}
# Remove extra whitespace
shows <- shows %>%
  str_trim() %>%
  str_squish()

# Fix manually
shows[1] <- shows[1] %>%
  str_replace(":", " -") %>%
  str_c(" (2018)")
shows[c(8:11, 147)] <- str_replace(shows[c(8:11, 147)], "Snl", "SNL")
shows[c(21, 47)] <- str_c(shows[c(21, 47)], " (1993)")
shows[25] <- str_replace(shows[25], "Hbo", "HBO")
shows[27] <- str_c(shows[27], "(2016)")
shows[41] <- str_remove(shows[41], "On The ")
shows[42] <- str_remove(shows[42], " Louis Ck")
shows[50] <- str_c(shows[50], " (1966)")
shows[53] <- str_c(shows[53], " (2015)")
shows[55] <- str_to_upper(shows[55])
shows[c(63, 81)] <- str_replace(shows[c(63, 81)], fixed("|"), fixed("-"))
shows[c(75, 77:78)] <- str_remove(shows[c(75, 77:78)], "Stand-Up - The ")
shows[81] <- str_c(shows[81], " (2019")
shows[89] <- word(shows[89])
shows[89] <- str_c(shows[89], " (2013)")
shows[c(96, 184, 261, 265)] <- str_remove(shows[c(96, 184, 261, 265)], ":")
shows[121] <- "One Show Fits All (2019)"
shows[123] <- "Latin History for Morons (2018)"
shows[130] <- "About USA (2004)"
shows[137] <- "Comedy Central Presents"
shows[137] <- str_c(shows[137], " (2011)")
shows[139] <- str_c(comedians[139], shows[139])
shows[141] <- str_replace(shows[141], "Saturday Night Live", "SNL")
shows[143] <- "SNL Monologue (2012)"
shows[144:145] <- "SNL Monologue (2014)"
shows[146] <- "SNL Monologue (2015)"
shows[148] <- "SNL Monologue (2016)"
shows[158] <- str_c(comedians[158], shows[158], sep = " ")
shows[166] <- "Science (2010)"
shows[175] <- "Live at the O2 London (2018)"
shows[212] <- str_c(comedians[212], shows[212], sep = " - ")
shows[235] <- "Politics (2004)"
shows[236] <- "Dingledodies (2009)"
shows[245] <- "Kill the Messenger (2008)"
shows[246] <- str_replace(shows[246], "D.c.", "D.C.")
shows[249] <- "Ouch!! (2006)"
shows[257] <- "SNL Monologue (1975)"
shows[263] <- "SNL Monologue (1996)"
shows[292] <- str_c(comedians[292], " at USC (1977)")
shows[311] <- str_remove(shows[311], fixed("*"))
shows[313] <- str_replace(shows[313], "Special", "Presents")
shows[332] <- str_remove(shows[332], "At ")
shows[335] <- "Live: Let a Playa Play (2006)"
shows[338] <- str_c(shows[338], " (1972)")
shows[348] <- str_remove(shows[348], "At The ")

names_dat <- cbind(names_dat, show = shows)

### REMOVALS -------------------------------------------------------------------
removals <- c(82, 86, 98, 118, 126, 132, 178, 216, 256, 266:268, 352)
names_dat <- names_dat[-removals,]

# 82 "Brazil..." is an episode of Patriot Act (not stand-up)
# 86 "Gun Compartment" is an excerpt of 112 "Irresponsible"
# 98 "On Babies and Abortion" is an excerpt of 306 "Deadbeat Hero"
# 118 "Politically Correct Language" is from a book
# 126 "Oh, Hello on Broadway" is a duo
# 132 "We've Been Thinking" is a duo
# 178 "On Nationalism" is an excerpt of "No Refunds" (we don't have in English)
# 216 "Live at Laff..." is nearly identical to 21 "The Censored Seven..."
# 256 "On Abortion and "Pro-Lifers"" is an excerpt of 347 "Back in Town"
# 266 "Religion is Bullshit" is an excerpt of 351 "You Are All Diseased"
# 267 "Saturday Night News with George Carlin" is a SNL sketch
# 268 "E Il Controllo..." is an Italian translation of an excerpt of 340 "Bare"
# 352 "On Gun Control" is an excerpt of 340 "Bare"

### YEARS ----------------------------------------------------------------------
names_dat <- cbind(
  names_dat,
  year = as.integer(str_extract(names_dat$show, regex("\\d{4}")))
)

### SAVE -----------------------------------------------------------------------
# write.csv(names_dat, "names.csv", row.names = FALSE)
