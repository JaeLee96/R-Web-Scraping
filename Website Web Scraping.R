---
title: "Stats 102A - Homework 3"
author: "Jaehyeong Lee"
date: "Fall 2017"
output: html_document
---

Modify this file with your answers and responses.

### Reading:

a. Introduction to dplyr vignette: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
b. How dplyr replaced my most common R idioms: http://www.onthelambda.com/2014/02/10/how-dplyr-replaced-my-most-common-r-idioms/
c. regular expressions tutorial http://regexone.com/

## 1. __Object Oriented Programming for tic-tac-toe__

We revisit the tic-tac-toe problem. 

In the last homework assignment, you had to call `display(state)` to get the board to display. We can take advantage of R's S3 object oriented system by creating a class of object, 'ttt', and creating a `print.ttt()` method.

You first want to create a constructor function `ttt()` that will take an optional character vector, and give it the class attribute 'ttt.'

Then you will want to create a `print.ttt()` method that will display the board as a tic-tac-toe board:

```c
> print(case1)
1 | 2 | 3
---------
4 | 5 | 6
---------
7 | 8 | 9
```

Then, if player "x" were to go in the middle of the board, we replace the "5" with "x". 

```{r exc1samp, error = TRUE}
state <- rep(NA_character_, 9)
state[5] <- "x"
```

```c
> state
1 | 2 | 3
---------
4 | x | 6
---------
7 | 8 | 9
```

In the space below, create two functions. One will be a constructor function called `ttt()` that will create an object of class 'ttt'. Function `ttt()` will accept an optional argument: a character vector of length 9 much like the above listed `state` object. If it is not given any vector, then it will create a new blank state.

The second function will be a `print()` method that is specific to objects of class `ttt` that will print the state into a form that looks like the tic-tac-toe gameboard as described above.

```{r exc1, error = TRUE}
# write your code here

ttt <- function(x = NA) structure(x, class = "ttt")

display <- function(vector) {
  if (is.na(vector[1]) | vector[1] == 1) {a <- 1} else if (tolower(vector[1]) == "x") {a <- "x"} else if (tolower(vector[1]) == "o") {a <- "o"}
  if (is.na(vector[2]) | vector[2] == 2) {b <- 2} else if (tolower(vector[2]) == "x") {b <- "x"} else if (tolower(vector[2]) == "o") {b <- "o"}
  if (is.na(vector[3]) | vector[3] == 3) {c <- 3} else if (tolower(vector[3]) == "x") {c <- "x"} else if (tolower(vector[3]) == "o") {c <- "o"}
  if (is.na(vector[4]) | vector[4] == 4) {d <- 4} else if (tolower(vector[4]) == "x") {d <- "x"} else if (tolower(vector[4]) == "o") {d <- "o"}
  if (is.na(vector[5]) | vector[5] == 5) {e <- 5} else if (tolower(vector[5]) == "x") {e <- "x"} else if (tolower(vector[5]) == "o") {e <- "o"}
  if (is.na(vector[6]) | vector[6] == 6) {f <- 6} else if (tolower(vector[6]) == "x") {f <- "x"} else if (tolower(vector[6]) == "o") {f <- "o"}
  if (is.na(vector[7]) | vector[7] == 7) {g <- 7} else if (tolower(vector[7]) == "x") {g <- "x"} else if (tolower(vector[7]) == "o") {g <- "o"}
  if (is.na(vector[8]) | vector[8] == 8) {h <- 8} else if (tolower(vector[8]) == "x") {h <- "x"} else if (tolower(vector[8]) == "o") {h <- "o"}
  if (is.na(vector[9]) | vector[9] == 9) {i <- 9} else if (tolower(vector[9]) == "x") {i <- "x"} else if (tolower(vector[9]) == "o") {i <- "o"}
  
  cat("\n", a, "|", b, "|", c, "\n ----------", "\n", d, "|", e, "|", f, "\n ----------", "\n", g, "|", h, "|", i)
}


print <- function(x) UseMethod("print")
print.ttt <- function(x) display(x)

```

After you have programmed the function, the following code should work.

```{r esc1test, error = TRUE}
# do not modify the following code
case1 <- rep(NA_character_, 9)  # or if you must, as.character(1:9)
case1[c(1,3)] <- "x"
case1[2] <- "o"
case1 <- ttt(case1)
case1  # should print as a tic-tac-toe board
rm(state)
state <- ttt()
state  # should also print as a tic-tac-toe board
state[5] <- "o"
state
```


## 2. __Basic dplyr exercises__

Install the package `fueleconomy` and load the dataset `vehicles`. Answer the following questions.

```{r exc2data, error = TRUE}
# install.packages("fueleconomy")
library(fueleconomy)
library(dplyr)
library(tidyr)
data(vehicles)
```

a. How many unique vehicle makers (variable `make`) are included in the dataset? 

```{r exc2a}
# write your code here, the output displayed should answer the question.
length(unique(vehicles$make))

```

b. How many vehicles made in 2014 are represented in the dataset?

```{r exc2b}
# write your code here, the output displayed should answer the question.
sum(vehicles$year == 2014)

```

c. For the year 2014, what was the average city mpg (gas mileage) for all compact cars? What was the average city mpg for midsize cars in 2014?

```{r exc2c}
# write your code here, the output displayed should answer the question.
vehicles %>% filter(year == 2014) %>% filter(grepl("compact|Compact", class)) %>%
  summarise(Mean = mean(cty)) 

```

d. For the year 2014, compare makers of midsize cars. Find the average city mpg of midsize cars for each manufacturer. For example, in 2014, Acura has 5 midsize cars with an average city mpg of 20.6, while Audi has 12 midsize cars with an average city mpg of 19.08. 

Produce a table showing the city mpg for 2014 midsize cars for the 27 manufacturers represented in the table. Arrange the results in descending order, so that the manufacturer with the highest average mpg will be listed first.

```{r exc2d}
# write your code here, the output displayed should answer the question.
vehicles %>% filter(year == 2014) %>% filter(grepl("Midsize Cars", class)) %>% group_by(make) %>% summarise(Mean = mean(cty)) %>% arrange(desc(Mean))

```

e. Finally, for the years 1994, 1999, 2004, 2009, and 2014, find the average city mpg of midsize cars for each manufacturer for each year. Use tidyr to transform the resulting output so each manufacturer has one row, and five columns (a column for each year). I have included sample output for the first two rows.

```{r exc2e}

vehicles %>% filter(year %in% c(1994, 1999, 2004, 2009, 2014)) %>% 
  filter(grepl("Midsize Cars", class)) %>% group_by(year, make) %>% 
  summarise(Mean = mean(cty)) %>% spread(year, Mean)

#             make     1994     1999     2004     2009     2014
# 1          Acura       NA 16.50000 17.33333 17.00000 20.60000
# 2           Audi       NA 15.25000 16.20000 15.83333 19.08333

```



## 3. __More advanced dplyr__

I have uploaded a dataset called dr4. It contains the dates that a user visited a website. The website is able to track if the same user visited the site more than once. For the particular date range, the site had 395 visitors, and 130 of them visited more than once. Some of them (13 people) visited the site 5 times.

Using dplyr, find the average time between repeated visits to the site.

You will want to find the total average. 

Be careful when calculating this.

For example, the first user to visit the site more than once (row 2, ,YPELGRZNOQUTNPOH) visited on 6-29, 7-27, 8-3, and 8-11. The time difference for the repeated visits are: 28 days, 7 days, and 8 days, respectively, for an average of 14.33 days. 

The next user with repeated visits is row 3 (SNTCUXUDIHCCSPJA). This person visited on 6-15 and 8-17, a difference of 63 days.

If your dataset had only these two rows, the average time between visits would be (28 + 7 + 8 + 63) / 4 = 26.5 days. It is not ( 14.33 + 63 ) / 2 = 38.66 days.

When I first attempted this, I used `filter(), mutate(), rowwise(), ungroup()`, and `summarise()`. Upon further review, I realized that it is entirely possible to complete this task using only `filter()` and `mutate()` commands. I do not care what combination of commands you use. I do care that you get the correct final result.

*Make sure your final output shows the desired average number of days between visits.*

```{r exc3}
load("dr4.Rdata")
head(dr4)

save <- dr4 %>% mutate(visit1_diff1 = as.numeric(abs(difftime(visit1, visit2, units = 'days'))), 
               visit1_diff2 = as.numeric(abs(difftime(visit2, visit3, units = 'days'))),
               visit1_diff3 = as.numeric(abs(difftime(visit3, visit4, units = 'days'))),
               visit1_diff4 = as.numeric(abs(difftime(visit4, visit5, units = 'days')))
               ) %>% select(visit1_diff1, visit1_diff2, visit1_diff3, visit1_diff4) 

mean(c(save$visit1_diff1, save$visit1_diff2, save$visit1_diff3, save$visit1_diff4), na.rm = TRUE) 

```


## 4. __Scrape baseball-reference.com with rvest__

You will use the package rvest to scrape data from the website baseball-reference.com.

Begin at the teams page <http://www.baseball-reference.com/teams/>.

For each active team (30), visit each team's page and download the "Franchise History" table. The node you will want to use is "#franchise_years". Combine all the tables in one. Note that some franchises have names and locations. To keep track of the team, add a column to the dataframe called "current" which will contain the current name of the team. (e.g. In the 'current' column, the row for 1965 Milwaukee Braves will contain the value 'Atlanta Braves')

__Hint:__ When I ran my code, my table had 2624 rows and 22 columns.

__Hint:__ _I used the function `html_table()` to extract the table from each team's page._

__Important:__ _It is bad manners to repeatedly hit a site with http requests, and could cause your IP to become banned. While you are testing out your code, be sure to test with only two or three teams at a time. Once you get your code running, then you may expand your code to download data for all 30 teams._

```{r exc4, error = TRUE}
library(rvest)
# starting page
teampage <- read_html("http://www.baseball-reference.com/teams/")

# write your r code here
# create a table called baseball that contains all of the teams' franchise histories


library(rvest)
library(data.table)

teampage <- read_html("http://www.baseball-reference.com/teams/")
team_names <- teampage %>% html_nodes("#teams_active .left a") %>% html_text()
team_page_session <- html_session("https://www.baseball-reference.com/teams/")
team_data <- list()

name_storage <- numeric(0)

for (i in team_names) {
  team_data[i] <- team_page_session %>% follow_link(i) %>% 
    html_nodes("#franchise_years") %>% html_table() 
  
  name_storage <- c(name_storage, rep(i, nrow(as.data.frame(team_data[i]))))
}

baseball <- rbindlist(team_data)
baseball$Current <- name_storage


# at the end, be sure to print out the dimensions of your baseball table.
dim(baseball)
head(baseball)
```

__Some light text clean up__

Unfortunately the baseball-reference site makes use the of the non-breaking space character and uses it in places like the space in "Atlanta Braves."

I've written some commands for you that will replace all instances of the non-breaking space and replace it with a standard space character in the baseball table. I've done this part for you.

```{r baseball_cleanup, error = TRUE}
library(stringr)
# This code checks to see if text in table has regular space character
# Because the text from the web uses a non-breaking space, we expect there to be a mismatch
# I'm converting to raw because when displayed on screen, we cannot see the difference between
# a regular breaking space and a non-breaking space.
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))

# identify which columns are character columns
char_cols <- which(lapply(baseball, typeof) == "character")

# for each character column, convert to UTF-8
# then replace the non-breaking space with a regular space
for(i in char_cols){
    baseball[[i]] <- str_conv(baseball[[i]], "UTF-8")
    baseball[[i]] <- str_replace_all(baseball[[i]],"\\s"," ")
    # baseball[[i]] <- str_replace_all(baseball[[i]],"[:space:]"," ")  # you might have to use this depending on your operating system and which meta characters it recognizes
}

# check to see if the conversion worked
## should now be TRUE
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))

```

## 5. __Using dplyr to summarize data__

Once you have created your table, use the data it contains to calculate some summary statistics.

For each franchise, filter the dataset to only include data from the years 2001 to 2016 (inclusive). If the franchise changed team names during this period, include the previous team's data as well. (e.g. the data for the Washington Nationals will also include data for the 2001-2004 Montreal Expos)

Then calculate the following summary statistics for each team across the 16 seasons:

+ _for the years 2001-2016_
+ _total wins_
+ _total losses_
+ _total runs scored_
+ _total runs allowed_
+ _total win percentage (wins / (wins + losses))_

Sort the resulting table (should have a total of 30 rows) by total win percentage. To make sure all 30 rows print, you may need to use `print.data.frame()`, rather than the normal `print()`, which will use the method for tbl.

_Hint:_ At the top of my table, I had the NY Yankees, with a total win percentage of 0.5813

```{r exc5}


summary_table <- baseball %>% filter(Year %in% c(2001:2016)) %>% group_by(Team = Current) %>%
  summarise(Win = sum(W), Loss = sum(L), Run_Scored = sum(R), Run_Allowed = sum(RA),
  Win_Percentage = (sum(W) / (sum(W) + sum(L)))) %>% arrange(desc(Win_Percentage))

print.data.frame(summary_table)

# Enter your r code here
# Your final line of code here should print the summary table in the report
# You may need to use print.data.frame() to make sure the entire table shows.
# All requested columns must appear in the html to receive full credit.
```

## 6. __Regular expressions to extract values in the Managers Column__

Using regular expressions, extract the wins and losses for the managers listed in the managers column. Do not use each season's number of wins or losses. You must extract the information from the managers column using regular expressions. That column has the information written in the form "F.LastName (82-80)". You will need to use capture groups in your regular expression to separate the different pieces of information.

Be careful as some of the rows contain information for more than one manager. Combine all of the manager information to get a total wins and loss value for each of the managers. Many managers have managed more than one team. Be sure to combine all of the win-loss information for the same manager. You may assume that entries that share the same first initial and last name are the same person.

Create a summary table with one line for each manager. The table should contain the following columns, and should be sorted descending by total number of games.

+ _Manager's name (First initial and Last Name)_
+ _Total number of games managed_
+ _Total number of wins across career_
+ _Total number of losses across career_
+ _Total win percentage_

You can independently verify if your information is correct on baseball-reference.com. Each manager has his own page with a total count of wins and losses.

Figuring out the regular expression here is probably the trickiest part. There is also an instance where there are two different people with the same first initial and the same last name. Unfortunately, their information will end up being combined. For this homework assignment, that's okay.

Regarding the regular expression, you will need to use capture groups, and thus `str_match_all()`. We use the _all variant because some of the entries will have multiple managers.

The first line of my table reads: C.Mack, 7679, 3731, 3948, 0.4858706, for manager, games, wins, losses, win percentage.

Watch out for T.La Russa who has a space in his name. He managed the second most number of games with a final record of 2728-2365.

```{r exc6}
# enter your r code here

total_win <- rep(0, 3099)
total_loss <- rep(0, 3099)

Managers <- baseball$Managers
Managers <- strsplit(Managers, "\\,|\\band\\b", "")
Managers <- unlist(Managers)
Managers <- gsub(" ", "", Managers)

managers_names <- sub("\\s*\\(.*", "", Managers)

for (i in 1:length(Managers)) {
total_win[i] <- as.numeric(str_match_all(Managers, "\\((.*)\\-")[[i]][2])
total_loss[i] <- as.numeric(str_match_all(Managers, "\\-(.*)\\)")[[i]][2])
}

summary_table <- data.frame(Managers_Names = managers_names, Total_Win = total_win,
                            Total_Loss = total_loss)

summary <- summary_table %>% group_by(Managers_Names) %>% 
  summarise(total_managed = sum(Total_Win + Total_Loss), total_win = sum(Total_Win),
  total_loss = sum(Total_Loss), win_percent =  total_win / total_managed) %>%
  arrange(desc(total_win))

head(summary, 10)

# your final line of code here should print the first 10 rows of 
# the summary table in the report
# All requested columns must appear in the html to receive full credit.
```
