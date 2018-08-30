
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
