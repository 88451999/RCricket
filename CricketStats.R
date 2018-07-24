library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(psych)

clean_bat <- function (batter_frame, match_no, team_name, innings_no) {
  
  # Add 3 cols for Match, Team and Innings
  batter_frame <- mutate(batter_frame, matchid = match_no)
  batter_frame <- mutate(batter_frame, team = team_name)
  batter_frame <- mutate(batter_frame, innings = innings_no)
  
  # Split BatterName to Batter and Dismaissal
  batter_frame <- separate(batter_frame, col = BatterName, into = c("Batter", "Dismissal"), sep = "\r\n", extra = "merge")
  
  # Clean up fields
  batter_frame$Dismissal  <- str_replace(batter_frame$Dismissal,  "\r\n", " ")
  batter_frame$Dismissal <- str_replace(batter_frame$Dismissal, "\r\n", " ")
  batter_frame$Batter <- str_trim(batter_frame$Batter)
  batter_frame$Dismissal <- str_trim(batter_frame$Dismissal)

  # Expand Dismissal
  batter_frame$Dismissal <- str_replace(batter_frame$Dismissal, "^b", "bowled")
  batter_frame$Dismissal <- str_replace(batter_frame$Dismissal, "^c", "caught")
  batter_frame$Dismissal <- str_replace(batter_frame$Dismissal, "^st", "stumped")
  batter_frame$Batter <- str_replace_all(batter_frame$Batter, "^[*|+]", "")
  
  # Add How Out and Out columns
  batter_frame <- mutate(batter_frame, howout = str_extract(batter_frame$Dismissal, "^(bowled|caught|run out|retired not out|lbw|not out|stumped|hit wicket|dnb)"))
  batter_frame <- mutate(batter_frame, out = grepl(x = batter_frame$howout, pattern = "(bowled|caught|run out|lbw|stumped|hit wicket)"))
  
  # Remove Totals/Extras/Overs rows
  batter_frame <- batter_frame[!grepl(x = batter_frame$Batter, pattern = "(Extras|Total|Overs)"), ]
  
  # Make numeric fields nu,eric
  #batter_frame$Runs <- as.numeric(batter_frame$Runs) 
  #batter_frame$BallsFaced <- as.numeric(batter_frame$BallsFaced) 
  #batter_frame$Fours <- as.numeric(batter_frame$Fours) 
  #batter_frame$Sixes <- as.numeric(batter_frame$Sixes) 
  #batter_frame$Runs[which(is.na(batter_frame$Runs))] <- 0 
  #batter_frame$Fours[which(is.na(batter_frame$Fours))] <- 0 
  #batter_frame$Sixes[which(is.na(batter_frame$Sixes))] <- 0 
  
  # Convert characters to numerics
  batter_frame <- mutate_at(batter_frame, vars(Runs:Sixes), funs(as.numeric))
  
  batter_frame <- mutate_at(batter_frame, vars(Runs), funs(replace(., is.na(.), 0)))
  batter_frame <- mutate_at(batter_frame, vars(Fours:Sixes), funs(replace(., is.na(.), 0)))
  #mutate_at(c(5:10), funs(replace(., is.na(.), 0)))
  #direct reference mutate_at(vars(var5:var10), funs(replace(., is.na(.), 0)))
  # Validate
  batter_frame$plus25 <- ifelse(batter_frame$Runs > 24, 1, 0)
  
  batter_frame
}


clean_total <- function (extraTotals, batter, matchResult, match_no, team_name, innings_no, toss) {
  
  extra <- extraTotals[grepl(x = extraTotals$Batter, pattern = "Extras"), ]$Batter
  extra <- sub("\r\n", "", extra)
  abc <- unlist(strsplit(extra, ","))
  abc <- sub("Extras", "", abc)
  abc <- sub("\\(", "", abc)
  abc <- sub("\\)", "", abc)
  abc <- str_trim(abc)
  abc <- as.data.frame(abc)
  extraTot <- extraTotals[grepl(x = extraTotals$Batter, pattern = "Extras"), ]$Runs
  
  abc <- separate(abc, col = abc, into = c("ExtraType", "ExtraRuns"), sep = " ", extra = "merge")
  
  abc <- spread(abc, ExtraType, ExtraRuns)
  
  total <- extraTotals[grepl(x = extraTotals$Batter, pattern = "Total"), ]$Runs
  total <- sub("\r\n", "", total)
  total <- str_trim(sub("\\(cc\\)", "", total))
  
  if (str_detect(total, "/")) {
    split_a <- str_split(total, "/")
    wickets <- as.numeric(split_a[[1]][1])
    runs <- as.numeric(split_a[[1]][2])
  } else {
    wickets <- as.numeric(sum(batter$out))
    runs <- as.numeric(total)
  }
  if (toss == team_name) {
     toss_win = "Win"
  } else {
    toss_win = "Loss"
  }
  overs <- as.numeric(extraTotals[grepl(x = extraTotals$Batter, pattern = "Overs"), ]$Runs)
  total_frame <- cbind(match_no, team_name, innings_no, abc, wickets, runs, overs, toss_win)
  
  if (matchResult[2][3, ] == 'def') {
    winner <- matchResult[1][3, ]
  } else if (matchResult[2][3, ] == 'def by') {
    winner <- matchResult[3][3, ]
  }
  
  if (winner == total_frame$team_name) {
    result <- "Win"
  } else {
    result <- "loss"
  }

  details = str_split(matchResult[2][2, ], "\\s+")

  roundNo <- as.character(details[[1]][2])
  matchDate <- dmy(paste(details[[1]][5], details[[1]][6], details[[1]][7], sep = " "))

  total_frame$b <- as.numeric(total_frame$b) 
  total_frame$w <- as.numeric(total_frame$w) 
  total_frame$nb <- as.numeric(total_frame$nb) 
  total_frame$lb <- as.numeric(total_frame$lb) 
  boundaryTotal <- sum(batter$Fours*4+batter$Sixes*6)
  total_frame <- mutate(total_frame, boundaryTotal = boundaryTotal)
  total_frame <- mutate(total_frame, result = result)
  total_frame <- mutate(total_frame, roundNo = roundNo)
  total_frame <- mutate(total_frame, matchDate = matchDate)
  total_frame <- mutate(total_frame, extraTotal = as.numeric(extraTotals[grepl(x = extraTotals$Batter, pattern = "Extras"), ]$Runs))
  total_frame$balls <- floor(total_frame$overs) * 6 + (total_frame$overs - floor(total_frame$overs))
  total_frame$rpo <- total_frame$runs / total_frame$balls * 6
  batTotal <- spread(as.data.frame(table(batter$howout)), Var1, Freq)
  # Validate
  total_frame$plus25 <- sum(batter$plus25)
  
  total_frame <- cbind(total_frame, ifelse(is.null(batTotal$caught), 0, batTotal$caught), 
                                          ifelse(is.null(batTotal$bowled), 0, batTotal$bowled),
                                          ifelse(is.null(batTotal$lbw), 0, batTotal$lbw),
                                          ifelse(is.null(batTotal$'hit wicket'), 0, batTotal$'hit wicket'), 
                                          ifelse(is.null(batTotal$'not out'), 0, batTotal$'not out'),
                                          ifelse(is.null(batTotal$'run out'), 0, batTotal$'run out'),
                                          ifelse(is.null(batTotal$'retired not out'), 0, batTotal$'retired not out'))
  
  total_frame
}

url <- "http://www.nsjca.asn.au/common/pages/public/rv/"
webpage <- read_html("http://www.nsjca.asn.au/common/pages/public/rv/draw.aspx?entityid=8599&id=RVFIXTURE&&seasonid=126&gradeid=5622_1&")

matches <- webpage %>%
  html_nodes(xpath = '//td/a[contains(@href, "match")]') %>%
  html_attr("href")
#length(matches)
for ( i in 1:length(matches)){
  print(matches[i])
  
  webpage <- read_html(paste(url, matches[i], sep=""))
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  matchTable <- webpage %>%
    html_nodes(xpath = '//table[contains(@class, "matchHeaderTableWide")]') %>%
    html_table(fill = TRUE) 
  bat1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "batting-container")]/table') %>%
    html_table(fill = TRUE)
  bowl1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "bowling-container")]/table') %>%
    html_table(fill = TRUE)
  field1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "fielding-container")]/table') %>%
    html_table(fill = TRUE)
  
  bat2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "batting-container")]/table') %>%
    html_table(fill = TRUE)
  bowl2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "bowling-container")]/table') %>%
    html_table(fill = TRUE)
  field2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "fielding-container")]/table') %>%
    html_table(fill = TRUE) 
  
  matchTable <- as.data.frame(matchTable)
  bat1 <- as.data.frame(bat1)[-1,]
  bat2 <- as.data.frame(bat2)[-1,]
  bowl1 <- as.data.frame(bowl1)[-1,]
  bowl2 <- as.data.frame(bowl2)[-1,]
  
  # rename table headings
  ColnameBat <- c("BatterName", "Runs", "BallsFaced", "Minutes", "Fours", "Sixes")
  ColnameBowl <- c("Bowler", "Overs", "Maidens", "Wickets", "Runs", "NoBalls", "Wides")
  ColnameField <- c("Fielder", "Caught", "RunOutAss", "RunOutUnass", "Stumping", "WkByes")
  ColnameTotal <- c("match_no", "team_name", "innings_no", "b", "lb", "nb", "w", "wickets",
                     "runs", "overs", "toss", "boundaryTotal", "result","roundNo", "matchDate", "extraTotal",
                    "balls", "rpo", "plus25", "caught", "bowled", "lbw", "hitWicket",
                    "notOut", "runOut", "retiredNotOut",
                     "opp_team_name", "opp_innings_no", "opp_b", "opp_lb", "opp_nb", "opp_w", "opp_wickets",
                     "opp_runs", "opp_overs", "opp_boundaryTotal", "opp_extraTotal", "opp_balls", "opp_rpo",
                    "plus25", "opp_caught", "opp_bowled", "opp_lbw", "opp_hitWicket",
                    "opp_notOut", "opp_runOut", "opp_retiredNotOut")
  ColnameTot <- c("match_no", "team_name", "innings_no", "b", "lb", "nb", "w", "wickets",
                    "runs", "overs", "toss", "boundaryTotal", "result","roundNo", "matchDate", "extraTotal",
                    "balls", "rpo", "plus25", "caught", "bowled", "lbw", "hitWicket",
                    "notOut", "runOut", "retiredNotOut")
    if (NROW(bat1) > 0) {
    colnames(bat1) <- ColnameBat
  }
  if (nrow(bat2) > 0) {
    colnames(bat2) <- ColnameBat
  }
  if (nrow(bowl1) > 0) {
    colnames(bowl1) <- ColnameBowl
  }
  if (nrow(bowl2) > 0) {
    colnames(bowl2) <- ColnameBowl
  }
  #colnames(tbls_ls[[5]]) <- ColnameField
  #colnames(tbls_ls[[8]]) <- ColnameField
  # remove row 1 that includes part of the headings
  
  first_innings <- webpage %>%
    html_nodes(xpath = "//div[contains(@class, 'innings-container left')]/div[contains(@class, 'title')]") %>%
    html_text()
  first_innings <- sub(".*- ", "", first_innings)
  second_innings <- webpage %>%
    html_nodes(xpath = "//div[contains(@class, 'innings-container right')]/div[contains(@class, 'title')]") %>%
    html_text()
  second_innings <- sub(".*- ", "", second_innings)
  
  matchid <- webpage %>%
    html_nodes(xpath = "//div[contains(@class, 'matchHeader2')]/div/span[contains(., 'Match')]") %>%
    html_text()
  toss <- webpage %>%
    html_nodes(xpath = "//div[contains(@class, 'matchHeader2')]/div/span[contains(., 'Toss won by')]") %>%
    html_text()
  matchid <- as.numeric(str_trim(sub("Match ID:", "", matchid)))
  toss <- sub("\r\n", "", toss)
  toss <- str_trim(sub("Toss won by:", "", toss))
  #tbls_ls[[3]] <- separate(tbls_ls[[3]], BatterName, c("Batter", "Dismissal"), sep = "\r\n")
  #hist(as.numeric(tbls_ls[[3]]$BallsFaced))
  #ggplot(as.numeric(tbls_ls[[3]][1:10, ]$BallsFaced), as.numeric(tbls_ls[[3]][1:10, ]$Runs))

  if (NROW(bat1) > 0  & NROW(bat2) > 0) {
    bat_one   <- clean_bat(bat1, matchid, first_innings, 1)
    total_one <- clean_total(bat1[grepl(x = bat1$Batter, pattern = "(Extras|Total|Overs)"), ], 
                             bat_one, matchTable, matchid, first_innings, 1, toss)
    bat_two <- clean_bat(bat2, matchid, second_innings, 2)
    total_two <- clean_total(bat2[grepl(x = bat2$Batter, pattern = "(Extras|Total|Overs)"), ], 
                             bat_two, matchTable, matchid, second_innings, 2, toss)

    colnames(total_one) <- ColnameTot
    colnames(total_two) <- ColnameTot
    all_one <- cbind(total_one, total_two$team_name, total_two$innings_no, total_two$b, total_two$lb, total_two$nb, total_two$w, total_two$wickets, 
                     total_two$runs, total_two$overs, total_two$boundaryTotal, total_two$extraTotal, total_two$balls, total_two$rpo, total_two$plus25,
                     total_two$caught, total_two$bowled, total_two$lbw, total_two$hitWicket, total_two$notOut, total_two$runOut, total_two$retiredNotOut)
    all_two <- cbind(total_two, total_one$team_name, total_two$innings_no, total_one$b, total_one$lb, total_one$nb, total_one$w, total_one$wickets, 
                     total_one$runs, total_one$overs, total_one$boundaryTotal, total_one$extraTotal, total_one$balls, total_one$rpo, total_one$plus25,
                     total_one$caught, total_one$bowled, total_one$lbw, total_one$hitWicket, total_one$notOut, total_one$runOut, total_one$retiredNotOut)
    
    colnames(all_one) <- ColnameTotal
    colnames(all_two) <- ColnameTotal
    if (i == 1) {
      all_bat <- rbind(bat_one, bat_two)
      
      all_total <- rbind(all_one, all_two)
    } else {
      all_bat <- rbind(all_bat, bat_one, bat_two)

      all_total <- rbind(all_total, all_one, all_two)
    }
  }
  
}

all_bat$plus25 <- ifelse(all_bat$Runs > 24, 1, 0)
all_bat %>% group_by(matchid, team) %>% summarize(num_plus = sum(plus25))

spread(abc, ExtraType, ExtraRuns)
bat_one <- cbind(bat_one, spread(as.data.frame(table(bat_one$howout)), Var1, Freq))
str(table(bat_one$howout))
head(all_bat, n = 40)
str(all_total)
filter(all_total, result == "Win")
all_bat <- rbind(bat_one, bat_two)
all_total <- rbind(total_one, total_two)

total_win <- all_total %>% 
  filter(result == "Win") 

all_out <- all_bat %>% 
  filter(out == TRUE)

prop.table(table(all_out$howout))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(total_win, aes(x=total_win$runs-total_win$opp_runs, y=total_win$boundaryTotal-total_win$opp_boundaryTotal, 
                      color= team_name)) +
  geom_point()
ggplot(total_win, aes(x=total_win$runs-total_win$opp_runs, y=total_win$extraTotal-total_win$opp_extraTotal, 
                      color= team_name)) +
  geom_point()
ggplot(total_win, aes(x=total_win$runs-total_win$boundaryTotal, y=total_win$opp_runs-total_win$opp_boundaryTotal, 
                      color= team_name)) +
  geom_point()
pairs.panels(total_win[c("runs", "boundaryTotal","extraTotal")])
cor(total_win$runs-total_win$opp_runs, total_win$boundaryTotal-total_win$opp_boundaryTotal)
cor(innOneWin$runs-innOneWin$opp_runs, innOneWin$boundaryTotal-innOneWin$opp_boundaryTotal)

prop.table(table(ifelse(total_win$boundaryTotal-total_win$opp_boundaryTotal > 0, 1, 0)))
prop.table(table(ifelse(total_win$extraTotal-total_win$opp_extraTotal > 0, 1, 0)))
prop.table(table(ifelse(innOneWin$boundaryTotal-innOneWin$opp_boundaryTotal > 0, 1, 0)))
prop.table(table(ifelse(total_win$b - total_win$opp_b > 0, 1, 0)))
prop.table(table(ifelse(innOneWin$extraTotal-innOneWin$opp_extraTotal > 0, 1, 0)))
count(aal_bat)
prop.table(table(ifelse(total_win$runs-total_win$boundaryTotal-total_win$extraTotal-total_win$opp_runs+total_win$opp_boundaryTotal+total_win$opp_extraTotal >= 0, 1, 0)))
innOneWin <- all_total %>% 
  filter(innings_no == 1, result == "Win")
tosswin <- all_total %>% 
  filter(toss == "Win")
barplot(prop.table(table(innOneWin$result)), main = "barplot")
barplot(prop.table(table(tosswin$innings_no)), main = "barplot")

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(innOneWin, aes(x=innOneWin$runs-innOneWin$opp_runs, y=innOneWin$boundaryTotal-innOneWin$opp_boundaryTotal, 
                      color= team_name)) +
  geom_point()
str(all_total)
filter(all_total, result == "Win") %>%
plot(all_total$runs-all_total$opp_runs, all_total$boundaryTotal-all_total$opp_boundaryTotal)
hist(total_win$boundaryTotal-total_win$opp_boundaryTotal)
hist(total_win$extraTotal-total_win$opp_extraTotal)
plot(all_bat$howout)
barplot(prop.table(table(all_bat$howout[c("bowled", "caught")])), main = "barplot")
?barplot
ns_bat <- all_bat %>% 
  filter(team == "North Sydney Red") 
str(ns_bat)
barplot(prop.table(table(ns_bat$howout)), main = "barplot")
mb_bat <- all_bat %>% 
  filter(team == "Mosman Border") 
barplot(table(mb_bat$howout), main = "barplot")
prop.table(table(mb_bat$howout))
library(plyr)
str(all_bat)
count(all_bat, 'team', 'howout')
all_bat %>% group_by(team,howout) %>% tally()
counts <- ddply(all_bat, .(all_bat$team_name, all_bat$howout), nrow)
may need to break up into :
  look for batting container 1st team, bowling container, fielding container
if fielding not there, shuffle...  
<div id="match-scorecard">
  <div class="title">
  1st Innings
</div>
  <div class="innings-container left">
  <div class="title">
  1st

Innings - Lane Cove Haddin</div>
  <div class="batting-container">
  
  test <- webpage %>%
  html_nodes(xpath = '//div[contains(@class, "batting-container")]/table') %>%
  html_attr("href")

for ( i in matches){
  print(i)
  
  webpage <- read_html(paste(url, i, sep=""))
  
  matchTable <- webpage %>%
    html_nodes(xpath = '//table[contains(@class, "matchHeaderTableWide")]') %>%
    html_table(fill = TRUE) 
  
  bat1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "batting-container")]/table') %>%
    html_table(fill = TRUE)
  bowl1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "bowling-container")]/table') %>%
    html_table(fill = TRUE)
  field1 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container left")]//div[contains(@class, "fielding-container")]/table') %>%
    html_table(fill = TRUE)
  
  bat2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "batting-container")]/table') %>%
    html_table(fill = TRUE)
  bowl2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "bowling-container")]/table') %>%
    html_table(fill = TRUE)
  field2 <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "innings-container right")]//div[contains(@class, "fielding-container")]/table') %>%
    html_table(fill = TRUE) 
  fielding-container
  test <- webpage %>%
    html_nodes(xpath = '//div[contains(@class, "batting-container")]/table') %>%
    html_table(fill = TRUE)
  
  i
  
  
  
