library(rvest)
library(tidyverse)


cricinfo_stat_scrape <- function (range_start="DAY MON YEAR",range_end="DAY MON YEAR",discipline=c("batting","bowling","fielding","allround"),match_type=c("Test","ODI","T20I"),captaincy=FALSE,wicketkeeping=FALSE,bat_range=c("0","12"),ball_range=c("min","max"),qualifier=c("rule","min","max"))
{
  from=str_split(range_start,pattern=" ",simplify = TRUE)
  to=str_split(range_end,pattern=" ",simplify = TRUE)
  if (captaincy==FALSE)
  {
    captain=""
  }
  else
  {
    captain="captain=1;"
  }
  if (wicketkeeping==FALSE)
  {
    keeper=""
  }
  else
  {
    keeper="keeper=1;"
  }
  if(match_type=="Test")
  {
    class="class=1;"
  }
  if(match_type=="ODI")
  {
    class="class=2;"
  }
  if(match_type=="T20I")
  {
    class="class=3;"
  }
  # #batting position range
  batting_range_min=paste0("batting_positionmin1=",bat_range[1],";batting_positionval1=batting_position;")
  batting_range_max=paste0("batting_positionmax1=",bat_range[2],";")
  if (discipline!="batting")
  {
    batting_range_min=""
    batting_range_max=""
  }

  #balls bowled range
  bowling_range_min=paste0("ballsmin1=",ball_range[1],";ballsval1=balls;")
  bowling_range_max=paste0("ballsmax1=",ball_range[2],";")
  if (ball_range[1]=="" | ball_range[1]=="min")
  {
    bowling_range_min=""
  }
  if (ball_range[2]=="" | ball_range[2]=="max")
  {
    bowling_range_max=""
  }

  if (discipline!="bowling")
  {
    bowling_range_min=""
    bowling_range_max=""
  }
  
  if (qualifier[1]!="rule")
  {
    qualval1=paste0("qualval1=",qualifier[1],";")
    if (qualifier[2]!="")
    {
      qualmin1=paste0("qualmin1=",qualifier[2],";")
    }
    else
    {
      qualmin1=""
    }
    if (qualifier[3]!="")
    {
      qualmax1=paste0("qualmax1=",qualifier[3],";")
    }
    else
    {
      qualmax1=""
    }
  }
  else
  {
    qualval1=""
    qualmin1=""
    qualmax1=""
  }
  
  
  url=paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?",class,"filter=advanced;",captain,keeper,qualmin1,qualmax1,batting_range_max,batting_range_min,bowling_range_max,bowling_range_min,qualval1,"page=1;spanmax1=",to[1],"+",to[2],"+",to[3],";spanmin1=",from[1],"+",from[2],"+",from[3],";spanval1=span;template=results;type=",discipline)
  webpage <- read_html(url)
  #get total number of pages
  page_num <- html_nodes(webpage,".left,b") %>% html_text() %>% grepl("Page",.)
  page_num <- html_nodes(webpage,".left,b")[page_num]
  page_num <- html_text(html_children(page_num[1])[2])
  page_range <- 1:as.numeric(page_num)
  urls=paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?",class,"filter=advanced;",captain,keeper,qualmin1,qualmax1,batting_range_max,batting_range_min,bowling_range_max,bowling_range_min,qualval1,"page=",page_range,";spanmax1=",to[1],"+",to[2],"+",to[3],";spanmin1=",from[1],"+",from[2],"+",from[3],";spanval1=span;template=results;type=",discipline)
  webpage_total <- lapply(X = urls,FUN = read_html)
  ##go through each page to add players
  players <- vector()
  for (i in page_range)
  {
    add <- webpage_total[[i]] %>% html_nodes(.,".data1") %>% html_text
    players <- c(players,add)
  }
  #arrange stats into matrix
  stats <- t(sapply(str_split(players,pattern="\n"),unlist))
  #get column names
  names <- html_nodes(webpage,"th") %>% html_text()
  stats <- stats[,1:length(names)]
  colnames(stats) <- names
  #tidy up whitespace
  stats <- as_tibble(stats) %>% mutate_if(is.character,str_trim)
  #tidy up empty last column
  stats <- as_tibble(stats[-length(stats)])
  return(stats)
}

##### Select Range
from="30 Dec 2017"
to="30 Dec 2019"

#### Get all eligible players
players_table <- cricinfo_stat_scrape(from,to,"allround","Test")
players <- players_table[[1]]
## get eligible captains
captains_table <- cricinfo_stat_scrape(from,to,"batting","Test",captaincy = TRUE,qualifier = c("innings","5",""))
captains <- captains_table[[1]]
## get eligible keepers
keepers_table <- cricinfo_stat_scrape(from,to,"batting","Test",wicketkeeping = TRUE)
keepers <- keepers_table[[1]]
## get eligible openers
openers_table<- cricinfo_stat_scrape(from,to,"batting","Test",bat_range = c("1","2"))
openers_table <- openers_table %>% mutate(IPM=as.numeric(Inns)/as.numeric(Mat)) %>% filter(IPM>1)
openers <- openers_table[[1]]
## eligible middle order batsmen
batters_table <- cricinfo_stat_scrape(from,to,"batting","Test",bat_range = c("3","7"))
batters_table <- batters_table %>% mutate(IPM=as.numeric(Inns)/as.numeric(Mat)) %>% filter(IPM>1)
batters <- batters_table[[1]]
# ## eligible bowlers
bowlers_table<- cricinfo_stat_scrape(from,to,"bowling","Test")
bowlers_table <- bowlers_table %>% mutate(OPI=as.numeric(Overs)/as.numeric(Inns)) %>% filter(OPI>=10)
bowlers <- bowlers_table[[1]]
### collate into table
table <- players_table %>% mutate(opener= players%in%openers,batter=players%in%batters,keeper=players%in%keepers,bowler=players%in%bowlers,captain=players%in%captains)

## check all-rounders                                                                           
all_rounder_table <- table %>% filter((opener == TRUE | batter == TRUE ) & bowler==TRUE)
all_rounders <- all_rounder_table[[1]]
## check ineligible players
ineligible_table <- table %>% filter(opener == FALSE & batter == FALSE & bowler==FALSE & keeper ==FALSE & captain == FALSE)
ineligible <- ineligible_table[[1]]


#consolidate roles
r=c("Opening Batsman","Batsman","Wicketkeeper","Bowler","Captain")
for (i in 1:nrow(table))
{
  table[i,20]=paste(r[unlist(table[i,15:19])],collapse='/') 
}







