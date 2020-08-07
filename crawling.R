## 주의~!
## 파일 경로명에 한글이 들어가 있으면 실행되지 않아요!!

library(dplyr)
library(rvest)

load(file="kbo.RData")

#######################################
# 선수(타자) -- 일자별 기록 (2020시즌)
#######################################
# 1. 기록을 가져올 선수들 리스트를 만든다.
players <- kbo$player %>% filter(GYEAR==2020) %>% select(PCODE, NAME) %>% distinct(PCODE, .keep_all=TRUE)

# 2. 기록을 담을 빈 데이타프레임 준비한다.
hitters_record <- data.frame()

# 3. 크롤링 시작!
for (player in players$PCODE) {
  url <- paste0("https://www.koreabaseball.com/Record/Player/HitterDetail/Daily.aspx?playerId=", player)
  text <- read_html(url)
  nodes <- html_nodes(text, "table.tbl.tt.mb5")
  len <- length(nodes)
  if (len == 0) next
  table <- data.frame()
  for (i in 1:len){
    Sys.setlocale("LC_ALL", "English") # 한글인식을 못하는 문제 때문에 시스템 설정을 바꿈
    tmp <- html_table(nodes[i])
    tmp <- tmp[[1]] %>% rename(gday = names(.[1])) %>% mutate(AVG1=as.numeric(AVG1), AVG2=as.numeric(AVG2))
    table <- bind_rows(table, tmp)
  }
  table <- table %>% mutate(P_ID=player) %>% filter(gday != "합계")
  hitters_record <- bind_rows(hitters_record, table)
}

Sys.setlocale("LC_ALL", "Korean") # 시스템 설정을 원상복귀함

########################################
# 선수(투수) --  일자별 기록 (2020시즌)
########################################
# 1. 기록을 가져올 선수들 리스트를 만든다.
players

# 1.2. url을 찾아 온다.
URL_RECORD_PITCHER_DAILY <- "https://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId="

# 2. 기록을 담을 빈 데이타프레임 준비한다.
pitchers_record <- data.frame()

# 3. 크롤링 시작!
for (player in players$PCODE) {
  url <- paste0(URL_RECORD_PITCHER_DAILY, player)
  text <- read_html(url)
  nodes <- html_nodes(text, "table.tbl.tt.mb5")
  len <- length(nodes)
  if (len == 0) next
  table <- data.frame()
  for (i in 1:len) {
    Sys.setlocale("LC_ALL", "English") # 한글인식을 못하는 문제 때문에 시스템 설정을 바꿈
    tmp <- html_table(nodes[i])
    tmp <- tmp[[1]] %>% rename(gday = names(.[1])) %>% 
      mutate(ERA1=as.numeric(ERA1), ERA2=as.numeric(ERA2))
    tmp <- tmp %>% 
      mutate(INN2=ifelse(is.character(IP), as.numeric(strsplit(IP, split=" ")[[1]][1])*3 + as.numeric(strsplit(IP, split=" ")[[1]][2])*3 , as.numeric(IP)*3)) %>% 
      select(-IP)
    table <- bind_rows(table, tmp)
  }
  table <- table %>% mutate(P_ID=player) %>% filter(gday != "합계")
  pitchers_record <- bind_rows(pitchers_record, table)
}

Sys.setlocale("LC_ALL", "Korean")
