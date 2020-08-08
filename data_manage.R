library(readxl)
library(dplyr)

#################################################
## 시즌별 엑셀 데이터 가져와서 R 데이터 만들기
#################################################

# 1. 읽어올 파일경로 설정용
# [참고] 경로 샘플은 이것: "../차철성/2020빅콘테스트 데이터분석분야-퓨쳐스리그_스포츠투아이_제공데이터(.xlsx)_시즌별 구분/2020빅콘테스트_스포츠투아이_제공데이터_2016.xlsx"
# R을 실행시킬 위치에 따라서 적절히 수정해 주세요!
path_head <- "../차철성/2020빅콘테스트 데이터분석분야-퓨쳐스리그_스포츠투아이_제공데이터(.xlsx)_시즌별 구분/2020빅콘테스트_스포츠투아이_제공데이터_"
path_tail <- ".xlsx"


# 2. 데이터 읽어오기

# 2-1. 데이터를 담을 빈 변수들을 준비한 뒤
team <- c()               # 1.팀
game <- c()               # 2.경기
player <- c()             # 3.선수
team_pitcher <- c()       # 4.팀투수
team_hitter <- c()        # 5. 팀타자
pitcher <- c()            # 6.개인투수
hitter <- c()             # 7.개인타자
player_register <- c()    # 8.등록선수


# 2-2. 연도별 루프를 돌려 엑셀파일 하나씩 읽기 시작!
for (year in (2016:2020)) {
  
  # 파일 경로 지정 
  # [참고] paste0는 아규먼트들을 붙여서 하나의 문자열로 만드는 기능
  path <- paste0(path_head, year, path_tail)
  
  # 첫번째 시트 읽어올 때는
  team_temp <- read_excel(path=path, sheet=1)
  # 특별히 년도 칼럼 추가
  # [참고] cbind는 칼럼으 붙이기
  team_temp <- cbind(team_temp, year=year)
  
  # 나머지 시트들을 그냥 읽어옴.
  game_temp <- read_excel(path=path, sheet=2)
  player_temp <- read_excel(path=path, sheet=3)
  team_pitcher_temp <- read_excel(path=path, sheet=4)
  team_hitter_temp <- read_excel(path=path, sheet=5)
  pitcher_temp <- read_excel(path=path, sheet=6)
  hitter_temp <- read_excel(path=path, sheet=7)
  player_register_temp <- read_excel(path=path, sheet=8)
  
  # 읽어온 시트들을 준비한 변수에 붙여 넣음
  # 루프가 돌면서 차곡차곡 붙여질 것임
  # [참고] rbind는 행으로 붙이기
  team <- rbind(team, team_temp)
  game <- rbind(game, game_temp)
  player <- rbind(player, player_temp)
  team_pitcher <- rbind(team_pitcher, team_pitcher_temp)
  team_hitter <- rbind(team_hitter, team_hitter_temp)
  pitcher <- rbind(pitcher, pitcher_temp)
  hitter <- rbind(hitter, hitter_temp)
  player_register <- rbind(player_register, player_register_temp)

}


# 3. 데이터 타입 변환

# 날짜가 num 형태로 저장되어 있음 (예시: 20160503)
# Date 타입으로 변환하여 덮어씌움. 
game$GDAY_DS <- as.Date(as.character(game$GDAY_DS), "%Y%m%d")
hitter$GDAY_DS <- as.Date(as.character(hitter$GDAY_DS), "%Y%m%d")
pitcher$GDAY_DS <- as.Date(as.character(pitcher$GDAY_DS), "%Y%m%d")
player_register$GDAY_DS <- as.Date(as.character(player_register$GDAY_DS), "%Y%m%d")
team_hitter$GDAY_DS <- as.Date(as.character(team_hitter$GDAY_DS), "%Y%m%d")
team_pitcher$GDAY_DS <- as.Date(as.character(team_pitcher$GDAY_DS), "%Y%m%d")

# 연도, 선수번호 ... 등은 그냥 num으로 두어도 별탈 없을 듯하여, 일단 그냥 둠.
# 이름, 팀명... 등을 chr 타입임. 일단 그냥 둠. 나중에 factor로 변경해야 햘 수 있음.


#############################
# kbo 리스트 객체 만들기
#############################

# 데이터를 하나의 리스트로 묶어서 저장
# [참고] 리스트는 R의 데이터 타입. 이것저것 하나로 묶을 수 있음.
kbo <-  list(team=team, 
             game=game, 
             player=player, 
             team_pitcher=team_pitcher, 
             team_hitter=team_hitter, 
             pitcher=pitcher, 
             hitter=hitter, 
             player_register=player_register)
# 리스트에서 데이터 추출 방법 :예시) kbo$team


####################
# kbo.RData 만들기
####################
# 공유할 수 있도록 리스트객체를 RData파일로 저장
save(kbo, file="kbo.RData")

# [참고!!] 
# load 함수로 불러서 쓸 수 있음 :예시) load(file="파일경로/kbo.RData")



##################
# 작업 편의를 위한 변수
##################
# 선수이름 라벨
player_names <- kbo$player %>% select(PCODE, NAME) %>% distinct()




#########################
# 선수별 타율 구하기
#########################

# (a) 특정 기간
start_date <- as.Date("2016-01-01", "%Y-%m-%d") # 시작일
end_date <- as.Date("2016-06-30", "%Y-%m-%d") # 종료일

# (b) 특정팀
t_id <- "HH"

# (c) 특정 선수
p_id <- c(60404, 62700)

# (a), (b), (c)를 만족하는 선수의 타율
# [참고] 데이터 가공은 전적으로 dplyr을 사용함.
result <- kbo$hitter %>% 
  filter(GDAY_DS >= start_date, GDAY_DS <= end_date) %>% 
  filter(T_ID == t_id) %>% 
  filter(P_ID %in% p_id) %>% 
  select(P_ID, AB, HIT) %>% 
  group_by(P_ID) %>% 
  summarise(sum_AB=sum(AB), sum_HIT=sum(HIT)) %>% 
  mutate(AVG=sum_HIT/sum_AB)
  

## 결과 출력
left_join(result, player_names, by=c("P_ID" = "PCODE"))
  
############################################################################
# (d) 특정 조건 (출전 경기수, 삼진 10개 이하 등등..)
############################################################################
# 기간 중 30경기 이상 타자로 출전
LOWER_BOUND_GAMES <- 30
# 기간 중 삼진 10개 이하
UPPER_BOUND_KK <- 10

result2 <- kbo$hitter %>% 
  filter(GDAY_DS >= start_date, GDAY_DS <= end_date) %>% 
  select(T_ID, P_ID, KK, AB, HIT) %>%
  group_by(T_ID, P_ID) %>% 
  summarise(sum_game=n(), sum_KK=sum(KK), sum_AB=sum(AB), sum_HIT=sum(HIT)) %>% 
  mutate(AVG=sum_HIT/sum_AB) %>% 
  filter(sum_game >= LOWER_BOUND_GAMES, sum_KK <= UPPER_BOUND_KK )


# 결과 출력
left_join(result2,  player_names, by=c("P_ID" = "PCODE"))


###################################################################
#
# 데이터 마트 만들기!!
#
###################################################################
# team_hitter 데이터와 team_pitcher 데이터 붙이기
team_play <- inner_join(kbo$team_hitter, kbo$team_pitcher, by = c("G_ID", "GDAY_DS", "T_ID", "VS_T_ID", "HEADER_NO", "TB_SC"))
team_play <- left_join(team_play, kbo$game[,c(1, 7)], by = "G_ID")
View(team_play)

save(team_play, file = "team_play.RData")
