library(readxl)
library(dplyr)

#################################################
## ?‹œì¦Œë³„ ?—‘??€ ?°?´?„° ê°€? ¸??€?„œ R ?°?´?„° ë§Œë“¤ê¸?
#################################################

# 1. ?½?–´?˜¬ ?ŒŒ?¼ê²½ë¡œ ?„¤? •?š©
# [ì°¸ê³ ] ê²½ë¡œ ?ƒ˜?”Œ??€ ?´ê²?: "../ì°¨ì² ?„±/2020ë¹…ì½˜?…Œ?Š¤?Š¸ ?°?´?„°ë¶„ì„ë¶„ì•¼-?“¨ì³ìŠ¤ë¦¬ê·¸_?Š¤?¬ì¸ íˆ¬?•„?´_? œê³µë°?´?„°(.xlsx)_?‹œì¦Œë³„ êµ¬ë¶„/2020ë¹…ì½˜?…Œ?Š¤?Š¸_?Š¤?¬ì¸ íˆ¬?•„?´_? œê³µë°?´?„°_2016.xlsx"
# R?„ ?‹¤?–‰?‹œ?‚¬ ?œ„ì¹˜ì— ?”°?¼?„œ ? ? ˆ?ˆ ?ˆ˜? •?•´ ì£¼ì„¸?š”!
path_head <- "../ì°¨ì² ?„±/2020ë¹…ì½˜?…Œ?Š¤?Š¸ ?°?´?„°ë¶„ì„ë¶„ì•¼-?“¨ì³ìŠ¤ë¦¬ê·¸_?Š¤?¬ì¸ íˆ¬?•„?´_? œê³µë°?´?„°(.xlsx)_?‹œì¦Œë³„ êµ¬ë¶„/2020ë¹…ì½˜?…Œ?Š¤?Š¸_?Š¤?¬ì¸ íˆ¬?•„?´_? œê³µë°?´?„°_"
path_tail <- ".xlsx"


# 2. ?°?´?„° ?½?–´?˜¤ê¸?

# 2-1. ?°?´?„°ë¥? ?‹´?„ ë¹? ë³€?ˆ˜?“¤?„ ì¤€ë¹„í•œ ?’¤
team <- c()               # 1.??€
game <- c()               # 2.ê²½ê¸°
player <- c()             # 3.?„ ?ˆ˜
team_pitcher <- c()       # 4.??€?ˆ¬?ˆ˜
team_hitter <- c()        # 5. ??€??€?
pitcher <- c()            # 6.ê°œì¸?ˆ¬?ˆ˜
hitter <- c()             # 7.ê°œì¸??€?
player_register <- c()    # 8.?“±ë¡ì„ ?ˆ˜


# 2-2. ?—°?„ë³? ë£¨í”„ë¥? ?Œ? ¤ ?—‘??€?ŒŒ?¼ ?•˜?‚˜?”© ?½ê¸? ?‹œ?‘!
for (year in (2016:2020)) {
  
  # ?ŒŒ?¼ ê²½ë¡œ ì§€? • 
  # [ì°¸ê³ ] paste0?Š” ?•„ê·œë¨¼?Š¸?“¤?„ ë¶™ì—¬?„œ ?•˜?‚˜?˜ ë¬¸ì?—´ë¡? ë§Œë“œ?Š” ê¸°ëŠ¥
  path <- paste0(path_head, year, path_tail)
  
  # ì²«ë²ˆì§? ?‹œ?Š¸ ?½?–´?˜¬ ?•Œ?Š”
  team_temp <- read_excel(path=path, sheet=1)
  # ?Š¹ë³„íˆ ?…„?„ ì¹¼ëŸ¼ ì¶”ê?€
  # [ì°¸ê³ ] cbind?Š” ì¹¼ëŸ¼?œ¼ ë¶™ì´ê¸?
  team_temp <- cbind(team_temp, year=year)
  
  # ?‚˜ë¨¸ì?€ ?‹œ?Š¸?“¤?„ ê·¸ëƒ¥ ?½?–´?˜´.
  game_temp <- read_excel(path=path, sheet=2)
  player_temp <- read_excel(path=path, sheet=3)
  team_pitcher_temp <- read_excel(path=path, sheet=4)
  team_hitter_temp <- read_excel(path=path, sheet=5)
  pitcher_temp <- read_excel(path=path, sheet=6)
  hitter_temp <- read_excel(path=path, sheet=7)
  player_register_temp <- read_excel(path=path, sheet=8)
  
  # ?½?–´?˜¨ ?‹œ?Š¸?“¤?„ ì¤€ë¹„í•œ ë³€?ˆ˜?— ë¶™ì—¬ ?„£?Œ
  # ë£¨í”„ê°€ ?Œë©´ì„œ ì°¨ê³¡ì°¨ê³¡ ë¶™ì—¬ì§? ê²ƒì„
  # [ì°¸ê³ ] rbind?Š” ?–‰?œ¼ë¡? ë¶™ì´ê¸?
  team <- rbind(team, team_temp)
  game <- rbind(game, game_temp)
  player <- rbind(player, player_temp)
  team_pitcher <- rbind(team_pitcher, team_pitcher_temp)
  team_hitter <- rbind(team_hitter, team_hitter_temp)
  pitcher <- rbind(pitcher, pitcher_temp)
  hitter <- rbind(hitter, hitter_temp)
  player_register <- rbind(player_register, player_register_temp)

}


# 3. ?°?´?„° ??€?… ë³€?™˜

# ?‚ ì§œê?€ num ?˜•?ƒœë¡? ??€?¥?˜?–´ ?ˆ?Œ (?˜ˆ?‹œ: 20160503)
# Date ??€?…?œ¼ë¡? ë³€?™˜?•˜?—¬ ?®?–´?”Œ??€. 
game$GDAY_DS <- as.Date(as.character(game$GDAY_DS), "%Y%m%d")
hitter$GDAY_DS <- as.Date(as.character(hitter$GDAY_DS), "%Y%m%d")
pitcher$GDAY_DS <- as.Date(as.character(pitcher$GDAY_DS), "%Y%m%d")
player_register$GDAY_DS <- as.Date(as.character(player_register$GDAY_DS), "%Y%m%d")
team_hitter$GDAY_DS <- as.Date(as.character(team_hitter$GDAY_DS), "%Y%m%d")
team_pitcher$GDAY_DS <- as.Date(as.character(team_pitcher$GDAY_DS), "%Y%m%d")

# ?—°?„, ?„ ?ˆ˜ë²ˆí˜¸ ... ?“±??€ ê·¸ëƒ¥ num?œ¼ë¡? ?‘?–´?„ ë³„íƒˆ ?—†?„ ?“¯?•˜?—¬, ?¼?‹¨ ê·¸ëƒ¥ ?‘ .
# ?´ë¦?, ??€ëª?... ?“±?„ chr ??€?…?„. ?¼?‹¨ ê·¸ëƒ¥ ?‘ . ?‚˜ì¤‘ì— factorë¡? ë³€ê²½í•´?•¼ ?–˜ ?ˆ˜ ?ˆ?Œ.


#############################
# kbo ë¦¬ìŠ¤?Š¸ ê°ì²´ ë§Œë“¤ê¸?
#############################

# ?°?´?„°ë¥? ?•˜?‚˜?˜ ë¦¬ìŠ¤?Š¸ë¡? ë¬¶ì–´?„œ ??€?¥
# [ì°¸ê³ ] ë¦¬ìŠ¤?Š¸?Š” R?˜ ?°?´?„° ??€?…. ?´ê²ƒì?€ê²? ?•˜?‚˜ë¡? ë¬¶ì„ ?ˆ˜ ?ˆ?Œ.
kbo <-  list(team=team, 
             game=game, 
             player=player, 
             team_pitcher=team_pitcher, 
             team_hitter=team_hitter, 
             pitcher=pitcher, 
             hitter=hitter, 
             player_register=player_register)
# ë¦¬ìŠ¤?Š¸?—?„œ ?°?´?„° ì¶”ì¶œ ë°©ë²• :?˜ˆ?‹œ) kbo$team


####################
# kbo.RData ë§Œë“¤ê¸?
####################
# ê³µìœ ?•  ?ˆ˜ ?ˆ?„ë¡? ë¦¬ìŠ¤?Š¸ê°ì²´ë¥? RData?ŒŒ?¼ë¡? ??€?¥
save(kbo, file="kbo.RData")

# [ì°¸ê³ !!] 
# load ?•¨?ˆ˜ë¡? ë¶ˆëŸ¬?„œ ?“¸ ?ˆ˜ ?ˆ?Œ :?˜ˆ?‹œ) load(file="?ŒŒ?¼ê²½ë¡œ/kbo.RData")



##################
# ?‘?—… ?¸?˜ë¥? ?œ„?•œ ë³€?ˆ˜
##################
# ?„ ?ˆ˜?´ë¦? ?¼ë²?
player_names <- kbo$player %>% select(PCODE, NAME) %>% distinct()




#########################
# ?„ ?ˆ˜ë³? ??€?œ¨ êµ¬í•˜ê¸?
#########################

# (a) ?Š¹? • ê¸°ê°„
start_date <- as.Date("2016-01-01", "%Y-%m-%d") # ?‹œ?‘?¼
end_date <- as.Date("2016-06-30", "%Y-%m-%d") # ì¢…ë£Œ?¼

# (b) ?Š¹? •??€
t_id <- "HH"

# (c) ?Š¹? • ?„ ?ˆ˜
p_id <- c(60404, 62700)

# (a), (b), (c)ë¥? ë§Œì¡±?•˜?Š” ?„ ?ˆ˜?˜ ??€?œ¨
# [ì°¸ê³ ] ?°?´?„° ê°€ê³µì?€ ? „? ?œ¼ë¡? dplyr?„ ?‚¬?š©?•¨.
result <- kbo$hitter %>% 
  filter(GDAY_DS >= start_date, GDAY_DS <= end_date) %>% 
  filter(T_ID == t_id) %>% 
  filter(P_ID %in% p_id) %>% 
  select(P_ID, AB, HIT) %>% 
  group_by(P_ID) %>% 
  summarise(sum_AB=sum(AB), sum_HIT=sum(HIT)) %>% 
  mutate(AVG=sum_HIT/sum_AB)
  

## ê²°ê³¼ ì¶œë ¥
left_join(result, player_names, by=c("P_ID" = "PCODE"))
  
############################################################################
# (d) ?Š¹? • ì¡°ê±´ (ì¶œì „ ê²½ê¸°?ˆ˜, ?‚¼ì§? 10ê°? ?´?•˜ ?“±?“±..)
############################################################################
# ê¸°ê°„ ì¤? 30ê²½ê¸° ?´?ƒ ??€?ë¡? ì¶œì „
LOWER_BOUND_GAMES <- 30
# ê¸°ê°„ ì¤? ?‚¼ì§? 10ê°? ?´?•˜
UPPER_BOUND_KK <- 10

result2 <- kbo$hitter %>% 
  filter(GDAY_DS >= start_date, GDAY_DS <= end_date) %>% 
  select(T_ID, P_ID, KK, AB, HIT) %>%
  group_by(T_ID, P_ID) %>% 
  summarise(sum_game=n(), sum_KK=sum(KK), sum_AB=sum(AB), sum_HIT=sum(HIT)) %>% 
  mutate(AVG=sum_HIT/sum_AB) %>% 
  filter(sum_game >= LOWER_BOUND_GAMES, sum_KK <= UPPER_BOUND_KK )


# ê²°ê³¼ ì¶œë ¥
left_join(result2,  player_names, by=c("P_ID" = "PCODE"))


###################################################################
#
# ?°?´?„° ë§ˆíŠ¸ ë§Œë“¤ê¸?!!
#
###################################################################
# team_hitter ?°?´?„°??€ team_pitcher ?°?´?„° ë¶™ì´ê¸?
team_play <- inner_join(kbo$team_hitter, kbo$team_pitcher, by = c("G_ID", "GDAY_DS", "T_ID", "VS_T_ID", "HEADER_NO", "TB_SC"))
team_play <- left_join(team_play, kbo$game[,c(1, 7)], by = "G_ID")
View(team_play)

save(team_play, file = "team_play.RData")


#################################################################
#
# ÆÀº° µ¥ÀÌÅÍ (2016³â~2020³â)
#
#################################################################
## Åõ¼öµ¥ÀÌÅÍ ¸ğÀ¸±â - µÎÈñ
path_head <- "../À±µÎÈñ/"
path_tail <- "_ÆÀÅõ¼ö.xlsx"
hitter_temp <- data.frame()
for(year in 2016:2020){
  for(team in unique(kbo$team$T_NM)){
    try(sheet <- read_excel(paste0(path_head, year, path_tail), sheet = team))
    hitter_temp <- bind_rows(hitter_temp, sheet)
    print(paste(year, team ))
  }
}
hitter_temp <- hitter_temp %>% distinct() %>% dplyr::select(G_ID, GDAY_DS, T_ID, WLS, ÇÇ¾ÈÅ¸À²:ÆøÅõ)

## Å¸ÀÚµ¥ÀÌÅÍ ¸ğÀ¸±â - Ã¶¼º
path_head <- "../Â÷Ã¶¼º/dt"
path_tail <- "_th_addvar.xlsx"
pitcher_temp <- data.frame()
for(year in 16:20){
  pitcher_temp <- bind_rows(pitcher_temp, read_excel(paste0(path_head, year, path_tail)))
}
pitcher_temp <- pitcher_temp %>% dplyr::select(G_ID, GDAY_DS, T_ID, Å¸À²:µµ·ç¼º°ø·ü)

## µ¥ÀÌÅÍ ÇÕÄ¡±â
data <- inner_join(hitter_temp,pitcher_temp,  by=c("G_ID", "GDAY_DS", "T_ID"))
data <- data %>% rename(P1=ÇÇ¾ÈÅ¸À²,
                          P2=Æò±ÕÀÚÃ¥Á¡,
                          P3=Å»»ïÁøÀ²,
                          P4="ÀÌ´×´ç Ãâ·çÇã¿ë·ü",
                          D1=½ÇÃ¥,
                          D2=º´»ìÅ¸,
                          D3=ÆøÅõ,
                          B1=Å¸À²,
                          B2=¼±±¸¾È,
                          B3=Ãâ·çÀ²,
                          B4=ÀåÅ¸À²,
                          C1=È¨·±,
                          C2=Å¸Á¡À²,
                          C3=µæÁ¡±ÇÅ¸À²,
                          C4=ÀÜ·ç,
                          S1=µµ·ç,
                          S2=µµ·ç¼º°ø·ü)


## µ¥ÀÌÅÍ ÀüÃ³¸®

data <- data %>% 
# YEAR : ½ÃÁğ
  mutate(YEAR= substr(G_ID, 1, 4)) %>% 
# ½ÃÁğ°ú ÆÀÀ¸·Î ±×·çÇÎ
  group_by(YEAR, T_ID) %>% 
# W :  ÀÌ±â¸é "WIN", Áö¸é "LOOSE", ºñ°Üµµ "WIN"
  mutate(W=ifelse(WLS=="L", "LOOSE", "WIN")) %>% 
# WS : ÀÌ±â¸é 3, Áö¸é 0, ºñ±â¸é 1
  mutate(WS=ifelse(WLS=="W", 3, ifelse(WLS=="D", 1, 0))) %>% 
# WC : WSÀÇ ´©°è
  mutate(WC=cumsum(WS))


# S2ÀÇ °áÃøÄ¡¸¦ ½ÃÁğº° µµ·ç¼º°ø·üÆò±ÕÀ¸·Î ´ëÃ¼
avg_S2 <- data %>% group_by(YEAR, T_ID) %>% summarise(mean = mean(S2, na.rm = TRUE))


for(year in 2016:2020){
  for(team in team_list$T_ID){
    year <- as.character(year)
    avg_S2_year_team <- avg_S2$mean[which(avg_S2$YEAR==year & avg_S2$T_ID==team)]
    data$S2[which(data$YEAR==year& data$T_ID==team & is.na(data$S2))] <- avg_S2_year_team
    }
}


# W : factor·Î º¯È¯
data$W <- as.factor(data$W)

# YEAR : numÀ¸·Î º¯È¯
data$YEAR <- as.numeric(data$YEAR)

str(data)


####################################################################################################
# µ¥ÀÌÅÍ normalizing ÇÏ¿© p1, p2, p3, d1, d2, d3, b1, b2, b3, b4, c1, c2, c3, c4, s1, s2 ¸¸µé±â
####################################################################################################

## Q1º¸´Ù ÀÛÀ¸¸é 0, Q3º¸´Ù Å©¸é 1, Áß°£ÀÌ¸é IQR·Î ³ª´²ÁÜ

data_norm <- data %>% group_by(YEAR, T_ID) %>%
  mutate(p1= ifelse(P1< quantile(P1, 0.25), 0, ifelse(P1< quantile(P1, 0.75), (P1-quantile(P1, 0.25))/IQR(P1), 1))) %>% 
  mutate(p2= ifelse(P2< quantile(P2, 0.25), 0, ifelse(P2< quantile(P2, 0.75), (P2-quantile(P2, 0.25))/IQR(P2), 1))) %>% 
  mutate(p3= ifelse(P3< quantile(P3, 0.25), 0, ifelse(P3< quantile(P3, 0.75), (P3-quantile(P3, 0.25))/IQR(P3), 1))) %>% 
  mutate(p4= ifelse(P4< quantile(P4, 0.25), 0, ifelse(P4< quantile(P4, 0.75), (P4-quantile(P4, 0.25))/IQR(P4), 1))) %>% 
  
  mutate(d1= ifelse(D1< quantile(D1, 0.25), 0, ifelse(D1< quantile(D1, 0.75), (D1-quantile(D1, 0.25))/IQR(D1), 1))) %>% 
  mutate(d2= ifelse(D2< quantile(D2, 0.25), 0, ifelse(D2< quantile(D2, 0.75), (D2-quantile(D2, 0.25))/IQR(D2), 1))) %>% 
  mutate(d3= ifelse(D3< quantile(D3, 0.25), 0, ifelse(D3< quantile(D3, 0.75), (D3-quantile(D3, 0.25))/IQR(D3), 1))) %>% 
  
  mutate(b1= ifelse(B1< quantile(B1, 0.25), 0, ifelse(B1< quantile(B1, 0.75), (B1-quantile(B1, 0.25))/IQR(B1), 1))) %>% 
  mutate(b2= ifelse(B2< quantile(B2, 0.25), 0, ifelse(B2< quantile(B2, 0.75), (B2-quantile(B2, 0.25))/IQR(B2), 1))) %>% 
  mutate(b3= ifelse(B3< quantile(B3, 0.25), 0, ifelse(B3< quantile(B3, 0.75), (B3-quantile(B3, 0.25))/IQR(B3), 1))) %>% 
  mutate(b4= ifelse(B4< quantile(B4, 0.25), 0, ifelse(B4< quantile(B4, 0.75), (B4-quantile(B4, 0.25))/IQR(B4), 1))) %>% 
  
  mutate(c1= ifelse(C1< quantile(C1, 0.25), 0, ifelse(C1< quantile(C1, 0.75), (C1-quantile(C1, 0.25))/IQR(C1), 1))) %>% 
  mutate(c2= ifelse(C2< quantile(C2, 0.25), 0, ifelse(C2< quantile(C2, 0.75), (C2-quantile(C2, 0.25))/IQR(C2), 1))) %>% 
  mutate(c3= ifelse(C3< quantile(C3, 0.25), 0, ifelse(C3< quantile(C3, 0.75), (C3-quantile(C3, 0.25))/IQR(C3), 1))) %>% 
  mutate(c4= ifelse(C4< quantile(C4, 0.25), 0, ifelse(C4< quantile(C4, 0.75), (C4-quantile(C4, 0.25))/IQR(C4), 1))) %>% 
  
  mutate(s1= ifelse(S1< quantile(S1, 0.25), 0, ifelse(S1< quantile(S1, 0.75), (S1-quantile(S1, 0.25))/IQR(S1), 1))) %>% 
  mutate(s2= ifelse(S2< quantile(S2, 0.25), 0, ifelse(S2< quantile(S2, 0.75), (S2-quantile(S2, 0.25))/IQR(S2), 1))) %>% 
  ungroup()
  
str(data_norm)

# Á¤±ÔÈ­(?) ÇÑ µ¥ÀÌÅÍ¸¦ ÀúÀå
save(data_norm, file="data_norm.RData")
write.csv(data_norm, file="data_norm.csv")



##################################################################################################
# µ¥ÀÌÅÍ normalizing ÇÏ¿© p1, p2, p3, d1, d2, d3, b1, b2, b3, b4, c1, c2, c3, c4, s1, s2 ¸¸µé±â
##################################################################################################

# inner fence º¸´Ù ÀÛÀ¸¸é 0, Å©¸é 1, »çÀÌ´Â Á¤±ÔÈ­
# Áï, ÇÏÇÑÀº Q1 - (1.5 * IQR), »óÇÑÀº Q3 + (1.5 * IQR)
# D1Àº IQRÀÌ 0ÀÌ¾î¼­ °è»êÁß¿¡ ºĞ¸ğ°¡ 0, d1´Â »ç¿ëÇÒ ¼ö ¾øÀ½.

data_norm_inner <- data %>% group_by(YEAR, T_ID) %>%
  mutate(p1= ifelse(P1< quantile(P1, 0.25)-1.5*IQR(P1), 0, ifelse(P1< quantile(P1, 0.75)+1.5*IQR(P1), (P1-quantile(P1, 0.25)+1.5*IQR(P1))/(IQR(P1)*4), 1))) %>% 
  mutate(p2= ifelse(P2< quantile(P2, 0.25)-1.5*IQR(P2), 0, ifelse(P2< quantile(P2, 0.75)+1.5*IQR(P2), (P2-quantile(P2, 0.25)+1.5*IQR(P2))/(IQR(P2)*4), 1))) %>% 
  mutate(p3= ifelse(P3< quantile(P3, 0.25)-1.5*IQR(P3), 0, ifelse(P3< quantile(P3, 0.75)+1.5*IQR(P3), (P3-quantile(P3, 0.25)+1.5*IQR(P3))/(IQR(P3)*4), 1))) %>% 
  mutate(p4= ifelse(P4< quantile(P4, 0.25)-1.5*IQR(P4), 0, ifelse(P4< quantile(P4, 0.75)+1.5*IQR(P4), (P4-quantile(P4, 0.25)+1.5*IQR(P4))/(IQR(P4)*4), 1))) %>% 
  
  mutate(d1= ifelse(D1< quantile(D1, 0.25)-1.5*IQR(D1), 0, ifelse(D1< quantile(D1, 0.75)+1.5*IQR(D1), (D1-quantile(D1, 0.25)+1.5*IQR(D1))/(IQR(D1)*4), 1))) %>% 
  mutate(d2= ifelse(D2< quantile(D2, 0.25)-1.5*IQR(D2), 0, ifelse(D2< quantile(D2, 0.75)+1.5*IQR(D2), (D2-quantile(D2, 0.25)+1.5*IQR(D2))/(IQR(D2)*4), 1))) %>% 
  mutate(d3= ifelse(D3< quantile(D3, 0.25)-1.5*IQR(D3), 0, ifelse(D3< quantile(D3, 0.75)+1.5*IQR(D3), (D3-quantile(D3, 0.25)+1.5*IQR(D3))/(IQR(D3)*4), 1))) %>% 
  
  mutate(b1= ifelse(B1< quantile(B1, 0.25)-1.5*IQR(B1), 0, ifelse(B1< quantile(B1, 0.75)+1.5*IQR(B1), (B1-quantile(B1, 0.25)+1.5*IQR(B1))/(IQR(B1)*4), 1))) %>% 
  mutate(b2= ifelse(B2< quantile(B2, 0.25)-1.5*IQR(B2), 0, ifelse(B2< quantile(B2, 0.75)+1.5*IQR(B2), (B2-quantile(B2, 0.25)+1.5*IQR(B2))/(IQR(B2)*4), 1))) %>% 
  mutate(b3= ifelse(B3< quantile(B3, 0.25)-1.5*IQR(B3), 0, ifelse(B3< quantile(B3, 0.75)+1.5*IQR(B3), (B3-quantile(B3, 0.25)+1.5*IQR(B3))/(IQR(B3)*4), 1))) %>% 
  mutate(b4= ifelse(B4< quantile(B4, 0.25)-1.5*IQR(B4), 0, ifelse(B4< quantile(B4, 0.75)+1.5*IQR(B4), (B4-quantile(B4, 0.25)+1.5*IQR(B4))/(IQR(B4)*4), 1))) %>% 
  
  mutate(c1= ifelse(C1< quantile(C1, 0.25)-1.5*IQR(C1), 0, ifelse(C1< quantile(C1, 0.75)+1.5*IQR(C1), (C1-quantile(C1, 0.25)+1.5*IQR(C1))/(IQR(C1)*4), 1))) %>% 
  mutate(c2= ifelse(C2< quantile(C2, 0.25)-1.5*IQR(C2), 0, ifelse(C2< quantile(C2, 0.75)+1.5*IQR(C2), (C2-quantile(C2, 0.25)+1.5*IQR(C2))/(IQR(C2)*4), 1))) %>% 
  mutate(c3= ifelse(C3< quantile(C3, 0.25)-1.5*IQR(C3), 0, ifelse(C3< quantile(C3, 0.75)+1.5*IQR(C3), (C3-quantile(C3, 0.25)+1.5*IQR(C3))/(IQR(C3)*4), 1))) %>% 
  mutate(c4= ifelse(C4< quantile(C4, 0.25)-1.5*IQR(C4), 0, ifelse(C4< quantile(C4, 0.75)+1.5*IQR(C4), (C4-quantile(C4, 0.25)+1.5*IQR(C4))/(IQR(C4)*4), 1))) %>% 
  
  mutate(s1= ifelse(S1< quantile(S1, 0.25)-1.5*IQR(S1), 0, ifelse(S1< quantile(S1, 0.75)+1.5*IQR(S1), (S1-quantile(S1, 0.25)+1.5*IQR(S1))/(IQR(S1)*4), 1))) %>% 
  mutate(s2= ifelse(S2< quantile(S2, 0.25)-1.5*IQR(S2), 0, ifelse(S2< quantile(S2, 0.75)+1.5*IQR(S2), (S2-quantile(S2, 0.25)+1.5*IQR(S2))/(IQR(S2)*4), 1))) %>% 
  ungroup()

str(data_norm_inner)

save(data_norm_inner, file="data_norm.RData")
write.csv(data_norm_inner, file="data_norm.csv")


#############################################################################################
# ¼öÁ¤µÈ data_norm_2.csv¸¦ º¯°æÇÏ±â
#############################################################################################
# ¼öÁ¤ ³»¿ëÀº p1,.... s1À» min max ³ë¸»¶óÀÌÁî ÇÏ±â

data_norm_2 <- read.csv("data_norm_2.csv")

str(data_norm_2)

data_norm_3 <- data_norm_2 %>% group_by(YEAR) %>% mutate(p1=(P1-min(P1))/(max(P1)-min(P1)),
                                          p2=(P2-min(P2))/(max(P2)-min(P2)),
                                          p3=(P3-min(P3))/(max(P3)-min(P3)),
                                          p4=(P4-min(P4))/(max(P4)-min(P4)),
                                          d1=(D1-min(D1))/(max(D1)-min(D1)),
                                          d2=(D2-min(D2))/(max(D2)-min(D2)),
                                          d3=(D3-min(D3))/(max(D3)-min(D3)),
                                          b1=(B1-min(B1))/(max(B1)-min(B1)),
                                          b2=(B2-min(B2))/(max(B2)-min(B2)),
                                          b3=(B3-min(B3))/(max(B3)-min(B3)),
                                          b4=(B4-min(B4))/(max(B4)-min(B4)),
                                          c1=(C1-min(C1))/(max(C1)-min(C1)),
                                          c2=(C2-min(C2))/(max(C2)-min(C2)),
                                          c3=(C3-min(C3))/(max(C3)-min(C3)),
                                          c4=(C4-min(C4))/(max(C4)-min(C4)),
                                          s1=(S1-min(S1))/(max(S1)-min(S1)),
                                          s2=(S2-min(S2))/(max(S2)-min(S2)))

write.csv(data_norm_3, file="data_norm_3.csv")

##############################################################################################################
# data_norm_3¿Í 2016_validation.csv ¸¦ °ËÁõÇÏ·Á ÇÔ. .... ÀÛµ¿ÇÏÁö ¾ÊÀ½.

validation_2016 <- read.csv("2016_validation.csv")
str(validation_2016)

data_norm_3_2016 <- read.csv("data_norm_3.csv") %>% filter(YEAR==2016)
str(data_norm_3_2016)
 
cbind(validation_2016, data_norm_3_2016)  # ¿À·ù!! number of rows of result is not a multiple of vector length (arg 1)
inner_join(data_norm_3_2016,validation_2016, by=c("no", "T_ID")) %>% mutate(v_p2=p2.x==p2.y) %>% select(v_p2) %>% table()
inner_join(data_norm_3_2016,validation_2016, by=c("no", "T_ID")) %>% select(no, T_ID, p1.x, p1.y) %>% mutate(v_p1=p1.x==p1.y)
table(data_norm_3_2016$p1 == validation_2016$p1)
table(data_norm_3_2016$p2 == validation_2016$p2)


##########################################################################
# ½ÂÁ¡ÀÇ ÃßÀÌ¸¦ ½Ã°¢È­
##########################################################################
library(ggplot2)
# install.packages("plotly")
library(plotly)
data_g <- data %>% mutate(GDAY_DS=as.Date(as.character(GDAY_DS), "%Y%m%d"))
 

year = 2019  # ¿¬µµ¸¦ 2016~ 2020 ÇÏ³ª¾¿ ¹Ù²ã°¡¸ç ÀúÀåÇÑ´Ù  

{
  data_g_year <- data_g %>% filter(YEAR==year)
  g <- ggplot(data=data_g_year, mapping = aes(x=GDAY_DS, y=WC, group= T_ID, color=T_ID))+geom_line()+geom_point()
  ggplotly(g)
}

# TODO ... ¹İº¹¹® ÄÚµùÇÏ±â±â






