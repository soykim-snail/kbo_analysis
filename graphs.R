library(dplyr)


#######################################
# ·¹ÀÌ´Ù Ã­Æ® ±×¸®±â
#######################################

# µ¥ÀÌÅÍ ºÒ·¯¿À±â
data <- read.csv(file = "data_mart_final_200814.csv")
head(data)
str(data)

# ÆÀº°·Î ÁöÇ¥ Á¤¸®
stat_by_Team <- data %>% group_by(T_ID) %>% summarise(P=mean(P), D=mean(D), B=mean(B), C=mean(C), S=mean(S) )
head(stat_by_Team)
stat_by_Team <- as.data.frame(stat_by_Team)
rownames(stat_by_Team) <- stat_by_Team$T_ID
stat_by_Team <- stat_by_Team[-1]
head(stat_by_Team)





# ½ÃÁğº°·Î ÁöÇ¥ Á¤¸®
stat_by_YEAR <- data %>% group_by(YEAR) %>%
  summarise(P=mean(P), D=mean(D), B=mean(B), C=mean(C), S=mean(S) ) 
stat_by_YEAR <- as.data.frame(stat_by_YEAR)
rownames(stat_by_YEAR) <- stat_by_YEAR$YEAR 
stat_by_YEAR <- stat_by_YEAR[-1]
stat_by_YEAR

# ÆÀº°, ½ÃÁğº°·Î ÁöÇ¥ Á¤¸®
stat_by_T_Y <-  data %>% group_by(T_ID, YEAR) %>% 
  summarise(P=mean(P), D=mean(D), B=mean(B), C=mean(C), S=mean(S) ) 
stat_by_T_Y <- as.data.frame(stat_by_T_Y)
rownames(stat_by_T_Y) <- paste(stat_by_T_Y$T_ID, stat_by_T_Y$YEAR, sep="_")
head(stat_by_T_Y)

###############################
# ÆÀº°, ³âµµº° ¹æ»çÇü Ã­Æ®
op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))

#######################
# ÆÀº°
# stars Ã­Æ®
stars(stat_by_Team, key.loc = c(7,2), scale = FALSE)
title(main = "Star Plot, ÆÀº° Àü±â°£")

# spider Ã­Æ®
stars(stat_by_Team, location = c(0,0), key.loc = c(0,0), scale = FALSE, col.lines = c(1:10))
title(main = "Spider Plot, ÆÀº° Àü±â°£")
legend(x=5, y=5, legend = rownames(stat_by_Team), lty = 2, col=c(1:10), lwd = 2)
#######################
# ¿¬µµº°
# stars Ã­Æ®
stars(stat_by_YEAR, key.loc = c(7,2), scale = FALSE)
title(main = "Star Plot, ½ÃÁğº° ÀüÃ¼ÆÀ")

# spider Ã­Æ®
stars(stat_by_YEAR, location = c(0,0), key.loc = c(0,0), scale = FALSE, col.lines = c(1:5))
title(main = "Spider Plot, ½ÃÁğº° ÀüÃ¼ÆÀ")
legend(x=1, y=1, legend = rownames(stat_by_YEAR), lty = 2, col = c(1:5), lwd = 2)



############################################
# °¢ÆÀÀÇ ¿¬µµº° ¹æ»çÇü Ã­Æ®

# starÃ­Æ® ÇÔ¼ö¸¦ Á¤ÀÇÇÔ
team_stars <- function(T_ID){
  stars(stat_by_T_Y[stat_by_T_Y$T_ID==T_ID, c("P", "D", "B", "C", "S")], key.loc = c(0,0), scale = FALSE)
  title(main = paste("Spider Plot,", T_ID, "ÆÀ °¢ ½ÃÁğ"))
}

team_stars("HH")
team_stars("LT")
team_stars("OB")

# spider Ã­Æ® ÇÔ¼ö¸¦ Á¤ÀÇÇÔ
team_spider <- function(T_ID){
  stars(stat_by_T_Y[stat_by_T_Y$T_ID==T_ID, c("P", "D", "B", "C", "S")], 
        location = c(0,0), key.loc = c(0,0), scale = FALSE,
        col.lines = c(1:length(2016:2020)))
  title(main = paste("Spider Plot,", T_ID, "ÆÀ °¢ ½ÃÁğ"))
}

###############################
# 10°³ ±¸´ÜÀÇ ½ÃÁğº° º¯µ¿ Ã­Æ®

op <- par(no.readonly = TRUE)
par(mfrow=c(2,5), oma=c(4, 1, 1, 1))
for(T_ID in team_list$T_ID){
  team_spider(T_ID)
  # box(which = "outer", lty = 1, col="red")
  # box(which = "inner", lty = 1, col="blue")
  # box(which = "figure", lty = 1, col="pink")
  # box(which = "plot", lty = 1, col="black")
}
par(op)
###############################

###############################################################
#### ±âÃÊ ½ºÅÈ¿¡ ´ëÇÑ 10°³ ±¸´ÜÀÇ ½ÃÁğº° º¯µ¿ Ã­Æ®
## Ã­Æ® ÇÔ¼ö¸¦ Á¤ÀÇ
team_spider_general <- function(T_ID){
  stars(stat_general_by_T_Y[stat_general_by_T_Y$T_ID==T_ID, colnames(stat_general_by_T_Y)[c(-1, -2)]], 
        location = c(0,0), key.loc = c(0,0), scale = TRUE,
        col.lines = c(1:length(2016:2020)))
  title(main = paste("General Spider Plot,", T_ID, "ÆÀ °¢ ½ÃÁğ"))
}

## µ¥ÀÌÅÍ Á¤¸® ..... P1, P2, .... S2¸¦ Á¤±ÔÈ­ ¾øÀÌ
stat_general_by_T_Y <-  data %>% group_by(T_ID, YEAR) %>% 
  summarise(P1=mean(P1), P2=mean(P2), P3=mean(P3), P4=mean(P4),
            D1=mean(D1), D2=mean(D2), D3=mean(D3),
            B1=mean(B1), B2=mean(B2), B3=mean(B3), B4=mean(B4), 
            C1=mean(C1), C2=mean(C2), C3=mean(C3), C4=mean(C4),
            S1=mean(S1), S2=mean(S2)) 
stat_general_by_T_Y <- as.data.frame(stat_general_by_T_Y)
rownames(stat_general_by_T_Y) <- paste(stat_general_by_T_Y$T_ID, stat_general_by_T_Y$YEAR, sep="_")
head(stat_general_by_T_Y)

## Ã­Æ® ±×¸®±â
op <- par(no.readonly = TRUE)
par(mfrow=c(2,5), oma=c(4, 1, 1, 1))
for(T_ID in team_list$T_ID){
  team_spider_general(T_ID)
}
# legend(x=5, y=5, legend = rownames(stat_general_by_T_Y), lty = 2, col=c(1:10), lwd = 2)
#######################

par(op)






