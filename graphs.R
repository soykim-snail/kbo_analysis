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

par(mfrow=c(2,5))
for(T_ID in team_list$T_ID){
  team_spider(T_ID)
}

####################################################
# doBy ÆĞÅ°Áö·Î °Å¹ÌÁÙ Ã­Æ® ±×¸®±â
####################################################
