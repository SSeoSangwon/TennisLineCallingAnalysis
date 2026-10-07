library(dplyr)
library(tidyr)
library(plm)
library(lmtest)

#------------------------------------------------------------------------------

match <- read.csv("2_data2.csv")
re <- read.csv("2_data1.csv")

#------------------------------------------------------------------------------

# player 열을 위쪽 행으로 올려 player_a를 생성하고 player_a와 player_b를 구분
match1 <- match %>%
  group_by(.x) %>%
  mutate(player_b = dplyr::lead(player, n = 1, default = NA))

# 각 경기당 4개의 행이 생성되므로 중복된 행 제거
match2 <- match1[!duplicated(match$.x),]

# .y 열을 tour와 year로 분리
match2 <- match2 %>% 
  separate(.y, c("tour", "year"), "-")

# date 열에서 날짜 부분만 분리
match2$date <- substr(match2$date, 1, 10)

# 날짜 형식을 YYYY-MM-DD로 변환
match3 <- match2 %>% 
  separate(date, c("day", "month", "year"), "[[:punct:]]")
match3$month <- as.numeric(match3$month)
match3$year <- as.numeric(match3$year)
match3$day <- as.numeric(match3$day)
match3$date <- as.Date(paste0(match3$year, "-",
                              sprintf("%02d", match3$month), "-",
                              sprintf("%02d", match3$day)), format="%Y-%m-%d")

# 진행된 시간 min단위로 맞추기
match3 <- match3 %>% 
  separate(time_tot, c("hour", "min"), ":")

#match5$hour <- as.numeric(match5$hour)
#match5$min <- as.numeric(match5$min)
#match5$hour2 <- match5$hour * 60
#match5$min2 <- match5$hour2 + match5$min

# odds 데이터가 누락되지 않은 행만 선택
match4 <-subset(match3, (!is.na(match3$odds_a)))
match5 <-subset(match4, (!is.na(match4$adds_b)))

# playerdate 열 생성 (date와 .x를 결합)
match5$playerdate <- paste0(match5$date, match5$.x)

#------------------------------------------------------------------------------

# 열 이름 변경
names(re) <- c("player_a", "player_b", "date", "point_a", "point_b", "url", ".y")

# .y 열을 tour와 year로 분리
re1 <- re %>% 
  separate(.y, c("tour", "year"), "-")

# date 열에서 날짜 부분만 분리
re1$date <- substr(re1$date, 1, 11)

# 날짜 형식을 YYYY-MM-DD로 변환
re2 <- re1 %>% 
  separate(date, c("day", "month", "year"), "[[:punct:]]")
re2$month <- as.numeric(re2$month)
re2$year <- as.numeric(re2$year)
re2$day <- as.numeric(re2$day)
re2$date <- as.Date(paste0(re2$year, "-",
                              sprintf("%02d", re2$month), "-",
                              sprintf("%02d", re2$day)), format="%Y-%m-%d")


# point_a와 point_b를 비율과 값으로 분리하고 필요 없는 문자를 제거
re2 <- re2 %>% 
  separate(point_a, c("point_per_a", "point_a"), " ")
re2 <- re2 %>% 
  separate(point_a, c("point_a", "tot_a"), "/")
re2$point_a <- str_replace_all(re2$point_a, '[[:punct:]]', '')
re2$tot_a <- str_replace_all(re2$tot_a, '[[:punct:]]', '')

re2 <- re2 %>% 
  separate(point_b, c("point_per_b", "point_b"), " ")
re2 <- re2 %>% 
  separate(point_b, c("point_b", "tot_b"), "/")
re2$point_b <- str_replace_all(re2$point_b, '[[:punct:]]', '')
re2$tot_b <- str_replace_all(re2$tot_b, '[[:punct:]]', '')

# player 이름에서 특수문자와 공백 제거
re2$player_a <- str_replace_all(re2$player_a, '[[:punct:]]', '')
re2$player_b <- str_replace_all(re2$player_b, '[[:punct:]]', '')
re2$player_a <- str_replace_all(re2$player_a, ' ', '')
re2$player_b <- str_replace_all(re2$player_b, ' ', '')

# playerdate 열 생성 (date와 url 결합)
re2$playerdate <- paste0(re2$date, re2$url)

#------------------------------------------------------------------------------

# match5와 re2 데이터를 playerdate 열을 기준으로 병합
data <- merge(match5, re2, by = "playerdate", all.x = T)

# point_a가 NA가 아닌 데이터만 선택
data2 <- subset(data, !is.na(data$point_a))

# 최종 데이터를 CSV로 저장
write.csv(data2, "3_final_data.csv", row.names = F)

