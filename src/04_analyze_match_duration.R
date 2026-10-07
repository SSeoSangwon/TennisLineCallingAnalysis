library(stargazer)
library(plm)
library(lmtest)

df <- read.csv("3_final_data.csv")

#------------------------------------------------------------------------------

# 데이터 열 이름 확인
names(df)

# 필요한 열만 선택
data1 <- select(df, c("player", "day.x", "month.x", "year.x", "score_a", "score_b", "hour", "min", "odds_a", "adds_b", "tour.x", ".x", "player_b.x",
                      "date.x", "point_per_a", "point_per_b", "point_a", "tot_a", "point_b", "tot_b"))


# 문자형 데이터를 숫자형 데이터로 변환
data1$hour <- as.numeric(data1$hour)
data1$min <- as.numeric(data1$min)

# 시간(hour)을 분(min) 단위로 변환
data1$hour2 <- data1$hour * 60
data1$min2 <- data1$hour2 + data1$min

# Difference-in-Differences(DID) 분석을 위한 변수 생성
# Grand Slam 대회 여부를 나타내는 더미 변수(tourdum) 생성
data1$tourdum <- ifelse(data1$tour == "ATP Australian Open" | data1$tour == "ATP US Open" | data1$tour == "WTA Australian Open" | data1$tour == "WTA US Open", 1, 0)

# 연도를 기준으로 처리 전후 여부를 나타내는 더미 변수(postdum) 생성
data1$postdum <- ifelse(data1$year == 2019 | data1$year == 2020 | data1$year == 2021, 1, 0)

# 세트당 경기 시간(time_set) 계산
data1$time_set <- (data1$min2 / (data1$score_a + data1$score_b))

# 상호작용 변수(did) 생성
data1$did <- data1$tourdum * data1$postdum

# 배당률을 이용해 승리 확률 계산
data1$win_a <- 1/data1$odds_a
data1$win_b <- 1/data1$adds_b

# 승리 확률 차이 계산
data1$windiff <- abs(data1$win_a - data1$win_b)

# 연도와 월을 결합하여 연도-월 변수(yearmonth) 생성
data1$yearmonth <- data1$year + data1$month

# 변환된 데이터 열 이름 확인
names(data1)

# 무한대 값 제거
data_final <- data1[is.finite(data1$time_set), ]

#------------------------------------------------------------------------------

# Difference-in-Differences(DID) 분석
# time_set을 종속 변수로 하고, 독립 변수로는 did, tot_a, windiff, factor(player), factor(point_a), factor(yearmonth)를 사용
re <- plm(time_set ~ did + tot_a + windiff + factor(player) + factor(point_a) + 
            factor(yearmonth), index = "tour.x", model = "within", data = data_final)

# 클러스터 표준 오차를 사용해 회귀 결과 보정
re <- coeftest(re, vcov = vcovHC(re))
re

# DID 분석 결과를 HTML 파일로 저장
stargazer(re, summary = TRUE, omit = "factor", 
          title = "Tinnes", type = "html", out = "result.html")

# 데이터 요약 결과를 HTML 파일로 저장
stargazer(data_final, summary = TRUE, omit = "factor", 
          title = "Tinnes", type = "html", out = "summary.html")
