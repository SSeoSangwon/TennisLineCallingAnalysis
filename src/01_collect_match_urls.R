library(XML)
library(RSelenium)
library(rvest)
library(httr) 
library(stringr)
library(magrittr)
library(tidyverse)
library(stringi)
library(qdapRegex)
library(dplyr)
library(jsonlite)

#------------------------------------------------------------------------------

# 크롬 드라이버 설정 및 시작
ch=wdman::chrome(port=4594L)
binman::list_versions("chromedriver")
remDr=remoteDriver(port=4594L, browserName='chrome', version="131.0.6778.85")

# R에서 제어하는 크롬 창 열기
remDr$open()

#------------------------------------------------------------------------------

# 페이지 접속 시 URL 구조에서 특정 부분을 변경해 접근 가능합니다.
# ATP: 남자 테니스, WTA: 여자 테니스
# 예시: US Open
#https://www.tennis24.com/atp-singles/us-open-2020/#/hIybRiWS/draw
#https://www.tennis24.com/wta-singles/us-open-2020/#/hIybRiWS/draw

# 예시: Australian Open
#https://www.tennis24.com/atp-singles/australian-open-2020/results/
#https://www.tennis24.com/wta-singles/australian-open-2020/results/

# 예시: Wimbledon Open
#https://www.tennis24.com/atp-singles/wimbledon-2019/#/M1mCsRoA/draw
##https://www.tennis24.com/wta-singles/wimbledon-2019/#/M1mCsRoA/draw

# 예시: French Open
#https://www.tennis24.com/atp-singles/french-open-2020/#/IuyHzXMn/draw
##https://www.tennis24.com/wta-singles/french-open-2020/#/IuyHzXMn/draw

# 홈페이지 접속
#remDr$navigate("https://www.tennis24.com/atp-singles/australian-open-2020/results/")

# '더 보기' 클릭
#remDr$findElement(using = "xpath", value = '//*[@id="live-table"]/div[1]/div/div/a')$clickElement()

# 페이지가 로딩될 때까지 대기
#Sys.time(4)

# 페이지 소스 가지고 오기
#html_Content <- remDr$getPageSource()[[1]]

# 가져온 HTML 소스에서 불필요한 문자 제거
# ex) [\n\t] 를 ''로 변경. 즉 [\n\t]를 제거하기
#html_Content_tmp <- str_replace_all(html_Content, '[\n\t]', '')
#html_Content_tmp <- str_replace_all(html_Content_tmp, ' ', '')
#html_Content_tmp <- str_replace_all(html_Content_tmp, "'", "")
#html_Content_tmp <- str_replace_all(html_Content_tmp, ":", "")
#html_Content_tmp <- str_replace_all(html_Content_tmp, "\\[", "")
#html_Content_tmp <- str_replace_all(html_Content_tmp, "]", "")

# HTML 소스에서 URL 패턴 추출
# 패턴: "www.tennis24.com/match/"와 "/#/match-summary" 사이의 값
#url <- qdapRegex::ex_between(html_Content_tmp, "www.tennis24.com/match/","/#/match-summary")[[1]] %>% {if(length(.) == 0) NA else .}

# 데이터 프레임 생성
#a <- data.frame(url)

# 페이지 소스에서 필요한 데이터를 가져올 수 있음을 확인한 상태입니다.
# 아래는 데이터를 수집하기 위한 반복문입니다.

#------------------------------------------------------------------------------

# US Open 남녀 경기 기록 사이트 URL 패턴 수집 시작
# Grand Slam(4대 대회) 모두 한 번에 수집 가능하지만, 단계별로 나눠 처리

# URL 구성 요소 정의
# atp(남자), wta(여자)로 나눠 데이터를 수집하며
# 대회 종류와 년도를 조합해 반복문으로 URL 생성
#https://www.tennis24.com/atp-singles/us-open-2020/#/hIybRiWS/draw
url1 <- "https://www.tennis24.com/"
url2 <- "-singles/"
url3 <- "-open-"
url4 <- "/#/"

#https://www.tennis24.com/atp-singles/wimbledon-2021/results/
#https://www.tennis24.com/atp-singles/wimbledon-open-2021/results/
#https://www.tennis24.com/atp-singles/us-open-2020/results/
# Wimbledon은 URL 구조가 달라 별도 처리 필요

#------------------------------------------------------------------------------

# 남자 경기 URL 수집 (US, Australian, French Open)

openyear <- expand.grid(open = c("us", "australian", "french"),year = 2020:2023)
url_atp <- map2_dfr(openyear$open, openyear$year, function(.x, .y){
  url1 <- "https://www.tennis24.com/atp-singles/"
  url2 <- "-open-"
  url3 <- "/results/"
  url <- paste0(url1, .x, url2, .y,  url3)
  remDr$navigate(url) # URL 이동
  print(url)
  Sys.sleep(10)
  
  # 페이지 소스 가져오기 및 불필요한 문자 제거
  html_Content <- remDr$getPageSource()[[1]]
  Sys.sleep(5)
  html_Content_tmp <- str_replace_all(html_Content, '[\n\t]', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, ' ', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, "'", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, ":", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "\\[", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "]", "")
  Sys.sleep(5)
  
  # 매치 관련 URL 추출
  df <- qdapRegex::ex_between(html_Content_tmp, "www.tennis24.com/match/","/#/match-summary")[[1]] %>% {if(length(.) == 0) NA else .}
  print(df)
  tour <- remDr$findElement(using = "xpath", '//*[@id="mc"]/div[4]/div[1]/div[2]/div[1]/div[1]')$getElementText()
  year <- data.frame(.y)
  Sys.sleep(3)
  df2 <- cbind(tour, year, df)
})

#------------------------------------------------------------------------------

# 여자 경기 URL 수집 (US, Australian, French Open)

openyear <- expand.grid(open = c("us", "australian", "french"),year = 2020:2023)

url_wta <- map2_dfr(openyear$open, openyear$year, function(.x, .y){
  url1 <- "https://www.tennis24.com/wta-singles/"
  url2 <- "-open-"
  url3 <- "/results/"
  url <- paste0(url1, .x, url2, .y,  url3)
  remDr$navigate(url) # URL 이동
  print(url)
  Sys.sleep(10)
  
  # 페이지 소스 가져오기 및 불필요한 문자 제거
  html_Content <- remDr$getPageSource()[[1]]
  Sys.sleep(10)
  html_Content_tmp <- str_replace_all(html_Content, '[\n\t]', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, ' ', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, "'", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, ":", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "\\[", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "]", "")
  Sys.sleep(5)
  
  # 매치 관련 URL 추출
  df <- qdapRegex::ex_between(html_Content_tmp, "www.tennis24.com/match/","/#/match-summary")[[1]] %>% {if(length(.) == 0) NA else .}
  tour <- remDr$findElement(using = "xpath", '//*[@id="mc"]/div[4]/div[1]/div[2]/div[1]/div[1]')$getElementText()
  year <- data.frame(.y)
  Sys.sleep(3)
  df2 <- cbind(tour, year, df)
})

#------------------------------------------------------------------------------

# 남자와 여자 데이터를 결합

names(url_atp) <- c("tour", "year", "url", "tour2", "tour3")
names(url_wta) <- c("tour", "year", "url", "tour2", "tour3")
url_US_AUS_FR <- rbind(url_atp, url_wta)

url_US_AUS_FR$tour <- ifelse(is.na(url_US_AUS_FR$tour),url_US_AUS_FR$tour2, url_US_AUS_FR$tour )
url_US_AUS_FR$tour <- ifelse(is.na(url_US_AUS_FR$tour),url_US_AUS_FR$tour3, url_US_AUS_FR$tour )

#------------------------------------------------------------------------------

# Wimbledon 경기 URL 수집
# 2020 시즌이 없어 2019 시즌으로 대체

openyear <- expand.grid(sex = c("atp", "wta"),year = c(2019, 2021, 2022,2023))

url_WIM <- map2_dfr(openyear$sex, openyear$year, function(.x, .y){
  url1 <- "https://www.tennis24.com/"
  url2 <- "-singles/wimbledon-"
  url3 <- "/results/"
  url <- paste0(url1, .x, url2, .y,  url3)
  remDr$navigate(url) # URL 이동
  print(url)
  Sys.sleep(5)
  
  # 페이지 소스 가져오기 및 불필요한 문자 제거
  html_Content <- remDr$getPageSource()[[1]]
  Sys.sleep(7)
  html_Content_tmp <- str_replace_all(html_Content, '[\n\t]', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, ' ', '')
  html_Content_tmp <- str_replace_all(html_Content_tmp, "'", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, ":", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "\\[", "")
  html_Content_tmp <- str_replace_all(html_Content_tmp, "]", "")
  Sys.sleep(5)
  
  # 매치 관련 URL 추출
  df <- qdapRegex::ex_between(html_Content_tmp, "www.tennis24.com/match/","/#/match-summary")[[1]] %>% {if(length(.) == 0) NA else .}
  tour <- remDr$findElement(using = "xpath", '//*[@id="mc"]/div[4]/div[1]/div[2]/div[1]/div[1]')$getElementText()
  year <- data.frame(.y)
  Sys.sleep(3)
  df2 <- cbind(tour,year, df)
})

#------------------------------------------------------------------------------

# Wimbledon 데이터를 결합하고 최종 URL 데이터 생성
names(url_WIM) <- c("tour", "year", "url", "tour2")

url_WIM$tour <- ifelse(is.na(url_WIM$tour),url_WIM$tour2, url_WIM$tour )

url_US_AUS_FR <- select(url_US_AUS_FR, c("tour", "year", "url"))
url_WIM <- select(url_WIM, c("tour", "year", "url"))

url_total <- rbind(url_US_AUS_FR, url_WIM)

# 최종 URL 데이터를 CSV로 저장
write.csv(url_total, "1_URLdata.csv", row.names = F)
