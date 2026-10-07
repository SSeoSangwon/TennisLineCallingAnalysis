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
library(jsonlite) # fromJSON()

# URL 데이터 로드
url <- read.csv("1_URLdata.csv")

#------------------------------------------------------------------------------

# 크롬 드라이버 설정 및 시작
ch=wdman::chrome(port=4595L)
binman::list_versions("chromedriver")
remDr=remoteDriver(port=4595L, browserName='chrome', version="131.0.6778.85")

# R에서 제어하는 크롬 창 열기
remDr$open()

#------------------------------------------------------------------------------

#remDr$navigate("https://www.tennis24.com/match/WQgZOm8J/#/match-summary/match-summary")

#remDr$findElement(using = "xpath", value = '//*[@id="detail"]/div[7]/div/a[2]/button')$clickElement()

#페이지 소스 가지고 오기
#html_Content <- remDr$getPageSource()[[1]]

# 필요한 데이터 추출
#player_a <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[2]/div[3]/div[2]/a')
#player_a <- player_a$getElementText()
#player_b <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[4]/div[3]/div[1]/a')
#player_b <- player_b$getElementText()
#time <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[1]/div')
#time <- time$getElementText()
#point_a <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[11]/div[7]/div[1]/div[1]/strong')
#oint_a <- point_a$getElementText()
#point_b <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[11]/div[7]/div[1]/div[3]/strong')
#oint_b <- point_b$getElementText()
#distance_a <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[12]/div[5]/div[1]/div[1]/strong')
#distance_a <- distance_a$getElementText()
#distance_b <- remDr$findElement(using = "xpath", '//*[@id="detail"]/div[12]/div[5]/div[1]/div[3]/strong')
#distance_b <- distance_b$getElementText()

#https://www.tennis24.com/match/fcbML4lc/#/match-summary/match-statistics/0

#------------------------------------------------------------------------------

# URL 패턴 생성 및 필요한 데이터 추출
url$url2 <- paste0(url$tour, "-", url$year)
url2 <- url[1:10,]

match <- map2(url$url,url$url2, function(.x, .y){
  url1 <- "https://www.tennis24.com/match/"
  url2 <- "/#/match-summary/match-statistics/0"
  url <- paste0(url1, .x, url2)
  remDr$navigate(url) # URL로 이동
  print(url)
  Sys.sleep(3)
  
  # HTML 소스 가져오기
  html_Content <- remDr$getPageSource()[[1]]
  
  # 필요한 데이터 추출
  player_a <- possibly(~ remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[2]/div[3]/div[2]/a')$getElementText(), NA)()
  player_b <- possibly(~ remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[4]/div[3]/div[1]/a')$getElementText(), NA)()
  time <- possibly(~ remDr$findElement(using = "xpath", '//*[@id="detail"]/div[4]/div[1]/div')$getElementText(), NA)()
  point_a <- possibly(~ remDr$findElement(using = "xpath", '//*[@id="detail"]/div[11]/div[7]/div[1]/div[1]/strong')$getElementText(), NA)()
  point_b <- possibly(~ remDr$findElement(using = "xpath", '//*[@id="detail"]/div[11]/div[7]/div[1]/div[3]/strong')$getElementText(), NA)()
  
  # 데이터프레임으로 저장
  df <- data.frame(player_a, player_b, time, point_a, point_b, .x, .y)
  do.call(rbind, df)
})

# 데이터를 변환하여 저장
match1 <- data.frame (match)
match2 <- as.data.frame(t(match1))  # 행과 열을 전환

names(match2)
match3 <- subset(match2, !is.na(X.49...159.322..))

write.csv(match3, "2_data1.csv", row.names = F)

#------------------------------------------------------------------------------

url <- read.csv("1_URLdata.csv")

#------------------------------------------------------------------------------

#remDr$navigate("https://www.tennis24.com/match/WQgZOm8J/#/match-summary/match-summary")

#페이지 소스 가지고 오기
#html_Content <- remDr$getPageSource()[[1]]

#대체하기 ex) [\n\t] 를 ''로 변경. 즉 [\n\t]를 제거하기
#html_Content1 <- str_replace_all(html_Content, '[\n\t]', '')
#html_Content1 <- str_replace_all(html_Content1, ' ', '')
#html_Content1 <- str_replace_all(html_Content1, "'", "")
##html_Content1 <- str_replace_all(html_Content1, ":", "")
#html_Content1 <- str_replace_all(html_Content1, "\\[", "")
#html_Content1 <- str_replace_all(html_Content1, "]", "")

#player <- qdapRegex::ex_between(html_Content1, "_image\"loading=\"lazy\"alt=\"",".\"src=\"https://static.flashscore.")[[1]] %>% {if(length(.) == 0) NA else .}
#time <- qdapRegex::ex_between(html_Content1, "startTime\"><divclass=\"\">","</div></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a <- qdapRegex::ex_between(html_Content1, "fixedScore\"><span>","</span><spanclass=\"fixedScore")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b <- qdapRegex::ex_between(html_Content1, "fixedScore__divider\">-</span><span>","</span></div><divclass=\"fixedScore__status")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a_1 <- qdapRegex::ex_between(html_Content1, "=\"smh__partsmh__homesmh__part--1\">","<sup></sup></div><divclass=\"smh__partsmh__homesmh__part--1-ex\"></div><divclass=\"smh__partsmh__homesmh__part--2\">")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a_2 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--2\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a_3 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--3\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a_4 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__homesmh__part--3-ex\"></div><divclass=\"smh__partsmh__homesmh__part--4\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_a_5 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__homesmh__part--4-ex\"></div><divclass=\"smh__partsmh__homesmh__part--5\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b_1 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__partsmh__awaysmh__part--1\">","<sup></sup></div><divclass=\"smh__partsmh__awaysmh__part--1-ex\"></div><divclass=\"smh__partsmh__awaysmh__part--2\">")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b_2 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__awaysmh__part--1-ex\"></div><divclass=\"smh__partsmh__awaysmh__part--2\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b_3 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__awaysmh__part--2-ex\"></div><divclass=\"smh__partsmh__awaysmh__part--3\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b_4 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__awaysmh__part--3-ex\"></div><divclass=\"smh__partsmh__awaysmh__part--4\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#score_b_5 <- qdapRegex::ex_between(html_Content1, "<sup></sup></div><divclass=\"smh__partsmh__awaysmh__part--4-ex\"></div><divclass=\"smh__partsmh__awaysmh__part--5\">","<sup></sup></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
#time_tot <- qdapRegex::ex_between(html_Content1, "Matchtime</div><divclass=\"smh__timesmh__time--overall\">","</div><divclass=\"smh__timesm")[[1]] %>% {if(length(.) == 0) NA else .}
#time_1 <- qdapRegex::ex_between(html_Content1, "><divclass=\"smh__timesmh__time--0\">","</div><divclass=\"smh__timesmh__time")[[1]] %>% {if(length(.) == 0) NA else .}
#time_2 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--1\">","</div><divclass=\"smh__timesmh__time--2\"")[[1]] %>% {if(length(.) == 0) NA else .}
#time_3 <- qdapRegex::ex_between(html_Content1, "=\"smh__timesmh__time--2\">","</div><divclass=\"smh__timesmh__time--3\">")[[1]] %>% {if(length(.) == 0) NA else .}
#time_4 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--3\">","</div><divclass=\"smh__timesmh__time--4\">")[[1]] %>% {if(length(.) == 0) NA else .}
#time_5 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--4\">","</div></div></div><divclass=\"loadablecomplete\"><")[[1]] %>% {if(length(.) == 0) NA else .}
#odds_a <- qdapRegex::ex_between(html_Content1, "</span><spanclass=\"oddsValueodds-wrapdown\"><spanclass=\"oddsValueInner\">","</span></span><svgclass=\"externalLink-icooddsLinkIcon\"><title></title><usexlink:href=\"/res/_fs/build/action.30f034d.svg#external-link\">")[[1]] %>% {if(length(.) == 0) NA else .}
#adds_b <- qdapRegex::ex_between(html_Content1, "</span><spanclass=\"oddsValueodds-wrapup\"><spanclass=\"oddsValueInner\">","</span></span><svgclass=\"externalLink-icooddsLinkIcon\"><title></title><usexlink:href=\"/res/_fs/build/action")[[1]] %>% {if(length(.) == 0) NA else .}

#------------------------------------------------------------------------------

# 선수 정보, 세트정보, 스코어, 세트 시간 가지고오기
url$url2 <- paste0(url$tour, "-", url$year)
url2 <- url[1:20,]
# https://www.tennis24.com/match/MmGfkuud/#/match-summary/match-summary

# Selenium을 활용해 페이지 데이터를 스크래핑
match <- map2(url$url,url$url2, function(.x, .y){
  url1 <- "https://www.tennis24.com/match/"
  url2 <- "/#/match-summary/match-summary"
  url <- paste0(url1, .x, url2)
  remDr$navigate(url) # URL로 이동
  print(url)
  Sys.sleep(5)
  
  # HTML 소스 가져오기 및 불필요한 문자 제거
  html_Content <- remDr$getPageSource()[[1]]
  Sys.sleep(7)
  html_Content1 <- str_replace_all(html_Content, '[\n\t]', '')
  html_Content1 <- str_replace_all(html_Content1, ' ', '')
  html_Content1 <- str_replace_all(html_Content1, "'", "")
  html_Content1 <- str_replace_all(html_Content1, "\\[", "")
  html_Content1 <- str_replace_all(html_Content1, "]", "")
  Sys.sleep(5)
  
  # 데이터 추출
  player <- qdapRegex::ex_between(html_Content1, "_image\"loading=\"lazy\"alt=\"",".\"src=\"https://static.flashscore.")[[1]] %>% {if(length(.) == 0) NA else .}
  date <- qdapRegex::ex_between(html_Content1, "startTime\"><divclass=\"\">","</div></div><divclass")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a <- qdapRegex::ex_between(html_Content1, "fixedScore\"><span>","</span><spanclass=\"fixedScore")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b <- qdapRegex::ex_between(html_Content1, "fixedScore__divider\">-</span><span>","</span></div><divclass=\"fixedScore__status")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a_1 <- qdapRegex::ex_between(html_Content1, "=\"smh__partsmh__homesmh__part--1\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a_2 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--2\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a_3 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--3\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a_4 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--4\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_a_5 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__homesmh__part--5\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b_1 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__partsmh__awaysmh__part--1\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b_2 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__awaysmh__part--2\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b_3 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__awaysmh__part--3\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b_4 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__awaysmh__part--4\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  score_b_5 <- qdapRegex::ex_between(html_Content1, "></div><divclass=\"smh__partsmh__awaysmh__part--5\">","<sup>")[[1]] %>% {if(length(.) == 0) NA else .}
  time_tot <- qdapRegex::ex_between(html_Content1, "Matchtime</div><divclass=\"smh__timesmh__time--overall\">","</div><divclass=\"smh__timesm")[[1]] %>% {if(length(.) == 0) NA else .}
  time_1 <- qdapRegex::ex_between(html_Content1, "><divclass=\"smh__timesmh__time--0\">","</div><divclass=\"smh__timesmh__time")[[1]] %>% {if(length(.) == 0) NA else .}
  time_2 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--1\">","</div><divclass=\"smh__timesmh__time--2\"")[[1]] %>% {if(length(.) == 0) NA else .}
  time_3 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--2\">","</div>")[[1]] %>% {if(length(.) == 0) NA else .}
  time_4 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--3\">","</div>")[[1]] %>% {if(length(.) == 0) NA else .}
  time_5 <- qdapRegex::ex_between(html_Content1, "</div><divclass=\"smh__timesmh__time--4\">","</div>")[[1]] %>% {if(length(.) == 0) NA else .}
  odds_a <- qdapRegex::ex_between(html_Content1, "</span><spanclass=\"oddsValueodds-wrapdown\"><spanclass=\"oddsValueInner\">","</span></span><svgclass=\"externalLink-icooddsLinkIcon\"><title></title><usexlink:href=\"/res/_fs/build/action.30f034d.svg#external-link\">")[[1]] %>% {if(length(.) == 0) NA else .}
  adds_b <- qdapRegex::ex_between(html_Content1, "</span><spanclass=\"oddsValueodds-wrapup\"><spanclass=\"oddsValueInner\">","</span></span><svgclass=\"externalLink-icooddsLinkIcon\"><title></title><usexlink:href=\"/res/_fs/build/action")[[1]] %>% {if(length(.) == 0) NA else .}
  
  # 추가 데이터 및 매치 정보 저장
  df <- rbind(player, date,score_a, score_b, score_a_1, score_a_2, score_a_3, score_a_4, score_a_5, score_b_1, score_b_2, score_b_3, 
              score_b_4, score_b_5,time_tot, time_1, time_2, time_3, time_4, time_5,odds_a, adds_b, .y)
  Sys.sleep(3)
  do.call(rbind, list(df))
})

# 데이터를 변환하여 저장
match1 <- data.frame (match)
match2 <- as.data.frame(t(match1)) # 행과 열을 전환

write.csv(match2, "2_data2.csv", row.names = F)
