## 형태소분석할 KoNLP 패키지 설치

install.packages("multilinguer")
install.packages("rJava")
library(multilinguer)
install_jdk()
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))


## 사용할 라이브러리 
library(KoNLP)
library(tidyverse)
useNIADic()
#install.packages("reshape2")
library(reshape2)

## 데이터 불러오기
m<-read_lines('test1.txt')

## 문장을 단어만 뽑아서 tibble 형태로 정리
m_df <- read_lines('test1.txt') %>%
  SimplePos09 %>%
  melt %>%
  as_tibble %>%
  select(3, 1)

## 필요한 명사만 추출
m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2])

## 뒷부분 확인
m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% tail()

## NA 결측값이 들어 가 있는 부분 제거
m_df %>% 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>% tail


## 해당 문장에 사용된 낱말 count
m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  count(noun, sort=TRUE)

## 각 행에 글자 숫자 계산
m_df %>% 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>% 
  mutate(length=str_length(noun))

## length가 2이상인 것 필터링작업
m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  count(noun, sort=TRUE)


## 워드클라우드 해당 단어로 잘 꾸며지는지 테스트
install.packages('wordcloud2')
library(wordcloud2)

m_df %>% 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>% 
  filter(str_length(noun)>=2) %>% 
  count(noun, sort=TRUE) %>%
  wordcloud2()

## 쓸모없는 단어 제거하여 더 가독성 좋게 워드클라우드 작성
m_df %>% 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>% 
  filter(str_length(noun)>=2) %>% 
  count(noun, sort=TRUE) %>%
  filter(n>=2) %>%
  wordcloud2(fontFamily='Noto Sans CJK KR Bold', minRotation=0, maxRotation=0)


### 네트워크 분석 ###

# 가. 낱말 빈도 표시한거 상위15개만 보이기
m_count <- m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  count(noun, sort=TRUE) %>%
  head(15)

m_count

# 나. 품사 꼬리표 필요없는 부분 제거
m_df2 <- m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  select(3, 1)

# 다. 어떤 값이 어떤변수에 들어있는지
m_df3 <- m_df2 %>%
  filter(noun %in% m_count$noun)

m_df3


### 네트워크분석 그래프 유형2

## 네트워크 분석을 위한 패키지 다운
install.packages('igraph')
library(igraph)

## 해당 단어 빈도수 깔끔하게 정리
mg <- graph_from_data_frame(m_df3)
mg

## 네트워크분석 그래프 유형1 그리기
V(mg)$type <- bipartite_mapping(mg)$type
mm <- as_incidence_matrix(mg) %*% t(as_incidence_matrix(mg))
diag(mm) <- 0
mg <- graph_from_adjacency_matrix(mm)
plot(mg)


### 네트워크 분석 그래프 유형2

## 패키지 다운
install.packages('tidygraph')
install.packages('ggraph')
library('tidygraph')
library('ggraph')

## 네트워크 분석 그래프 유형2 그리기
mg %>% as_tbl_graph() %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

### 네트워크 분석 그래프 유형3 그리기 (바이그램)
m_df2 %>%
  select(noun) %>%
  mutate(lead=lead(noun))

## 낱말 두개가 따로 있기에 합침
m_df2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun)) %>%
  unite(bigram, c(noun, lead), sep=' ') 

m_df2

## 글자 카운트하기
m_df2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun)) %>%
  unite(bigram, c(noun, lead), sep=' ') %>% 
  count(bigram, sort=TRUE)

## 낱말 분리
bigram_df <- m_df2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun)) %>%
  unite(bigram, c(noun, lead), sep=" ") %>%
  count(bigram, sort=TRUE) %>%
  head(19) %>%
  separate(bigram, c('word1', 'word2'), sep=' ')

bigram_df

## 네트워크분석 유형 3번째 그리기
bigram_df %>%
  as_tbl_graph %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

png("그림2.png",1000,800)
dev.off()
