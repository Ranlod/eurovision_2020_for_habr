# статья https://habr.com/ru/post/492326/

library(tuber) # пакет взаимодействует с API YouTube, выгрузит нам статистику по роликам
library(dplyr) # пакет для работы с таблицами
library(ggplot2) # рисует графики
library(rvest) # парсер html страниц
library(tidyr) # пакет для работы с таблицами

options(scipen = 99999)

# авторизация
yt_oauth("id_user", "secret_code")

# получаем список роликов из плэйлиста
list_videos <- get_playlist_items(filter = c(playlist_id = "PLmWYEDTNOGUL69D2wj9m2onBKV2s3uT5Y"))

# Собираем статистику по просмотрам, функция get_stats
stats_videos <- lapply(as.character(list_videos$contentDetails.videoId), get_stats) %>% 
  bind_rows()
stats_videos <- stats_videos %>% 
  mutate_at(vars(-id), as.integer)

# Получаем описание роликов, функция get_video_details
description_videos <- lapply(as.character(list_videos$contentDetails.videoId), get_video_details)
description_videos <- lapply(description_videos, function(x) {
  list(
    id = x[["items"]][[1]][["id"]],
    name_video = x[["items"]][[1]][["snippet"]][["title"]]
  )
}) %>% 
  bind_rows()

description_videos$name_video <- description_videos$name_video %>% 
  gsub("[^[:alnum:][:blank:]?&/\\-]", '', .) %>% 
  gsub("(  .*)|( - Offic.*)", '', .)


df <- description_videos %>% 
  left_join(stats_videos, by = 'id') %>% 
  rowwise() %>% 
  mutate( # считаем долю лайков
    proc_like = round(likeCount / (likeCount + dislikeCount), 2)
  ) %>% 
  ungroup()

# Hurricane - Hasta La Vista - Serbia две композиции в одном плейлисте, суммируем
df <- df %>% 
  group_by(name_video) %>% 
  summarise(
    id = first(id),
    viewCount = sum(viewCount),
    likeCount = sum(likeCount),
    dislikeCount = sum(dislikeCount),
    commentCount = sum(commentCount),
    proc_like = round(likeCount / (likeCount + dislikeCount), 2)
  )

df$color <- ifelse(df$name_video == 'Little Big - Uno - Russia','red','gray')

# Кол-во просмотров
ggplot(df, aes(x = reorder(name_video, viewCount), y = viewCount, fill = color)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "Кол-во просмотров") +
  guides(fill = F) +
  scale_fill_manual(values = c('gray', 'red')) +
  scale_y_continuous(labels = scales::number_format(big.mark = " "))

# Доля лайков к дизлайкам
ggplot(df, aes(x = reorder(name_video, proc_like), y = proc_like, fill = color)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "Доля лайков к дизлайкам") +
  guides(fill = F) +
  scale_fill_manual(values = c('gray', 'red')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Кол-во комментариев
ggplot(df, aes(x = reorder(name_video, commentCount), y = commentCount, fill = color)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "Кол-во комментариев") +
  guides(fill = F) +
  scale_fill_manual(values = c('gray', 'red')) +
  scale_y_continuous(labels = scales::number_format(big.mark = " "))

# Доля комментариев к просмотрам
ggplot(df, aes(x = reorder(name_video, commentCount/viewCount), y = commentCount/viewCount, fill = color)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "Доля комментариев к просмотрам") +
  guides(fill = F) +
  scale_fill_manual(values = c('gray', 'red')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25))


####### ------- Смотрим зависимость просмотров от численности населения ------- ########

# Выгружаем численность населения
hdoc <- read_html('https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population')

tnode <- html_node(hdoc, xpath = '/html/body/div[3]/div[3]/div[4]/div/table')
df_population <- html_table(tnode)

df_population <- df_population %>% filter(`Country (or dependent territory)` != 'World')

df_population$Population <- as.integer(gsub(',','',df_population$Population,fixed = T))

df_population$`Country (or dependent territory)` <- gsub('\\[.*\\]','', df_population$`Country (or dependent territory)`)

df_population <- df_population %>% 
  select(
    `Country (or dependent territory)`,
    Population
    ) %>% 
  rename(Country = `Country (or dependent territory)`)

# добавляем численность населения в копию основной таблицы
df2 <- df %>% 
  separate(name_video, c('compozitor', 'name_track', 'Country'), ' - ', remove = F) %>% 
  mutate(Country = ifelse(Country == 'The Netherlands', 'Netherlands', Country)) %>% 
  left_join(df_population, by = 'Country')


# Зависимость просмотров от численности населения
cor(df2$viewCount,df2$Population)
ggplot(df2, aes(x = Population, y =  viewCount)) +
  geom_point() +
  theme_light() +
  geom_smooth(method = 'lm') +
  labs(x = "Население, чел", y = "Кол-во просмотров") +
  scale_y_continuous(labels = scales::number_format(big.mark = " ")) +
  scale_x_continuous(labels = scales::number_format(big.mark = " "))

# Зависимость просмотров от численности населения, без России
cor(df2[df2$Country != 'Russia',]$viewCount,df2[df2$Country != 'Russia',]$Population)
ggplot(df2 %>% filter(Country != 'Russia') , aes(x = Population, y = viewCount)) +
  geom_point() +
  theme_light() +
  geom_smooth(method = 'lm') +
  labs(x = "Население, чел", y = "Кол-во просмотров") +
  scale_y_continuous(labels = scales::number_format(big.mark = " ")) +
  scale_x_continuous(labels = scales::number_format(big.mark = " "))

# Зависимость просмотров от численности населения, ранги
cor(df2$viewCount,df2$Population, method = "spearman")
ggplot(df2 , aes(x = rank(Population), y = rank(viewCount))) +
  geom_point() +
  theme_light() +
  geom_smooth(method = 'lm') +
  labs(x = "Население, чел (Ранги от 1 до 40)", y = "Кол-во просмотров (Ранги от 1 до 40)") +
  guides(fill = F)

# Доля от общей численности населения
ggplot(df2, aes(x = reorder(name_video, viewCount/Population), y = viewCount/Population, fill = color)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "Доля от общей численности населения") +
  guides(fill = F) +
  scale_fill_manual(values = c('gray', 'red')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25))
