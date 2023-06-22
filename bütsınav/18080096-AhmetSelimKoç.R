# -------------------------------------------------------------------------- ###
# Soru 1a ----https://github.com/ahmetselim545/newproject_23_butunleme.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ---- 

# dplyr paketini yükle
library(dplyr)

# Veri setini R'a aktar
titanic <- read.csv("https://bit.ly/3vTgDjZ") %>% as_tibble()

titanic %>%
  group_by(sex) %>%
  summarize(mean_fare = mean(fare))
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ---- 

# tidyverse paketini yükle
library(tidyverse)

# Kayıp olmayan gözlemler için cinsiyete göre yaşlara ait kutu grafiği çiz
titanic %>%
  na.omit() %>%
  ggplot(aes(x = sex, y = age)) +
  geom_boxplot()
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ----
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a ---- 

x <- 10:20
x[seq(1, 5, by = 3)]

# X vektörü 10:20 olarak tanımlanmıştır, yani 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 
# elemanlarını içerir. Bu durumda, x[seq(1, 5, by = 3)] ifadesi 10 ve 13 elemanlarını 
# içeren bir vektörü döndürür.

# Yani, x[seq(1, 5, by = 3)] ifadesinin sonucu c(10, 13) olacaktır.
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b ---- 

library(dplyr)
dat3 <- inner_join(dat1, dat2)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----
# dat tibble'ını kullanarak regresyon analizi grafiği çizimi
ggplot(data = dat, aes(x = x_sutunu, y = y_sutunu)) +
  geom_point() +
  geom_smooth(method = "lm")

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ----

[1] 2.0 4.0

#Bu durumda, myresult vektörü [2.0, 4.0] şeklinde olur.
# İlk eleman, 1:3 vektörünün ortalama değeri olan 2.0'dır 
# ve ikinci eleman, 3:5 vektörünün ortalama değeri olan 4.0'dır.

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ----
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----

simulate_dice_roll <- function() {
  dice1 <- sample(1:6, 1, replace = TRUE)  # İlk zar atışı
  dice2 <- sample(1:6, 1, replace = TRUE)  # İkinci zar atışı
  
  cat("Zar 1: ", dice1, "\n")
  cat("Zar 2: ", dice2, "\n")
  
  return(list(dice1 = dice1, dice2 = dice2))
}
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----

# Kurtulan ve kurtulamayan yolcuları ayırın
survived <- titanic %>%
  filter(survived == 1) %>%
  select(age)

not_survived <- titanic %>%
  filter(survived == 0) %>%
  select(age)

# T-testini gerçekleştirin
t_test <- t.test(survived$age, not_survived$age)

# Test istatistiği ve p-değerini görüntüleyin
print(t_test)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----
library(tidyr)

dat2 <- tibble(
  country = c("_Ingiltere", "_Ingiltere", "_Ingiltere", "Almanya", "Almanya", "Almanya"),
  year = c("2018", "2019", "2020", "2018", "2019", "2020"),
  gdp = c(8000, 8100, 8500, 10000, 11000, 10200)
)

dat2 <- spread(dat2, key = year, value = gdp)


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 5a ----
# -------------------------------------------------------------------------- ###