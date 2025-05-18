# Gerekli paketler
install.packages("tidyr")
install.packages("readr")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(scales)



# 1. Bölgelere Göre İstihdam Oranı

#2023 yılında 15 ve daha yukarı yaştaki kadın nüfusun istihdam oranı %31,3, erkeklerde ise %65,7 olmuştur.
#İBBS 2.Düzeye göre en yüksek kadın istihdam oranı, %38,9 ile TR61 (Antalya, Isparta, Burdur) bölgesinde, en düşük kadın istihdam oranı ise %19,8 ile TRC3 (Mardin, Batman, Şırnak, Siirt) bölgesinde gerçekleşmiştir.

# Veriyi oku
veri <- read_excel("bolge_duzeyi.xlsx")

# Kadın istihdam oranına göre sıralama için sıralı bölge listesi oluştur
sirali_bolgeler <- veri %>%
  filter(cinsiyet == "Kadın") %>%
  arrange(desc(istihdam_orani)) %>%
  pull(bolge)

# Faktör olarak ayarla (sıralı grafik için)
veri$bolge <- factor(veri$bolge, levels = sirali_bolgeler)

# Türkiye ortalamaları
ortalama_kadin <- 31.3
ortalama_erkek <- 65.7

# Grafik oluştur
ggplot(veri, aes(x = istihdam_orani, y = bolge, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = ortalama_kadin, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = ortalama_erkek, color = "steelblue", linetype = "dashed", size = 1) +
  labs(title = "Bölgelere Göre İstihdam Oranı",
       x = "İstihdam Oranı (%)", y = "Bölge", fill = "Cinsiyet") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))



#2.Eğitim Durumuna Göre Yıllık Kazanç Durumu
#2023 yılı verilerine göre kazanç düzeylerinin hem erkeklerde hem de kadınlarda eğitim durumu ile birlikte yükseldiği görülmüştür. 
#Eğitim durumuna göre en yüksek yıllık ortalama brüt kazancı yükseköğretim eğitim düzeyine sahip olanlar elde etmiş olup, bu eğitim düzeyinde yıllık ortalama brüt kazanç erkeklerde 431 bin 364 TL, kadınlarda ise 354 bin 149 TL olmuştur.

library(scales)
# Veriyi oku
veri <- read_excel("yillik_kazanc.xlsx")

# Grafik oluşturma
ggplot(veri, aes(x = reorder(egitim_duzeyi, yillik_ort_brüt_kazanc), 
                 y = yillik_ort_brüt_kazanc, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(format(yillik_ort_brüt_kazanc, big.mark = ".", decimal.mark = ","), ",00")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 2.5) + # Sayıları çubuğun ortasına yerleştir
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) + # Y eksenindeki sayıları düzelt
  labs(title = "Eğitim Düzeyine Göre Yıllık Ortalama Brüt Kazanç",
       x = "Eğitim Düzeyi", y = "Yıllık Ortalama Brüt Kazanç (TL)", fill = "Cinsiyet") +
  theme_minimal()



# 3.Meslek Gruplarına Göre Kazanç Durumu
# 2023 yılı verilerine göre, kadınlarda en yüksek yıllık ortalama brüt kazancı 530.663,00 TL ile yöneticiler meslek grubunda çalışanlar elde etmiştir.
#En düşük yıllık ortalama brüt kazanç ise 185.860,00 TL ile nitelikli tarım, ormancılık ve su ürünleri çalışanları grubunda gerçekleşmiştir.

# Excel dosyasını okuyun
veri <- read_excel("meslek_gruplarina_gore_kazanc.xlsx")

# Veriyi meslek ve cinsiyete göre sıralayın
veri <- veri %>%
  group_by(meslek, cinsiyet) %>%
  summarize(yillik_ort_kazanc = mean(yillik_ort_kazanc, na.rm = TRUE, .groups = "drop"))

# Kadınlara göre meslek sıralaması
sirali_meslekler <- veri %>%
  filter(cinsiyet == "Kadın") %>%
  arrange(yillik_ort_kazanc) %>%
  pull(meslek)

# Sıralı faktör olarak ayarla
veri$meslek <- factor(veri$meslek, levels = sirali_meslekler)

# Grafik oluşturma
ggplot(veri, aes(x = meslek, y = yillik_ort_kazanc, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = format(yillik_ort_kazanc, big.mark = ".", decimal.mark = ",", nsmall = 2)),
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 2.5) +  # ← etiketler çubuğun sağında
  coord_flip() +
  scale_fill_manual(values = c("Kadın" = "yellow", "Erkek" = "lightgreen")) +
  scale_y_continuous(
    breaks = c(0, 150000, 250000, 350000, 450000),
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0, 0.15))  # ← boşluk bırak, çubukların sonunda etiketler için alan yarat
  ) +
  labs(title = "Meslek Grubuna Göre Yıllık Ortalama Kazanç",
       x = "Meslek Grubu", y = "Yıllık Ortalama Kazanç (TL)", fill = "Cinsiyet") +
  theme_minimal() +
  theme(axis.text.y = element_text(margin = margin(r = 10)))



#4. Üst ve Orta Düzey Yönetici Pozisyonundaki Görünüm
#Üst ve orta düzey yönetici pozisyonundaki kadın oranı 2012 yılında %14.4 iken 2023 yılında %20.6 olmuştur.
#Bu oran erkeklerde 2012 yılında %85.6 iken 2023 yılında %79.4 olarak gerçekleşmiştir.

# Veriyi yükle ve cinsiyet sütununu doğrudan karakter olarak al
veri <- read_excel("orta_üst_düzey_yonetici.xlsx")

# Cinsiyet sütununu karakter olarak dönüştür
veri$cinsiyet <- as.character(veri$cinsiyet)

# Cinsiyet sütunundaki hatalı değerleri düzeltme
veri$cinsiyet[!veri$cinsiyet %in% c("Kadın", "Erkek")] <- NA

# Grafik oluşturma
ggplot(veri, aes(x = as.factor(yil), y = orta_üst_yönetici_orani, color = cinsiyet, group = cinsiyet)) +
  geom_line(size = 1, position = position_dodge(width = 0.05)) +
  geom_point(size = 2, position = position_dodge(width = 0.05)) +
  geom_text(aes(label = round(orta_üst_yönetici_orani, 1)), 
            vjust = -0.5, size = 3, show.legend = FALSE, position = position_dodge(width = 0.05)) +
  labs(title = "Yıllara Göre Orta ve Üst Düzey Yönetici Olma Oranı",
       x = "Yıl",
       y = "Orta ve Üst Düzey Yönetici Olma Oranı (%)",
       color = "Cinsiyet") +
  scale_color_manual(values = c("Kadın" = "purple", "Erkek" = "orange")) +
  scale_y_continuous(breaks = c(0, 20, 50, 80)) +
  scale_x_discrete(labels = as.character(unique(veri$yil))) +
  theme_minimal()


#4. bar grafik ile alternatif++
# Veriyi yükle ve cinsiyet sütununu doğrudan karakter olarak al
veri <- read_excel("orta_üst_düzey_yonetici.xlsx")

# Cinsiyet sütununu karakter olarak dönüştür
veri$cinsiyet <- as.character(veri$cinsiyet)

# Cinsiyet sütunundaki hatalı değerleri düzeltme
veri$cinsiyet[!veri$cinsiyet %in% c("Kadın", "Erkek")] <- NA
# Grafik oluşturma (Bar Grafiği)
ggplot(veri, aes(x = as.factor(yil), y = orta_üst_yönetici_orani, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(orta_üst_yönetici_orani, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Yıllara Göre Orta ve Üst Düzey Yönetici Olma Oranı",
       x = "Yıl",
       y = "Orta ve Üst Düzey Yönetici Oranı (%)",
       fill = "Cinsiyet") +
  scale_fill_manual(values = c("Kadın" = "purple", "Erkek" = "orange")) +
  theme_minimal()



#5.Çocuğa Bağlı İstihdam Oranı
#2023 yılında hanesinde 3 yaşın altında çocuğu olan 25-49 yaş grubundaki kadınların istihdam oranının %27,1, erkeklerin istihdam oranının ise %90,6 olduğu görülmüştür.

# Veriyi oku
veri <- read_excel("cocuga_bagli_istihdam_orani.xlsx")

# Veriyi düzenle
veri_long <- veri %>%
  pivot_longer(cols = c("3yasalti_cocuk_olan_istihdam_orani", "cocuk_olmayan_istihdam_orani"), 
               names_to = "cocuk_durumu", 
               values_to = "istihdam_orani") %>%
  mutate(cocuk_durumu = recode(cocuk_durumu, 
                               "3yasalti_cocuk_olan_istihdam_orani" = "3 Yaş Altı Çocuk Sahibi",
                               "cocuk_olmayan_istihdam_orani" = "Çocuk Sahibi Değil"))

# Grafik oluşturma
ggplot(veri_long, aes(x = as.factor(yil), y = istihdam_orani, color = cinsiyet, 
                      linetype = cocuk_durumu, group = interaction(cinsiyet, cocuk_durumu))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = round(istihdam_orani, 1)), vjust = -0.9, size = 3, show.legend = FALSE) +
  labs(title = "3 Yaş Altı Çocuk Sahibi Olma Durumuna Göre İstihdam Oranı",
       x = "Yıl",
       y = "İstihdam Oranı (%)",
       color = "Cinsiyet",
       linetype = "Çocuk Durumu") +
  scale_x_discrete(breaks = unique(veri_long$yil)) +
  scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  theme_minimal() +
  scale_color_manual(values = c("royalblue", "hotpink"))



#ANALİZ
# Excel dosyasını oku
veri <- read_excel("Isgucu_verisi.xlsx")

# Değişken adlarını gör
names(veri)

# Eğitim düzeyi sırasını oluştur
veri <- veri %>%
  mutate(egitim_sira = case_when(
    egitim_duzeyi == "Okuryazar değil" ~ 1,
    egitim_duzeyi == "Lise altı" ~ 2,
    egitim_duzeyi == "Lise" ~ 3,
    egitim_duzeyi == "Mesleki veya Teknik Lisesi" ~ 4,
    egitim_duzeyi == "Yükseköğretim" ~ 5,
    TRUE ~ NA_real_
  ))

# Cinsiyeti faktör olarak tanımla
veri$cinsiyet <- factor(veri$cinsiyet)

#  1. İstihdam Oranı Modeli
model_istihdam <- lm(istihdam_orani ~ egitim_sira * cinsiyet, data = veri)
summary(model_istihdam)

#  2. İşsizlik Oranı Modeli
model_issizlik <- lm(issizlik_orani ~ egitim_sira * cinsiyet, data = veri)
summary(model_issizlik)

#  3. İşgücüne Katılım Modeli
model_katilim <- lm(isgucune_katilma_orani ~ egitim_sira * cinsiyet, data = veri)
summary(model_katilim)


install.packages(c("ggplot2", "ggpmisc", "patchwork"))
library(ggplot2)
library(ggpmisc)
library(patchwork)

# Renk paleti ve tema
my_colors <- c("Kadın" = "darkred", "Erkek" = "steelblue")
my_theme <- theme_minimal(base_size = 12)
my_formula <- y ~ x

# 1. İstihdam Oranı
g1 <- ggplot(veri, aes(x = egitim_sira, y = istihdam_orani, color = cinsiyet)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Okuryazar değil", "Lise altı", "Lise", "Mesleki", "Yükseköğretim")
  ) +
  labs(title = "Eğitim Düzeyine Göre İstihdam Oranı",
       x = "Eğitim Düzeyi", y = "İstihdam Oranı (%)", color = "Cinsiyet") +
  my_theme

# 2. İşsizlik Oranı
g2 <- ggplot(veri, aes(x = egitim_sira, y = issizlik_orani, color = cinsiyet)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Okuryazar değil", "Lise altı", "Lise", "Mesleki", "Yükseköğretim")
  ) +
  labs(title = "Eğitim Düzeyine Göre İşsizlik Oranı",
       x = "Eğitim Düzeyi", y = "İşsizlik Oranı (%)", color = "Cinsiyet") +
  my_theme

# 3. Katılım Oranı
g3 <- ggplot(veri, aes(x = egitim_sira, y = isgucune_katilma_orani, color = cinsiyet)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Okuryazar değil", "Lise altı", "Lise", "Mesleki", "Yükseköğretim")
  ) +
  labs(title = "Eğitim Düzeyine Göre İşgücüne Katılım Oranı",
       x = "Eğitim Düzeyi", y = "Katılım Oranı (%)", color = "Cinsiyet") +
  my_theme

# 3 grafiği üst üste tek sayfada göster
g1 / g2 / g3 + plot_layout(ncol = 1)