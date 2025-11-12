library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# Data asli dari gambar
data_raw <- data.frame(
  Indikator = c(
    "Keluarga Mendapat Pendampingan",
    "Sasaran Hadir Posyandu",
    "Perkembangan Anak",
    "Genting",
    "Tamasya",
    "Edukasi KBPP",
    "Lansia (SIDAYA)",
    "Remaja (PIK-R)",
    "Ayah Teladan (GATI)",
    "Peserta KB Aktif"
  ),
  September = c(59.33, 76.79, 33238, 6487, 37, 54.57, 2820, 15025, 11554, 35127),
  Oktober = c(57.68, 79.96, 35749, 6487, 37, 68.7, 2829, 15900, 21544, 34581)
)

# Tambahkan kolom jenis data
data_raw <- data_raw %>%
  mutate(
    Jenis = case_when(
      Indikator %in% c("Keluarga Mendapat Pendampingan", "Sasaran Hadir Posyandu", 
                       "Edukasi KBPP") ~ "Persentase",
      TRUE ~ "Absolut"
    )
  )

# Pisahkan data
data_persen <- data_raw %>% filter(Jenis == "Persentase")
data_absolut <- data_raw %>% filter(Jenis == "Absolut")

# Tambahkan perubahan untuk label
data_persen <- data_persen %>%
  mutate(
    Perubahan = Oktober - September,
    Label_Perubahan = ifelse(Perubahan > 0, paste0("+", round(Perubahan, 2)), 
                             ifelse(Perubahan < 0, round(Perubahan, 2), "0"))
  )

data_absolut <- data_absolut %>%
  mutate(
    Perubahan = Oktober - September,
    Label_Perubahan = ifelse(Perubahan > 0, paste0("+", format(Perubahan, big.mark = ",", scientific = FALSE)), 
                             ifelse(Perubahan < 0, format(Perubahan, big.mark = ",", scientific = FALSE), "0"))
  )