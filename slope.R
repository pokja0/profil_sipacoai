library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(stringr)

# Data asli dari gambar
data_raw <- data.frame(
  Indikator = c(
    "Pendampingan Keluarga",
    "Kehadiran Posyandu",
    "Perkembangan Anak",
    "Genting",
    "Tamasya",
    "Edukasi KBPP",
    "Lansia (SIDAYA)",
    "Remaja (PIK-R)",
    "Ayah Teladan (GATI)",
    "Peserta KB Aktif"
  ),
  September = c(64.07, 78.48, 541, 163,2, 94.42, 56, 178, 251, 758),
  Oktober = c(62.34, 83.28, 632, 163, 2, 95.43, 73, 170, 825, 749)
)

# Tambahkan kolom jenis data
data_raw <- data_raw %>%
  mutate(
    Jenis = case_when(
      Indikator %in% c("Pendampingan Keluarga", "Kehadiran Posyandu", 
                       "Edukasi KBPP") ~ "Persentase",
      TRUE ~ "Absolut"
    )
  )

# Pisahkan data

data_absolut <- data_raw %>% filter(Jenis == "Absolut")


data_absolut <- data_absolut %>%
  mutate(
    Perubahan = Oktober - September,
    Label_Perubahan = ifelse(Perubahan > 0, paste0("+", format(Perubahan, big.mark = ".", scientific = FALSE)), 
                             ifelse(Perubahan < 0, format(Perubahan, big.mark = ".", scientific = FALSE), "0")),
    Label_Perubahan = str_squish(Label_Perubahan),
    Keterangan = ifelse(Perubahan > 0, "Naik", 
                        ifelse(Perubahan < 0, "Turun", "Tetap"))
  )

data_absolut <- data_absolut |>
  pivot_longer(
    cols = c(September, Oktober),
    names_to = "Bulan",
    values_to = "Nilai"
  )

data_absolut$Bulan <- factor(data_absolut$Bulan, levels = c("September", "Oktober"))


ggplot(data = data_absolut, aes(x = Bulan, y = Nilai, group = Indikator, fill=Keterangan)) +
  geom_line(aes(color = Keterangan), size = 2) +
  geom_point(aes(color = Keterangan, alpha = 1), size = 4) +
  geom_text_repel(data = data_absolut %>% filter(Bulan == "September"), 
                  aes(label = paste0(Indikator, " - ", format(Nilai, big.mark = ".", scientific = FALSE))) , 
                  hjust = 1.35, 
                  fontface = "bold", 
                  size = 3.5, color = "black") +
  geom_text_repel(data = data_absolut %>% filter(Bulan == "Oktober"), 
                  aes(label = paste0(Indikator, " - ", format(Nilai, big.mark = ".", scientific = FALSE), " (", Label_Perubahan, ")")) , 
                  hjust = -.35, 
                  fontface = "bold", 
                  size = 3.5, color = "black") +
  geom_vline(xintercept = 1, linetype="dashed", size=.1, color = "white") + 
  geom_vline(xintercept=2, linetype="dashed", size=.1, color="white") +
  scale_color_manual(labels = c("Naik", "Tetap", "Turun"), 
                     values = c("Naik"="#ecd76a", "Tetap"="#696969", "Turun"="#5dabdc")) +
  geom_text(label="September", x=1, y=890, hjust=1.2, size=5, color="black") +  # title
  geom_text(label="Oktober", x=2, y=890, hjust=-0.1, size=5, color="black") +
  ylim(0, 900) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), panel.grid.minor.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "white"),
        axis.ticks = element_blank(), #
        #plot.title = element_markdown(size=14, face = "bold", hjust = 0.5, color = "white"),
        #plot.subtitle =  element_markdown(hjust = 0.5, color = "white"),
        ##plot.caption = element_text(color="white"),
        #plot.background = element_rect("black"),
        #panel.background = element_rect(fill = "black", color  =  NA),
        axis.text.x = element_blank()
  ) 

ggsave("I:/Datin/durasi_medsos.png", dpi = 320, width = 11.5, height = 5)

##
data_persen <- data_raw %>% filter(Jenis == "Persentase")

data_persen <- data_persen %>%
  mutate(
    Perubahan = Oktober - September,
    Label_Perubahan = ifelse(Perubahan > 0, paste0("+", gsub("\\.", ",", round(Perubahan,2))), 
                             ifelse(Perubahan < 0, gsub("\\.", ",", round(Perubahan,2)), "0")),
    Label_Perubahan = paste0(str_squish(Label_Perubahan), "%"),
    Keterangan = ifelse(Perubahan > 0, "Naik", 
                        ifelse(Perubahan < 0, "Turun", "Tetap"))
  )

data_persen <- data_persen |>
  pivot_longer(
    cols = c(September, Oktober),
    names_to = "Bulan",
    values_to = "Nilai"
  )

data_persen$Bulan <- factor(data_persen$Bulan, levels = c("September", "Oktober"))

ggplot(data = data_persen, aes(x = Bulan, y = Nilai, group = Indikator, fill=Keterangan)) +
  geom_line(aes(color = Keterangan), size = 2) +
  geom_point(aes(color = Keterangan, alpha = 1), size = 4) +
  geom_text_repel(data = data_persen %>% filter(Bulan == "September"), 
                  aes(label = paste0(Indikator, ": ", gsub("\\.", ",", round(Nilai,2)))) , 
                  hjust = 1.35, 
                  fontface = "bold", 
                  size = 4, color = "black") +
  geom_text_repel(data = data_persen %>% filter(Bulan == "Oktober"), 
                  aes(label = paste0(Indikator, ": ", gsub("\\.", ",", round(Nilai,2)), " (", Label_Perubahan, ")")) , 
                  hjust = -.35, 
                  fontface = "bold", 
                  size = 4, color = "black") +
  geom_vline(xintercept = 1, linetype="dashed", size=.1, color = "white") + 
  geom_vline(xintercept=2, linetype="dashed", size=.1, color="white") +
  scale_color_manual(labels = c("Naik", "Tetap", "Turun"), 
                     values = c("Naik"="#ecd76a", "Tetap"="#696969", "Turun"="#5dabdc")) +
  geom_text(label="September", x=1, y=100, hjust=1.2, size=5, color="black") +  # title
  geom_text(label="Oktober", x=2, y=100, hjust=-0.1, size=5, color="black") +
  ylim(55, 100) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), panel.grid.minor.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "white"),
        axis.ticks = element_blank(), #
        #plot.title = element_markdown(size=14, face = "bold", hjust = 0.5, color = "white"),
        #plot.subtitle =  element_markdown(hjust = 0.5, color = "white"),
        ##plot.caption = element_text(color="white"),
        #plot.background = element_rect("black"),
        #panel.background = element_rect(fill = "black", color  =  NA),
        axis.text.x = element_blank()
  ) 

ggsave("I:/Datin/persen.png", dpi = 320, width = 11.5, height = 5)
