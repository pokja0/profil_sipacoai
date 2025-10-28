library(dplyr)
library(stringr)
library(tidyverse)

data_sidaya <- readxl::read_excel("I:/Datin/Project/Scraping Profil Desa/hasil/Tabulasi_PKL_Provinsi_Kelurahan_2025-10-27_08-02-27.xlsx", skip = 4)

# Misalkan dataframe Anda bernama 'df'
df_bersih <- data_sidaya %>%
  mutate(
    PROVINSI = "SULAWESI BARAT",
    KABUPATEN = str_remove(`Kota/Kabupaten`, "KAB\\.\\s*"),
    KECAMATAN = toupper(Kecamatan),
    KELURAHAN = toupper(`Kelurahan/Desa`),
    SIDAYA = replace_na(`Total Peserta`, 0)
  ) |>
  select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, SIDAYA)


fst::write_fst(df_bersih, "I:/Datin/Project/profil_sipacoai/data/sidaya.fst")


setwd("I:/Datin/Project/profil_desa_r")
