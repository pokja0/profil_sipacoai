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

cars |>
  dplyr::mutate(
    dist = dist / 120
  ) |>
  e_charts(speed) |>
  e_scatter(dist, symbol_size = 5) |>
  e_tooltip(
    trigger = "axis",
    formatter = htmlwidgets::JS("
      function(params) {
        return params[0].axisValue + '<br/>' +
               params[0].seriesName + ': ' +
               params[0].value.toString().replace('.', ',');
      }
    ")
  )


filter_kabupaten = c("PASANGKAYU", "PASANGKAYU", "MAMUJU", "MAMUJU")
filter_kecamatan = c("PASANGKAYU", "PASANGKAYU", "PAPALANG", "PAPALANG")
filter_desa = c("AKO", "GUNUNG SARI", "BODA-BODA", "SISANGO")
filter_bulan = c('SEPTEMBER')


bkb_filtered <- fsubset(data_bkb_keluarga, 
                        KABUPATEN %in% filter_kabupaten &
                          KECAMATAN %in% filter_kecamatan &
                          KELURAHAN %in% filter_desa &
                          BULAN %in% filter_bulan)

bkb_hadir <- fgroup_by(bkb_filtered, PROVINSI) %>%
  fsummarise(Keluarga_Hadir_BKB = fmean(`JUMLAH KELUARGA ANGGOTA BKB HADIR PERTEMUAN` / `JUMLAH KELUARGA ANGGOTA BKB`, na.rm = TRUE) * 100)


# --- Agregasi BKR ---
# Note: Assuming data_bkr exists - you'll need to load it
bkr_filtered <- fsubset(data_bkr,
                        KABUPATEN %in% filter_kabupaten &
                          KECAMATAN %in% filter_kecamatan &
                          KELURAHAN %in% filter_desa &
                          BULAN %in% filter_bulan)

bkr_hadir <- fgroup_by(bkr_filtered, PROVINSI) %>%
  fsummarise(Keluarga_Hadir_BKR = fmean(`JUMLAH KELUARGA ANGGOTA BKR HADIR PERTEMUAN` / `JUMLAH KELUARGA ANGGOTA BKR`, na.rm = TRUE) * 100)

# For demonstration, creating dummy BKR data
# bkr_hadir <- data.frame(
#   PROVINSI = unique(bkb_hadir$PROVINSI),
#   Keluarga_Hadir_BKR = 0
# )

# --- Agregasi BKL ---
# Note: Assuming data_bkl exists - you'll need to load it
bkl_filtered <- fsubset(data_bkl,
                        KABUPATEN %in% filter_kabupaten &
                          KECAMATAN %in% filter_kecamatan &
                          KELURAHAN %in% filter_desa &
                          BULAN %in% filter_bulan)

bkl_hadir <- fgroup_by(bkl_filtered, PROVINSI) %>%
  fsummarise(Keluarga_Hadir_BKL = fmean(`JUMLAH ANGGOTA KELUARGA HADIR` / `JUMLAH ANGGOTA BKL`, na.rm = TRUE) * 100)

# For demonstration, creating dummy BKL data
# bkl_hadir <- data.frame(
#   PROVINSI = unique(bkb_hadir$PROVINSI),
#   Keluarga_Hadir_BKL = 0
# )

# --- Agregasi PUS ---
# Note: Assuming data_pus exists - you'll need to load it
pus_filtered <- fsubset(data_pus,
                        KABUPATEN %in% filter_kabupaten &
                          KECAMATAN %in% filter_kecamatan &
                          KELURAHAN %in% filter_desa &
                          BULAN %in% filter_bulan)

pus_total <- fgroup_by(pus_filtered, PROVINSI) %>%
  fsummarise(JUMLAH_PUS = fsum(PUS, na.rm = TRUE))

pa_filtered <- fsubset(data_mix,
                       KABUPATEN %in% filter_kabupaten &
                         KECAMATAN %in% filter_kecamatan &
                         KELURAHAN %in% filter_desa &
                         BULAN %in% filter_bulan)

pa_total <- fgroup_by(pa_filtered, PROVINSI) %>%
  fsummarise(JUMLAH_PA = fsum(PA, na.rm = TRUE))

pa_persen <- merge(pa_total, pus_total, by = "PROVINSI", all = TRUE) |>
  fgroup_by(PROVINSI) |>
  fsummarise(PERSEN_PA = JUMLAH_PA / JUMLAH_PUS * 100)

