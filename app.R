library(shiny)
library(bslib)
library(collapse)
library(rlang)
library(bsicons)
library(data.table)
library(echarts4r)
library(gt)

daftar_bulan = c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
data_nama_desa = fread("data/profil_poktan.csv", header = T)

warna_kuning = "#d4a017"

warna_biru = "#3498db"

fa_icon <- function(name) {
  icon(name, lib = "font-awesome")
}
# UI
ui <- page_navbar(
  title = "Aplikasi Multi-Page",fillable = F,
  theme = bs_theme(version = 5),
  
  # Page 1 - Dashboard
  nav_panel(
    title = "Dashboard",
    icon = icon("dashboard"),
    fluidRow(
      column(3, 
             selectInput("pilih_kab", "Daftar Kabupaten",
                         choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                     "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
      ),
      column(3, selectInput("pilih_kec", "Daftar Kecamatan", choices = NULL)),
      column(3, selectInput("pilih_desa_kel", "Pilih Desa/Kel", choices = NULL)),
      column(3, selectInput("pilih_bulan", "Pilih Bulan", choices = daftar_bulan[1:9], selected = "SEPTEMBER"))
    ),
    br(
      
    ),
    fluidRow(
      column(
        6,
        input_task_button(
          label_busy = "Sedang Proses",
          id = "cari",
          label = "Cari"
        )
      )
    ),
    br(
      
    ),
    fluidRow(
      textOutput("tes_input_rekap")
    ),
    br(
      
    ),
    navset_card_pill(
      nav_panel(
        title = "Ringkasan", 
        # Baris 1: PKB/PLKB & Tim Pendamping Keluarga (Layout 2 kolom)
        fluidRow(
          column(6,
                 value_box(
                   title = "PKB / PLKB",
                   value = textOutput("nama_pkb"), 
                   showcase = fa_icon("id-card"),
                   showcase_layout = "left center",
                   textOutput("jumlah_wilker"),
                   # Sesuaikan warna jika perlu, default-nya netral
                   theme = "white" # Gunakan tema terang untuk yang ini
                 )
          ),
          column(6,
                 value_box(
                   title = "Tim Pendamping Keluarga",
                   value = textOutput("jumlah_tpk"),
                   showcase = bs_icon("universal-access"),
                   showcase_layout = "left center",
                   theme = "white"
                 )
          )
        ),
        
        # Baris 2: Pasangan Usia Subur, Unmet Need, KB MKJP, Kontrasepsi Favorit (Layout 4 kolom)
        fluidRow(
          column(3,
                 value_box(
                   title = "Pasangan Usia Subur",
                   value = textOutput("jumlah_pus"),
                   showcase = bs_icon("person-hearts"),
                   theme = "primary" # Gunakan warna biru/primary
                 )
          ),
          column(3,
                 value_box(
                   title = "Unmet Need",
                   value = textOutput("persentase_unmet_need"),
                   showcase = bs_icon("person-fill-exclamation"),
                   theme = "primary"
                 )
          ),
          column(3,
                 value_box(
                   title = "KB MKJP",
                   value = textOutput("persentase_mkjp"),
                   showcase = bs_icon("journal-medical"),
                   theme = "primary"
                 )
          ),
          column(3,
                 value_box(
                   title = "Kontrasepsi Favorit",
                   value = textOutput("kontrasepsi_favorit"),
                   showcase = bs_icon("capsule-pill"), # Gunakan ikon yang sama atau serupa
                   theme = "primary"
                 )
          )
        ),
        
        # Baris 3: Tempat Pelayanan KB, Tempat Pelayanan KB Terlatih, Tenaga Kesehatan Pelayanan KB, Tenaga Kesehatan Pelayanan KB Terlatih (Layout 4 kolom)
        fluidRow(
          column(3,
                 value_box(
                   title = "Tempat Pelayanan KB",
                   value = textOutput("tempat_pelayanan_kb"),
                   showcase = bs_icon("house"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          ),
          column(3,
                 value_box(
                   title = "Tempat Pelayanan KB Terlatih",
                   value = textOutput("tempat_pelayanan_kb_terlatih"),
                   showcase = bs_icon("hospital-fill"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          ),
          column(3,
                 value_box(
                   title = "Tenaga Kesehatan KB",
                   value = textOutput("tenaga_kesehatan_kb"),
                   showcase = bs_icon("person-bounding-box"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          ),
          column(3,
                 value_box(
                   title = "Tenaga Kesehatan KB Terlatih",
                   value = textOutput("tenaga_kesehatan_kb_terlatih"),
                   showcase = bs_icon("person-badge-fill"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          )
        ),
        
        # Baris 4: Jumlah KRS, Keluarga Punya BADUTA, PUS Hamil (Layout 3 kolom, mengambil 3/4 lebar)
        fluidRow(
          column(4,
                 value_box(
                   title = "Jumlah KRS",
                   value = textOutput("jumlah_krs"),
                   showcase = bs_icon("person-fill-x"),
                   theme = "primary"
                 )
          ),
          column(4,
                 value_box(
                   title = "Keluarga Punya BADUTA",
                   value = textOutput("jumlah_keluarga_punya_baduta"),
                   showcase = bs_icon("person"),
                   theme = "primary"
                 )
          ),
          column(4,
                 value_box(
                   title = "PUS Hamil",
                   value = textOutput("jumlah_pus_hamil"),
                   showcase = bs_icon("person-standing-dress"),
                   theme = "primary"
                 )
          )
        ),
        
        # Baris 5: Sasaran BKB, Sasaran BKR, Sasaran BKL (Layout 3 kolom, dengan value box putih/terang)
        fluidRow(
          column(4,
                 value_box(
                   title = "Sasaran BKB",
                   value = textOutput("jumlah_sasaran_bkb"),
                   showcase = bs_icon("person-fill-check"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          ),
          column(4,
                 value_box(
                   title = "Sasaran BKR",
                   value = textOutput("jumlah_sasaran_bkr"),
                   showcase = bs_icon("person-standing"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          ),
          column(4,
                 value_box(
                   title = "Sasaran BKL",
                   value = textOutput("jumlah_sasaran_bkl"),
                   showcase = bs_icon("person-wheelchair"),
                   showcase_layout = "left center",
                   theme = "white" 
                 )
          )
        )
      ),
      nav_panel(
        title = "Keluarga Berencana", 
        layout_column_wrap(
          card(
            full_screen = TRUE,
            echarts4rOutput("tren_pus")
          ),
          card(
            full_screen = TRUE,
            echarts4rOutput("tren_pa")
          ),
          card(
            full_screen = TRUE,
            echarts4rOutput("tren_unmet_need")
          )
        ),
        layout_column_wrap(
          card(
            full_screen = T,
            echarts4rOutput("tren_mkjp")
          ),
          card(
            full_screen = T,
            echarts4rOutput("bar_mix_kontrasepsi")
          ),
          card(
            full_screen = T,
            echarts4rOutput("donut_perbandingan_tenaga_kb")
          )
        )
      ),
      nav_panel(
        title = "Poktan", 
        layout_column_wrap(
          # Create value box
          uiOutput("jumlah_desa"),
          uiOutput("kepemilikan_bkb"),
          uiOutput("kepemilikan_bkr"),
          uiOutput("kepemilikan_bkl"),
          uiOutput("kepemilikan_uppka"),
          uiOutput("kepemilikan_pikr"),
          uiOutput("kepemilikan_kkb"),
          uiOutput("kepemilikan_rdk"),
        ),
        fluidRow(
          column(12,
                 div(
                   style = "margin-bottom: 10px;",
                   downloadButton("download_data_poktan", "Unduh Data", 
                                  class = "btn-primary",
                                  icon = icon("download"))
                 ),
                 gt_output("tabel_poktan_reactable")
          )
        )
      ),
      nav_panel(
        title = "Stunting", 
        layout_column_wrap(
          value_box(
            title = "Jumlah Keluarga",
            value = textOutput("jumlah_keluarga"), 
            showcase = fa_icon("child-reaching"),
            showcase_layout = "left center",
            # Sesuaikan warna jika perlu, default-nya netral
            theme = "white" # Gunakan tema terang untuk yang ini
          ),
          value_box(
            title = "Jumlah Sasaran",
            value = textOutput("jumlah_keluarga_sasaran"), 
            showcase = fa_icon("child-reaching"),
            showcase_layout = "left center",
            # Sesuaikan warna jika perlu, default-nya netral
            theme = "white" # Gunakan tema terang untuk yang ini
          ),
          value_box(
            title = "Jumlah KRS",
            value = textOutput("jumlah_krs_menu"), 
            showcase = fa_icon("child-reaching"),
            showcase_layout = "left center",
            # Sesuaikan warna jika perlu, default-nya netral
            theme = "white" # Gunakan tema terang untuk yang ini
          )
        ),
        layout_column_wrap(
          card(
            echarts4rOutput("peringkat_kesejahteraan")
          ),
          card(
            echarts4rOutput("faktor_krs")
          )
        ),
        layout_column_wrap(
          card(
            echarts4rOutput("pie_punya_baduta")
          ),
          card(
            echarts4rOutput("pie_punya_balita")
          ),
          card(
            echarts4rOutput("pie_pus_hamil")
          )
        )
      )
    )
  ),
  
  # Page 2 - Analisis
  nav_panel(
    title = "SIPACOAI",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("pilih_kab_sipacoai", "Daftar Kabupaten",
                    choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA")),
        selectInput("pilih_kec_sipacoai", "Daftar Kecamatan", choices = NULL),
        selectInput("pilih_desa_kel_sipacoai", "Pilih Desa/Kel", choices = NULL),
        selectInput("pilih_bulan_sipacoai", "Pilih Bulan", choices = daftar_bulan[9:9], selected = "SEPTEMBER"),
        input_task_button(
          label_busy = "Sedang Proses",
          id = "cari_sipacoai",
          label = "Cari"
        )
      ),
      textOutput("tes_input_rekap_sipacoai"),
      # Baris pertama value boxes
      layout_column_wrap(
        value_box(
          title = "Keluarga Mendapat Pendampingan",
          value = textOutput("jumlah_pendampingan_keluarga"),
          showcase = bsicons::bs_icon("house-heart"),
          showcase_layout = "top right",
          theme = "warning",
          p("Edukasi, Fasilitasi Bantuan dan Fasilitasi Rujukan")
        ),
        value_box(
          title = "Sasaran Hadir Posyandu", 
          value = "%",
          showcase = bsicons::bs_icon("person-hearts"),
          showcase_layout = "top right",
          theme = "primary",
          p("Bumil dan Baduta")
        ),
        value_box(
          title = "Perkembangan Anak",
          value = textOutput("jumlah_penggunaan_kka"),
          showcase = bsicons::bs_icon("people"),
          showcase_layout = "top right", 
          theme = "warning",
          p("Jumlah Balita Dipantau dengan KKA")
        )
      ),
      
      # Baris kedua value boxes
      layout_column_wrap(
        value_box(
          title = "Genting",
          value = "%",
          showcase = fa_icon("baby"),
          showcase_layout = "top right",
          theme = "primary",
          p("Gerakan Orang Tua Asuh Cegah Stunting")
        ),
        value_box(
          title = "Tamasya", 
          value = "%",
          showcase = bsicons::bs_icon("emoji-smile"),
          showcase_layout = "top right",
          theme = "warning",
          p("Taman Asuh Sayang Anak")
        ),
        value_box(
          title = "Edukasi KBPP",
          value = textOutput("jumlah_edukasi_kbpp"),
          showcase = bsicons::bs_icon("person-plus"),
          showcase_layout = "top right",
          theme = "primary",
          p("Ibu Hamil dan Pascasalin")
        )
      ),
      
      # Baris ketiga value boxes
      layout_column_wrap(
        value_box(
          title = "Lansia (SIDAYA)",
          value = "Jumlah",
          showcase = bsicons::bs_icon("person-walking"),
          showcase_layout = "top right",
          theme = "warning",
          p("Mendapatkan Pemeriksaan Kesehatan")
        ),
        value_box(
          title = "Remaja (PIK-R)",
          value = textOutput("jumlah_pikr_sipacoai"),
          showcase = bsicons::bs_icon("people"),
          showcase_layout = "top right",
          theme = "primary",
          p("Jumlah Remaja Hadir Pertemuan PIK-R")
        ),
        value_box(
          title = "Ayah Teladan (GATI)",
          value = "%", 
          showcase = bsicons::bs_icon("person-arms-up"),
          showcase_layout = "top right",
          theme = "primary",
          p("Calon Ayah, Ayah dan Remaja yang Mendapatkan Edukasi")
        ),
        value_box(
          title = "Peserta KB Aktif",
          value = textOutput("jumlah_mkjp_sipacoai"),
          showcase = bsicons::bs_icon("person-plus"),
          showcase_layout = "top right", 
          theme = "warning",
          p("Akseptor KB MKJP")
        )
      ),
      # Versi yang lebih ringkas
      p("Silakan klik tombol di bawah ini untuk mengakses halaman Data BNBA Kemendukbangga/BKKBN:"),
      tags$a(
        "Akses Rekapitulasi Data BKKBN",
        href = "https://newsiga-siga.bkkbn.go.id/#/form/rekapitulasi",
        target = "_blank",
        class = "btn btn-primary btn-lg",
        style = "margin-top: 15px; margin-bottom: 15px;"
      )
    )
  )   
)

# Server
server <- function(input, output, session) {
  # gu input
  observeEvent(input$pilih_kab, {
    if (input$pilih_kab == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN == input$pilih_kab) |>
        fselect(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec, {
    if (input$pilih_kec == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel",
                        choices = c("SEMUA DESA/KEL"))
    } else {
      daftar_kel = data_nama_desa |>
        fselect(KECAMATAN, KELURAHAN) |>
        fsubset(KECAMATAN == input$pilih_kec) |>
        fselect(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel",
                        choices = c("SEMUA DESA/KEL", 
                                    daftar_kel))
    }
  })
  
  ## batas gu input
  
  ## gu filter
  # filter kab
  value_filter_kab <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari, {
    kondisi_input = input$pilih_kab
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_nama_desa$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab
    }
    value_filter_kab(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari, {
    kondisi_input = input$pilih_kec
    filter_kabupaten = value_filter_kab()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN %in% filter_kabupaten) |>
        fselect(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec
    }
    value_filter_kec(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari, {
    kondisi_input = input$pilih_desa_kel
    filter_kabupaten = value_filter_kab()
    filter_kecamatan = value_filter_kec()
    
    if (kondisi_input == "SEMUA DESA/KEL"){
      daftar_kel = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN, KELURAHAN) |>
        fsubset(
          KABUPATEN %in% filter_kabupaten) |>
        fsubset(KECAMATAN %in% filter_kecamatan) |>
        fselect(KELURAHAN)
      filter_desa_kel = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel = input$pilih_desa_kel
    }
    value_filter_desa_kel(filter_desa_kel) 
  })
  
  # bulan
  value_filter_bulan <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari, {
    value_filter_bulan(input$pilih_bulan) 
  })
  
  ## batas gu filter
  
  ## gu judul
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cari,{
    values$default <- input$cari
  })
  
  teks_judul_rekap <- eventReactive(input$cari, {
    if(input$pilih_kab == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel == "SEMUA DESA/KEL"){
      nama_daerah = input$pilih_kec
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste(tingkat_daerah, nama_daerah, "-", input$pilih_bulan)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
  })
  
  output$tes_input_rekap <- renderText({
    if(values$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap()
    }
  })
  
  ## batas gu judul
  
  ##filter bulan hingga
  # Data bulan untuk sorting
  month_order <- c("JANUARI" = 1, "FEBRUARI" = 2, "MARET" = 3, "APRIL" = 4, 
                   "MEI" = 5, "JUNI" = 6, "JULI" = 7, "AGUSTUS" = 8,
                   "SEPTEMBER" = 9, "OKTOBER" = 10, "NOVEMBER" = 11, "DESEMBER" = 12)
  
  # Fungsi helper untuk bulan hingga
  bulan_hingga <- function(bulan_terpilih) {
    urutan_bulan <- names(month_order)[1:month_order[bulan_terpilih]]
    return(urutan_bulan)
  }
  ## filter bulan hingga
  data_pkb <- fread("data/nama pkb.csv", sep = ";")
  output$nama_pkb <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    
    if (length(filter_desa) == 1) {
      # Filter data and get the first NAMA_PKB value
      filtered_data <- fsubset(data_pkb, 
                               KABUPATEN %in% filter_kabupaten & 
                                 KECAMATAN %in% filter_kecamatan & 
                                 KELURAHAN %in% filter_desa
      )
      nama_pkb <- filtered_data$`NAMA PKB`[1]
    } else {
      # Filter data, select columns, get unique rows, and count
      filtered_data <- fsubset(data_pkb, 
                               KABUPATEN %in% filter_kabupaten & 
                                 KECAMATAN %in% filter_kecamatan & 
                                 KELURAHAN %in% filter_desa
      )
      
      selected_data <- fselect(filtered_data, KABUPATEN, `NAMA PKB`)
      unique_data <- funique(selected_data)
      
      nama_pkb <- paste0("Jumlah: ", fnrow(unique_data))
    }
    nama_pkb
  })
  
  output$jumlah_wilker <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
      # Filter data and get the first NAMA_PKB value
    paste("Jumlah Wilker: ", 
          fnrow(
            filtered_data <- fsubset(data_pkb, 
                                     KABUPATEN %in% filter_kabupaten & 
                                       KECAMATAN %in% filter_kecamatan & 
                                       KELURAHAN %in% filter_desa
            )
          ))
  }) 
  
  data_tpk <- fread("data/nama_tpk.csv", sep = ",")
  output$jumlah_tpk <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    
    # Filter data based on the conditions
    filtered_data <- fsubset(data_tpk, 
                             Kota %in% filter_kabupaten & 
                               Kecamatan %in% filter_kecamatan & 
                               Kelurahan %in% filter_desa
    )
    
    # Select columns and get unique rows
    selected_data <- fselect(filtered_data, Kecamatan, Register)
    unique_data <- funique(selected_data)
    
    # Return the result string
    result <- paste0("Jumlah TPK: ", fnrow(unique_data))
  })
  
  data_pus <- fread("data/data_pus.csv", sep = ";", header = T)
  output$jumlah_pus <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    filtered_data <- fsubset(data_pus, 
                             KABUPATEN %in% filter_kabupaten & 
                               KECAMATAN %in% filter_kecamatan & 
                               KELURAHAN %in% filter_desa &
                               BULAN %in% filter_bulan
    )
    
    pus_sum <- fsum(filtered_data$PUS)
    
    paste0(format(pus_sum, big.mark = ".", scientific = FALSE))
  })
  
  output$persentase_unmet_need <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    data_terfilter <- fsubset(data_pus, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = c("PUS", "UNMET NEED"))
    
    persentase <- (data_grup$`UNMET NEED` / data_grup$PUS) * 100
    persentase_format <- format(round(persentase, 2), decimal.mark = ",")
    
    paste0(persentase_format, "%")
  })
  
  data_mix <- fread("data/data_mix_kontra.csv", sep = ";", header = T)
  output$persentase_mkjp <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    data_terfilter <- fsubset(data_mix, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = c("IMPLAN", "IUD", "VASEKTOMI", "TUBEKTOMI", "KB MODERN"))
    
    mkjp_persentase <- ((data_grup$IUD + data_grup$IMPLAN + data_grup$VASEKTOMI + data_grup$TUBEKTOMI) / 
                          data_grup$`KB MODERN`) * 100
    
    mkjp_format <- format(round(mkjp_persentase, 2), decimal.mark = ",")
    
    paste0(mkjp_format, "%")
  })
  
  output$kontrasepsi_favorit <- renderText({
    req(input$cari)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    data_terfilter <- fsubset(data_mix, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Group by PROVINSI dan hitung sum untuk semua kolom kontrasepsi
    mix_kontra <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                         cols = c("SUNTIK", "PIL", "KONDOM", "MAL", "IMPLAN", 
                                  "IUD", "VASEKTOMI", "TUBEKTOMI"))
    
    # Ambil baris pertama dan exclude kolom PROVINSI
    kolom_kontrasepsi <- c("SUNTIK", "PIL", "KONDOM", "MAL", "IMPLAN", 
                           "IUD", "VASEKTOMI", "TUBEKTOMI")
    
    # Perbaikan: gunakan sintaks data.table dengan ..
    nilai_kontrasepsi <- unlist(mix_kontra[1, ..kolom_kontrasepsi])
    
    # Temukan nilai maksimum dan nama kolom
    nilai_max <- max(nilai_kontrasepsi)
    kolom_max <- names(nilai_kontrasepsi)[which.max(nilai_kontrasepsi)]
    
    # Format angka dengan titik sebagai pemisah ribuan
    nilai_format <- format(nilai_max, big.mark = ".", scientific = FALSE)
    
    # Hasil akhir
    paste0(kolom_max, " (", nilai_format, ")")
  })
  
  # Baca data faskes_sdm
  faskes_sdm <- fread("data/data_faskes_siga.csv", sep = ";", header = T)
  
  # Reactive value untuk menyimpan hasil
  tempat_pelayanan_result <- reactiveVal("")
  
  # Observer untuk tombol
  observeEvent(input$cari, {
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    data_terfilter <- fsubset(faskes_sdm, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    jumlah_unik <- fnunique(data_terfilter$`NO REGISTRASI`)
    hasil_format <- format(jumlah_unik, big.mark = ".", scientific = FALSE)
    
    tempat_pelayanan_result(hasil_format)
  })
  
  # Output text
  output$tempat_pelayanan_kb <- renderText({
    tempat_pelayanan_result()
  })
  
  # Output untuk tempat pelayanan KB terlatih
  output$tempat_pelayanan_kb_terlatih <- renderText({
    req(input$cari)  # Menunggu tombol ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(faskes_sdm, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Filter berdasarkan jenis pelatihan (IUD, IMPLAN, Tubektomi, Vasektomi)
    data_terlatih <- fsubset(data_terfilter,
                             grepl("IUD", PELATIHAN, ignore.case = TRUE) |
                               grepl("IMPLAN", PELATIHAN, ignore.case = TRUE) |
                               grepl("Tubektomi", PELATIHAN, ignore.case = TRUE) |
                               grepl("Vasektomi", PELATIHAN, ignore.case = TRUE)
    )
    
    # Hitung jumlah unique NO REGISTRASI
    jumlah_unik <- fnunique(data_terlatih$`NO REGISTRASI`)
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_unik, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk tenaga kesehatan KB
  output$tenaga_kesehatan_kb <- renderText({
    req(input$cari)  # Menunggu tombol ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(faskes_sdm, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Hitung jumlah baris (tenaga kesehatan)
    jumlah_nakes <- fnrow(data_terfilter)
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_nakes, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk tenaga kesehatan KB terlatih
  output$tenaga_kesehatan_kb_terlatih <- renderText({
    req(input$cari)  # Menunggu tombol ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(faskes_sdm, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Filter berdasarkan jenis pelatihan (IUD, IMPLAN, Tubektomi, Vasektomi)
    data_terlatih <- fsubset(data_terfilter,
                             grepl("IUD", PELATIHAN, ignore.case = TRUE) |
                               grepl("IMPLAN", PELATIHAN, ignore.case = TRUE) |
                               grepl("Tubektomi", PELATIHAN, ignore.case = TRUE) |
                               grepl("Vasektomi", PELATIHAN, ignore.case = TRUE)
    )
    
    # Hitung jumlah baris (tenaga kesehatan terlatih)
    jumlah_nakes_terlatih <- fnrow(data_terlatih)
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_nakes_terlatih, big.mark = ".", scientific = FALSE)
  })
  
  # Baca data KRS verval
  data_krs_verval <- fread("data/data_verval_krs_2024_sem2.csv", sep = ";")
  
  # Atau jika menggunakan readr:
  # data_krs_verval <- read_csv2("data/data_verval_krs_2024_sem2.csv")
  
  # Output untuk jumlah KRS
  output$jumlah_krs <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    
    # Filter data berdasarkan lokasi
    data_terfilter <- fsubset(data_krs_verval, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa
    )
    
    # Group by PROVINSI dan hitung sum JUMLAH KRS
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "JUMLAH KRS")
    
    # Ambil nilai sum
    jumlah_krs_total <- data_grup$`JUMLAH KRS`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_krs_total, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk jumlah keluarga punya baduta
  output$jumlah_keluarga_punya_baduta <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    
    # Filter data berdasarkan lokasi
    data_terfilter <- fsubset(data_krs_verval, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa
    )
    
    # Group by PROVINSI dan hitung sum PUNYA BADUTA
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "PUNYA BADUTA")
    
    # Ambil nilai sum
    jumlah_keluarga_baduta <- data_grup$`PUNYA BADUTA`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_keluarga_baduta, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk jumlah PUS hamil
  output$jumlah_pus_hamil <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    
    # Filter data berdasarkan lokasi
    data_terfilter <- fsubset(data_krs_verval, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa
    )
    
    # Group by PROVINSI dan hitung sum PUS HAMIL
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "PUS HAMIL")
    
    # Ambil nilai sum
    jumlah_pus_hamil_total <- data_grup$`PUS HAMIL`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_pus_hamil_total, big.mark = ".", scientific = FALSE)
  })
  
  # Baca data sasaran poktan
  sasaran_poktan <- fread("data/sasaran_poktan_gabung.csv", sep = ";")
  
  # Atau jika menggunakan readr:
  # sasaran_poktan <- read_csv2("data/sasaran_poktan_gabung.csv")
  
  # Output untuk jumlah sasaran BKB
  output$jumlah_sasaran_bkb <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(sasaran_poktan, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Group by PROVINSI dan hitung sum SASARAN BKB
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "SASARAN BKB")
    
    # Ambil nilai sum
    jumlah_sasaran_bkb_total <- data_grup$`SASARAN BKB`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_sasaran_bkb_total, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk jumlah sasaran BKR
  output$jumlah_sasaran_bkr <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(sasaran_poktan, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Group by PROVINSI dan hitung sum SASARAN BKR
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "SASARAN BKR")
    
    # Ambil nilai sum
    jumlah_sasaran_bkr_total <- data_grup$`SASARAN BKR`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_sasaran_bkr_total, big.mark = ".", scientific = FALSE)
  })
  
  # Output untuk jumlah sasaran BKL
  output$jumlah_sasaran_bkl <- renderText({
    req(input$cari)  # Menunggu tombol cari ditekan
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data berdasarkan lokasi dan bulan
    data_terfilter <- fsubset(sasaran_poktan, 
                              KABUPATEN %in% filter_kabupaten & 
                                KECAMATAN %in% filter_kecamatan & 
                                KELURAHAN %in% filter_desa &
                                BULAN %in% filter_bulan
    )
    
    # Group by PROVINSI dan hitung sum SASARAN BKL
    data_grup <- collap(data_terfilter, ~ PROVINSI, FUN = list(fsum), 
                        cols = "SASARAN BKL")
    
    # Ambil nilai sum
    jumlah_sasaran_bkl_total <- data_grup$`SASARAN BKL`
    
    # Format angka dengan titik sebagai pemisah ribuan
    format(jumlah_sasaran_bkl_total, big.mark = ".", scientific = FALSE)
  })
  
  ## KB
  # Output untuk tren PUS (label hanya angka)
  output$tren_pus <- renderEcharts4r({
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter dan aggregate data
    data_plot <- data_pus |>
      fsubset(
        KABUPATEN %in% filter_kabupaten & 
          KECAMATAN %in% filter_kecamatan & 
          KELURAHAN %in% filter_desa &
          BULAN %in% bulan_hingga(filter_bulan)
      ) |>
      collap(~ PROVINSI + BULAN, FUN = list(fsum), cols = "PUS")
    
    # Sort berdasarkan urutan bulan
    data_plot <- data_plot[order(match(data_plot$BULAN, names(month_order))), ]
    
    # Hitung batas Y axis
    nilai_min <- min(data_plot$PUS, na.rm = TRUE)
    nilai_max <- max(data_plot$PUS, na.rm = TRUE)
    y_min <- nilai_min - (0.05 * nilai_min)
    y_max <- nilai_max + (0.05 * nilai_max)
    
    # Buat grafik dengan label hanya angka
    data_plot |>
      e_charts(BULAN) |>
      e_line(PUS, 
             smooth = TRUE, 
             symbol = "circle", 
             symbolSize = 8,
             lineStyle = list(width = 3),
             # Alternatif formatter yang lebih robust
             label = list(
               show = TRUE,
               position = "top",
               fontSize = 10,
               fontWeight = "bold"
             )) |>
      e_tooltip(
        trigger = "axis"
      ) |>
      e_title("Tren Jumlah PUS") |>
      e_x_axis(
        axisLabel = list(
          interval = 0, 
          rotate = 0,
          formatter = htmlwidgets::JS(
            "function(value) { return value.substring(0, 3); }"
          )
        )
      ) |>
      e_y_axis(
        min = round(y_min),
        max = round(y_max),
        axisLabel = list(
          formatter = htmlwidgets::JS(
            "function(value) { return value.toLocaleString('id-ID'); }"
          )
        )
      ) |>
      e_theme("default") |>
      e_legend(show = FALSE)
  })
  
  # Output untuk tren PA
  output$tren_pa <- renderEcharts4r({
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter dan aggregate data
    data_plot <- data_mix |>
      fsubset(
        KABUPATEN %in% filter_kabupaten & 
          KECAMATAN %in% filter_kecamatan & 
          KELURAHAN %in% filter_desa &
          BULAN %in% bulan_hingga(filter_bulan)
      ) |>
      collap(~ PROVINSI + BULAN, FUN = list(fsum), cols = "PA")
    
    # Sort berdasarkan urutan bulan
    data_plot <- data_plot[order(match(data_plot$BULAN, names(month_order))), ]
    
    # Hitung batas Y axis (sama seperti Python: min*0.95, max*1.05)
    nilai_min <- min(data_plot$PA, na.rm = TRUE)
    nilai_max <- max(data_plot$PA, na.rm = TRUE)
    y_min <- nilai_min * 0.95
    y_max <- nilai_max * 1.05
    
    # Buat grafik dengan warna kuning seperti Python
    data_plot |>
      e_charts(BULAN) |>
      e_line(PA, 
             smooth = TRUE, 
             symbol = "circle", 
             symbolSize = 8,
             lineStyle = list(width = 3, color = "#FFD700"), # warna kuning
             itemStyle = list(color = "#FFD700"), # warna point kuning
             label = list(
               show = TRUE,
               position = "top",
               fontSize = 10,
               fontWeight = "bold",
               color = "#1f77b4", # warna biru untuk label
               formatter = htmlwidgets::JS(
                 "function(params) {
                  var value = typeof params.value === 'number' ? params.value : params.value[1];
                  return value.toLocaleString('en-US').replace(/,/g, '.');
                }"
               )
             )) |>
      e_tooltip(
        trigger = "axis"
      ) |>
      e_title("Tren Jumlah PA",
              textStyle = list(fontSize = 16, fontWeight = "bold")) |>
      e_x_axis(
        axisLabel = list(
          interval = 0, 
          rotate = 0,
          formatter = htmlwidgets::JS(
            "function(value) { return value.substring(0, 3); }"
          )
        ),
        axisLine = list(show = TRUE),
        axisTick = list(show = FALSE)
      ) |>
      e_y_axis(
        name = "JUMLAH PA",
        nameTextStyle = list(fontSize = 12),
        min = round(y_min),
        max = round(y_max),
        splitNumber = 5, # Batasi 5 tick seperti Python
        axisLabel = list(
          formatter = htmlwidgets::JS(
            "function(value) { 
             return value.toLocaleString('en-US').replace(/,/g, '.'); 
           }"
          )
        ),
        splitLine = list(show = TRUE) # Hilangkan grid
      ) |>
      e_grid(
        left = "80px",
        right = "50px", 
        top = "80px",
        bottom = "80px"
      ) |>
      e_theme("default") |>
      e_legend(show = FALSE)
  })
  
  # Output untuk tren Unmet Need
  output$tren_unmet_need <- renderEcharts4r({
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter dan aggregate data (hitung persentase)
    data_terfilter <- data_pus |>
      fsubset(
        KABUPATEN %in% filter_kabupaten & 
          KECAMATAN %in% filter_kecamatan & 
          KELURAHAN %in% filter_desa &
          BULAN %in% bulan_hingga(filter_bulan)
      )
    
    # Group by dan hitung total PUS dan UNMET NEED
    data_grup <- collap(data_terfilter, ~ PROVINSI + BULAN, 
                        FUN = list(fsum), cols = c("PUS", "UNMET NEED"))
    
    # Hitung persentase Unmet Need
    data_plot <- data_grup
    data_plot$UNMET_NEED_PERSEN <- round((data_plot$`UNMET NEED` / data_plot$PUS) * 100, 2)
    
    # Sort berdasarkan urutan bulan
    data_plot <- data_plot[order(match(data_plot$BULAN, names(month_order))), ]
    
    # Hitung batas Y axis (sama seperti Python: min*0.95, max*1.05)
    nilai_min <- min(data_plot$UNMET_NEED_PERSEN, na.rm = TRUE)
    nilai_max <- max(data_plot$UNMET_NEED_PERSEN, na.rm = TRUE)
    y_min <- nilai_min * 0.95
    y_max <- nilai_max * 1.05
    
    # Buat grafik dengan echarts4r
    data_plot |>
      e_charts(BULAN) |>
      e_line(UNMET_NEED_PERSEN, 
             smooth = TRUE, 
             symbol = "circle", 
             symbolSize = 8,
             lineStyle = list(width = 3),
             label = list(
               show = TRUE,
               position = "top",
               fontSize = 10,
               fontWeight = "bold",
               color = "#ff7f0e"
             )) |>
      e_tooltip(
        trigger = "axis"
        
      ) |>
      e_title("Tren Persentase Unmet Need",
              textStyle = list(fontSize = 16, fontWeight = "bold")) |>
      e_x_axis(
        axisLabel = list(
          interval = 0, 
          rotate = 0,
          formatter = htmlwidgets::JS(
            "function(value) { return value.substring(0, 3); }"
          )
        )
      ) |>
      e_y_axis(
        name = "%",
        nameTextStyle = list(fontSize = 12),
        min = round(y_min),
        max = round(y_max),
        splitLine = list(show = FALSE)
      ) |>
      e_theme("default") |>
      e_legend(show = FALSE)
  })
  
  # Output untuk tren MKJP
  output$tren_mkjp <- renderEcharts4r({
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data
    data_terfilter <- data_mix |>
      fsubset(
        KABUPATEN %in% filter_kabupaten & 
          KECAMATAN %in% filter_kecamatan & 
          KELURAHAN %in% filter_desa &
          BULAN %in% bulan_hingga(filter_bulan)
      )
    
    # Group by dan aggregate semua kolom MKJP
    data_grup <- collap(data_terfilter, ~ PROVINSI + BULAN, 
                        FUN = list(fsum), 
                        cols = c("IMPLAN", "IUD", "VASEKTOMI", "TUBEKTOMI", "KB MODERN"))
    
    # Hitung persentase MKJP
    data_plot <- data_grup
    data_plot$MKJP_TOTAL <- data_plot$IUD + data_plot$IMPLAN + data_plot$VASEKTOMI + data_plot$TUBEKTOMI
    data_plot$MKJP_PERSEN <- round((data_plot$MKJP_TOTAL / data_plot$`KB MODERN`) * 100, 2)
    
    # Sort berdasarkan urutan bulan
    data_plot <- data_plot[order(match(data_plot$BULAN, names(month_order))), ]
    
    # Hitung batas Y axis
    nilai_min <- min(data_plot$MKJP_PERSEN, na.rm = TRUE)
    nilai_max <- max(data_plot$MKJP_PERSEN, na.rm = TRUE)
    y_min <- nilai_min * 0.95
    y_max <- nilai_max * 1.05
    
    # Buat grafik dengan echarts4r (struktur sama dengan tren sebelumnya)
    data_plot |>
      e_charts(BULAN) |>
      e_line(MKJP_PERSEN, 
             smooth = TRUE, 
             symbol = "circle", 
             symbolSize = 8,
             lineStyle = list(width = 3, color = "#FFD700"), # warna kuning
             itemStyle = list(color = "#FFD700"),
             label = list(
               show = TRUE,
               position = "top",
               fontSize = 10,
               fontWeight = "bold",
               color = "#1f77b4" # label biru
             )) |>
      e_tooltip(
        trigger = "axis"
      ) |>
      e_title("Tren Persentase MKJP",
              textStyle = list(fontSize = 16, fontWeight = "bold")) |>
      e_x_axis(
        axisLabel = list(
          interval = 0, 
          rotate = 0
        )
      ) |>
      e_y_axis(
        name = "%",
        nameTextStyle = list(fontSize = 12),
        min = round(y_min),
        max = round(y_max)
      ) |>
      e_theme("default") |>
      e_legend(show = FALSE)
  })
  
  # Output untuk bar mix kontrasepsi
  output$bar_mix_kontrasepsi <- renderEcharts4r({
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Filter data untuk bulan terpilih saja
    data_terfilter <- data_mix |>
      fsubset(
        KABUPATEN %in% filter_kabupaten & 
          KECAMATAN %in% filter_kecamatan & 
          KELURAHAN %in% filter_desa &
          BULAN == filter_bulan
      )
    
    # Group by PROVINSI dan aggregate semua metode KB
    data_grup <- collap(data_terfilter, ~ PROVINSI, 
                        FUN = list(fsum), 
                        cols = c("SUNTIK", "PIL", "KONDOM", "MAL", "IMPLAN", 
                                 "IUD", "VASEKTOMI", "TUBEKTOMI"))
    
    # Reshape ke format long (unpivot)
    kolom_kb <- c("SUNTIK", "PIL", "KONDOM", "MAL", "IMPLAN", "IUD", "VASEKTOMI", "TUBEKTOMI")
    
    # Manual unpivot untuk data mix kontrasepsi
    data_long <- data.frame(
      PROVINSI = rep(data_grup$PROVINSI, length(kolom_kb)),
      METODE_KB = rep(kolom_kb, each = nrow(data_grup)),
      JUMLAH = c(data_grup$SUNTIK, data_grup$PIL, data_grup$KONDOM, data_grup$MAL,
                 data_grup$IMPLAN, data_grup$IUD, data_grup$VASEKTOMI, data_grup$TUBEKTOMI)
    )
    
    # Aggregate by METODE_KB dan hitung total
    data_plot <- collap(data_long, ~ METODE_KB, FUN = list(fsum), cols = "JUMLAH")
    
    # Sort descending berdasarkan jumlah
    data_plot <- data_plot[order(data_plot$JUMLAH), ]
    
    # Hitung persentase
    total_jumlah <- sum(data_plot$JUMLAH)
    data_plot$PERSENTASE <- round((data_plot$JUMLAH / total_jumlah) * 100, 2)
    
    # Format angka untuk tooltip
    data_plot$JUMLAH_FORMAT <- format(data_plot$JUMLAH, big.mark = ".", scientific = FALSE)
    
    # Buat horizontal bar chart
    data_plot |>
      e_charts(METODE_KB) |>
      e_bar(JUMLAH, 
            name = "Jumlah Pengguna",
            itemStyle = list(color = "#1f77b4"), # warna biru
            label = list(
              show = TRUE,
              position = "right",
              fontSize = 10,
              fontWeight = "bold"
            )) |>
      e_flip_coords() |> # Membuat horizontal bar
      e_tooltip(
        trigger = "axis"
      ) |>
      e_title("Perbandingan Pengguna Metode KB",
              textStyle = list(fontSize = 16, fontWeight = "bold")) |>
      e_x_axis(
        name = "Jumlah Pengguna",
        nameTextStyle = list(fontSize = 12),
        splitNumber = 3, # 3 tick seperti Python
        axisLabel = list(
          formatter = htmlwidgets::JS(
            "function(value) { 
             return value.toLocaleString('en-US').replace(/,/g, '.'); 
           }"
          )
        ),
        splitLine = list(show = FALSE)
      ) |>
      e_y_axis(
        name = "Metode KB",
        nameTextStyle = list(fontSize = 12),
        type = "category"
      ) |>
      e_grid(
        left = "100px",
        right = "100px", 
        top = "80px",
        bottom = "60px"
      ) |>
      e_theme("default") |>
      e_legend(show = FALSE)
  })
  
  faskes_sdm = fread("data/data_faskes_siga.csv", sep=";")
  output$donut_perbandingan_tenaga_kb <- renderEcharts4r({
    # Trigger reactivity with action button
    req(input$cari)
    
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()  
    filter_desa <- value_filter_desa_kel()
    #filter_bulan <- input$pilih_bulan
    
    # Klasifikasi tenaga kerja berdasarkan kolom PELATIHAN
    data_with_classification <- faskes_sdm |>
      fmutate(
        KLASIFIKASI = fifelse(
          grepl("IUD|Implan|Tubektomi|Vasektomi", PELATIHAN, ignore.case = TRUE),
          "Sudah Terlatih",
          "Belum Terlatih"
        )
      )
    
    # Hitung jumlah tenaga kerja untuk setiap klasifikasi
    summary_data <- data_with_classification |>
      fcount(KLASIFIKASI, name = "count")
    
    # Tambahkan kolom persentase
    total_count <- fsum(summary_data$count)
    summary_data <- summary_data |>
      fmutate(
        PERSENTASE = round((count / total_count) * 100, 2),
        PERSENTASE_FORMAT = paste0(
          format(PERSENTASE, decimal.mark = ",", nsmall = 2), "%"
        )
      )
    
    # Definisi warna
    colors <- c(warna_biru, warna_kuning)  # Biru untuk Sudah Terlatih, Kuning untuk Belum
    
    # Buat donut chart menggunakan echarts4r
    summary_data |>
      e_charts(KLASIFIKASI) |>
      e_pie(
        count, 
        radius = c("50%", "80%"),  # Inner dan outer radius untuk donut effect
        name = "Status Pelatihan"
      ) |>
      e_color(colors) |>
      e_title(
        text = "Perbandingan Status Pelatihan Tenaga Kesehatan KB",
        subtext = "Status Pelatihan Dokter/Bidan",
        left = "center"
      ) |>
      e_legend(show = FALSE) |>  # Sembunyikan legend
      e_tooltip(
        formatter = htmlwidgets::JS("
        function(params){
          return '<strong>' + params.name + '</strong><br/>' +
                 'Jumlah: ' + params.value + '<br/>' +
                 'Persentase: ' + params.percent + '%'
        }
      ")
      ) |>
      e_labels(
        show = TRUE,
        position = "outside",
        formatter = htmlwidgets::JS("
        function(params) {
          return params.name + '\\n' + params.percent + '%';
        }
      "),
        fontSize = 11
      ) |>
      e_grid(
        left = 0,
        right = 3,
        top = 20,
        bottom = 0
      ) |>
      e_theme_custom('{"backgroundColor": "#f6f8fa"}') |>
      e_animation(TRUE)
  })
  ##KB
  
  ## poktan
  # Load data efficiently at startup
  data_bkb <- fread("data/data_bkb.csv", sep = ";")
  data_bkr <- fread("data/data_bkr.csv", sep = ";")
  data_bkl <- fread("data/data_bkl.csv", sep = ";")
  data_uppka <- fread("data/data_uppka.csv", sep = ";")
  data_pikr <- fread("data/data_pikr.csv", sep = ";")
  data_kkb <- fread("data/data_kkb.csv", sep = ";")
  data_rdk <- fread("data/data_rdk.csv", sep = ";")
  
  # Reactive filter values
  filter_data <- reactive({
    list(
      # Trigger reactivity with action button
      kabupaten <- value_filter_kab(),
      kecamatan <- value_filter_kec()    ,
      desa <- value_filter_desa_kel(),
      bulan <- input$pilih_bulan
    )
  }) |> bindEvent(input$cari)
  
  # Base filtering function
  apply_filter <- function(data, include_bulan = TRUE) {
    filters <- filter_data()
    result <- data |>
      fsubset(KABUPATEN %in% filters[[1]] &
                KECAMATAN %in% filters[[2]] &
                KELURAHAN %in% filters[[3]])

    if(include_bulan) {
      result <- result |> fsubset(BULAN == filters[[4]])
    }
    result
  }
  
  # Cached reactive for jumlah desa (used by multiple value boxes)
  jumlah_desa_val <- reactive({
    apply_filter(data_bkb) |> fnrow()
  }) |> bindEvent(input$cari)
  
  # Helper function to create value box
  create_value_box <- function(title, value, comparison_value) {
    if(comparison_value <= value) {
      theme_color <- "#0B538E"
      icon_name <- "check2-square"
    } else {
      theme_color <- "#B22222" 
      icon_name <- "exclamation-octagon"
    }
    
    value_box(
      title = title,
      value = value,
      theme = value_box_theme(bg = theme_color, fg = "#f6f8fa"),
      showcase = bsicons::bs_icon(icon_name),
      showcase_layout = "bottom",
      class = "custom-box"
    )
  }
  
  # Value box outputs
  output$jumlah_desa <- renderUI({
    value_box(
      title = "Desa / Kelurahan",
      value = jumlah_desa_val(),
      theme = value_box_theme(bg = "#0B538E", fg = "#f6f8fa"),
      showcase = bsicons::bs_icon("check2-square"),
      showcase_layout = "bottom",
      class = "custom-box"
    )
  })
  
  output$kepemilikan_bkb <- renderUI({
    jumlah_bkb <- apply_filter(data_bkb) |>
      fselect(`YANG ADA`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah BKB", jumlah_bkb, jumlah_desa_val())
  })
  
  output$kepemilikan_bkr <- renderUI({
    jumlah_bkr <- apply_filter(data_bkr) |>
      fselect(`YANG ADA`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah BKR", jumlah_bkr, jumlah_desa_val())
  })
  
  output$kepemilikan_bkl <- renderUI({
    jumlah_bkl <- apply_filter(data_bkl) |>
      fselect(`YANG ADA`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah BKL", jumlah_bkl, jumlah_desa_val())
  })
  
  output$kepemilikan_uppka <- renderUI({
    jumlah_uppka <- apply_filter(data_uppka) |>
      fselect(`YANG ADA`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah UPPKA", jumlah_uppka, jumlah_desa_val())
  })
  
  output$kepemilikan_pikr <- renderUI({
    jumlah_pikr <- apply_filter(data_pikr) |>
      fselect(`YANG ADA`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah PIK-R", jumlah_pikr, jumlah_desa_val())
  })
  
  output$kepemilikan_kkb <- renderUI({
    jumlah_kkb <- apply_filter(data_kkb, include_bulan = FALSE) |>
      fselect(`JUMLAH KAMPUNG KB`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah KKB", jumlah_kkb, jumlah_desa_val())
  })
  
  output$kepemilikan_rdk <- renderUI({
    jumlah_rdk <- apply_filter(data_rdk, include_bulan = FALSE) |>
      fselect(`JUMLAH RUMAH DATAKU`) |>
      fsum(na.rm = TRUE)
    
    create_value_box("Jumlah RDK", jumlah_rdk, jumlah_desa_val())
  })

  # Buat reactive value untuk data tabel
  tabel_data <- reactive({
    # Trigger saat tombol cari ditekan
    input$cari
    
    # Ambil nilai filter
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- input$pilih_bulan
    
    # Tentukan jenjang berdasarkan pilihan
    if(input$pilih_kab == "SEMUA KABUPATEN") {
      jenjang <- "KABUPATEN"
    } else if(input$pilih_kab != "SEMUA KABUPATEN" && input$pilih_kec == "SEMUA KECAMATAN") {
      jenjang <- "KECAMATAN" 
    } else {
      jenjang <- "KELURAHAN"
    }
    
    # Fungsi untuk memproses data
    process_data <- function(df, unit_name, group_col) {
      # Pastikan kolom numerik
      df <- df |>
        fmutate(
          YANG_ADA = as.integer(`YANG ADA`),
          YANG_LAPOR = as.integer(`YANG LAPOR`)
        )
      
      # Filter data
      filtered_df <- df |>
        fsubset(KABUPATEN %in% filter_kabupaten &
                  BULAN == filter_bulan &
                  KECAMATAN %in% filter_kecamatan &
                  KELURAHAN %in% filter_desa)
      
      # Agregasi berdasarkan jenjang yang dipilih
      if(group_col == "KABUPATEN") {
        processed_df <- filtered_df |>
          fgroup_by(KABUPATEN) |>
          fsummarise(
            JUMLAH = fsum(YANG_ADA, na.rm = TRUE),
            JUMLAH_LAPOR = fsum(YANG_LAPOR, na.rm = TRUE)
          ) |>
          fmutate(
            BELUM_LAPOR = JUMLAH - JUMLAH_LAPOR
          )
        
        # Rename kolom
        names(processed_df)[names(processed_df) == "JUMLAH"] <- paste0("JUMLAH ", unit_name)
        names(processed_df)[names(processed_df) == "JUMLAH_LAPOR"] <- paste0("JUMLAH ", unit_name, " LAPOR")
        names(processed_df)[names(processed_df) == "BELUM_LAPOR"] <- paste0(unit_name, " BELUM LAPOR")
        
      } else if(group_col == "KECAMATAN") {
        processed_df <- filtered_df |>
          fgroup_by(KECAMATAN) |>
          fsummarise(
            JUMLAH = fsum(YANG_ADA, na.rm = TRUE),
            JUMLAH_LAPOR = fsum(YANG_LAPOR, na.rm = TRUE)
          ) |>
          fmutate(
            BELUM_LAPOR = JUMLAH - JUMLAH_LAPOR
          )
        
        # Rename kolom
        names(processed_df)[names(processed_df) == "JUMLAH"] <- paste0("JUMLAH ", unit_name)
        names(processed_df)[names(processed_df) == "JUMLAH_LAPOR"] <- paste0("JUMLAH ", unit_name, " LAPOR")
        names(processed_df)[names(processed_df) == "BELUM_LAPOR"] <- paste0(unit_name, " BELUM LAPOR")
        
      } else {
        processed_df <- filtered_df |>
          fgroup_by(KELURAHAN) |>
          fsummarise(
            JUMLAH = fsum(YANG_ADA, na.rm = TRUE),
            JUMLAH_LAPOR = fsum(YANG_LAPOR, na.rm = TRUE)
          ) |>
          fmutate(
            BELUM_LAPOR = JUMLAH - JUMLAH_LAPOR
          )
        
        # Rename kolom
        names(processed_df)[names(processed_df) == "JUMLAH"] <- paste0("JUMLAH ", unit_name)
        names(processed_df)[names(processed_df) == "JUMLAH_LAPOR"] <- paste0("JUMLAH ", unit_name, " LAPOR")
        names(processed_df)[names(processed_df) == "BELUM_LAPOR"] <- paste0(unit_name, " BELUM LAPOR")
      }
      
      return(processed_df)
    }
    
    # Proses setiap DataFrame
    result_bkb <- process_data(data_bkb, "BKB", jenjang)
    result_bkr <- process_data(data_bkr, "BKR", jenjang) 
    result_bkl <- process_data(data_bkl, "BKL", jenjang)
    result_uppka <- process_data(data_uppka, "UPPKA", jenjang)
    result_pikr <- process_data(data_pikr, "PIK-R", jenjang)
    
    # Gabungkan semua hasil
    final_result <- result_bkb |>
      join(result_bkr, on = jenjang, how = "inner") |>
      join(result_bkl, on = jenjang, how = "inner") |>
      join(result_uppka, on = jenjang, how = "inner") |>
      join(result_pikr, on = jenjang, how = "inner")
    
    return(final_result)
  })
  
  # Output tabel GT menggunakan reactive data
  output$tabel_poktan_reactable <- render_gt({
    tabel_data() |>
      gt() |>
      tab_header(
        title = "Data Laporan Kelompok Kegiatan/Setara",
        subtitle = paste("New SIGA - ", input$pilih_bulan, " 2025")
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      ) |>
      cols_align(
        align = "left", 
        columns = 1
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "#f8f9fa")
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(size = px(16)),
        locations = cells_body()
      ) |>
      opt_interactive(
        use_pagination = FALSE,
        use_sorting = TRUE,
        use_search = TRUE,
        use_filters = FALSE,
        use_resizers = TRUE,
        use_highlight = TRUE,
        use_compact_mode = FALSE,
        use_text_wrapping = TRUE,
        use_page_size_select = FALSE,
        page_size_default = 20,
      ) |>
      opt_table_font(
        font = list(
          google_font(name = "Source Sans Pro"),
          "Arial", "sans-serif"
        )
      ) |>
      tab_options(
        table.width = pct(100),
        container.height = px(500),
        container.overflow.y = "auto",
        container.overflow.x = "auto"
      )
  })
  
  
  # Download handler untuk CSV (optional)
  output$download_data_poktan <- downloadHandler(
    filename = function() {
      paste0("data_laporan_poktan_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(tabel_data(), file, row.names = FALSE, sep = ";")
    }
  )

  ##poktan
  
  ## stunting
  data_krs_verval = fread("data/data_verval_krs_2024_sem2.csv", sep=";")
  # Single reactive for filtered data - calculated once and reused
  filtered_data_krs <- reactive({
    filters <- filter_data()
    
    data_krs_verval |>
      fsubset(KABUPATEN %in% filters[[1]] &
                KECAMATAN %in% filters[[2]] &
                KELURAHAN %in% filters[[3]]) |>
      fgroup_by(PROVINSI) |>
      fsummarise(
        JUMLAH_KELUARGA = fsum(`JUMLAH KELUARGA`, na.rm = TRUE),
        JUMLAH_KELUARGA_SASARAN = fsum(`JUMLAH KELUARGA SASARAN`, na.rm = TRUE),
        JUMLAH_KRS = fsum(`JUMLAH KRS`, na.rm = TRUE)
      )
  })
  
  # Efficient output functions - use shared filtered data
  output$jumlah_keluarga <- renderText({
    paste0(format(filtered_data_krs()$JUMLAH_KELUARGA, big.mark = ".", scientific = FALSE))
    
  })
  
  output$jumlah_keluarga_sasaran <- renderText({
    paste0(format(filtered_data_krs()$JUMLAH_KELUARGA_SASARAN, big.mark = ".", scientific = FALSE))
  })
  
  output$jumlah_krs_menu <- renderText({
    paste0(format(filtered_data_krs()$JUMLAH_KRS, big.mark = ".", scientific = FALSE))
  })
  ##
  
  # SIPACOAI
  # gu input
  observeEvent(input$pilih_kab_sipacoai, {
    if (input$pilih_kab_sipacoai == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec_sipacoai",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN == input$pilih_kab_sipacoai) |>
        fselect(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec_sipacoai",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec_sipacoai, {
    if (input$pilih_kec_sipacoai == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel_sipacoai",
                        choices = c("SEMUA DESA/KEL"))
    } else {
      daftar_kel = data_nama_desa |>
        fselect(KECAMATAN, KELURAHAN) |>
        fsubset(KECAMATAN == input$pilih_kec_sipacoai) |>
        fselect(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel_sipacoai",
                        choices = c("SEMUA DESA/KEL", 
                                    daftar_kel))
    }
  })
  
  ## batas gu input
  
  ## gu filter
  # filter kab
  value_filter_kab_sipacoai <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_sipacoai, {
    kondisi_input = input$pilih_kab_sipacoai
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_nama_desa$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab_sipacoai
    }
    value_filter_kab_sipacoai(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec_sipacoai <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_sipacoai, {
    kondisi_input = input$pilih_kec_sipacoai
    filter_kabupaten = value_filter_kab_sipacoai()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN %in% filter_kabupaten) |>
        fselect(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec_sipacoai
    }
    value_filter_kec_sipacoai(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel_sipacoai <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_sipacoai, {
    kondisi_input = input$pilih_desa_kel_sipacoai
    filter_kabupaten = value_filter_kab_sipacoai()
    filter_kecamatan = value_filter_kec_sipacoai()
    
    if (kondisi_input == "SEMUA DESA/KEL"){
      daftar_kel = data_nama_desa |>
        fselect(KABUPATEN, KECAMATAN, KELURAHAN) |>
        fsubset(
          KABUPATEN %in% filter_kabupaten) |>
        fsubset(KECAMATAN %in% filter_kecamatan) |>
        fselect(KELURAHAN)
      filter_desa_kel = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel = input$pilih_desa_kel_sipacoai
    }
    value_filter_desa_kel_sipacoai(filter_desa_kel) 
  })
  
  # bulan
  value_filter_bulan_sipacoai <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_sipacoai, {
    value_filter_bulan(input$pilih_bulan_sipacoai) 
  })
  
  ## batas gu filter
  
  ## gu judul
  values_sipacoai <- reactiveValues(default = 0)
  
  observeEvent(input$cari_sipacoai,{
    values_sipacoai$default <- input$cari_sipacoai
  })
  
  teks_judul_rekap_sipacoai <- eventReactive(input$cari_sipacoai, {
    if(input$pilih_kab_sipacoai == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec_sipacoai == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab_sipacoai
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel_sipacoai == "SEMUA DESA/KEL"){
      nama_daerah = input$pilih_kec_sipacoai
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel_sipacoai()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste(tingkat_daerah, nama_daerah, "-", input$pilih_bulan_sipacoai)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
  })
  
  output$tes_input_rekap_sipacoai <- renderText({
    if(values_sipacoai$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap_sipacoai()
    }
  })
  
  ## batas gu judul
  data_bkb_keluarga <- fread("data/data_bkb_keluarga.csv")
  output$jumlah_pendampingan_keluarga <- renderText({
    # React to action button
    req(input$cari_sipacoai)
    
    filter_kabupaten <- value_filter_kab_sipacoai()
    filter_kecamatan <- value_filter_kec_sipacoai() 
    filter_desa <- value_filter_desa_kel_sipacoai()
    filter_bulan <- input$pilih_bulan_sipacoai
    
    # --- Agregasi BKB ---
    # Filter and aggregate BKB data
    bkb_filtered <- fsubset(data_bkb_keluarga, 
                            KABUPATEN %in% filter_kabupaten &
                              KECAMATAN %in% filter_kecamatan &
                              KELURAHAN %in% filter_desa &
                              BULAN %in% filter_bulan)
    
    bkb_hadir <- fgroup_by(bkb_filtered, PROVINSI) %>%
      fsummarise(Keluarga_Hadir_BKB = fsum(`JUMLAH KELUARGA ANGGOTA BKB HADIR PERTEMUAN`, na.rm = TRUE))
    
    # --- Agregasi BKR ---
    # Note: Assuming data_bkr exists - you'll need to load it
    bkr_filtered <- fsubset(data_bkr,
                           KABUPATEN %in% filter_kabupaten &
                           KECAMATAN %in% filter_kecamatan &
                           KELURAHAN %in% filter_desa &
                           BULAN %in% filter_bulan)

    bkr_hadir <- fgroup_by(bkr_filtered, PROVINSI) %>%
      fsummarise(Keluarga_Hadir_BKR = fsum(`JUMLAH KELUARGA ANGGOTA BKR HADIR PERTEMUAN`, na.rm = TRUE))
    
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
      fsummarise(Keluarga_Hadir_BKL = fsum(`JUMLAH ANGGOTA KELUARGA HADIR`, na.rm = TRUE))
    
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
      fsummarise(Jumlah_PUS = fsum(PUS, na.rm = TRUE))
    
    # For demonstration, creating dummy PUS data
    # pus_total <- data.frame(
    #   PROVINSI = unique(bkb_hadir$PROVINSI),
    #   Jumlah_PUS = 100 # dummy value
    # )
    
    # Gabungkan semua data
    df_gabungan <- merge(bkb_hadir, bkr_hadir, by = "PROVINSI", all = TRUE)
    df_gabungan <- merge(df_gabungan, bkl_hadir, by = "PROVINSI", all = TRUE)
    df_gabungan <- merge(df_gabungan, pus_total, by = "PROVINSI", all = TRUE)
    
    # Isi nilai NA dengan 0
    df_gabungan[is.na(df_gabungan)] <- 0
    
    # Terapkan rumus: ((BKB + BKR + BKL) / 3) / PUS
    df_gabungan$Pembilang <- (df_gabungan$Keluarga_Hadir_BKB + 
                                df_gabungan$Keluarga_Hadir_BKR + 
                                df_gabungan$Keluarga_Hadir_BKL) / 3
    
    # Pembagian akhir dengan penanganan pembagian dengan nol
    df_gabungan$Jumlah_Hasil_Akhir <- ifelse(df_gabungan$Jumlah_PUS > 0,
                                             df_gabungan$Pembilang / df_gabungan$Jumlah_PUS,
                                             0)
    
    # Ambil hasil akhir
    hasil_akhir <- df_gabungan$Jumlah_Hasil_Akhir[1]
    
    # Format hasil sebagai persentase
    hasil_persen <- round(hasil_akhir * 100, 2)
    hasil_formatted <- gsub("\\.", ",", format(hasil_persen, nsmall = 2))
    
    paste0(hasil_formatted, "%")

  }) 
  
  # Reactive function untuk jumlah penggunaan KKA
  output$jumlah_penggunaan_kka <- renderText({
    # Trigger reactive event berdasarkan action button
    input$cari_sipacoai
    
    filter_kabupaten <- value_filter_kab_sipacoai()
    filter_kecamatan <- value_filter_kec_sipacoai() 
    filter_desa <- value_filter_desa_kel_sipacoai()
    filter_bulan <- input$pilih_bulan_sipacoai
    
    result <- data_bkb_keluarga |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa &
          BULAN %in% filter_bulan
      ) |>
      fgroup_by(PROVINSI) |>
      fsummarise(KKA = fsum(KKA, na.rm = TRUE)) |>
      fselect(KKA) |>
      unlist() |>
      as.numeric()
    paste0(format(result, big.mark = ".", scientific = FALSE))
  })
  
  # Load data untuk edukasi KBPP
  edukasi_kbpp_bumil <- fread("data/2025-elsimil-bumil.csv", sep = ";")
  edukasi_kbpp_pascasalin <- fread("data/2025-elsimil-pascasalin.csv", sep = ";")
  
  # Reactive function untuk jumlah edukasi KBPP
  output$jumlah_edukasi_kbpp <- renderText({
    # Trigger reactive event berdasarkan action button
    input$cari_sipacoai
    
    filter_kabupaten <- value_filter_kab_sipacoai()
    filter_kecamatan <- value_filter_kec_sipacoai() 
    filter_desa <- value_filter_desa_kel_sipacoai()
    filter_bulan <- input$pilih_bulan_sipacoai
    
    # Filter dan aggregate data bumil
    bumil_agg <- edukasi_kbpp_bumil |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa
      ) |>
      fgroup_by(PROVINSI) |>
      fsummarise(
        JUMLAH_BUMIL = fsum(`JUMLAH BUMIL`, na.rm = TRUE),
        KIE_PERSEORANGAN_BUMIL = fsum(`KIE PERSEORANGAN`, na.rm = TRUE),
        KIE_KELOMPOK_BUMIL = fsum(`KIE KELOMPOK`, na.rm = TRUE),
        KIE_PERSEORANGAN_DAN_KELOMPOK_BUMIL = fsum(`KIE PERSEORANGAN DAN KELOMPOK`, na.rm = TRUE)
      )
    
    # Filter dan aggregate data pascasalin
    pascasalin_agg <- edukasi_kbpp_pascasalin |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa
      ) |>
      fgroup_by(PROVINSI) |>
      fsummarise(
        JUMLAH_PASCASALIN = fsum(`JUMLAH PASCASALIN`, na.rm = TRUE),
        KIE_PERSEORANGAN_PASCASALIN = fsum(`KIE PERSEORANGAN`, na.rm = TRUE),
        KIE_KELOMPOK_PASCASALIN = fsum(`KIE KELOMPOK`, na.rm = TRUE),
        KIE_PERSEORANGAN_DAN_KELOMPOK_PASCASALIN = fsum(`KIE PERSEORANGAN DAN KELOMPOK`, na.rm = TRUE)
      )
    
    # Join data dan hitung persentase edukasi KBPP
    edukasi_kbpp <- join(bumil_agg, pascasalin_agg, on = "PROVINSI", how = "left") |>
      ftransform(
        EDUKASI_KBPP = ((KIE_PERSEORANGAN_BUMIL + KIE_PERSEORANGAN_PASCASALIN + 
                           KIE_KELOMPOK_BUMIL + KIE_KELOMPOK_PASCASALIN + 
                           KIE_PERSEORANGAN_DAN_KELOMPOK_BUMIL + KIE_PERSEORANGAN_DAN_KELOMPOK_PASCASALIN) / 
                          (JUMLAH_BUMIL + JUMLAH_PASCASALIN)) * 100
      ) |>
      fselect(EDUKASI_KBPP) |>
      unlist() |>
      as.numeric()
    
    paste0(format(round(edukasi_kbpp, 2), decimal.mark = ","), "%")
  })
  
  # Reactive function untuk jumlah PIKR
  output$jumlah_pikr_sipacoai <- renderText({
    # Trigger reactive event berdasarkan action button
    req(input$cari_sipacoai)
    
    filter_kabupaten <- value_filter_kab_sipacoai()
    filter_kecamatan <- value_filter_kec_sipacoai() 
    filter_desa <- value_filter_desa_kel_sipacoai()
    filter_bulan <- input$pilih_bulan_sipacoai
    
    result <- data_pikr |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa &
          BULAN %in% filter_bulan
      ) |>
      fgroup_by(PROVINSI) |>
      fsummarise(JUMLAH_REMAJA = fsum(`JUMLAH REMAJA HADIR DALAM PERTEMUAN`, na.rm = TRUE)) |>
      fselect(JUMLAH_REMAJA) |>
      unlist() |>
      as.numeric()
    
    paste0(format(result, big.mark = ".", scientific = FALSE))
  })
  
  # Reactive function untuk jumlah MKJP
  output$jumlah_mkjp_sipacoai <- renderText({
    # Trigger reactive event berdasarkan action button
    input$caro_sipacoai
    
      filter_kabupaten <- value_filter_kab_sipacoai()
      filter_kecamatan <- value_filter_kec_sipacoai() 
      filter_desa <- value_filter_desa_kel_sipacoai()
      filter_bulan <- input$pilih_bulan_sipacoai
      
      result <- data_mix |>
        fsubset(
          KABUPATEN %in% filter_kabupaten &
            KECAMATAN %in% filter_kecamatan &
            KELURAHAN %in% filter_desa &
            BULAN %in% filter_bulan
        ) |>
        fgroup_by(PROVINSI) |>
        fsummarise(
          IMPLAN = fsum(IMPLAN, na.rm = TRUE),
          IUD = fsum(IUD, na.rm = TRUE),
          VASEKTOMI = fsum(VASEKTOMI, na.rm = TRUE),
          TUBEKTOMI = fsum(TUBEKTOMI, na.rm = TRUE),
          KB_MODERN = fsum(`KB MODERN`, na.rm = TRUE)
        ) |>
        ftransform(MKJP = IUD + IMPLAN + VASEKTOMI + TUBEKTOMI) |>
        fselect(MKJP) |>
        unlist() |>
        as.numeric()
    
      paste0(format(result, big.mark = ".", scientific = FALSE))
  })
  
  # Reactive function untuk peringkat kesejahteraan
  output$peringkat_kesejahteraan <- renderEcharts4r({
    # Trigger reactive event berdasarkan action button
    req(input$cari)

    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec() 
    filter_desa <- value_filter_desa_kel()
    
    # Filter dan aggregate data
    data_kesejahteraan <- data_krs_verval |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa
      ) |>
      fgroup_by(PROVINSI) |>
      fsummarise(
        `KESEJAHTERAAN 1` = fsum(`KESEJAHTERAAN 1`, na.rm = TRUE),
        `KESEJAHTERAAN 2` = fsum(`KESEJAHTERAAN 2`, na.rm = TRUE),
        `KESEJAHTERAAN 3` = fsum(`KESEJAHTERAAN 3`, na.rm = TRUE),
        `KESEJAHTERAAN 4` = fsum(`KESEJAHTERAAN 4`, na.rm = TRUE),
        `KESEJAHTERAAN > 4` = fsum(`KESEJAHTERAAN > 4`, na.rm = TRUE)
      )
    
    # Reshape data dari wide ke long format menggunakan base R
    kesejahteraan_cols <- c("KESEJAHTERAAN 1", "KESEJAHTERAAN 2", "KESEJAHTERAAN 3", 
                            "KESEJAHTERAAN 4", "KESEJAHTERAAN > 4")
    
    # Manual reshape ke long format
    data_long <- data.frame(
      PROVINSI = rep(data_kesejahteraan$PROVINSI, length(kesejahteraan_cols)),
      TINGKAT_KESEJAHTERAAN = rep(kesejahteraan_cols, each = nrow(data_kesejahteraan)),
      JUMLAH = c(data_kesejahteraan$`KESEJAHTERAAN 1`,
                 data_kesejahteraan$`KESEJAHTERAAN 2`,
                 data_kesejahteraan$`KESEJAHTERAAN 3`,
                 data_kesejahteraan$`KESEJAHTERAAN 4`,
                 data_kesejahteraan$`KESEJAHTERAAN > 4`)
    )
    
    # Custom order untuk tingkat kesejahteraan
    custom_order <- c("KESEJAHTERAAN 1", "KESEJAHTERAAN 2", "KESEJAHTERAAN 3", 
                      "KESEJAHTERAAN 4", "KESEJAHTERAAN > 4")
    
    # Hitung total jumlah
    total_jumlah <- sum(data_long$JUMLAH, na.rm = TRUE)
    
    # Tambahkan kolom persentase dan formatting
    data_long$PERSENTASE <- round((data_long$JUMLAH / total_jumlah) * 100, 2)
    data_long$JUMLAH_FORMATTED <- format(data_long$JUMLAH, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    data_long$PERSENTASE_FORMAT <- paste0(format(data_long$PERSENTASE, decimal.mark = ","), "%")
    data_long$TINGKAT_KESEJAHTERAAN <- factor(data_long$TINGKAT_KESEJAHTERAAN, levels = custom_order)
    
    # Order data sesuai factor levels
    data_long <- data_long[order(data_long$TINGKAT_KESEJAHTERAAN), ]
    
    # Warna untuk setiap tingkat kesejahteraan
    colors <- c("#FF4500", "#FFA500", "#FFD700", "#3CB371", "#2E8B57")
    
    # Buat chart dengan echarts4r
    chart <- data_long |>
      e_charts(TINGKAT_KESEJAHTERAAN) |>
      e_bar(JUMLAH, name = "Jumlah") |>
      e_flip_coords() |>  # Membuat horizontal bar
      e_color(colors) |>
      e_tooltip(
        trigger = "item"
      ) |>
      e_title("Perbandingan Tingkat Kesejahteraan KRS") |>
      e_legend(show = FALSE) |>
      e_x_axis(
        name = "Jumlah Pengguna",
        nameLocation = "middle",
        nameGap = 30,
        splitLine = list(show = FALSE)
      ) |>
      e_y_axis(
        name = "",
        nameLocation = "middle",
        nameGap = 50,
        splitLine = list(show = TRUE)
      ) |>
      e_grid(
        left = "20%",
        right = "10%",
        top = "15%",
        bottom = "15%"
      ) |>
      e_labels(
        show = TRUE,
        position = "right"
      )
    
    chart
  })
  
  # Reactive function untuk faktor KRS
  output$faktor_krs <- renderEcharts4r({
    # Trigger reactive event berdasarkan action button
    req(input$cari)
    
    isolate({
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec() 
      filter_desa <- value_filter_desa_kel()
      
      # Filter dan aggregate data
      faktor_krs <- data_krs_verval |>
        fsubset(
          KABUPATEN %in% filter_kabupaten &
            KECAMATAN %in% filter_kecamatan &
            KELURAHAN %in% filter_desa
        ) |>
        fgroup_by(PROVINSI) |>
        fsummarise(
          `SUMBER AIR MINUM TIDAK LAYAK` = fsum(`SUMBER AIR MINUM TIDAK LAYAK`, na.rm = TRUE),
          `JAMBAN TIDAK LAYAK` = fsum(`JAMBAN TIDAK LAYAK`, na.rm = TRUE),
          `TERLALU MUDA` = fsum(`TERLALU MUDA`, na.rm = TRUE),
          `TERLALU TUA` = fsum(`TERLALU TUA`, na.rm = TRUE),
          `TERLALU DEKAT` = fsum(`TERLALU DEKAT`, na.rm = TRUE),
          `TERLALU BANYAK` = fsum(`TERLALU BANYAK`, na.rm = TRUE),
          `BUKAN PESERTA KB MODERN` = fsum(`BUKAN PESERTA KB MODERN`, na.rm = TRUE)
        )
      
      # Reshape data dari wide ke long format menggunakan base R
      faktor_cols <- c("SUMBER AIR MINUM TIDAK LAYAK", "JAMBAN TIDAK LAYAK", "TERLALU MUDA", 
                       "TERLALU TUA", "TERLALU DEKAT", "TERLALU BANYAK", "BUKAN PESERTA KB MODERN")
      
      # Manual reshape ke long format
      data_long <- data.frame(
        PROVINSI = rep(faktor_krs$PROVINSI, length(faktor_cols)),
        FAKTOR_KRS = rep(faktor_cols, each = nrow(faktor_krs)),
        JUMLAH = c(faktor_krs$`SUMBER AIR MINUM TIDAK LAYAK`,
                   faktor_krs$`JAMBAN TIDAK LAYAK`,
                   faktor_krs$`TERLALU MUDA`,
                   faktor_krs$`TERLALU TUA`,
                   faktor_krs$`TERLALU DEKAT`,
                   faktor_krs$`TERLALU BANYAK`,
                   faktor_krs$`BUKAN PESERTA KB MODERN`)
      )
      
      # Format angka dengan pemisah titik
      data_long$JUMLAH_FORMATTED <- format(data_long$JUMLAH, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      
      # Urutkan data berdasarkan jumlah (terbesar ke terkecil)
      data_long <- data_long[order(data_long$JUMLAH), ]
      
      # Buat urutan factor berdasarkan jumlah
      data_long$FAKTOR_KRS <- factor(data_long$FAKTOR_KRS, levels = unique(data_long$FAKTOR_KRS))
      
      # Warna biru (sesuaikan dengan warna_biru dari Python)
      warna_biru <- "#1f77b4"  # Default blue color, sesuaikan jika perlu
      
      # Buat chart dengan echarts4r
      chart <- data_long |>
        e_charts(FAKTOR_KRS) |>
        e_bar(JUMLAH, name = "Jumlah") |>
        e_flip_coords() |>  # Membuat horizontal bar
        e_color(warna_biru) |>
        e_tooltip(
          trigger = "item"
        ) |>
        e_title("Perbandingan Faktor KRS") |>
        e_legend(show = FALSE) |>
        e_x_axis(
          name = "Jumlah",
          nameLocation = "middle",
          nameGap = 30,
          splitLine = list(show = FALSE),
          axisLabel = list(
            interval = "auto"
          )
        ) |>
        e_y_axis(
          name = "",
          nameLocation = "middle",
          nameGap = 80,
          splitLine = list(show = FALSE),
          axisLabel = list(
            fontSize = 12
          )
        ) |>
        e_grid(
          left = "25%",
          right = "15%",
          top = "15%",
          bottom = "15%"
        ) |>
        e_labels(
          show = TRUE,
          position = "right"
        ) |>
        e_theme("default")
      
      return(chart)
    })
  })
  
  # Reactive function untuk pie chart punya baduta
  output$pie_punya_baduta <- renderEcharts4r({
    # Trigger reactive event berdasarkan action button
    req(input$cari)
    
    isolate({
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec() 
      filter_desa <- value_filter_desa_kel()
      
      # Conditional logic berdasarkan pilihan filter
      if (is.null(input$pilih_kab) || input$pilih_kab == "SEMUA KABUPATEN") {
        data_kel_baduta <- data_krs_verval |>
          fgroup_by(KABUPATEN) |>
          fsummarise(PUNYA_BADUTA = fsum(`PUNYA BADUTA`, na.rm = TRUE)) |>
          frename(KABUPATEN = "KATEGORI")
        
        subtitle_pie <- "Berdasarkan Kabupaten"
        atur_radius <- 100
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 (is.null(input$pilih_kec) || input$pilih_kec == "SEMUA KECAMATAN")) {
        data_kel_baduta <- data_krs_verval |>
          ftransform(
            KATEGORI = ifelse(KABUPATEN == input$pilih_kab, input$pilih_kab, "LAINNYA")
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BADUTA = fsum(`PUNYA BADUTA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kab, "dan Kabupaten Lainnya")
        atur_radius <- 150
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 !is.null(input$pilih_kec) && input$pilih_kec != "SEMUA KECAMATAN" && 
                 (is.null(input$pilih_desa) || input$pilih_desa == "SEMUA DESA/KELURAHAN")) {
        data_kel_baduta <- data_krs_verval |>
          fsubset(KABUPATEN %in% filter_kabupaten) |>
          ftransform(
            KATEGORI = ifelse(is.na(KECAMATAN) | is.null(KECAMATAN), "LAINNYA",
                              ifelse(KECAMATAN == input$pilih_kec, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BADUTA = fsum(`PUNYA BADUTA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kec, "dan Kec Lainnya di Kab.", input$pilih_kab)
        atur_radius <- 150
        
      } else {
        data_kel_baduta <- data_krs_verval |>
          fsubset(
            KABUPATEN %in% filter_kabupaten &
              KECAMATAN %in% filter_kecamatan
          ) |>
          ftransform(
            KATEGORI = ifelse(is.na(KELURAHAN) | is.null(KELURAHAN), "LAINNYA",
                              ifelse(KELURAHAN == input$pilih_desa, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BADUTA = fsum(`PUNYA BADUTA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_desa, "dan Desa/Kelurahan Lainnya di Kec.", input$pilih_kec)
        atur_radius <- 150
      }
      
      
      # Hitung persentase
      total_baduta <- sum(data_kel_baduta$PUNYA_BADUTA, na.rm = TRUE)
      data_kel_baduta$PERSENTASE <- round((data_kel_baduta$PUNYA_BADUTA / total_baduta) * 100, 2)
      
      # Format persentase dan angka
      data_kel_baduta$PERSENTASE_FORMAT <- paste0(format(data_kel_baduta$PERSENTASE, decimal.mark = ","), "%")
      data_kel_baduta$PUNYA_BADUTA_FORMATTED <- format(data_kel_baduta$PUNYA_BADUTA, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      
      # Buat label gabungan
      data_kel_baduta$LABEL <- paste(data_kel_baduta$KATEGORI, data_kel_baduta$PERSENTASE_FORMAT, sep = "\n")
      
      # Urutkan berdasarkan persentase
      data_kel_baduta <- data_kel_baduta[order(data_kel_baduta$PERSENTASE), ]
      
      # Definisi warna (sesuaikan dengan warna_biru dan warna_kuning dari Python)
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      
      # Buat pie chart dengan echarts4r
      chart <- data_kel_baduta |>
        e_charts(KATEGORI) |>
        e_pie(
          PUNYA_BADUTA, 
          name = "Jumlah",
          radius = c("0%", "70%"),
          center = c("50%", "50%"),
          label = list(
            show = TRUE,
            position = "outside",
            formatter = htmlwidgets::JS("
            function(params) {
              return params.name + '\\n' + params.percent + '%';
            }
          ")
          ),
          labelLine = list(show = TRUE)
        ) |>
        e_color(colors) |>
        e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
          function(params) {
            var value = params.value.toLocaleString('id-ID');
            return '<strong>' + params.name + '</strong><br/>' +
                   'Jumlah: ' + value + '<br/>' +
                   'Persentase: ' + params.percent + '%';
          }
        ")
        ) |>
        e_title(
          text = "Perbandingan Keluarga Memiliki Baduta",
          subtext = subtitle_pie,
          left = "center",
          textStyle = list(fontSize = 16),
          subtextStyle = list(fontSize = 12)
        ) |>
        e_legend(
          show = FALSE
        ) |>
        e_theme("default")
      
      return(chart)
    })
  })
  
  # Reactive function untuk pie chart punya balita
  output$pie_punya_balita <- renderEcharts4r({
    # Trigger reactive event berdasarkan action button
    req(input$cari)
    
    isolate({
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec() 
      filter_desa <- value_filter_desa_kel()
      
      # Conditional logic berdasarkan pilihan filter
      if (is.null(input$pilih_kab) || input$pilih_kab == "SEMUA KABUPATEN") {
        data_kel_balita <- data_krs_verval |>
          fgroup_by(KABUPATEN) |>
          fsummarise(PUNYA_BALITA = fsum(`PUNYA BALITA`, na.rm = TRUE)) |>
          frename(KABUPATEN = "KATEGORI")
        
        subtitle_pie <- "Berdasarkan Kabupaten"
        atur_radius <- 100
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 (is.null(input$pilih_kec) || input$pilih_kec == "SEMUA KECAMATAN")) {
        data_kel_balita <- data_krs_verval |>
          ftransform(
            KATEGORI = ifelse(KABUPATEN == input$pilih_kab, input$pilih_kab, "LAINNYA")
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BALITA = fsum(`PUNYA BALITA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kab, "dan Kabupaten Lainnya")
        atur_radius <- 150
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 !is.null(input$pilih_kec) && input$pilih_kec != "SEMUA KECAMATAN" && 
                 (is.null(input$pilih_desa) || input$pilih_desa == "SEMUA DESA/KELURAHAN")) {
        data_kel_balita <- data_krs_verval |>
          fsubset(KABUPATEN %in% filter_kabupaten) |>
          ftransform(
            KATEGORI = ifelse(is.na(KECAMATAN) | is.null(KECAMATAN), "LAINNYA",
                              ifelse(KECAMATAN == input$pilih_kec, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BALITA = fsum(`PUNYA BALITA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kec, "dan Kec Lainnya di Kab.", input$pilih_kab)
        atur_radius <- 150
        
      } else {
        data_kel_balita <- data_krs_verval |>
          fsubset(
            KABUPATEN %in% filter_kabupaten &
              KECAMATAN %in% filter_kecamatan
          ) |>
          ftransform(
            KATEGORI = ifelse(is.na(KELURAHAN) | is.null(KELURAHAN), "LAINNYA",
                              ifelse(KELURAHAN == input$pilih_desa, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUNYA_BALITA = fsum(`PUNYA BALITA`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_desa, "dan Desa/Kelurahan Lainnya di Kec.", input$pilih_kec)
        atur_radius <- 150
      }
      
      
      # Hitung persentase
      total_balita <- sum(data_kel_balita$PUNYA_BALITA, na.rm = TRUE)
      data_kel_balita$PERSENTASE <- round((data_kel_balita$PUNYA_BALITA / total_balita) * 100, 2)
      
      # Format persentase dan angka
      data_kel_balita$PERSENTASE_FORMAT <- paste0(format(data_kel_balita$PERSENTASE, decimal.mark = ","), "%")
      data_kel_balita$PUNYA_BALITA_FORMATTED <- format(data_kel_balita$PUNYA_BALITA, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      
      # Buat label gabungan
      data_kel_balita$LABEL <- paste(data_kel_balita$KATEGORI, data_kel_balita$PERSENTASE_FORMAT, sep = "\n")
      
      # Urutkan berdasarkan persentase
      data_kel_balita <- data_kel_balita[order(data_kel_balita$PERSENTASE), ]
      
      # Definisi warna (sesuaikan dengan warna_biru dan warna_kuning dari Python)
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      
      # Buat pie chart dengan echarts4r
      chart <- data_kel_balita |>
        e_charts(KATEGORI) |>
        e_pie(
          PUNYA_BALITA, 
          name = "Jumlah",
          radius = c("0%", "70%"),
          center = c("50%", "50%"),
          label = list(
            show = TRUE,
            position = "outside",
            formatter = htmlwidgets::JS("
            function(params) {
              return params.name + '\\n' + params.percent + '%';
            }
          ")
          ),
          labelLine = list(show = TRUE)
        ) |>
        e_color(colors) |>
        e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
          function(params) {
            var value = params.value.toLocaleString('id-ID');
            return '<strong>' + params.name + '</strong><br/>' +
                   'Jumlah: ' + value + '<br/>' +
                   'Persentase: ' + params.percent + '%';
          }
        ")
        ) |>
        e_title(
          text = "Perbandingan Keluarga Memiliki Baduta",
          subtext = subtitle_pie,
          left = "center",
          textStyle = list(fontSize = 16),
          subtextStyle = list(fontSize = 12)
        ) |>
        e_legend(
          show = FALSE
        ) |>
        e_theme("default")
      
      return(chart)
    })
  })
  
  #hamil
  # Reactive function untuk pie chart punya balita
  output$pie_pus_hamil <- renderEcharts4r({
    # Trigger reactive event berdasarkan action button
    req(input$cari)
    
    isolate({
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec() 
      filter_desa <- value_filter_desa_kel()
      
      # Conditional logic berdasarkan pilihan filter
      if (is.null(input$pilih_kab) || input$pilih_kab == "SEMUA KABUPATEN") {
        data_kel_balita <- data_krs_verval |>
          fgroup_by(KABUPATEN) |>
          fsummarise(PUS_HAMIL = fsum(`PUS HAMIL`, na.rm = TRUE)) |>
          frename(KABUPATEN = "KATEGORI")
        
        subtitle_pie <- "Berdasarkan Kabupaten"
        atur_radius <- 100
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 (is.null(input$pilih_kec) || input$pilih_kec == "SEMUA KECAMATAN")) {
        data_kel_balita <- data_krs_verval |>
          ftransform(
            KATEGORI = ifelse(KABUPATEN == input$pilih_kab, input$pilih_kab, "LAINNYA")
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUS_HAMIL = fsum(`PUS HAMIL`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kab, "dan Kabupaten Lainnya")
        atur_radius <- 150
        
      } else if (!is.null(input$pilih_kab) && input$pilih_kab != "SEMUA KABUPATEN" && 
                 !is.null(input$pilih_kec) && input$pilih_kec != "SEMUA KECAMATAN" && 
                 (is.null(input$pilih_desa) || input$pilih_desa == "SEMUA DESA/KELURAHAN")) {
        data_kel_balita <- data_krs_verval |>
          fsubset(KABUPATEN %in% filter_kabupaten) |>
          ftransform(
            KATEGORI = ifelse(is.na(KECAMATAN) | is.null(KECAMATAN), "LAINNYA",
                              ifelse(KECAMATAN == input$pilih_kec, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUS_HAMIL = fsum(`PUS HAMIL`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_kec, "dan Kec Lainnya di Kab.", input$pilih_kab)
        atur_radius <- 150
        
      } else {
        data_kel_balita <- data_krs_verval |>
          fsubset(
            KABUPATEN %in% filter_kabupaten &
              KECAMATAN %in% filter_kecamatan
          ) |>
          ftransform(
            KATEGORI = ifelse(is.na(KELURAHAN) | is.null(KELURAHAN), "LAINNYA",
                              ifelse(KELURAHAN == input$pilih_desa, input$pilih_kec, "LAINNYA"))
          ) |>
          fgroup_by(KATEGORI) |>
          fsummarise(PUS_HAMIL = fsum(`PUS HAMIL`, na.rm = TRUE))
        
        subtitle_pie <- paste(input$pilih_desa, "dan Desa/Kelurahan Lainnya di Kec.", input$pilih_kec)
        atur_radius <- 150
      }
      
      
      # Hitung persentase
      total_balita <- sum(data_kel_balita$PUS_HAMIL, na.rm = TRUE)
      data_kel_balita$PERSENTASE <- round((data_kel_balita$PUS_HAMIL / total_balita) * 100, 2)
      
      # Format persentase dan angka
      data_kel_balita$PERSENTASE_FORMAT <- paste0(format(data_kel_balita$PERSENTASE, decimal.mark = ","), "%")
      data_kel_balita$PUS_HAMIL_FORMATTED <- format(data_kel_balita$PUS_HAMIL, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      
      # Buat label gabungan
      data_kel_balita$LABEL <- paste(data_kel_balita$KATEGORI, data_kel_balita$PERSENTASE_FORMAT, sep = "\n")
      
      # Urutkan berdasarkan persentase
      data_kel_balita <- data_kel_balita[order(data_kel_balita$PERSENTASE), ]
      
      # Definisi warna (sesuaikan dengan warna_biru dan warna_kuning dari Python)
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      
      # Buat pie chart dengan echarts4r
      chart <- data_kel_balita |>
        e_charts(KATEGORI) |>
        e_pie(
          PUS_HAMIL, 
          name = "Jumlah",
          radius = c("0%", "70%"),
          center = c("50%", "50%"),
          label = list(
            show = TRUE,
            position = "outside",
            formatter = htmlwidgets::JS("
            function(params) {
              return params.name + '\\n' + params.percent + '%';
            }
          ")
          ),
          labelLine = list(show = TRUE)
        ) |>
        e_color(colors) |>
        e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
          function(params) {
            var value = params.value.toLocaleString('id-ID');
            return '<strong>' + params.name + '</strong><br/>' +
                   'Jumlah: ' + value + '<br/>' +
                   'Persentase: ' + params.percent + '%';
          }
        ")
        ) |>
        e_title(
          text = "Perbandingan Keluarga Memiliki Baduta",
          subtext = subtitle_pie,
          left = "center",
          textStyle = list(fontSize = 16),
          subtextStyle = list(fontSize = 12)
        ) |>
        e_legend(
          show = FALSE
        ) |>
        e_theme("default")
      
      return(chart)
    })
  })
  

  
  #SIPACOAI

}

# Run the app
shinyApp(ui = ui, server = server)
