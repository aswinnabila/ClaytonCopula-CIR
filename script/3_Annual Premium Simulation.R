# Fungsi Perhitungan mortalitas gabungan t_qxy 
## t menyatakan waktu pertanggungan; x dan y menyatakan usia tertanggung
hitung_t_qxy <- function(qxy_df, usia_x_start, n) {
  t_qxy <- numeric(n + 1)  
  t_qxy[1] <- 0
  
  for (t in 1:n) {
    qxy_filtered <- qxy_df[qxy_df$'usia_x' >= usia_x_start & qxy_df$'usia_x' < (usia_x_start + t), ]
    
    if (nrow(qxy_filtered) < t) {
      stop("Data tidak cukup untuk menghitung t_qxy dengan t yang diberikan.")
    }
    t_qxy[t + 1] <- 1 - prod(1 - qxy_filtered$qxy)
  }
  
  return(t_qxy)
}

## Skema 2 - Copula
t_qxy_2C <- hitung_t_qxy(qxy_copula2, 55, 19)
t <- seq(0, 19, by = 1)
tqxy_2C <- data.frame(t, t_qxy = t_qxy_2C )
print(tqxy_2C)

## Skema 2 - Bebas
t_qxy_2B <- hitung_t_qxy(qxy_bebas2, 55, 19)
tqxy_2B <- data.frame(t, t_qxy = t_qxy_2B )
print(tqxy_2B)

## Skema 3 - Copula
t_qxy_3C <- hitung_t_qxy(qxy_copula3, 55, 19)
tqxy_3C <- data.frame(t, t_qxy = t_qxy_3C )
print(tqxy_3C)

## Skema 3 - Bebas
t_qxy_3B <- hitung_t_qxy(qxy_bebas3, 55, 19)
tqxy_3B <- data.frame(t, t_qxy = t_qxy_3B )
print(tqxy_3B)

# PERHITUNGAN PREMI
# Fungsi Perhitungan Premi Besih Tahunan
fungsi_premi_bersih_tahunan <- function(t_qxy, Vt, n, santunan) {
  # Menghitung Anuitas Awal (a_dot)
  if (length(Vt) < n || length(t_qxy) < n) {
    stop("Panjang Vt atau t_qxy tidak mencukupi untuk perhitungan anuitas.")
  }
  a_dot <- 1 + sum(Vt[1:(n - 1)] * (1 - t_qxy[2:n]))
  
  # Menghitung Nilai Tunai Aktuaria
  nilai_tunai <- 0
  for (t in 1:n) {
    nilai_tunai <- nilai_tunai + Vt[t] * (t_qxy[t + 1] - t_qxy[t])
  }
  
  # Menghitung Premi Bersih Tahunan
  premi_bersih <- santunan * (nilai_tunai / a_dot)
  
  # Mencetak Hasil
  cat("Ringkasan Hasil:", "\n",
      skema, "\n",
      asumsi, "\n",
      "-----------------------------------------\n",
      "Anuitas Awal:", a_dot, "\n",
      "Nilai Tunai Aktuaria:", nilai_tunai, "\n",
      "Premi Bersih Tahunan: Rp", premi_bersih, "\n")
  
  return(premi_bersih)
}

## Skema 2 - Copula
skema <- "Skema 2: Usia Suami Lebih Tua dari Istri"
asumsi <- "Asumsi Dependensi Copula Clayton"
premi_bersih_2C <- fungsi_premi_bersih_tahunan(t_qxy_2C, faktor_diskonto$'Vt', 19, 100000000)

## Skema 2 - Bebas
skema <- "Skema 2: Usia Suami Lebih Tua dari Istri"
asumsi <- "Asumsi Kebebasan Mortalitas"
premi_bersih_2B <- fungsi_premi_bersih_tahunan(t_qxy_2B, faktor_diskonto$'Vt', 19, 100000000)

## Skema 3 - Copula
skema <- "Skema 3: Usia Suami Lebih Muda dari Istri"
asumsi <- "Asumsi Dependensi Copula Clayton"
premi_bersih_3C <- fungsi_premi_bersih_tahunan(t_qxy_3C, faktor_diskonto$'Vt', 19, 100000000)

## Skema 3 - Bebas
skema <- "Skema 3: Usia Suami Lebih Tua Muda Istri"
asumsi <- "Asumsi Kebebasan Mortalitas"
premi_bersih_3B <- fungsi_premi_bersih_tahunan(t_qxy_3B, faktor_diskonto$'Vt', 19, 100000000)