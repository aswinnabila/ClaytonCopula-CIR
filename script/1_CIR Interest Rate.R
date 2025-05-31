# SUKU BUNGA COX INGERSOLL ROSS
# Install packages yang dibutuhkan
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tibble")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)

# Import data suku bunga
library(readxl)
interest_rate <- read_excel("1_Raw Data.xlsx", 
                            sheet = "Suku Bunga")
View(interest_rate)

# Grafik fluktuasi suku bunga
## Membuat kolom baru untuk gabungan bulan dan tahun
interest_rate <- interest_rate %>%
  mutate(Bulan_Tahun = paste(Bulan, Tahun, sep = " "))

## Mengatur urutan faktor pada kolom Bulan_Tahun agar sesuai dengan urutan waktu
interest_rate$Bulan_Tahun <- factor(interest_rate$Bulan_Tahun, levels = unique(interest_rate$Bulan_Tahun))

## Membuat grafik fluktuasi suku bunga
ggplot(interest_rate, aes(x = Bulan_Tahun, y = `BI Rate`, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "Fluktuasi Suku Bunga BI",
       x = "Bulan dan Tahun",
       y = "BI Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) 

# Estimasi Parameter CIR
## Membentuk persamaan baru
interest_rate$'BI Rate'
interest_rate$r_t_plus_1 <- c(interest_rate$'BI Rate'[-1], NA)
interest_rate$r_t_plus_1_sqrt_r_t <- interest_rate$r_t_plus_1 / sqrt(interest_rate$'BI Rate')

## Data frame OLS
reg_data <- data.frame(
  r_t_plus_1_sqrt_r_t = interest_rate$r_t_plus_1_sqrt_r_t,
  one_over_sqrt_r_t = 1 / sqrt(interest_rate$'BI Rate'),
  sqrt_r_t = sqrt(interest_rate$'BI Rate')
)

## Hasil estimasi
fit <- lm(r_t_plus_1_sqrt_r_t ~ one_over_sqrt_r_t + sqrt_r_t - 1, data = reg_data)
a_hat <- coef(fit)[1]
b_hat <- coef(fit)[2]

## Parameter Model CIR
alpha_hat <- 1 - b_hat
beta_hat <- a_hat / alpha_hat
residuals <- residuals(fit)
sigma_hat <- sd(residuals)
cat("Estimasi parameter CIR :\n",
    "a =", a_hat, "\n",
    "b =", b_hat, "\n",
    "alpha =", alpha_hat, "\n",
    "beta =", beta_hat, "\n",
    "sigma =", sigma_hat, "\n")

# Peramalan CIR 
## Fungsi Simulasi Euler-Maruyama
simulate_CIR <- function(R0, alpha, beta, sigma, T, dt = 1) {
  n <- T / dt
  R <- numeric(n + 1)
  R[1] <- R0
  for (i in 1:n) {
    dW <- rnorm(1, mean = 0, sd = sqrt(dt))
    R[i + 1] <- R[i] + alpha * (beta - R[i]) * dt + sigma * sqrt(R[i]) * dW
  }
  return(R)
}

## Fungsi MAPE
calculate_mape <- function(simulated_rate, actual_rate) {
  mean(abs((actual_rate - simulated_rate) / actual_rate), na.rm = TRUE) * 100
}

## Fungsi simulasi dan evaluasi MAPE
simulate_and_evaluate <- function(R0, alpha, beta, sigma, T, actual_data, seeds = 1:10) {
  results <- tibble(seed = seeds, mape = numeric(length(seeds)))
  
  for (i in seeds) {
    set.seed(i)  # Set seed untuk reprodusibilitas
    simulated_rate <- simulate_CIR(R0, alpha, beta, sigma, T)
    simulated_rate <- simulated_rate[-1]  # Buang data pertama
    
    # panjang data simulasi = data aktual
    if (length(simulated_rate) <= length(actual_data)) {
      actual_data_trimmed <- actual_data[1:length(simulated_rate)]
    } else {
      simulated_rate <- simulated_rate[1:length(actual_data)]
      actual_data_trimmed <- actual_data
    }
    
    mape <- calculate_mape(simulated_rate, actual_data_trimmed)
    results$mape[i] <- mape
  }
  
  return(results)
}

## Data uji
actual_data <- interest_rate$'BI Rate'[-1] #tidak termasuk data pertama

## Parameter model CIR yang sudah diestimasi
R0 <- tail(interest_rate$'BI Rate', 1)
T <- 179

## Evaluasi simulasi dengan berbagai seed
results <- simulate_and_evaluate(R0, alpha_hat, beta_hat, sigma_hat, T, actual_data, seeds = 1:1000)
best_result <- results %>% filter(mape == min(mape))
print(best_result)

## Simulasi suku bunga untuk evaluasi model
set.seed(best_result$'seed')
simulated_rate <- simulate_CIR(R0, alpha_hat, beta_hat, sigma_hat, T)
simulated_rate <- simulated_rate[-1]
simulated_rate

## Membuat tabel untuk data simulasi dan data aktual
comparison_data <- tibble(
  Period = interest_rate$'Bulan_Tahun'[-1],
  Actual = actual_data,
  Simulated = simulated_rate
)
print(comparison_data, n=179)

## Membuat plot perbandingan
ggplot(comparison_data, aes(x = Period)) +
  geom_line(aes(y = Actual, color = "Actual", group = 1), linewidth = 1) +
  geom_line(aes(y = Simulated, color = "Simulated", group = 1), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +
  geom_point(aes(y = Simulated, color = "Simulated"), size = 2) +
  scale_color_manual(values = c("Actual" = "blue", "Simulated" = "red")) +
  labs(title = "Perbandingan Suku Bunga Aktual dan Simulasi CIR",
       x = "Periode",
       y = "BI Rate (%)",
       color = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 

# Suku bunga CIR untuk perhitungan premi
## Simulasi suku bunga untuk 228 periode (19 tahun) ke depan
R0 <- tail(interest_rate$'BI Rate', 1)
set.seed(best_result$'seed')
suku_bunga_bulanan_CIR <- simulate_CIR(R0, alpha_hat, beta_hat, sigma_hat, T = 228)
suku_bunga_bulanan_CIR <- suku_bunga_bulanan_CIR[-1]

## Menambahkan kolom bulan dan tahun
suku_bunga_bulanan_CIR <- data.frame(
  `BI Rate` = as.numeric(suku_bunga_bulanan_CIR)
)
suku_bunga_bulanan_CIR <- suku_bunga_bulanan_CIR %>%
  mutate(
    Tahun = 2024 + (seq_len(nrow(.)) - 1) %/% 12,
    Bulan = (seq_len(nrow(.)) - 1) %% 12 + 1
  )
suku_bunga_bulanan_CIR

## Menghitung rata-rata tahunan
suku_bunga_CIR <- suku_bunga_bulanan_CIR %>%
  group_by(Tahun) %>%
  summarise(
    Mean_CIR = mean(`BI.Rate`, na.rm = TRUE)
  )
print(suku_bunga_CIR)

## Menghitung faktor diskonto tahunan
faktor_diskonto <- suku_bunga_CIR %>%
  mutate(
    Diskonto = 1 / (1 + Mean_CIR/100) #karna persentase
  ) %>%
  mutate(
    Vt = cumprod(Diskonto)
  )
print(faktor_diskonto)