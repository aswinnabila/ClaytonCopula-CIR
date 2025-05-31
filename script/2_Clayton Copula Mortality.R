# DATA MORTALITAS - COPULA CLAYTON 
# Memuat package yang diperlukan
install.packages("fBasics")
install.packages("copula")
install.packages("MASS")
library(fBasics)
library(copula)
library(MASS)

# Menginput data
library(readxl)
mortalitas <- read_excel("1_Raw Data.xlsx", 
                         sheet = "Mortalitas")
View(mortalitas)
qx <- mortalitas$'qx_lakilaki'
qy <- mortalitas$'qy_perempuan'
usia <- mortalitas$'Usia'

# Statistika Deskriptif Data Mortalitas tunggal
basicStats(qx)
basicStats(qy)

# Uji Korelasi Kendall's Tau
## Skema 1: Usia x = y
data_skema1 <- data.frame('qx_0' = mortalitas$'qx_0', 'qy_0' = mortalitas$'qy_0')
data_skema1<- na.omit(data_skema1)
data_rank_skema1 <- apply(data_skema1[, 1:2], 2, rank) / (nrow(data_skema1) + 1)
kendalls_tau1 <- cor.test(data_rank_skema1[, 1], data_rank_skema1[, 2], method = "kendall")
print(kendalls_tau1)

## Skema 2: Usia x = 5 saat y = 0
data_skema2 <- data.frame('qx_5' = mortalitas$'qx_5', 'qy_0' = mortalitas$'qy_0')
data_skema2 <- na.omit(data_skema2)
data_rank_skema2 <- apply(data_skema2[, 1:2], 2, rank) / (nrow(data_skema2) + 1)
kendalls_tau2 <- cor.test(data_rank_skema2[, 1], data_rank_skema2[, 2], method = "kendall")
print(kendalls_tau2)

## Skema 3: Usia x = 0 saat y = 5
data_skema3 <- data.frame('qx_0' = mortalitas$'qx_0', 'qy_5' = mortalitas$'qy_5')
data_skema3 <- na.omit(data_skema3)
data_rank_skema3 <- apply(data_skema3[, 1:2], 2, rank) / (nrow(data_skema3) + 1)
kendalls_tau3 <- cor.test(data_rank_skema3[, 1], data_rank_skema3[, 2], method = "kendall")
print(kendalls_tau3)

# Estimasi Parameter Copula Clayton dan Tail Dependensi
copula_clayton <- claytonCopula(dim=2)
## Skema 1
fit_clayton1 <- fitCopula(copula_clayton, data_rank_skema1, method="itau")
summary(fit_clayton1)
lambda_1 <- lambda(claytonCopula(fit_clayton1@estimate))
print(lambda_1)
## Skema 2
fit_clayton2 <- fitCopula(copula_clayton, data_rank_skema2, method="itau")
summary(fit_clayton2)
lambda_2 <- lambda(claytonCopula(fit_clayton2@estimate))
print(lambda_2)
## Skema 3
fit_clayton3 <- fitCopula(copula_clayton, data_rank_skema3, method="itau")
summary(fit_clayton3)
lambda_3 <- lambda(claytonCopula(fit_clayton3@estimate))
print(lambda_3)

# Probabilitas gabungan asumsi dependensi Copula Clayton
## Skema 2
clayton_copula <- claytonCopula(fit_clayton2@estimate, dim=2)
qxy2 <- pCopula(cbind(data_skema2$'qx_5', data_skema2$'qy_0'), clayton_copula)
qxy2 <- data_skema2$'qx_5'+ data_skema2$'qy_0'- qxy2
usia_x5 <- seq(5, 111, by = 1)
usia_y0 <- seq(0, 106, by = 1)
qxy_copula2 <- data.frame(usia_x = usia_x5, usia_y = usia_y0, qxy = qxy2)
print(qxy_copula2)

## Skema 3
clayton_copula <- claytonCopula(fit_clayton3@estimate, dim=2)
qxy3 <- pCopula(cbind(data_skema3$'qx_0', data_skema3$'qy_5'), clayton_copula)
qxy3 <- data_skema3$'qx_0'+ data_skema3$'qy_5' - qxy3
usia_x0 <- seq(0, 106, by = 1)
usia_y5 <- seq(5, 111, by = 1)
qxy_copula3 <- data.frame(usia_x = usia_x0, usia_y = usia_y5, qxy = qxy3)
print(qxy_copula3)

# Probabilitas gabungan asumsi kebebasan mortalitas
## Skema 2
qxy_2 <- 1-((1-data_skema2$'qx_5')*(1-data_skema2$'qy_0'))
qxy_bebas2 <- data.frame(usia_x = usia_x5, usia_y = usia_y0, qxy = qxy_2)
print(qxy_bebas2)
## Skema 3
qxy_3 <- 1-((1-data_skema3$'qx_0')*(1-data_skema3$'qy_5'))
qxy_bebas3 <- data.frame(usia_x = usia_x0, usia_y = usia_y5, qxy_3)
print(qxy_bebas3)

# Plot Perbandingan Asumsi Skema 2
data_skema2_comparison <- rbind(
  data.frame(usia_x = qxy_copula2$usia_x, qxy = qxy_copula2$qxy, Asumsi = "Copula Clayton"),
  data.frame(usia_x = qxy_bebas2$usia_x, qxy = qxy_bebas2$qxy, Asumsi = "Kebebasan Mortalitas")
)
ggplot(data_skema2_comparison, aes(x = usia_x, y = qxy, color = Asumsi)) +
  geom_line(linewidth = 1) +
  labs(title = "Perbandingan Probabilitas Gabungan untuk Skema 2",
       x = "Usia x",
       y = "Probabilitas Gabungan (qxy)",
       color = "Asumsi") +  
  theme_minimal() +
  theme(legend.position = "bottom",  
        legend.title = element_blank())  

# Plot Perbandingan Asumsi Skema 3
data_skema3_comparison <- rbind(
  data.frame(usia_x = qxy_copula3$usia_x, qxy = qxy_copula3$qxy, Asumsi = "Copula Clayton"),
  data.frame(usia_x = qxy_bebas3$usia_x, qxy = qxy_bebas3$qxy, Asumsi = "Kebebasan Mortalitas")
)
ggplot(data_skema3_comparison, aes(x = usia_x, y = qxy, color = Asumsi)) +
  geom_line(linewidth = 1) +
  labs(title = "Perbandingan Probabilitas Gabungan untuk Skema 3",
       x = "Usia x",
       y = "Probabilitas Gabungan (qxy)",
       color = "Asumsi") +  
  theme_minimal() +
  theme(legend.position = "bottom",  
        legend.title = element_blank()) 

# Visualisasi Copula Clayton
## Simulasi Model Copula Clayton Skema 2
clayton_copula <- claytonCopula(fit_clayton2@estimate, dim = 2)
set.seed(100)
sim_data <- rCopula(107, clayton_copula)
sim_data_df <- as.data.frame(sim_data)
colnames(sim_data_df) <- c("qx_sim", "qy_sim")

## CDF data simulasi 2
kde_result <- kde2d(sim_data_df$qx_sim, sim_data_df$qy_sim, n = 107)
contour_data <- data.frame(expand.grid(x = kde_result$x, y = kde_result$y))
contour_data$z <- as.vector(kde_result$z)

## Plot kontur dan scatterplot Skema 2
ggplot() +
  geom_contour(data = contour_data, aes(x = x, y = y, z = z), color = "black") +
  geom_point(data = sim_data_df, aes(x = qx_sim, y = qy_sim), color = "black", alpha = 0.5) +
  labs(title = "Scatterplot & Contour Lines Copula Clayton Skema 2",
       x = "qx", y = "qy") +
  theme_minimal()

## Simulasi Model Copula Clayton Skema 3
clayton_copula <- claytonCopula(fit_clayton3@estimate, dim = 2)
set.seed(100)
sim_data <- rCopula(107, clayton_copula)
sim_data_df <- as.data.frame(sim_data)
colnames(sim_data_df) <- c("qx_sim", "qy_sim")

## CDF data simulasi 3
kde_result <- kde2d(sim_data_df$qx_sim, sim_data_df$qy_sim, n = 107)
contour_data <- data.frame(expand.grid(x = kde_result$x, y = kde_result$y))
contour_data$z <- as.vector(kde_result$z)

## Plot kontur dan scatterplot Skema 3
ggplot() +
  geom_contour(data = contour_data, aes(x = x, y = y, z = z), color = "black") +
  geom_point(data = sim_data_df, aes(x = qx_sim, y = qy_sim), color = "black", alpha = 0.5) +
  labs(title = "Scatterplot & Contour Lines Copula Clayton Skema 3",
       x = "qx", y = "qy") +
  theme_minimal()