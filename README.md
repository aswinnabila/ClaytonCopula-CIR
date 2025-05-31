# Clayton Copula-CIR: Joint Life Insurance Premium Calculation

This repository contains the source files, data, and documentation for my undergraduate thesis titled:

**"Penerapan Model Copula Clayton dalam Perhitungan Premi Bersih Tahunan Asuransi Jiwa Joint Life Pasangan Suami Istri Menggunakan Suku Bunga Cox-Ingersoll-Ross (CIR)"**

## 📁 Repository Structure
📁 data/  
│   ├── 1_Raw Data.xlsx                      
│   └── 2_Manual Excel Calculation.xlsx      
📁 script/  
│   ├── 1_CIR Interest Rate.R              
│   ├── 2_Clayton Copula Mortality.R       
│   ├── 3_Annual Premium Simulation.R      
📁 summary/  
│   └── Research Overview.png            
README.md

## 📌 Overview
This project models the **dependency of mortality** between spouses using the **Clayton Copula**, and incorporates **stochastic interest rates** using the **Cox-Ingersoll-Ross (CIR) model** to calculate:

- Annual net premiums  
- Annuity values  
- Insurance benefits  

All calculations were implemented and simulated in **R**.

## 📊 Data
- `1_Raw Data.xlsx` include interest rates (BI Rate) and mortality tables (Tabel Mortalitas Indonesia IV).  
- `2_Manual Excel Calculation.xlsx` are included for transparency and validation.

## 📜 Scripts
- `1_CIR Interest Rate.R` – Parameter estimation and simulation of the CIR model.  
- `2_Clayton Copula Mortality.R` – Joint-life mortality modeling using the Clayton Copula.  
- `3_Annual Premium Simulation.R` – Annuity valuation, insurance benefit calculation, and premium simulation.

## 🧾 Summary
The `summary/` folder contains a one-page research poster that visualizes the methodology and results of the study.

## 🎓 Author
**Aswin Nabila**  
Department of Statistics, Faculty of Science and Mathematics  
Diponegoro University
2024

## 📄 License
This project is shared for academic and educational purposes only. Please cite appropriately.
