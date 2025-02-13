# A Novel Hybrid Approach to Contraceptive Demand Forecasting: Integrating Point Predictions with Probabilistic Distributions

## Authors

- **Harsha Chamara Hewage**  
  *Email:* [HalgamuweHewageHR@cardiff.ac.uk](mailto:HalgamuweHewageHR@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Data Lab for Social Good Group, Cardiff Business School, United Kingdom, CF10 3EU  
  *Corresponding Author*

- **Bahman Rostami-Tabar**  
  *Email:* [rostami-tabarb@cardiff.ac.uk](mailto:rostami-tabarb@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Data Lab for Social Good Group, Cardiff Business School, United Kingdom, CF10 3EU  

- **Aris Syntetos**  
  *Email:* [syntetosa@cardiff.ac.uk](mailto:syntetosa@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Cardiff Business School, United Kingdom, CF10 3EU  

- **Federico Liberatore**  
  *Email:* [liberatoref@cardiff.ac.uk](mailto:liberatoref@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Cardiff School of Computer Science & Informatics, United Kingdom, CF24 4AG  

- **Glenn Milano**  
  *Email:* [gmilano@usaid.gov](mailto:gmilano@usaid.gov)  
  *Affiliation:* United States Agency for International Development, Bureau for Global Health, United States, 20523  

## Abstract

Accurate demand forecasting is vital for ensuring reliable access to contraceptive products, supporting key processes like procurement, inventory, and distribution. However, forecasting contraceptive demand in developing countries presents challenges, including incomplete data, poor data quality, and the need to account for multiple geographical and product factors. Current methods often rely on simple forecasting techniques, which fail to capture demand uncertainties arising from these factors, warranting expert involvement. Our study aims to improve contraceptive demand forecasting by combining probabilistic forecasting methods with expert knowledge. We developed a hybrid model that combines point forecasts from domain-specific models with probabilistic distributions from statistical and machine learning approaches, enabling human input to fine-tune and enhance the system-generated forecasts. This approach helps address the uncertainties in demand and is particularly useful in resource-limited settings. We evaluate different forecasting methods, including time series, Bayesian, machine learning, and foundational time series methods alongside our new hybrid approach. By comparing these methods, we provide insights into their strengths, weaknesses, and computational requirements. Our research fills a gap in forecasting contraceptive demand and offers a practical framework that combines algorithmic and human expertise. Our proposed model can also be generalized to other humanitarian contexts with similar data patterns.

## Keywords

- Family planning supply chain  
- Hybrid forecasting  
- Forecast distributions  
- Contraceptive demand  
- Forecast combination  

## Repository Structure

- **data/**  
  This folder contains all the data used in the project, including logistics data, population data, and all the processed (tidy) datasets.

- **scripts/**  
  This folder includes all scripts used in the study, organized into three main subfolders:
  - **data_preprocessing/** - Scripts for cleaning and preparing data.
  - **forecast_models/** - Scripts for running different forecasting models.
  - **analysis/** - Scripts for analyzing and evaluating the forecasts.

- **manuscript/**  
  Contains the Quarto workflow used to generate the manuscript.

## How to Use

1. Clone the repository:
   ```sh
   git clone <repo-url>
   cd <repo-name>
   ```
2. Ensure all dependencies are installed.
3. Use scripts in `scripts/` for data preprocessing, forecasting, and analysis.
4. Refer to the `manuscript/` folder for manuscript generation using Quarto.

## Contact
For any inquiries, please contact the corresponding author: Harsha Chamara Hewage ([HalgamuweHewageHR@cardiff.ac.uk](mailto:HalgamuweHewageHR@cardiff.ac.uk)).
