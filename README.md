# Weather-Forecast-Post-Processing

This project utilizes the EUPPBench dataset (EUMETNET Postprocessing Benchmark, Demayer et al. 2023) to compare statistical postprocessing techniques in numerical weather prediction (NWP). Postprocessing corrects forecast errors and biases from NWP models using past forecasts and observations, commonly referred to as MOS (Model Output Statistics). This project focuses specifically on Germany within the central Europe coverage of the dataset.

## Dataset Overview
- **Forecasts**: Produced by the Integrated Forecasting System (IFS) of ECMWF for 2017, with daily forecasts at 6-hour intervals, up to 5 days ahead. The dataset includes deterministic forecasts (control runs) for simplicity.
- **Reforecasts**: Twice-weekly reforecasts spanning 20 years (1997–2016), paired with corresponding observations.
- **Observations**: Weather data from 49 German stations, provided by the German Weather Service (DWD), covering 1997–2017.

## Objective
The goal is to develop a machine learning model to improve postprocessing of 2-metre air temperature forecasts at a fixed lead time of 120 hours (5 days). The model will be trained using reforecasts (1997–2016) and tested on 2017 data. Finally, it will predict temperatures for 2018 (without knowing the true values).

## Key Steps
1. **Model**: Following the MOS approach, the statistical model will predict future temperatures based on a vector of NWP forecast variables at specific locations and times.
2. **Focus**: The project is limited to a forecast lead time of 120 hours, requiring the selection of suitable predictors and parameters for the model.
3. **Overfitting**: Special attention is required to prevent overfitting through appropriate model selection and tuning.

This project combines machine learning with operational weather forecasting to refine temperature prediction accuracy.
