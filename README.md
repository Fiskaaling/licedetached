# LiceDetached

Scripts generated in the LiceDetached project


## Paper: Direct and indirect infection


### Temperature Quality Assurance


**Common practice for older temperature registrations:**
Temperature measurements only required to be measured in *n* amount of cages per cycle, where *n* most commonly is one.
The temperature measurement is then copied to other cages in the cycle before the data is handed in to the relevant authorities.


#### Assumtions

- If a temperature reading is repeated over *X* consecutive days, it is assumed to be an input error
  - the temperature is set to NA for the repeated inputs 
- the temperature is the same in all of the cages within a cycle
  - the mean temperature is calculated pr. cycle pr. day

*X* = consecutive days with repeated temperature measurement pr. cage  
*Y* = consecutive days with missing data pr. cycle  
*Z* = residuals limit to detect outliers  
*U* = window used for the moving average calculations 


#### Method

1. Database flags temperature errors < 2 and > 15 deg C
2. Input errors set to NA, if repeated measurements is > *X*
3. mean temperatures are calculated pr. cycle pr. day (mean cycle temperature) and just pr. day (mean temperature)
4. outliers are removed by abs(residuals) > *Z* from mean cycle temperature - mean temperature (all of the data)
5. the daily mean temperature is calculated again
6. two linear interpolations:
    - pr. day pr. cycle
    - pr. day
7. the missing mean temperatures pr. cycle are replaced with:
    - if missing data is < *Y* consecutive days = cycle interpolations
    - if missing data is > *Y* consecutive days = daily interpolations
8. Moving average (*MA*) over *U* days is calculated
9. outliers are found by abs(residuals) > *Z* from mean cycle temperature (included interpolated) - *MA*
10. outliers are replaced with either cycle interpolations, daily interpolations, or *MA* depending on the origin of the outlier. ie. if the outlier was a raw measurement then it is replaced with the cycle interpolation and if the outlier was a cycle interpolation then it is replaced with the daily interpolation and so forth.
11. the temperatures at the beginning and end of the cycle are out of range for the MA calculated pr. cycle, in these cases the residuals are calculated from the


*use this script:* [temperature_QA.R](scripts/temperature_QA.R), note that only a small test dataset is provided in the data folder.