1. Install R & RStudio. The version should not matter. I ran it on Windows. All necessary packages are loaded in the beginning of the code script. If you do not have the 
library installed, use "install.packages("<package>"). I used the following R version most recently on my local computer:
R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

2. Download TerraClimate AET, PET, soil moisture, SWE, P, and T data from: https://www.climatologylab.org/terraclimate.html
3. Download Snow Classifcation data from: https://nsidc.org/data/nsidc-0768/versions/1
4. Follow code in order (i.e., 1 to 12).

Notes:
-No original algorithms were developed for this paper. The main part of the code is when I calculate yearly snow drought (script 4a_*) using an already established
drought designation calculation that has been published on (Harpold et al, 2017; Dierauer et al, 2019). This is mostly data analysis. 
-The expected output can be found in each of the code scripts