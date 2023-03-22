# Calculation of indicator-values

This is a collection of R-scripts that:
- ... extract the indicator_value data from
    - [Vegedaz 2021](https://www.wsl.ch/en/services-und-produkte/software-websites-und-apps/vegedaz.html) (*read_data_from_Vegedaz.r*)
      - based on Flora Indicativa (Flora indicativa und NISM Mai 2019 Nuller korrigiert)
      - based on Landolt (Zeigerwerte Landolt und NISM Mai 2019)
      - - ... extract the indicator_value data from
    - [Vegedaz 2023](https://www.wsl.ch/en/services-und-produkte/software-websites-und-apps/vegedaz.html) (*read_data_from_Vegedaz2023.r*)
      - based on Landolt (???)
    - [Veg, a software to manage vegetation data](https://www.maerki.com/maerki_informatik/veg/index.html)  (read_data_from_Veg_2015.r)
      - based on Flora Helvetica v.5 und Flora Indicativa	ca. 2014
- ... transpose your data frame from one survey per column to one survey per row. This is a prerequisite to do the analysis (*transpose_data.r*)
- ... align your species names with the collected data (*Correct_species_names.r*)
- ... calculate the weighted or mean indicator values for your vegetation surveys (*Calculate_indicator_values.r*)

- There is also an example that illustrates how it should work. (*example.r*)

## Download all scripts

use this link: [https://github.com/bazz-the-spazz/indival/zipball/master/](https://github.com/bazz-the-spazz/indival/zipball/master/)
