# Habitat suitability models for Antarctic fur seals (***Arctocephalus gazella***) in climate change scenarios
This repository contains all the R code used in the article "Future climate-induced distribution shifts in a sexually dimorphic key predator of the Southern Ocean" published in Global Change Biology in February 2024. The tracking data processing & model fitting frameworks have been adapted from David March's work (https://zenodo.org/records/5701286 & https://github.com/dmarch/agazella). 

Ouled-Cheikh, J., March, D., Borras-Chavez, R., Drago, M., Goebel, M. E., Fariña, J. M., Gazo, M., Coll, M., Cardona L., 2024. Future climate-induced distribution shifts in a sexually dimorphic key predator of the Southern Ocean. ***Global Change Biology***. 

The response to climate change in highly dimorphic species can be hindered by differences between sexes in habitat preferences and movement patterns. The Antarctic fur seal, Arctocephalus gazella, is the most abundant pinniped in the Southern Hemisphere, and one of the main consumers of Antarctic krill, Euphausia superba, in the Southern Ocean. However, the populations breeding in the Atlantic Sector of the Southern Ocean are declining because of climate change. Male and female Antarctic fur seals differ greatly in body size and foraging ecology, and little is known about their sex-specific responses to climate change. We used satellite tracking data and Earth System Models to predict changes in habitat suitability for male and female Antarctic fur seals from the Western Antarctic Peninsula under different climate change scenarios. Under the most extreme scenario (SSP5-8.5; global average temperature +4.4ºC projected by 2100), suitable habitat patches will shift southward during the non-breeding season, leading to a minor overall habitat loss. The impact will be more pronounced for females than for males. The reduction of winter foraging grounds might decrease the survival of post-weaned females, reducing recruitment and jeopardizing population viability. During the breeding season, when males fast on land, suitable foraging grounds for females off the South Shetland Islands will remain largely unmodified, and new ones will emerge in the Bellingshausen Sea. As Antarctic fur seals, are income breeders, the foraging grounds of females should be reasonably close to the breeding colony. As a result, the new suitable foraging grounds will be useful for females only if nearby beaches currently covered by sea ice emerge by the end of the century. Furthermore, the colonization of these new, ice-free breeding locations might be limited by strong female philopatry. These results should be considered when managing the fisheries of Antarctic krill in the Southern Ocean.


## Repository structure

| Folder    | Description                                             |
|-----------|---------------------------------------------------------|
| 00functions    | Script containing custom functions to read, process and visualize tracking data |
| 01enviro       | Scripts used to obtain and process Earth System Model data from ISIMIP (https://www.isimip.org) |
| 02analysis  | Scripts used to analyse the data. Includes two subfolders for the processing of satellite tracks (/00tracking) and fitting Boosted Regression Trees (/01habitatModel) |
| 03figures     | Scripts used to generate maps & plots                           |
| setup       | Script to get all needed packages ready                           |



## Data availability

Tracking data, environmental predictors and results from this project can be openly found at the repository of the University of Barcelona: https://doi.org/10.34810/data1064. 

## License

Copyright (c) 2024 Jazel Ouled-Cheikh
Licensed under the MIT license.

## Acknowledgements
In memoriam of Dr Francisco Bozinovic (1959-2023), whose contribution to this research was key to its success. LC, MG and MD acknowledge that this project was partially funded by grant CTM2017-83319-P from Ministerio de Ciencia, Innovación y Universidades (Spain) and supported by AEI/FEDER/UE. We thank the members of the Spanish Army at the Gabriel de Castilla research station for assistance and logistic support during fieldwork in Deception Island. JO was supported by Universitat de Barcelona through the PREDOCS-UB grant (2021). MD acknowledges support from the Secretaria d’Universitats i Recerca, Generalitat de catalunya (Spain) under the Beatriu de Pinós programme postdoctoral fellowship (2016 BP 00151). MG and RB thank the US Antarctic Ecosystem Research Division (AERD) of the National Oceanic and Atmospheric Administration (NOAA) and the Chilean Antarctic Institute (INACH) Project: DT-02-15 for funding and logistic support at Cape Shirreff. RB thanks to The Chilean Government and the Agencia Nacional de Investigación y Desarrollo (ANID, Project N° 21130059). CAPES members thanks to ANID PIA/BASAL FB0002. DM acknowledges support from the CIDEGENT program of the Generalitat Valenciana (CIDEGENT/2021/058). MC was supported by the ProOceans project (Ministerio de Ciencia e Innovación, Proyectos de I+D+I (RETOS-PID2020-118097RB-I00) and acknowledges the ‘Severo Ochoa Centre of Excellence accreditation (CEX2019-000928-S). 
