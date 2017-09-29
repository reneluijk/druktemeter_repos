## Repos voor druktemeter
Repos voor druktemeter. Bevat een aantal scripts, ieder met hieronder een korte uitleg. Voor meer informatie, zie desbetreffende scripts zelf.


### CMSA
#### cmsa.R
Leest CMSA data in en schrijft als csv


### GVB
#### gvb_leaflet.R
Script om de locaties van de haltes te plotten op een kaart. Puur ter controle of de locaties kloppen.
#### gvb.py
Script om de GVB data in te lezen, de data te bewerken, en als csv weg te schrijven.


### KNMI
#### knmi.R
Script om ruwe historische KNMI data in te lezen en weg te schrijven. Daarnaast worden ook statistieken per dag berekend en opgeslagen. 


### MORA
#### mora.R
Leest MORA dump in en manipuleert datums.


### Schiphol
#### schiphol_api.py
Script om de Schiphol API aan te roepen. Vereist een app ID en een app key, beide zouden gegeven moeten worden in schiphol_keys.json.
#### schiphol_keys.json
JSON file met de app ID en key. Moeten bij de Schiphol API zelf worden opgevraagd: https://developer.schiphol.nl/


### Relateren van verschillende databronnen
Hier wordt geprobeerd de verschillende bronnen aan elkaar te relateren.
#### weer_cmsa.R
KNMI data en CMSA tellingen.
#### weer_mora.R
KNMI data en verschillende type meldingen.