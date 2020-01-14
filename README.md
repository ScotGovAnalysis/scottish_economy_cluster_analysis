# Louvain_analysis
Set of function for cluster analysis of io tables

For cluster detection of io or leontief tables

Used in the production of experimental statistics analysing the structure of the Scottish economy
https://www2.gov.scot/Resource/0054/00549095.pdf

To use:
* Have R, RStudio and required packages installed
* Download files into the same folder
* Run generate_louvain_clusters.R
  (this will generate initial clusters and provide scores for them, sense checking and domain knowledge still required)
* Run app.R shiny app in RStudio
  (this will show the relative strengths of the attachment of industries to clusters, allowing in depths exploration)
