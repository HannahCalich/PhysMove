## PhysMove News & Updates

#### <ins>PhysMove 1.1</ins>
##### Key update
* Updated PhysMove for compatibility with R version 4.4.0

##### Minor updates
* Removed references to Infomap.exe to avoid confusion with Mac users
* Included `sf()` in dependencies because it is being called on through ggplot but was being missed by dev checks. Hard coded into `gyrationrad()` so that cran will see it is called on to remove note about dependencies not being used. 

#### <ins>PhysMove 1.0.1</ins>
##### Key update
* Initial CRAN submission.
