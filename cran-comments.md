## R CMD check results

Package was archived yesterday, 2023-01-09, despite efforts to address outstanding issues. 

Latest submission failed the recursive `noSuggest` check. An example using an imported package (sf) failed because the imported function (sf::read_sf) used a suggested package (tibble suggested by sf). All instances of `sf::read_sf()` have been changed to `sf::st_read()` which, I believe, does not use any suggested packages. 
