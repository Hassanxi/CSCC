> # Function to load or install required packages
> load_or_install <- function(package) {
+   if (!require(package, character.only = TRUE)) {
+     install.packages(package, dependencies = TRUE)
+     library(package, character.only = TRUE)
+   }
+ }
> # List of required packages
> packages <- c("readr", "dplyr", "tidyr", "ggplot2", "parallel", "sf", 
+               "stringr", "data.table", "pbapply", "caret", "mlogit", 
+               "bayesm", "rstanarm")
> lapply(packages, load_or_install)
[[1]]
NULL

[[2]]
NULL

[[3]]
NULL

[[4]]
NULL

[[5]]
NULL

[[6]]
NULL

[[7]]
NULL

[[8]]
NULL

[[9]]
NULL

[[10]]
NULL

[[11]]
NULL

[[12]]
NULL

[[13]]
NULL

> # STEP 1: Load the raw data
> # Load tablet attribute data for reference (not used for modeling)
> tablet_data_unf <- fread("/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/tablets.csv")
> # STEP 2: Load the respondent estimation data
> path_e_data_mod <- "/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/Estimation_Data_tab.Rdata"
> load(path_e_data_mod)
> # Inspect the structure of the loaded estimation data
> str(E_Data_mod)
List of 2
 $ p      : int 4
 $ lgtdata:List of 1046
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 3 4 4 2 2 1 1 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 3 3 3 2 3 2 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 1 1 3 1 4 4 1 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 1 1 1 1 1 3 3 2 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 1 1 2 1 1 3 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 4 4 4 2 2 4 1 2 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 2 3 3 3 2 3 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 3 3 1 4 2 4 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 1 4 2 4 4 4 4 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 3 3 1 3 3 2 1 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 3 4 1 4 4 1 4 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 3 2 2 1 2 1 3 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 3 1 4 4 4 2 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 1 1 3 1 1 4 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 3 2 1 1 1 3 1 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 4 4 4 4 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 1 4 3 4 4 4 4 3 4 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 2 1 1 3 3 3 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 2 3 2 3 2 3 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 1 1 4 1 3 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 3 3 2 3 3 3 3 1 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 1 1 2 2 3 2 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 2 3 2 2 2 1 2 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 1 3 3 3 2 1 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 3 2 3 1 1 1 1 3 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 3 2 1 1 4 1 1 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 3 3 1 3 2 3 2 2 1 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 3 4 2 1 1 4 1 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 2 1 4 4 2 3 4 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 2 1 1 3 2 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 1 3 4 1 4 1 4 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 4 2 2 2 2 3 1 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 2 3 1 1 1 2 1 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 4 2 1 1 3 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 4 4 4 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 2 3 3 3 2 3 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 2 3 3 3 2 1 1 4 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 3 2 3 3 4 1 2 2 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 3 1 4 4 1 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 2 3 3 2 1 1 1 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 3 3 4 4 4 1 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 3 3 1 1 3 2 2 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 3 1 3 3 1 2 2 2 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 4 4 4 4 3 4 3 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 1 2 1 3 1 2 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 2 1 2 1 2 2 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 3 3 1 4 4 4 4 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 3 1 4 2 4 4 4 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 2 1 2 2 4 1 4 3 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 2 3 2 3 3 2 1 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 3 2 2 1 1 2 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 4 4 3 1 4 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 4 2 1 2 3 3 1 3 4 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 4 2 3 2 2 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 4 4 4 4 1 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 2 1 4 2 2 1 4 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 4 2 4 1 3 3 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 1 1 3 1 1 1 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 4 2 4 3 1 4 3 3 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 1 2 2 2 1 2 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 4 4 4 2 4 4 4 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 4 4 1 1 4 4 1 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 2 2 2 2 2 3 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 3 2 3 3 3 2 1 3 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 4 4 1 3 2 1 2 4 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 3 3 2 2 1 3 1 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 3 1 2 4 4 3 4 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 1 4 4 4 2 4 3 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 2 2 1 3 3 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 1 2 2 3 3 3 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 2 1 1 1 3 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 1 1 3 1 3 3 3 2 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 4 4 4 4 4 1 1 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 1 2 2 1 3 1 1 2 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 1 2 4 3 4 1 2 1 1 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 2 1 2 1 1 2 4 2 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 1 4 4 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 4 4 2 4 2 4 4 4 1 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 4 3 3 2 4 4 1 3 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 1 1 2 1 1 3 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 2 2 3 3 3 3 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 1 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 3 4 3 3 3 3 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 4 3 4 2 4 4 2 3 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 4 4 3 4 2 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 2 1 2 4 4 2 4 1 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 1 3 1 2 2 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 1 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 1 1 3 1 2 1 1 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 2 2 2 3 3 3 1 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 2 4 4 4 1 2 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 2 1 2 3 2 1 3 3 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 1 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 1 1 1 2 2 1 2 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 4 3 4 4 3 4 3 4 ...
  .. ..$ X: num [1:52, 1:36] 1 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 1 2 4 4 4 4 4 4 4 4 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 1 2 2 2 4 4 1 3 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 3 1 2 3 3 3 1 1 3 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 2 2 1 1 2 3 2 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 3 3 2 2 1 1 3 3 2 2 ...
  .. ..$ X: num [1:52, 1:36] 0 0 1 0 1 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 4 4 4 4 3 4 4 4 4 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  ..$ :List of 2
  .. ..$ y: num [1:13] 2 4 4 2 1 1 3 1 4 1 ...
  .. ..$ X: num [1:52, 1:36] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : NULL
  .. .. .. ..$ : chr [1:36] "Brand 1" "Brand 2" "Brand 3" "Brand 4" ...
  .. [list output truncated]
> # Function to prepare data for all respondents
> prepare_data <- function(E_Data_mod) {
+   data_list <- lapply(seq_along(E_Data_mod$lgtdata), function(i) {
+     # Extract respondent data
+     respondent <- E_Data_mod$lgtdata[[i]]
+     y <- respondent$y  # Choices made
+     X <- respondent$X  # Design matrix
+     
+     # Validate number of alternatives per task
+     num_tasks <- length(y)
+     num_alts <- nrow(X) / num_tasks
+     if (num_alts != 4) {
+       stop("Number of alternatives per task is not 4.")
+     }
+     
+     # Create identifiers
+     TaskID <- rep(1:num_tasks, each = num_alts)
+     AltID  <- rep(1:num_alts, times = num_tasks)
+     RespID <- rep(i, nrow(X))
+     
+     # Binary chosen indicator
+     Chosen <- as.integer(AltID == rep(y, each = num_alts))
+     
+     # Combine into data.frame
+     df <- data.frame(
+       RespID = RespID,
+       TaskID = TaskID,
+       AltID  = AltID,
+       Chosen = Chosen
+     )
+     df <- cbind(df, X)
+     return(df)
+   })
+   
+   combined_data <- bind_rows(data_list)
+   return(combined_data)
+ }
> # Prepare the combined data
> combined_data <- prepare_data(E_Data_mod)
> # Data cleaning and attribute transformation
> combined_data <- combined_data %>%
+   rename_with(~ gsub("\\s+", "_", .)) %>%
+   rename_with(~ gsub("\\(|\\)", "", .)) %>%
+   rename_with(~ gsub("-", "", .)) %>%
+   mutate(chid = paste0(RespID, "_", TaskID)) %>%
+   mutate(
+     Brand = case_when(
+       Brand_1 == 1 ~ "A",
+       Brand_2 == 1 ~ "B",
+       Brand_3 == 1 ~ "C",
+       Brand_4 == 1 ~ "D",
+       Brand_5 == 1 ~ "E",
+       Brand_6 == 1 ~ "F",
+       Brand_7 == 1 ~ "G",
+       TRUE ~ "Outside Option"
+     ),
+     Resolution = ifelse(High_Resolution_264_ppi == 1, "High", "Standard"),
+     Memory = case_when(
+       `16_GB` == 1 ~ "16GB",
+       `32_GB` == 1 ~ "32GB",
+       `64_GB` == 1 ~ "64GB",
+       `128GB` == 1 ~ "128GB",
+       TRUE ~ "8GB"
+     ),
+     SD_Slot = ifelse(Without_SDSlot == 1, "Without", "With"),
+     Performance = case_when(
+       `1.6_GHz` == 1 ~ "1.6 GHz",
+       `2.2_GHz` == 1 ~ "2.2 GHz",
+       TRUE ~ "1 GHz"
+     ),
+     Battery_Run_Time = ifelse(`812_h._Runtime` == 1, "8-12 hours", "4-8 hours"),
+     Connections = case_when(
+       `WLAN_+_UMTS/3G` == 1 ~ "WLAN + UMTS (3G)",
+       `WLAN_+_LTE/4G` == 1 ~ "WLAN + LTE (4G)",
+       TRUE ~ "WLAN"
+     ),
+     Sync_to_Smartphone = ifelse(Sphone_Synch. == 1, "Yes", "No"),
+     Value_Pack = ifelse(Value_Pack == 1, "Yes", "No"),
+     Equipment = case_when(
+       Cover == 1 ~ "Cover",
+       Keyboard == 1 ~ "Keyboard",
+       Mouse == 1 ~ "Mouse",
+       Pencil == 1 ~ "Pencil",
+       `32_GB_Memory_Card` == 1 ~ "32GB Memory Card",
+       `Keyboard_+_Pencil` == 1 ~ "Keyboard + Pencil",
+       `Keyboard_+_Mouse_+_Pencil` == 1 ~ "Keyboard + Mouse + Pencil",
+       TRUE ~ "None"
+     ),
+     Cash_Back = case_when(
+       `50_Cash_Back` == 1 ~ "50 EUR",
+       `100_Cash_Back` == 1 ~ "100 EUR",
+       `150_Cash_Back` == 1 ~ "150 EUR",
+       TRUE ~ "No Cash Back"
+     ),
+     Display_Size = case_when(
+       `8_Inches` == 1 ~ "8 Inches",
+       `10_Inches` == 1 ~ "10 Inches",
+       `12_Inches` == 1 ~ "12 Inches",
+       `13_Inches` == 1 ~ "13 Inches",
+       TRUE ~ "7 Inches"
+     )
+   ) %>%
+   select(-starts_with("Brand_"), -High_Resolution_264_ppi, -`16_GB`, -`32_GB`, -`64_GB`, -`128GB`, 
+          -Without_SDSlot, -`1.6_GHz`, -`2.2_GHz`, -`812_h._Runtime`, -`WLAN_+_UMTS/3G`, -`WLAN_+_LTE/4G`, 
+          -Sphone_Synch., -Cover, -Keyboard, -Mouse, -Pencil, -`32_GB_Memory_Card`, 
+          -`Keyboard_+_Pencil`, -`Keyboard_+_Mouse_+_Pencil`, -`50_Cash_Back`, -`100_Cash_Back`, 
+          -`150_Cash_Back`, -`8_Inches`, -`10_Inches`, -`12_Inches`, -`13_Inches`)
> # Relevel factors
> combined_data <- combined_data %>%
+   mutate(
+     Brand = relevel(factor(Brand), ref = "Outside Option"),
+     Resolution = relevel(factor(Resolution), ref = "Standard"),
+     Memory = relevel(factor(Memory), ref = "8GB"),
+     SD_Slot = relevel(factor(SD_Slot), ref = "With"),
+     Performance = relevel(factor(Performance), ref = "1 GHz"),
+     Battery_Run_Time = relevel(factor(Battery_Run_Time), ref = "4-8 hours"),
+     Connections = relevel(factor(Connections), ref = "WLAN"),
+     Sync_to_Smartphone = relevel(factor(Sync_to_Smartphone), ref = "No"),
+     Value_Pack = relevel(factor(Value_Pack), ref = "No"),
+     Equipment = relevel(factor(Equipment), ref = "None"),
+     Cash_Back = relevel(factor(Cash_Back), ref = "No Cash Back"),
+     Display_Size = relevel(factor(Display_Size), ref = "7 Inches")
+   )
> # Convert to mlogit format
> mlogit_data <- mlogit.data(
+   combined_data,
+   choice = "Chosen",
+   shape  = "long",
+   alt.var = "AltID",
+   chid.var = "chid"
+ )
> # 1.1 Base formula (no interactions)
> mnl_formula <- Chosen ~ 
+   Price + 
+   System_B + 
+   Brand + 
+   Resolution + 
+   Memory +
+   SD_Slot + 
+   Performance + 
+   Battery_Run_Time + 
+   Connections +
+   Sync_to_Smartphone + 
+   Value_Pack + 
+   Equipment + 
+   Cash_Back +
+   Display_Size | 0
> # 1.2 Estimate the BASE MNL model
> mnl_model <- mlogit(
+   formula = mnl_formula,
+   data    = mlogit_data
+ )
> # 1.3 View summary
> summary(mnl_model)

Call:
mlogit(formula = Chosen ~ Price + System_B + Brand + Resolution + 
    Memory + SD_Slot + Performance + Battery_Run_Time + Connections + 
    Sync_to_Smartphone + Value_Pack + Equipment + Cash_Back + 
    Display_Size | 0, data = mlogit_data, method = "nr")

Frequencies of alternatives:choice
      1       2       3       4 
0.22922 0.27982 0.22503 0.26592 

nr method
4 iterations, 0h:0m:2s 
g'(-H)^-1g = 3.59E-07 
gradient close to zero 

Coefficients :
                                     Estimate Std. Error  z-value  Pr(>|z|)    
Price                              -0.2008174  0.0066263 -30.3062 < 2.2e-16 ***
System_B                            0.0246109  0.0331488   0.7424 0.4578218    
BrandA                              0.2386235  0.0679642   3.5110 0.0004464 ***
BrandB                              0.4051072  0.0656535   6.1704 6.813e-10 ***
BrandC                              0.1421554  0.0785445   1.8099 0.0703157 .  
BrandD                             -0.1682400  0.0711896  -2.3633 0.0181146 *  
BrandE                             -0.2941234  0.0727747  -4.0416 5.310e-05 ***
BrandF                             -0.0962360  0.0691163  -1.3924 0.1638081    
BrandG                             -0.1255138  0.0714673  -1.7562 0.0790474 .  
ResolutionHigh                      0.0667515  0.0217670   3.0666 0.0021648 ** 
Memory128GB                         0.3501807  0.0357967   9.7825 < 2.2e-16 ***
Memory16GB                          0.1541137  0.0368092   4.1868 2.829e-05 ***
Memory32GB                          0.2183950  0.0362602   6.0230 1.712e-09 ***
Memory64GB                          0.3412295  0.0357583   9.5427 < 2.2e-16 ***
SD_SlotWithout                     -0.1839536  0.0275082  -6.6872 2.274e-11 ***
Performance1.6 GHz                  0.0802412  0.0262160   3.0608 0.0022077 ** 
Performance2.2 GHz                  0.1409665  0.0257989   5.4640 4.654e-08 ***
Battery_Run_Time8-12 hours          0.0826472  0.0217897   3.7929 0.0001489 ***
ConnectionsWLAN + LTE (4G)          0.2138867  0.0259288   8.2490 2.220e-16 ***
ConnectionsWLAN + UMTS (3G)         0.2079653  0.0260499   7.9833 1.332e-15 ***
Sync_to_SmartphoneYes               0.1087581  0.0217877   4.9917 5.985e-07 ***
Value_PackYes                       0.0565656  0.0248637   2.2750 0.0229045 *  
Equipment32GB Memory Card           0.0362549  0.0461561   0.7855 0.4321687    
EquipmentCover                      0.0137772  0.0463839   0.2970 0.7664478    
EquipmentKeyboard                   0.0549600  0.0457314   1.2018 0.2294409    
EquipmentKeyboard + Mouse + Pencil  0.1373377  0.0456955   3.0055 0.0026515 ** 
EquipmentKeyboard + Pencil          0.0713373  0.0458141   1.5571 0.1194458    
EquipmentMouse                     -0.0072622  0.0463190  -0.1568 0.8754129    
EquipmentPencil                    -0.0271308  0.0465177  -0.5832 0.5597342    
Cash_Back100 EUR                    0.2554544  0.0373527   6.8390 7.976e-12 ***
Cash_Back150 EUR                    0.1156120  0.0531584   2.1749 0.0296407 *  
Cash_Back50 EUR                     0.1267758  0.0276818   4.5798 4.655e-06 ***
Display_Size10 Inches               0.2450670  0.0387466   6.3249 2.535e-10 ***
Display_Size12 Inches               0.2674559  0.0389371   6.8689 6.469e-12 ***
Display_Size13 Inches               0.3040154  0.0383425   7.9289 2.220e-15 ***
Display_Size8 Inches                0.0997575  0.0305660   3.2637 0.0010998 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Log-Likelihood: -17763
> # 2.1 Log-likelihood
> log_likelihood <- logLik(mnl_model)
> # 2.2 Null log-likelihood (uniform distribution among 4 alts)
> num_tasks <- nrow(mlogit_data) / 4
> null_loglik_uniform <- num_tasks * log(1/4)
> # 2.3 Number of parameters (k) & observations (n)
> k <- length(coef(mnl_model))
> n <- nrow(mlogit_data)
> # 2.4 McFadden's R^2
> mcfadden_r2 <- 1 - (as.numeric(log_likelihood) / null_loglik_uniform)
> # 2.5 Likelihood Ratio (LR) test
> lr_test <- -2 * (null_loglik_uniform - as.numeric(log_likelihood))
> lr_p_value <- pchisq(lr_test, df = k, lower.tail = FALSE)
> # 2.6 AIC & BIC
> aic <- AIC(mnl_model) 
> bic <- -2 * as.numeric(log_likelihood) + k * log(n)
> # 2.7 Predictive accuracy
> mnl_predictions   <- predict(mnl_model, newdata = mlogit_data)
> predicted_choice  <- apply(mnl_predictions, 1, which.max)
> actual_choice     <- mlogit_data$AltID[mlogit_data$Chosen == 1]
> accuracy          <- mean(predicted_choice == actual_choice)
> accuracy_percent  <- accuracy * 100
> cat("\nMODEL FIT METRICS (Base Model)\n")

MODEL FIT METRICS (Base Model)
> cat("----------------------------------------\n")
----------------------------------------
> cat("Log-Likelihood (Model):  ", as.numeric(log_likelihood), "\n")
Log-Likelihood (Model):   -17763.16 
> cat("Log-Likelihood (Null):   ", null_loglik_uniform, "\n")
Log-Likelihood (Null):    -18850.83 
> cat("McFadden's R-squared:    ", mcfadden_r2, "\n")
McFadden's R-squared:     0.05769877 
> cat("Likelihood Ratio (LR):   ", lr_test, "\n")
Likelihood Ratio (LR):    2175.34 
> cat("LR Test p-value:         ", lr_p_value, "\n")
LR Test p-value:          0 
> cat("AIC:                     ", aic, "\n")
AIC:                      35598.32 
> cat("BIC:                     ", bic, "\n")
BIC:                      35918.86 
> cat("Predictive Accuracy (%): ", accuracy_percent, "\n")
Predictive Accuracy (%):  37.00544 
> # 4.2 2nd-order condition: Hessian negative definite
> hessian_eigs <- eigen(mnl_model$hessian)$values
> cat("\nHessian Eigenvalues (Base Model):\n")

Hessian Eigenvalues (Base Model):
> print(hessian_eigs)
 [1]    -31.34173   -150.09518   -302.94146   -318.47975   -362.13778   -424.23573   -673.22992   -847.51038   -863.29898   -913.06121   -925.41712
[12]   -936.31483   -959.91453   -975.29721   -989.21658  -1019.14937  -1062.40661  -1126.96539  -1161.31306  -1165.77180  -1199.05470  -1276.40736
[23]  -1560.09236  -1588.84939  -1687.78527  -1756.09126  -1815.88265  -1922.49990  -2032.35248  -2252.60246  -2307.40886  -2601.99040  -3250.55066
[34]  -3305.49453  -4937.39913 -88195.41406
> cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")

If all these eigenvalues are negative, we have a local maximum.
> #############################################
> ## 5. Model Specification and Estimation (INTERACTION MODEL)
> #############################################
> 
> # 5.1 Define formula with interactions
> #     We add Price:Cash_Back, Price:Brand, Battery_Run_Time:Price, Memory:SD_Slot
> 
> mnl_formula_interactions <- Chosen ~ 
+   # main effects
+   Price + 
+   System_B + 
+   Brand + 
+   Resolution + 
+   Memory +
+   SD_Slot + 
+   Performance + 
+   Battery_Run_Time + 
+   Connections +
+   Sync_to_Smartphone + 
+   Value_Pack + 
+   Equipment + 
+   Cash_Back +
+   Display_Size +
+   
+   # interaction effects
+   Price:Cash_Back +
+   # Price:Brand +
+   # Price:Performance +
+    Memory:SD_Slot |
+   0
> 
> # 5.2 Estimate the MNL model with interactions
> mnl_model_int <- mlogit(
+   formula = mnl_formula_interactions,
+   data    = mlogit_data
+ )
> 
> # 5.3 View summary
> summary(mnl_model_int)

Call:
mlogit(formula = Chosen ~ Price + System_B + Brand + Resolution + 
    Memory + SD_Slot + Performance + Battery_Run_Time + Connections + 
    Sync_to_Smartphone + Value_Pack + Equipment + Cash_Back + 
    Display_Size + Price:Cash_Back + Memory:SD_Slot | 0, data = mlogit_data, 
    method = "nr")

Frequencies of alternatives:choice
      1       2       3       4 
0.22922 0.27982 0.22503 0.26592 

nr method
5 iterations, 0h:0m:3s 
g'(-H)^-1g = 2.49E-06 
successive function values within tolerance limits 

Coefficients :
                                     Estimate Std. Error  z-value  Pr(>|z|)    
Price                              -0.1845817  0.0089093 -20.7179 < 2.2e-16 ***
System_B                            0.0243725  0.0331712   0.7347 0.4624919    
BrandA                              0.2696641  0.0805715   3.3469 0.0008172 ***
BrandB                              0.4332673  0.0785625   5.5149 3.489e-08 ***
BrandC                              0.1741965  0.0897251   1.9414 0.0522040 .  
BrandD                             -0.1348678  0.0832848  -1.6194 0.1053705    
BrandE                             -0.2652041  0.0843489  -3.1441 0.0016658 ** 
BrandF                             -0.0687078  0.0812571  -0.8456 0.3977978    
BrandG                             -0.0990117  0.0834463  -1.1865 0.2354122    
ResolutionHigh                      0.0696010  0.0217937   3.1936 0.0014050 ** 
Memory128GB                         0.1972127  0.0669640   2.9451 0.0032290 ** 
Memory16GB                          0.0825189  0.0693306   1.1902 0.2339588    
Memory32GB                          0.1763240  0.0678933   2.5971 0.0094021 ** 
Memory64GB                          0.2306386  0.0666173   3.4621 0.0005359 ***
SD_SlotWithout                     -0.3037396  0.0622094  -4.8825 1.047e-06 ***
Performance1.6 GHz                  0.0809989  0.0262681   3.0835 0.0020455 ** 
Performance2.2 GHz                  0.1423568  0.0258416   5.5088 3.613e-08 ***
Battery_Run_Time8-12 hours          0.0806202  0.0218393   3.6915 0.0002229 ***
ConnectionsWLAN + LTE (4G)          0.2140569  0.0259648   8.2441 2.220e-16 ***
ConnectionsWLAN + UMTS (3G)         0.2106696  0.0260906   8.0746 6.661e-16 ***
Sync_to_SmartphoneYes               0.1098389  0.0218225   5.0333 4.821e-07 ***
Value_PackYes                       0.0514139  0.0250970   2.0486 0.0405009 *  
Equipment32GB Memory Card           0.0333838  0.0462144   0.7224 0.4700687    
EquipmentCover                      0.0139998  0.0464067   0.3017 0.7628988    
EquipmentKeyboard                   0.0557175  0.0457708   1.2173 0.2234843    
EquipmentKeyboard + Mouse + Pencil  0.1333194  0.0457446   2.9144 0.0035634 ** 
EquipmentKeyboard + Pencil          0.0745485  0.0458518   1.6259 0.1039794    
EquipmentMouse                     -0.0088815  0.0463164  -0.1918 0.8479321    
EquipmentPencil                    -0.0247327  0.0465538  -0.5313 0.5952305    
Cash_Back100 EUR                    0.3255803  0.1088400   2.9914 0.0027773 ** 
Cash_Back150 EUR                    0.1238685  0.2738697   0.4523 0.6510601    
Cash_Back50 EUR                     0.2806422  0.0560239   5.0093 5.462e-07 ***
Display_Size10 Inches               0.2412112  0.0391265   6.1649 7.052e-10 ***
Display_Size12 Inches               0.2621950  0.0392585   6.6787 2.411e-11 ***
Display_Size13 Inches               0.3040934  0.0384995   7.8986 2.887e-15 ***
Display_Size8 Inches                0.0999914  0.0305846   3.2693 0.0010780 ** 
Price:Cash_Back100 EUR             -0.0196766  0.0193466  -1.0171 0.3091242    
Price:Cash_Back150 EUR             -0.0097229  0.0388134  -0.2505 0.8021971    
Price:Cash_Back50 EUR              -0.0422340  0.0134921  -3.1303 0.0017464 ** 
Memory128GB:SD_SlotWithout          0.2292556  0.0839442   2.7310 0.0063133 ** 
Memory16GB:SD_SlotWithout           0.1105726  0.0866677   1.2758 0.2020182    
Memory32GB:SD_SlotWithout           0.0661913  0.0844182   0.7841 0.4329886    
Memory64GB:SD_SlotWithout           0.1715502  0.0840634   2.0407 0.0412784 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Log-Likelihood: -17753
> 
> 
> #############################################
> ## 6. Compute Model Fit Metrics (INTERACTION MODEL)
> #############################################
> 
> # 6.1 Log-likelihood
> log_likelihood_int <- logLik(mnl_model_int)
> 
> # 6.2 Same null log-likelihood as before
> #     (still uniform across 4 alts)
> null_loglik_int <- null_loglik_uniform
> 
> # 6.3 Number of parameters (k2) & observations (n2)
> k2 <- length(coef(mnl_model_int))
> n2 <- nrow(mlogit_data)
> 
> # 6.4 McFadden's R^2
> mcfadden_r2_int <- 1 - (as.numeric(log_likelihood_int) / null_loglik_int)
> 
> # 6.5 Likelihood Ratio (LR) test
> lr_test_int <- -2 * (null_loglik_int - as.numeric(log_likelihood_int))
> lr_p_value_int <- pchisq(lr_test_int, df = k2, lower.tail = FALSE)
> 
> # 6.6 AIC & BIC
> aic_int <- AIC(mnl_model_int) 
> bic_int <- -2 * as.numeric(log_likelihood_int) + k2 * log(n2)
> 
> # 6.7 Predictive accuracy
> mnl_predictions_int <- predict(mnl_model_int, newdata = mlogit_data)
> predicted_choice_int <- apply(mnl_predictions_int, 1, which.max)
> actual_choice_int    <- mlogit_data$AltID[mlogit_data$Chosen == 1]
> accuracy_int         <- mean(predicted_choice_int == actual_choice_int)
> accuracy_percent_int <- accuracy_int * 100
> 
> #############################################
> ## 7. Print Model Fit Results (INTERACTION MODEL)
> #############################################
> 
> cat("\nMODEL FIT METRICS (Interaction Model)\n")

MODEL FIT METRICS (Interaction Model)
> cat("----------------------------------------\n")
----------------------------------------
> cat("Log-Likelihood (Model):  ", as.numeric(log_likelihood_int), "\n")
Log-Likelihood (Model):   -17753.34 
> cat("Log-Likelihood (Null):   ", null_loglik_int, "\n")
Log-Likelihood (Null):    -18850.83 
> cat("McFadden's R-squared:    ", mcfadden_r2_int, "\n")
McFadden's R-squared:     0.05821953 
> cat("Likelihood Ratio (LR):   ", lr_test_int, "\n")
Likelihood Ratio (LR):    2194.973 
> cat("LR Test p-value:         ", lr_p_value_int, "\n")
LR Test p-value:          0 
> cat("AIC:                     ", aic_int, "\n")
AIC:                      35592.69 
> cat("BIC:                     ", bic_int, "\n")
BIC:                      35975.56 
> cat("Predictive Accuracy (%): ", accuracy_percent_int, "\n")
Predictive Accuracy (%):  37.34373 
> 
> 
> #############################################
> ## 8. Diagnostics (INTERACTION MODEL)
> #############################################
> 
> # 8.1 1st-order condition
> #     Check summary(mnl_model_int) for gradient info
> 
> # 8.2 2nd-order condition: Hessian negative definite
> hessian_eigs_int <- eigen(mnl_model_int$hessian)$values
> cat("\nHessian Eigenvalues (Interaction Model):\n")

Hessian Eigenvalues (Interaction Model):
> print(hessian_eigs_int)
 [1]     -12.98983     -17.21087     -55.91149     -82.56758    -163.06268    -180.31520    -193.44323    -203.44369    -312.87074    -359.69081
[11]    -392.45300    -450.68963    -713.76538    -867.77938    -913.24984    -916.84894    -935.65666    -958.12303    -971.24643    -988.82322
[21]   -1022.89108   -1066.23061   -1127.38374   -1161.92018   -1170.79796   -1230.71064   -1282.60328   -1573.01824   -1811.84780   -2024.57558
[31]   -2217.73508   -2428.34308   -2466.80739   -2545.56119   -2625.24649   -2697.54915   -3264.32761   -3308.73069   -4658.62429   -9567.82917
[41]  -32510.49117  -46278.00696 -107176.86894
> cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")

If all these eigenvalues are negative, we have a local maximum.
