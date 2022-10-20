# Must manually move image files from eia/ to eia/vignettes/ after knit

time = Sys.time()
library(knitr)
knit(here::here("vignettes/benchmark.Rmd.orig"), here::here("vignettes/benchmark.Rmd"))
# unlink(here::here("plot-benchmark-small-1.png"))
unlink(here::here("plot-benchmark-1.png"))
Sys.time() - time
