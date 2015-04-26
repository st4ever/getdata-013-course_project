require(knitr)
require(markdown)
knit("run_analysis.Rmd", encoding="UTF-8")
markdownToHTML("run_analysis.md", "run_analysis.html")