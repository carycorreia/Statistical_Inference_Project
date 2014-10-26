# Set working directory
setwd("/users/carycorreia/documents/Coursera_Assignments/Statistical_Inference_Project/")

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("StatInferProject.Rmd")
markdownToHTML('StatInferProject.md', 'StatInferProject.html', options=c("use_xhml"))
system("pandoc -s StatInferProject.html -o StatInferProject.pdf")