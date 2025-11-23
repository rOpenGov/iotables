devtools::document()
devtools::check(build_args = "--compact-vignettes=gs+qpdf") # local run
# Then try rd2pdf locally to confirm LaTeX is happy
tools::Rd2pdf("man", output = "manual.pdf")
