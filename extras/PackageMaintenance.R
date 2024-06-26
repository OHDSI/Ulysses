# @file PackageMaintenance
#
# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# 1) Format and check code --------------
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("Ulysses")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()


# 2) Create manual and vignettes: --------------------------
unlink("extras/Ulysses.pdf")
# for windows
shell("R CMD Rd2pdf ./ --output=extras/Ulysses.pdf")

# 3) Add vignette pdf ------------------------------

# create pdf_vignette path
dir.create(path = "./extras/pdf_vignette/", showWarnings = FALSE)

# render markdown to pdf

### start study
rmarkdown::render("vignettes/start_study.Rmd",
                  output_file = "../extras/pdf_vignette/start_study.pdf",

  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/start_study.tex")


### credentials
rmarkdown::render("vignettes/credentials.Rmd",
                  output_file = "../extras/pdf_vignette/credentials.pdf",

                  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/credentials.tex")


### atlas
rmarkdown::render("vignettes/atlas.Rmd",
                  output_file = "../extras/pdf_vignette/atlas.pdf",

                  rmarkdown::pdf_document(latex_engine = "pdflatex", toc = TRUE, number_sections = TRUE))
unlink("extras/pdf_vignette/atlas.tex")

# 4) build site ------------------------
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

