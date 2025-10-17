

devtools::load_all()
devtools::document()  # Actualiza documentación
devtools::build()     # Arma el paquete para mi
# devtools::test()      # Ejecuta pruebas
devtools::check()     # Verifica el paquete
# Instalar el paquete
devtools::install()

######################################
# Desinstalar el paquete
remove.packages("exp002RShiny")
remove.packages("Rscience.import")
remove.packages("Rscience.menu")
remove.packages("Rscience.GeneralLM")

# Limpiar el caché de devtools
devtools::clean_dll()

# Regenerar documentación
devtools::document()


remotes::install_github("deliaspanigo/Rscience.import")
remotes::install_github("deliaspanigo/Rscience.menu")
remotes::install_github("deliaspanigo/Rscience.GeneralLM")
devtools::install()

# Instalar nuevamente
devtools::install()
########################################
library(Rscience2)
Rscience2::run_app()

library(Rscience2)
Rscience2::run_app()

library(Rscience.GeneralLM)
Rscience.GeneralLM::run_app()

remotes::install_github("deliaspanigo/Rscience.import")

#########################################
remotes::install_github("deliaspanigo/Rscience2")
library(Rscience2)
Rscience2::run_app()

