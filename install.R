## install packages from the project DESCRIPTION
remotes::install_deps()

## install the additional packages the module chapters need
SpaDES.docs::installModulePkgs("modules")
