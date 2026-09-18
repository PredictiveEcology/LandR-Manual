## install packages from the project DESCRIPTION
remotes::install_deps()

## install the additional packages the module chapters need.
## Not piped into Require::Install(): its first parameter is named `packages` and
## it calls substitute() on it, so a piped expression resolves to the literal
## string "packages" and pak is asked for a package by that name. pkgDep2() also
## returns a named list, which Install() wants flattened.
modPkgs <- SpaDES.core::packages(modules = list.files("modules"), paths = "modules") |>
  unlist() |>
  unique()
modDeps <- unique(unlist(Require::pkgDep2(modPkgs)))
Require::Install(modDeps)
