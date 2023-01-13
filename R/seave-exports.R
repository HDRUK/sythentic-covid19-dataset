# Export the "Student" C++ class by explicitly requesting Student be
# exported via roxygen2's export tag.
#' @export Variant
loadModule(module = "RcppSeaveEx", TRUE)

#' @export Pandemic
loadModule(module = "RcppSeavePandemic", TRUE)

#' @export Person
loadModule(module = "RcppSeavePerson", TRUE)

#' @export Population
loadModule(module = "RcppSeavePopulation", TRUE)

