% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_file_utils.R
\name{get_input_file}
\alias{get_input_file}
\title{Load an ASCII input file}
\usage{
get_input_file(infile = "nofile")
}
\arguments{
\item{infile}{File name with full path.
If no file is specified, a window opens to choose a file interactively.}
}
\value{
A tibble with each row corresponding to one line in the input file.
}
\description{
TerrainWorks scripts read instructions and parameter values
from an ASCII file. These files are formatted using a "keyword: argument"
format. The keywords and associated arguments can come in any order
and each keyword may have any number of arguments, including no argument.
Arguments use a "parameter = value" format, with each argument separated
by a comma. For example, here is the keyword: argument input line for
specifying reach length in a synthetic channel network:
REACH LENGTH: FIXED=100., BREAK AT JUNCTIONS.
Here, "REACH LENGTH" is the keyword, there are two arguments, one of which
requires a value.
}
\examples{
get_input_file("c:\\\\data\\\\Umpqua\\\\input_bldgrds.txt")
  opens the specified file.

get_input_file() opens a Windows Explorer window for interactive
  file selection.

}
\seealso{
* [get_keyword()] extracts a keyword from an input file line.
* [get_args()] extracts the arguments from an input line.
* [parse_arg()] extracts the parameter and value from a single argument.
}
