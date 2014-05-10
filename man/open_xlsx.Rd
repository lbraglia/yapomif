\name{open_xlsx}
\alias{open_xlsx}
\title{Open an xlsx file}
\usage{
open_xlsx(xlsx=NULL, bin=getOption("open_xlsx.bin", find_xlsx_bin()))
}
\arguments{
  \item{xlsx}{path to the \code{xlsx} file}

  \item{bin}{path to the application bin}
}
\description{
This function tries to open an xlsx file with the proper
application, in a portable manner. The user can set up the
application, giving its path (or setting the option
\code{open_xlsx.bin}); otherwise the handler path is
seeked.
}
\author{
Luca Braglia
}

