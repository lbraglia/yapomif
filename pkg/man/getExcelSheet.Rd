\name{getExcelSheet}
\alias{getExcelSheet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Retrieve an MS Excel (R) sheet from an xls or xlsx file.
}
\description{
  
  This function is a wrapper around RODBC function to get a sheet from
  an xls or xlsx file.
  
}

\usage{
getExcelSheet(file=NULL, sheet=NULL,  ...)
}

\arguments{
  \item{file}{path to the xls/xlsx file}
  \item{sheet}{sheet name}
  \item{...}{other arguments passed to sqlFetch)}
  
}

% \details{
  

% }

\value{
  The function return a data.frame with data from the specified sheet.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Excel }
\keyword{ sheet }
\keyword{ get }

