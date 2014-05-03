\name{getAccessTable}
\alias{getAccessTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Retrieve an MS Access (R) table from an mdb or accdb file.
}
\description{
  
  This function is a wrapper around RODBC function to get a table from
  an mdb or accdb file.
  
}

\usage{
getAccessTable(file=NULL, table=NULL, user="admin", pw=NULL, ...)
}

\arguments{
  \item{file}{path to the mdb/accdb file}
  \item{table}{table name}
  \item{user}{username (login to the file)}
  \item{pw}{password (login to the file)}
  \item{...}{other arguments passed to sqlFetch)}
  
}

% \details{
  

% }

\value{
  The function return a data.frame with data from the specified table.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Access }
\keyword{ table }
\keyword{ get }

