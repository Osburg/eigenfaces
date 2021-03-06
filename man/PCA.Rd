% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageset_ef.R
\name{PCA}
\alias{PCA}
\title{Performs principle component analysis for image files}
\usage{
PCA(td, showEigenvals = TRUE, quick = FALSE)
}
\arguments{
\item{td}{an object of class 'imageset_ef', training data.}

\item{showEigenvals}{logical vector (TRUE or FALSE).}

\item{quick}{logical vector (TRUE or FALSE).}
}
\value{
list of length 1 (when showEigenvals <- FALSE; contains an 'imageset_ef' object consisting of the eigenvectors as 'image_ef' objects) or 2 (when showEigenvals <- TRUE; additionally contains a list of the eigenvalues).
}
\description{
Calculates the data covariance matrix of the original data.
Returns eigenvectors (and eigenvalues) of the covariance matrix as an 'imageset_ef' object.
}
\details{
\code{td} is the 'imageset_ef'-object where the images are saved as 'image_ef' objects. \code{showEigenvals} determins
whether the eigenvalues are returned in addition to the eigenvectors (FALSE means only the eigenvectors are returned).
When the number of pixels of each images is much bigger than the number of images in \code{td} it is faster to diagonalize t(A) \%*\% A
instead of the covariance matrix. However, this causes that only a subset of the eigenvectors of the covariance matrix is returned.
If quick is set TRUE, this option is activated.
}
\examples{
# Import Olivetti-faces
olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
td <- load_imageset_ef(olivetti, c(64,64))
PCA(td, showEigenvals = FALSE, quick=TRUE)
}
\references{
Marinovsky F., Wagner P., Gesichtserkennung mit Eigenfaces, FH Zittau/Görlitz
}
