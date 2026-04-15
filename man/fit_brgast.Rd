\name{fit_brgast}
\alias{fit_brgast}
\title{Fit a Break-Regime GAS-t Model}
\description{
Fits the BR-GAS-t model by maximum likelihood.
}
\usage{
fit_brgast(y, indicator, start = NULL)
}
\arguments{
  \item{y}{A numeric return series.}
  \item{indicator}{A 0/1 break-regime indicator with the same length as \code{y}.}
  \item{start}{Optional starting values on the unconstrained optimization scale.}
}
\value{
An object of class \code{"brgas_fit"} containing coefficients, fit statistics,
filtered states, and the original inputs.
}
\examples{
sample_df <- sample_hbea_data()
fit <- fit_brgast(sample_df$ret, sample_df$post_break)
print(fit)
}

