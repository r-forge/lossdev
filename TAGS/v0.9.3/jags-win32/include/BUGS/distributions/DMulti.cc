#include <config.h>
#include "DMulti.h"
#include <rng/RNG.h>
#include <util/dim.h>
#include <util/nainf.h>

#include <stdexcept>
#include <cmath>

#include <JRmath.h>

using std::vector;
using std::length_error;
using std::logic_error;

#define PROB(par) (par[0])
#define SIZE(par) (*par[1])

DMulti::DMulti()
  : VectorDist("dmulti", 2) 
{}

bool DMulti::isDiscreteValued(vector<bool> const &mask) const
{
    return true;
}

bool DMulti::checkParameterLength(vector<unsigned int> const &len) const
{
    //Check that PROB is a vector and SIZE is a scalar
    return len[0] > 1  && len[1] == 1;
}

bool DMulti::checkParameterDiscrete(vector<bool> const &mask) const
{
    return mask[1]; //SIZE is discrete-valued
}

bool 
DMulti::checkParameterValue(vector<double const *> const &par,
			    vector<unsigned int> const &len) const
{
    if (SIZE(par) < 1)
	return false;

    for (unsigned int i = 0; i < len[0]; ++i) {
	if (PROB(par)[i] < 0)
	    return false;
    }

    return true;
}

double DMulti::logLikelihood(double const *x, unsigned int length,
			     vector<double const *> const &par,
			     vector<unsigned int> const &len,
			     double const *lower, double const *upper) const
{
    double loglik = 0.0;
    double sump = 0.0;
    for (unsigned int i = 0; i < length; i++) {
	if (x[i] != 0) {
	    if (PROB(par)[i] == 0) {
		return JAGS_NEGINF;
	    }
	    else {
		loglik += x[i] * log(PROB(par)[i]) - lgamma(x[i] + 1);
	    }
	}
	sump += PROB(par)[i];
    }
    loglik += lgamma(SIZE(par) + 1) - SIZE(par) * log(sump);

    return loglik;
}

void DMulti::randomSample(double *x, unsigned int length,
			  vector<double const *> const &par,
			  vector<unsigned int> const &len,
			  double const *lower, double const *upper,
			  RNG *rng) const
{
    /* Sample multinomial as a series of binomial distributions */

    double N = SIZE(par);
    double const *prob = PROB(par);

    //Normalize probability
    double sump = 0;
    for (unsigned int i = 0; i < length; ++i) {
	sump += PROB(par)[i];
    }

    for (unsigned int i = 0; i < length - 1; i++) {
	if (N == 0) {
	    x[i] = 0;
	}
	else {
	    x[i] = rbinom(N, prob[i]/sump, rng);
	    N -= x[i];
	    sump -= prob[i];
	}
    }
    x[length - 1] = N;
}

void DMulti::support(double *lower, double *upper, unsigned int length,
	     vector<double const *> const &par,
	     vector<unsigned int> const &len) const
{
    for (unsigned int i = 0; i < length; ++i) {
	lower[i] = 0;
        if (PROB(par)[i] == 0) 
           upper[i] = 0;
        else
	   upper[i] = SIZE(par);
    }
}

unsigned int DMulti::length(vector<unsigned int> const &len) const
{
    return len[0];
}


void DMulti::typicalValue(double *x, unsigned int length,
			  vector<double const *> const &par,
			  vector<unsigned int> const &len,
			  double const *lower, double const *upper) const
{
    /* Draw a typical value in the same way as a random sample, but
       substituting the median at each stage */

    double N = SIZE(par);
    double const *prob = PROB(par);

    double sump = 0.0;
    for (unsigned int i = 0; i < length; ++i) {
	sump += prob[i];
    }

    for (unsigned int i = 0; i < length - 1; i++) {
	if (N == 0) {
	    x[i] = 0;
	}
	else {
	    x[i] = qbinom(0.5, N, prob[i]/sump, true, false);
	    N -= x[i];
	    sump -= prob[i];
	}
    }
    x[length - 1] = N;
}

bool DMulti::isSupportFixed(std::vector<bool> const &fixmask) const
{
    return fixmask[1];
}

unsigned int DMulti::df(vector<unsigned int> const &len) const
{
    return len[0] - 1;
} 
