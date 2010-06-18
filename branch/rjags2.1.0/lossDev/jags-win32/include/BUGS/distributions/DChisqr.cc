#include <config.h>
#include "DChisqr.h"

#include <JRmath.h>

using std::vector;

#define DF(par) (*par[0])

DChisqr::DChisqr()
    : RScalarDist("dchisqr", 1, DIST_POSITIVE)
{}


bool 
DChisqr::checkParameterValue (vector<double const *> const &par) const
{
    return (DF(par) > 0);
}

double 
DChisqr::d(double x, vector<double const *> const &par, bool give_log) const
{
    return dchisq(x, DF(par), give_log);
}

double 
DChisqr::p(double q, vector<double const *> const &par, bool lower, bool log_p)
  const
{
    return pchisq(q, DF(par), lower, log_p);
}

double
DChisqr::q(double p, vector<double const *> const &par, bool lower, bool log_p)
const
{
    return qchisq(p, DF(par), lower, log_p);
}

double DChisqr::r(vector<double const *> const &par, RNG *rng) const
{
    return rchisq(DF(par), rng);
}
