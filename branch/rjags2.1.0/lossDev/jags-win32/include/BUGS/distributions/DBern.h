#ifndef DBERN_H_
#define DBERN_H_

#include "RScalarDist.h"

/**
 * @short Bernoulli distribution
 * <pre>
 * R ~ dbern(p)
 * f(r | p) = p^r * (1 - p)^(1 -r) ; r in 0:1
 * </pre>
 */
class DBern : public ScalarDist {
public:
    DBern();
    double logLikelihood(double x, 
			 std::vector<double const *> const &parameters,
			 double const *lbound, double const *ubound) const;
    double randomSample(std::vector<double const *> const &parameters, 
			double const *lbound, double const *ubound,
			RNG *rng) const;
    double typicalValue(std::vector<double const *> const &parameters,
			double const *lbound, double const *ubound) const;
    /** Checks that p lies in the open interval (0,1) */
    bool checkParameterValue(std::vector<double const *> const &parameters) 
	const;
    /** Bernoulli distribution cannot be bounded */
    bool canBound() const;
    /** Bernoulli distribution is discrete valued */
    bool isDiscreteValued(std::vector<bool> const &mask) const;
};

#endif /* DBERN_H_ */
