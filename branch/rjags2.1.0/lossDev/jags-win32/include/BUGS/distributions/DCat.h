#ifndef DCAT_H_
#define DCAT_H_

#include <distribution/VectorDist.h>

/**
 * @short Categorical distribution
 * <pre>
 * R ~ dcat(p[])
 * f(r|p[]) = p[r] ; r in 1:dim(p)
 * </pre>
 */
class DCat : public VectorDist {
public:
    DCat();

    double logLikelihood(double const *x, unsigned int length,
			 std::vector<double const *> const &parameters,
			 std::vector<unsigned int> const &lengths,
			 double const *lower, double const *upper) const;
    void randomSample(double *x, unsigned int length,
		      std::vector<double const *> const &parameters,
		      std::vector<unsigned int> const &lengths,
		      double const *lbound, double const *ubound,
		      RNG *rng) const;
    void typicalValue(double *x, unsigned int length,
		      std::vector<double const *> const &parameters,
		      std::vector<unsigned int> const &lengths,
		      double const *lbound, double const *ubound) const;
    /**
     * Checks that all elements of p are positive
     */
    bool checkParameterValue(std::vector<double const*> const &parameters,
			     std::vector<unsigned int> const &lengths) const;
    void support(double *lower, double *upper, unsigned int length,
		 std::vector<double const *> const &parameters,
		 std::vector<unsigned int> const &lengths) const;
    bool isSupportFixed(std::vector<bool> const &fixmask) const;
    bool isDiscreteValued(std::vector<bool> const &mask) const;
    bool checkParameterLength(std::vector<unsigned int> const &lengths) const;
    unsigned int length(std::vector<unsigned int> const &lengths) const;
};

#endif /* DCAT_H_ */
