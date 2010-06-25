#ifndef VECTOR_DIST_H_
#define VECTOR_DIST_H_

#include <distribution/Distribution.h>

#include <vector>
#include <string>

struct RNG;

/**
 * @short Vector-valued Distribution 
 *
 * The VectorDist class represents distributions for which either the
 * value or the parameters are vectors.
 */
class VectorDist : public Distribution
{
  public:
    /**
     * Constructor.
     * @param name name of the distribution as used in the BUGS language
     * @param npar number of parameters, excluding upper and lower bounds
     */
    VectorDist(std::string const &name, unsigned int npar);
    /**
     * @param x Value at which to evaluate the likelihood.
     *
     * @param parameters Vector of parameter values of the
     * distribution.
     * 
     * @param lengths Vector of parameter lengths corresponding to the
     * parameter vector.
     *
     * @returns the log likelihood.  If the likelihood should be zero
     * because x is inconsistent with the parameters then -Inf is
     * returned. If the parameters are invalid
     * (i.e. checkParameterValue returns false), then the return value
     * is undefined.
     * 
     */
    virtual double 
	logLikelihood(double const *x, unsigned int length,
		      std::vector<double const *> const &parameters,
		      std::vector<unsigned int> const &lengths,
		      double const *lbound, double const *ubound) const = 0;
    /**
     * Draws a random sample from the distribution. 
     *
     * @param x Array to which the sample values are written
     *
     * @param parameters  Vector of parameter values at which
     * to evaluate the likelihood. This vector should be of length
     * npar().
     *
     * @param rng pseudo-random number generator to use.
     *
     * @exception length_error 
     */
    virtual void randomSample(double *x, unsigned int length,
			      std::vector<double const *> const &parameters,
			      std::vector<unsigned int> const &lengths, 
			      double const *lbound, double const *ubound,
			      RNG *rng) const = 0;
    /**
     * Returns a typical value from the distribution.  The meaning of
     * this will depend on the distribution, but it will normally be a
     * mean, median or mode.
     *
     * @param x Array to which the sample values are written
     *
     * @param parameters  Vector of parameter values at which
     * to evaluate the likelihood. This vector should be of length
     * npar().
     *
     * @param dims Vector of parameter dimensions.
     *
     * @param lbound Lower bound, for truncated distributions, or a NULL
     * pointer if the distribution is not truncated.
     *
     * @param ubound Upper bound, for truncated distributions, or a NULL
     * pointer if the distribution is not truncated.

     *
     * @exception length_error 
     */
    virtual void typicalValue(double *x, unsigned int length,
			      std::vector<double const *> const &parameters,
			      std::vector<unsigned int> const &lengths,
			      double const *lbound, double const *ubound)
	const = 0;
    /**
     * Returns the support of an unbounded distribution
     */
    virtual void support(double *lower, double *upper, unsigned int length,
			 std::vector<double const *> const &params, 
			 std::vector<unsigned int> const &lengths) const = 0;
    /**
     * Indicates whether the support of the distribution is fixed.
     *
     * @param fixmask Boolean vector of length npar() indicating which
     * parameters have fixed values.
     */
    virtual bool isSupportFixed(std::vector<bool> const &fixmask) const = 0;
    /**
     * Checks that dimensions of the parameters are correct.
     *
     * This function only needs to be run once for each parameter
     * vector. Thereafter, the values of the parameters will change,
     * but the dimensions will not.
     */
    virtual bool 
	checkParameterLength (std::vector<unsigned int> const &parameters) 
	const = 0;
    /**
     * Checks that the values of the parameters are consistent with
     * the distribution. For example, some distributions require
     * than certain parameters are positive, or lie in a given
     * range.
     *
     * This function assumes that checkParameterDim returns true.
     */
    virtual bool 
	checkParameterValue(std::vector<double const *> const &parameters,
			    std::vector<unsigned int> const &lengths) const = 0;
    /**
     * Calculates what the length of the distribution should be, based
     * on the lengths of its parameters.
     *
     * @param par vector of lengths of the parameters.
     */
    virtual unsigned int 
	length (std::vector<unsigned int> const &par) const = 0;
    /**
     * Returns the number of degrees of freedom of the distribution
     * given the parameter lengths. By default this is the same as
     * VectorDist#length. However, some distributions are constrained:
     * and the support occupies a lower dimensional subspace. In this
     * case, the df member function must be overrideen.
     */
    virtual unsigned int df(std::vector<unsigned int> const &lengths) const;
};

#endif /* VECTOR_DISTRIBUTION_H_ */
