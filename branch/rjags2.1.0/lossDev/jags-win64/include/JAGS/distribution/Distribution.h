#ifndef DISTRIBUTION_H_
#define DISTRIBUTION_H_

#include <vector>
#include <string>

struct RNG;

/**
 * @short Distribution of a random variable
 *
 * Distribution objects contain only constant data members and all
 * member functions are constant. Hence only one object needs to be
 * instantiated for each subclass.
 *
 * The DistTab class provides a convenient way of storing Distribution
 * objects and referencing them by name.
 *
 * @see DistTab
 */
class Distribution
{
    const std::string _name;
    const unsigned int _npar;
public:
    /**
     * Constructor.
     * @param name name of the distribution as used in the BUGS language
     * @param npar number of parameters, excluding upper and lower bounds
     */
    Distribution(std::string const &name, unsigned int npar);
    virtual ~Distribution();
    /**
     * @returns the BUGS language name of the distribution
     */
    std::string const &name() const;
    /**
     * Indicates whether the support of the distribution is fixed.
     *
     * @param fixmask Boolean vector indicating which parameters have
     * fixed values.
     */
    virtual bool isSupportFixed(std::vector<bool> const &fixmask) const = 0;
    /**
     * Checks that a vector of parameters of length npar is consistent
     * with the distribution.
     */
    bool checkNPar(unsigned int npar) const;
    /**
     * Some distributions require some of the parameters to be discrete
     * valued. As most distributions do not require discrete valued paremeters,
     * a default implementation is provided which always returns true.
     *
     * @param mask Boolean vector indicating which parameters are
     * discrete valued.
     */
    virtual bool checkParameterDiscrete(std::vector<bool> const &mask) const;
    /**
     * Returns true if the distribution has support on the integers.The
     * default implementation returns false, so this must be overridden
     * for discrete-valued distributions.
     *
     * @param mask Vector indicating whether parameters are discrete
     * or not. Most implementations will ignore this argument, as a
     * distribution normally has support either on the real line or on
     * the integers. However, this argument is required in order to
     * support observable functions, for which the support may depend
     * on the arguments.
     *
     * @see Function#isDiscreteValued
     */
    virtual bool isDiscreteValued(std::vector<bool> const &mask) const;
    /**
     * Tests for a location parameter.  A parameter of a distribution
     * is considered to be a location parameter if, when it's value is
     * incremented by X, the whole distribution is shifted by X,
     * indpendently of the other parameter values.
     * 
     * This is a virtual function, for which the default implementation
     * always returns false. Distributions with location parameters must
     * overload this function.
     *
     * @param index Index number (starting from 0) of the parameter to be
     * tested.
     */
    virtual bool isLocationParameter(unsigned int index) const;
    /**
     * Tests for a scale parameter.  A parameter of a distribution is
     * considered to be a scale parameter if, when it's value is
     * multiplied by X, the whole distribution multiplied by X,
     * indpendently of the other parameter values.
     * 
     * Note that this definition excludes "location-scale" models:
     * i.e. if the density of y takes the form (1/b)*f((y-a)/b) then b
     * is not considered a scale parameter.
     *
     * This is a virtual function, for which the default
     * implementation always returns false. Distributions with scale
     * parameters must overload this function.
     *
     * @param index Index number (starting from 0) of the parameter to
     * be tested.
     */
    virtual bool isScaleParameter(unsigned int index) const;
    /**
     * Indicates whether the distribution can be bounded. The default
     * implementation returns false.
     */
    virtual bool canBound() const;
};

#endif /* DISTRIBUTION_H_ */
