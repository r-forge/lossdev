#ifndef METROPOLIS_H_
#define METROPOLIS_H_

#include <sampler/SampleMethod.h>
#include <vector>

class StochasticNode;

/**
 * @short Metropolis-Hastings sampling method
 *
 * This class is used by Metropolis Hastings samplers.  It provides
 * only basic infrastructure.  
 *
 * The Metropolis class provides no update member function. A subclass
 * of Metropolis must provide this. It should contain one or more
 * calls to Metropolis#setValue, calculate the acceptance probability,
 * and then call the function Metropolis#accept.
 */
class Metropolis : public SampleMethod
{
    std::vector<double> _last_value;
    bool _adapt;
    Metropolis(Metropolis const &);
    Metropolis &operator=(Metropolis const &);
public:
    Metropolis(std::vector<double> const &value);
    ~Metropolis();
    /**
     * Gets the current value array of the Metropolis object. 
     */
    virtual void getValue(std::vector<double> &value) const = 0;
    /**
     * Sets the value of the Metropolis object. 
     *
     * @param value Pointer to the beginning of an array of values
     *
     * @param length Length of the supplied value array
     */
    virtual void setValue(std::vector<double> const &value) = 0;
    /**
     * Accept current value with probabilty p. If the current value is
     * not accepted, the Metropolis object reverts to the value at the
     * last successful call to accept. The first call to accept is
     * always successful and neither rng nor prob is referenced. A
     * subclass of Metropolis may therefore call accept in the
     * constructor in order to store the initial value.
     *
     * @param rng Random number generator.
     *  
     * @param prob Probability of accepting the current value.
     *
     * @returns success indicator
     */
    bool accept(RNG *rng, double prob);
    /**
     * Rescales the proposal distribution. This function is called by
     * Metropolis#accept when the sampler is in adaptive
     * mode. Rescaling may depend on the acceptance probability.
     *
     * @param prob Acceptance probability
     */
    virtual void rescale(double prob) = 0;
    /**
     * The Metropolis-Hastings method is adaptive. The process of
     * adaptation is specific to each subclass and is defined by the
     * rescale member function
     */
    bool isAdaptive() const;
    /**
     * Turns off adaptive mode
     */
    bool adaptOff();
    /**
     * Tests whether adaptive mode has been successful (e.g. by testing
     * that the acceptance rate lies in an interval around the target 
     * value). This function is called by Metropolis#adaptOff;
     */
    virtual bool checkAdaptation() const = 0;
    /**
     * length of the value vector
     */
    unsigned int length() const;
};

#endif /* METROPOLIS_H_ */
