#ifndef NODE_H_
#define NODE_H_

#include <set>
#include <vector>
#include <string>

class RNG;
class StochasticNode;
class DeterministicNode;
class Graph;

/**
 * @short Node in a directed acyclic graph
 */
class Node {
    std::vector<Node const *> _parents;
    std::set<StochasticNode*> *_stoch_children;
    std::set<DeterministicNode *> *_dtrm_children;

    /* Forbid copying of Node objects */
    Node(Node const &orig);
    Node &operator=(Node const &rhs);

protected:
    std::vector<unsigned int> const &_dim;
    unsigned int _length;
    const unsigned int _nchain;
    double *_data;

public:
    /**
     * Constucts a Node with no parents.
     * @param dim Dimension of new Node.
     * @param nchain Number of chains that the Node will contain data on.
     */
    Node(std::vector<unsigned int> const &dim, unsigned int nchain);
    /**
     * Constructs a node with parents.  Each parent must contain data on
     * the same number of chains. Subclasses of Node may give specific
     * meaning to the ordering of the parents.
     *
     * @param dim Dimension of new Node.
     *
     * @param parents vector of parent nodes. A node may not be its own
     * parent.
     */
    Node(std::vector<unsigned int> const &dim, 
	 std::vector<Node const *> const &parents);
    /**
     * Destructor. 
     */
    virtual ~Node();
    /**
     * Number of chains.
     */ 
    unsigned int nchain() const;
    /**
     * Vector of parents.
     */
    std::vector<Node const *> const &parents() const;
    /**
     * Draws a random sample from the node's prior distribution.
     * @param rng Pointer to random number generator
     * @param chain Number of chain from which to draw sample
     */
    virtual void randomSample(RNG *rng, unsigned int chain) = 0;
    /**
     * Calculates a value for the node based on its parents' values.
     * @param chain Number of chain from which to draw sample
     */
    virtual void deterministicSample(unsigned int chain) = 0;
    /**
     * Checks whether the parents of the Node have valid values.
     */
    virtual bool checkParentValues(unsigned int chain) const = 0;
    /**
     * Returns the stochastic children of the node
     */
    std::set<StochasticNode*> const *stochasticChildren();
    /**
     * Returns the deterministic children of the node
     */
    std::set<DeterministicNode*> const *deterministicChildren();
    /**
     * Initializes the node for the given chain. The value array of a
     * newly constructed Node consists of missing values (denoted by
     * the special value JAGS_NA).  This function sets the value of
     * the node by forward sampling from its parents.  If the Node has
     * previously had its value set, the function will do nothing and
     * return the value true.  Initialization will fail if any of the
     * parent nodes is uninitialized, and in this case the return
     * value is false.
     *
     * @param chain Index number of chain to initialize.
     *
     * @returns a logical value indicating success
     */
    bool initialize(unsigned int chain);
    /**
     * Returns the BUGS-language representation of the node, based on the 
     * names of its parents
     *
     * @param parents Vector of names of parent nodes
     */
    virtual std::string deparse(std::vector<std::string> const &parents) const = 0;
    /**
     * Returns true if the node represents a random variable.
     */
    virtual bool isRandomVariable() const = 0;
    /**
     * Sets the value of the node, in all chains, and marks the node
     * as observed. This function can only be called once for a given
     * Node.
     * 
     * @param value vector of values
     */
    void setObserved(std::vector<double> const &value);
    /**
     * Indicates whether the node is observed. 
     */
    virtual bool isObserved() const = 0;
    /**
     * Sets the value of the node for a given chain
     * @param value Array of values to be assigned
     * @param length Length of the value array
     * @param chain number of chain (starting from zero) to modify
     *
     * @see SArray#setValue
     */
    void setValue(double const *value, unsigned int length, unsigned int chain);
    /**
     * Indicates whether a node is discrete-valued or not.
     * @see SArray#isDiscreteValued
     */
    virtual bool isDiscreteValued() const = 0;
    /**
     * Returns a pointer to the start of the array of values for 
     * the given chain.
     */
    double const *value(unsigned int chain) const;
    /**
     * Returns the length of the value array
     */
    unsigned int length() const;
    /**
     * Returns the dimensions of the Node
     */
    std::vector<unsigned int> const &dim() const;
    /**
     * Swaps the values in the given chains
     */
    void swapValue(unsigned int chain1, unsigned int chain2);
    void addChild(StochasticNode *node) const;
    void removeChild(StochasticNode *node) const;
    void addChild(DeterministicNode *node) const;
    void removeChild(DeterministicNode *node) const;
};

/**
 * Calculate the number of chains of parameter vector. Returns 0
 * if the parameters are inconsistent
 */
unsigned int countChains(std::vector<Node const *> const &parameters);

#endif /* NODE_H_ */
