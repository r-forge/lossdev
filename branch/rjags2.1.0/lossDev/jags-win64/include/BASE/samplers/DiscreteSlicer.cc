#include <config.h>
#include <graph/StochasticNode.h>
#include <graph/NodeError.h>
#include <distribution/Distribution.h>
#include <sampler/GraphView.h>

#include "DiscreteSlicer.h"

#include <cmath>
#include <stdexcept>

using std::floor;
using std::vector;
using std::logic_error;
using std::string;

namespace base {

    DiscreteSlicer::DiscreteSlicer(GraphView const *gv, unsigned int chain, 
				   double width, long ndoubles)
	: Slicer(width, ndoubles), _gv(gv), _chain(chain), _x(0)
    {
	if (gv->nodes().size() != 1 || !canSample(gv->nodes()[0])) {
	    throw logic_error("Invalid DiscreteSlicer");
	}
	
	_x = _gv->nodes()[0]->value(chain)[0];
    }

    bool DiscreteSlicer::canSample(StochasticNode const *node)
    {
	if (!node->isDiscreteValued() || node->length() != 1)
	    return false;
	
	return true;
    }

    void DiscreteSlicer::setValue(double x)
    {
	_x = x;
	x = floor(x);
	_gv->setValue(&x, 1, _chain);
    }
  
    double DiscreteSlicer::value() const
    {
	return _x;
    }

    void DiscreteSlicer::getLimits(double *lower, double *upper) const
    {
	StochasticNode const *snode = _gv->nodes()[0];
        snode->support(lower, upper, 1, _chain);
	*upper += 1;
    }
    
    void DiscreteSlicer::update(RNG *rng)
    {
	updateDouble(rng);
    }

    string DiscreteSlicer::name() const
    {
	return "DiscreteSlicer";
    }
    
    double DiscreteSlicer::logDensity() const
    {
	return _gv->logFullConditional(_chain);
    }

}
