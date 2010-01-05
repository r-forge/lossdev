/*
##################################################################################################
##                                                                                              ##
##    lossDev is an R-package.                                                                  ##
##    It is a Bayesian time series model of loss development.                                   ##
##    Features include skewed Student-t distribution with time-varying scale parameters,        ##
##    an expert prior for the calendar year effect,                                             ##
##    and accommodation for structural breaks in the consumption path of services.              ##
##                                                                                              ##
##    Copyright � 2009, National Council On Compensation Insurance Inc.,                        ##
##                                                                                              ##
##    This file is part of lossDev.                                                             ##
##                                                                                              ##
##    lossDev is free software: you can redistribute it and/or modify                           ##
##    it under the terms of the GNU General Public License as published by                      ##
##    the Free Software Foundation, either version 3 of the License, or                         ##
##    (at your option) any later version.                                                       ##
##                                                                                              ##
##    This program is distributed in the hope that it will be useful,                           ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of                            ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                             ##
##    GNU General Public License for more details.                                              ##
##                                                                                              ##
##    You should have received a copy of the GNU General Public License                         ##
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.                     ##
##                                                                                              ##
##################################################################################################
*/

#include "ExpandableArray.h"

ExpandableArray::ExpandableArray()
{
    _value = new double[1];
    _length = 1;
}
ExpandableArray::~ExpandableArray()
{
    delete [] _value;
}

void ExpandableArray::makeSufficientWithoutCopy(unsigned int const & length)
{
    if(length > _length)
    {
	delete [] _value;
	_value = new double[length];
	_length=length;
    }
}

void ExpandableArray::makeSufficientWithCopy(unsigned int const & length)
{
    if(length > _length)
    {
	double *tmp = _value;
	_value = new double[length];

	for(unsigned int i = 0; i < _length; ++i)
	    _value[i] = tmp[i];

	delete[] tmp;
	
	_length = length;
    }
}

double* ExpandableArray::value()
{
    return _value;
}
