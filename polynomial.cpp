#include "polynomial.h"

#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <cfloat>
#include <cmath>

using std::istream;
using std::ostream;
using std::string;
using std::stringstream;
using std::fixed;
using std::setprecision;
using std::showpos;

/**
 constructor that allocates memory for a new polynomial object

 @param degree the size of the polynomial degree
 */
Polynomial::Polynomial(size_t degree) : _degree(degree)
{
    _coefficients = new float[_degree + 1];
    for (size_t i = 0; i < _degree + 1; i++)
    {
        _coefficients[i] = 0.0;
    }
}

/**
 constructor that allocates memory for a new polynomial object

 @param degree is the size of the polynomial degree
 @param coefficients is a pointer to the stored coeffeciants array
 */
Polynomial::Polynomial(size_t degree, const float* coefficients): _degree(degree)
{
    _coefficients = new float[_degree + 1];
    for (size_t i = 0; i < _degree + 1; i++)
    {
        _coefficients[i] = coefficients[i];
    }
}

/**
 constructor that allocates memory for a new polynomial object

 @param polynomial is an object with coefficients and a degree stored within it
 */
Polynomial::Polynomial(const Polynomial& polynomial): _degree(polynomial._degree)
{
    _coefficients = new float[_degree + 1];
    for (size_t i = 0; i < _degree + 1; i++)
    {
        _coefficients[i] = polynomial._coefficients[i];
    }
}

/**
 destructor that deallocates memory for the _coeffeciants array
 **/
Polynomial::~Polynomial()
{
    delete [] _coefficients;
}

/**
 Takes a polynomial and adds the like terms together to store into a new polynomial

 @param rhs  is a constant polynomial object
 @return a new polynomial with new coeffeciants and new degree
 */
const Polynomial Polynomial::Sum(const Polynomial& rhs)const
{
    size_t maxDegree = 0;
    size_t minDegree = 0;
    if(_degree > rhs._degree)
    {
        maxDegree = _degree;
        minDegree = rhs._degree;
    }
    else
    {
        maxDegree = rhs._degree;
        minDegree = _degree;
    }
    float newCoefs[maxDegree + 1];
    if(_degree > rhs._degree)
    {
        for(int i = 0; i < maxDegree + 1; i++)
        {
            newCoefs[i] = _coefficients[i];
        }
        for(int j = 0; j < minDegree + 1; j++)
        {
            newCoefs[j] += rhs._coefficients[j];
        }
    }
    else
    {
        for(int i = 0; i < maxDegree; i++)
        {
            newCoefs[i] = rhs._coefficients[i];
        }
        for(int j = 0; j < minDegree + 1; j++)
        {
            newCoefs[j] += _coefficients[j];
        }
    }
    return Polynomial(maxDegree, newCoefs);
}

/**
 Takes a polynomial and subtracts the like terms together to store into a new polynomial
 
 @param rhs is a constant polynomial object
 @return a new polynomial with new coeffeciants and new degree
 */
const Polynomial Polynomial::Subtract(const Polynomial& rhs)const
{
    size_t maxDegree = 0;
    size_t minDegree = 0;
    
    if(_degree > rhs._degree)
    {
        maxDegree = _degree;
        minDegree = rhs._degree;
    }
    else
    {
        maxDegree = rhs._degree;
        minDegree = _degree;
    }
    
    float newCoefs[maxDegree + 1];
    if(_degree > rhs._degree)
    {
        for(int i = 0; i < maxDegree + 1; i++)
        {
            newCoefs[i] = _coefficients[i];
        }
        for(int j = 0; j < minDegree + 1; j++)
        {
            newCoefs[j] =  newCoefs[j] - rhs._coefficients[j];
        }
    }
    else
    {
        for(int i = 0; i < maxDegree; i++)
        {
            newCoefs[i] = rhs._coefficients[i];
        }
        for(int j = 0; j < minDegree + 1; j++)
        {
            newCoefs[j] = newCoefs[j] - _coefficients[j];
        }
        
    }
    return Polynomial(maxDegree, newCoefs);
}

/**
 Turns the coefficients negative

 @return the string stream operator "retVal".
 */
const Polynomial Polynomial::Minus()const
{
    Polynomial retVal(*this);
    for (size_t i = 0; i < _degree + 1; i++)
    {
        retVal._coefficients[i] *= -1;
    }
    return retVal;
}

/**
 Takes a polynomial and foils all terms together to store into a new polynomial
 
 @param rhs is a constant polynomial object
 @return a new polynomial with new coeffeciants and new degree
 */
const Polynomial Polynomial::Multiply(const Polynomial& rhs)const
{
    size_t  newDegree = rhs._degree + _degree;
    float newCoefs[newDegree + 1];
    size_t degA = _degree;
    size_t degB = rhs._degree;
    
    for(int p = 0; p < newDegree; p++)
    {
        newCoefs[p] = 0;
    }
    
    for( int j = 0; j <= degA; j++ )
    {
        for( int t = 0; t <= degB; t++ )
        {
            newCoefs[ j + t ] += _coefficients[ j ] * rhs._coefficients[ t ];
        }
    }
    return Polynomial(newDegree, newCoefs);
}

/**
 Extra credit that was not completed, its purpose was to use synthetic
 division to divide a polynomial
 **/
const Polynomial Polynomial::Divide(const Polynomial& rhs)const
{
    return Polynomial(0);
}

/**
 Takes a polynomial and derives it to store into a new polynomial with a new degree
 
 @return a new polynomial with new coeffeciants and new degree
 */
const Polynomial Polynomial::Derive()const
{
    float newCoef[_degree];
    size_t newDegree = _degree - 1;
    
    for(size_t i = 0; i < _degree; i++)
    {
        newCoef[i] = _coefficients[i + 1] * (i + 1);
    }
    return Polynomial(newDegree, newCoef);
}

/**
 Gets an inputed value to be then calculated in the polynomial

 @param x the inputed  value for the polynomial to evaluate
 @return the final calculated sum of the evaluated polynomial
 constructor that allocates memory for a new polynomial object
 **/
float Polynomial::Evaluate(float x)const
{
    int factor = 1, sum = 0;
    for(int i = 0; i <= _degree; i++)
    {
        sum += _coefficients[i] * factor;
        factor *= x;
    }
    return sum;
}

/**
 Integral of the polynomial

 @param start is the starting point
 @param end the end point
 @return sum of the final intergrated polynomial
 */
float Polynomial::Integrate(float start, float end)const
{
    float newCoef[_degree + 2];
    float endPoint = 0.0;
    float startPoint = 0.0;
    float sum = 0.0;
    
    for(int i = 0; i < _degree + 1; i++)
    {
        newCoef[i] = _coefficients[i];
    }
    for(size_t j = 0; j < _degree + 1; j++)
    {
        endPoint += ((newCoef[j] / (j + 1)) * pow(end, (j + 1)));
        startPoint += ((newCoef[j] / (j + 1)) * pow(start, (j+1)));
        
    }
    sum = endPoint - startPoint;
    return sum;
}

/**
 lets us use the '=' as an operator

 @param rhs is a constant polynomial object
 @return "this" polynomial
 */
const Polynomial& Polynomial::operator=(const Polynomial& rhs)
{
    if (&rhs == this)
    {
        return *this;
    }
    if (_degree != rhs._degree)
    {
        if (_coefficients)
        {
            delete[] _coefficients;
        }
        _degree = rhs._degree;
        _coefficients = new float[_degree + 1];
    }
    for (size_t i = 0; i < _degree + 1; i++)
    {
        _coefficients[i] = rhs._coefficients[i];
    }
    return *this;
}

/**
 If the degree is not the same as the other polynomials degree
 it returns false, and another if statement  making sure teh value created by absolute value
 is within a 1000th of a percent of 0 otherwise true

 @param rhs is a constant polynomial object
 @return boolina true or false bsaed in comparison result
 */
bool Polynomial::Equals(const Polynomial& rhs)const
{
    if (_degree != rhs._degree)
    {
        return false;
    }
    for (size_t i=0; i < _degree; i++)
    {
        if (std::abs(_coefficients[i] - rhs._coefficients[i]) > 0.0001)
        {
            return false;
        }
    }
    return true;
}

/**
 adds the x to the power of and organizes and formats the polyno]omial
 after being calculated

 @return the stringstream variable to later be outputed or stored
 */
string Polynomial::ToString()const
{
    stringstream ss;
    for (size_t i = _degree; i > 0; i--)
    {
        ss << showpos << fixed << setprecision(2) << _coefficients[i] << "x^" << i << " ";
    }
    ss << showpos << fixed << setprecision(2) << _coefficients[0];
    return ss.str();
}

/**
 writes the coeffecients to the screen or stores them in a file

 @param output ostream object that gets passed the coeffecients to output
 @return the string stream variable to output
 */
ostream& Polynomial::Write(ostream& output)const
{
    output << _degree << " ";
    for (size_t i = 0; i < _degree + 1; i++)
    {
        output << _coefficients[i] << " ";
    }
    return output;
}

/**
 reads in from a file the polynomial coefficients and degree and stores
 them backwards

 @param input is the source file or stream that contains the
 polynomial information
 @return the istream file
 */
istream& Polynomial::Read(istream& input)
{
    size_t degree;
    input >> degree;
    if (input.fail())
    {
        return input;
    }
    float* coefficients = new float[degree + 1];
    for (size_t i = 0; i < degree + 1; i++)
    {
        input >> coefficients[i];
        if (input.fail())
        {
            delete[] coefficients;
            return input;
        }
    }
    if (degree != _degree)
    {
        if (_coefficients)
        {
            delete[] _coefficients;
        }
        _degree = degree;
        _coefficients = coefficients;
    }
    else
    {
        for (size_t i = 0; i < _degree + 1; i++)
        {
            _coefficients[i] = coefficients[i];
        }
        delete[] coefficients;
    }
    return input;
}
