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

Polynomial::Polynomial(size_t degree) : _degree(degree){
	_coefficients = new float[_degree + 1];
	for (size_t i = 0; i < _degree + 1; i++) {
		_coefficients[i] = 0.0;
	}
}
Polynomial::Polynomial(size_t degree, const float* coefficients): _degree(degree){
	_coefficients = new float[_degree + 1];
	for (size_t i = 0; i < _degree + 1; i++) {
		_coefficients[i] = coefficients[i];
	}
}
Polynomial::Polynomial(const Polynomial& polynomial): _degree(polynomial._degree){
	_coefficients = new float[_degree + 1];
	for (size_t i = 0; i < _degree + 1; i++) {
		_coefficients[i] = polynomial._coefficients[i];
	}
}
Polynomial::~Polynomial(){
     delete [] _coefficients;
    
}
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
const Polynomial Polynomial::Subtract(const Polynomial& rhs)const{
	
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

const Polynomial Polynomial::Minus()const{
	Polynomial retVal(*this);
	for (size_t i = 0; i < _degree + 1; i++) {
		retVal._coefficients[i] *= -1;
	}
	return retVal;
}

const Polynomial Polynomial::Multiply(const Polynomial& rhs)const
{
    
    size_t  newDegree = rhs._degree + _degree;
    float newCoefs[newDegree + 1];
    
    size_t degA, degB;
    
    degA = _degree;
    degB = rhs._degree;
    
    for( int p = 0; p < newDegree; p++ )
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
const Polynomial Polynomial::Divide(const Polynomial& rhs)const{
	return Polynomial(0);
}
const Polynomial Polynomial::Derive()const{
   
    float newCoef[_degree];
     size_t newDegree = _degree - 1;
     std::cout << newDegree << " ";
    for(size_t i = 0; i < _degree; i++)
    {
        newCoef[i] = _coefficients[i + 1] * (i + 1);
        
    }
   
	return Polynomial(newDegree, newCoef);
}
float Polynomial::Evaluate(float x)const{
    
    int sum = 0;
    for(int i = 1; i < _degree + 1; i++)
    {
        _coefficients[i] = pow(_coefficients[i], i);
        _coefficients[i] = _coefficients[i] * x;
        sum += _coefficients[i];
    }
    sum += _coefficients[0];
	return sum;
}
float Polynomial::Integrate(float start, float end)const{
	return FLT_MAX;
}
const Polynomial& Polynomial::operator=(const Polynomial& rhs){
	if (&rhs == this){
		return *this;
	}
	if (_degree != rhs._degree){
		if (_coefficients){
			delete[] _coefficients;
		}
		_degree = rhs._degree;
		_coefficients = new float[_degree + 1];
	}
	for (size_t i = 0; i < _degree + 1; i++) {
		_coefficients[i] = rhs._coefficients[i];
	}
	return *this;
}
bool Polynomial::Equals(const Polynomial& rhs)const{
	if (_degree != rhs._degree){
		return false;
	}
	for (size_t i=0; i < _degree; i++){
        if (std::abs(_coefficients[i] - rhs._coefficients[i]) > 0.0001){
			return false;
		}
	}
	return true;
}
string Polynomial::ToString()const{
	stringstream ss;
	for (size_t i = _degree; i > 0; i--) {
		ss << showpos << fixed << setprecision(2) << _coefficients[i] << "x^" << i << " ";
	}
	ss << showpos << fixed << setprecision(2) << _coefficients[0];
	return ss.str();
}
ostream& Polynomial::Write(ostream& output)const{
	output << _degree << " ";
	for (size_t i = 0; i < _degree + 1; i++) {
		output << _coefficients[i] << " ";
	}
	return output;
}
istream& Polynomial::Read(istream& input){
	size_t degree;
	input >> degree;
	if (input.fail()){
		return input;
	}
	float* coefficients = new float[degree + 1];
	for (size_t i = 0; i < degree + 1; i++) {
		input >> coefficients[i];
		if (input.fail()){
			delete[] coefficients;
			return input;
		}
	}

	if (degree != _degree){
		if (_coefficients){
			delete[] _coefficients;
		}
		_degree = degree;
		_coefficients = coefficients;
	}else{
		for (size_t i = 0; i < _degree + 1; i++) {
			_coefficients[i] = coefficients[i];
		}
		delete[] coefficients;
	}
	return input;
}
