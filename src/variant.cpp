#include <iostream>
#include <string>
#include <vector>
#include "math.h"
#include "variant.h"

double norm_pdf(double x, double mu, double sigma){
  return 1.0 / (sigma * sqrt(2.0 * M_PI)) * exp(-(pow((x - mu)/sigma, 2)/2.0));
}

Variant::Variant(double scale, double mean, double sigma){
    this->scale = scale;
    this->mean = mean;
    this->sigma = sigma;
  };

double Variant::get_abundance(double x){
    return this->scale*norm_pdf(x,this->mean,this->sigma);
};


std::vector<double> Variant::get_abundance2(std::vector<double> _x){
  std::vector<double> retval;
  for(auto x : _x){
    retval.push_back(this->get_abundance(x));
  }
  return retval;
};

