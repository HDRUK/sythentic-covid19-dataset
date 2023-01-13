#ifndef VARIANT_H
#define VARIANT_H

#include <iostream>
#include <string>
#include <vector>

class Variant{
public:
  Variant(double scale, double mean, double sigma);
  double get_abundance(double x);
  std::vector<double> get_abundance2(std::vector<double> _x);
private:
  double scale;
  double mean;
  double sigma;
};

#endif /* VARIANT_H */
