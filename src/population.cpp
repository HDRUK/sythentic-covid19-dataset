#include "population.h"
#include <map>
#include <typeinfo>
#include <cmath>
#include <cfloat>

std::normal_distribution<> random_effect(1, 0.2);

Population::Population(){
  std::random_device rd;
  this->gen = std::mt19937_64(rd());

  //create a pandemic
  //this->pandemic = new Pandemic();

  this->comorbidities["diabetes"] = new Comorbidity(
    [](Person* p){
      return 0.4*(pow(p->age/70.,0.5))*(pow(p->bmi/25.,2));
    },
    [](Person* p){
      return 0.8;
    }
  );

  this->comorbidities["asthma"] = new Comorbidity(
    [](Person* p){
      return 0.2*(pow(p->age/50.,0.5))*(pow(p->bmi/25.,2));
    },
    [](Person* p){
      return 0.7;//*(pow(p->age/50.,0.5));
    }
  );

  this->comorbidities["fracture"] = new Comorbidity(
    [](Person* p){
      return 0.1*(pow(p->age/60.,1.2))*(pow(p->bmi/25.,0.5));
    },
    [](Person* p){
      return 1;
    }
  );


  this->comorbidities["blood_cancer"] = new Comorbidity(
    [](Person* p){
      return 0.01*(p->age/80);
      },
    [](Person* p){
        return 0.1;
    }
  );

  this->comorbidities["respiratory_cancer"] = new Comorbidity(
    [](Person* p){
      return 0.02*(p->age/80);
    },
    [](Person* p){
      return 0.2;
    }
  );

  this->comorbidities["copd"] = new Comorbidity(
    [](Person* p){
      return 0.07*(p->age/80)*pow(p->bmi/30.,3);
    },
    [](Person* p){
      return 0.9;
    }
  );

  this->comorbidities["chd"] = new Comorbidity(
    [](Person* p){
      return 0.04*(p->age/80)*(pow(p->bmi/25.,2));
    },
    [](Person* p){
      return 0.8;
    }
  );


  this->comorbidities["parkinsons"] = new Comorbidity(
    [](Person* p){
      return 0.01*(pow(p->age/70,3));
    },
    [](Person* p){
      return 0.65;
    }
  );

  //this->comorbidities["blood cancer"] = blood_cancer;
  //this->comorbidities["respiratory cancer"] = resp_cancer;
  //this->comorbidities["copd"] = copd;
  //this->comorbidities["chd"] = chd;
  //this->comorbidities["parkinsons"] = parkinsons;

}

int Population::get_imd(){
  std::uniform_int_distribution<int> imd_dist(1,5);
  return imd_dist(this->gen);
}

double Population::get_bmi(){
  std::normal_distribution<double> bmi_dist(25,5);
  return bmi_dist(this->gen);
}


int Population::get_age(){
  std::vector<int> i{0, this->age_turn, this->age_end};
  std::vector<double> w{1,  1,  0};
  std::piecewise_linear_distribution<> age_dist{i.begin(), i.end(), w.begin()};
  return age_dist(this->gen);
}

std::string Population::get_sex(){
  std::discrete_distribution<> w{0.49,0.51};
  std::vector<std::string> sexes{"Male","Female"};
  int i = w(this->gen);
  std::string sex = sexes[i];
  return sex;
}

std::string Population::get_ethnicity(){
  std::discrete_distribution<> w{0.8,0.2};
  std::vector<std::string> ethnicities{"White","Other"};
  int i = w(this->gen);
  std::string ethnicity = ethnicities[i];
  return ethnicity;
}


double fn_igg_response(double x, double b, double lambda)
{
  return b*x*exp(-1.0*lambda*x);
}

double lognormal(double x, double mean, double stddev)
{
  return (1 / (x * stddev * sqrt(2*M_PI))) * exp(-pow((log(x) - mean), 2) / (2*pow(stddev, 2)));
}

double normal(double x, double mu, double sigma){
  return 1.0 / (sigma * sqrt(2.0 * M_PI)) * exp(-(pow((x - mu)/sigma, 2)/2.0));
}

std::normal_distribution<> vdismean(7, 3);
std::normal_distribution<> vdiswidth(2, 1);
std::normal_distribution<> vdisscale(0.1, 0.05);
std::random_device rd;
std::mt19937_64 gen(rd());
void Person::create_infection_response(){
  for(auto start: this->infection_dates){
    //simulate 28 days from infection
    double mean = vdismean(gen);
    double scale = vdisscale(gen);
    if(scale<0) scale=0;
    double width = vdiswidth(gen);


   auto res = [start,mean,width,scale](int x){
     double retval = scale*normal(x,start + mean, width);
     return double(retval);
   };
   this->_virus_response.push_back(res);
  }
}


void Person::create_immune_response(){
  int nvaccine = 0;

  std::uniform_real_distribution<> sdis(0, 10);
  std::random_device rd;
  std::mt19937_64 gen(rd());
  double scale = sdis(gen);
  double width = 0.5;
  double s = 2;

  for(auto start: this->vaccine_dates){
     nvaccine++;

     double scale = 300;//*pow(nvaccine*pow(nvaccine + exp(-2*nvaccine),-1),3);
     double lambda = 0.1;//*(pow(nvaccine,-0.5));

     double m_age = pow((1+this->age)/50.,-0.3);//-0.5);
     double m_bmi = pow((1+this->bmi)/30.,-0.1);//-0.7);

     double m_c = m_age*m_bmi;
     for(auto c: this->comorbidities){
       m_c *= c.second->get_immune_influence(this);
     }

     //scale *= m_c*random_effect(gen);
     //lambda *= pow(m_c,0.5)*random_effect(gen);

     auto res = [start,scale,lambda](int x){
         double _x = x-start;
         if(_x<0) {
           return 0.;
         }
         double retval = fn_igg_response(_x,
                                         scale,
                                         lambda);
         return retval;
     };
     /*
     auto res = [scale,start,width,s](int x){
         double _x = (x-start)/100.;
         double part1 = scale*lognormal(_x,log(width),log(s));
         if(std::isnan(part1)) part1 = 0;
         //return (part1);
         //double _x = (x-start);

         //part1 = scale*normal(_x,25,10);
         //return (part1);

         //if(std::isnan(part1)) part1 = 0;
         //part1 = 0;

         double part2 = (scale*0.8)*(_x/(_x + 0.1));
         if (x<start) part2 = 0;
         //part2 = 0;

         return double(part1 + part2);//
     };*/
     this->_immune_response.push_back(res);
  }

  //width = 0.5;
  //s = 2;

  /*for(auto start: this->infection_dates){
    auto res = [scale,start,width,s](int x){
      double _x = (x-start)/100.;
      double part1 = scale*lognormal(_x,log(width),log(s));
      if(std::isnan(part1)) part1 = 0;

      double part2 = (scale*0.1)*(_x/(_x + 0.1));
      if (x<start) part2 = 0;
      part2=0;


      return double(part1 + part2);
    };
    this->_immune_response.push_back(res);
  }*/

}


std::vector<std::string> Person::get_comorbidities() {
  std::vector<std::string> retval;
  for(auto const& c: this->comorbidities){
    retval.push_back(c.first);
  }
  return (retval);
}




double Person::get_hazard(int x){

  double p = this->get_immune_response(x);
  double v = this->get_infection_level(x);

  //double nrisks = this->comorbidities.size();

  double pbad = v*pow(p+1,-0.5);
  return (pbad);

}

double Person::get_immune_response(int x){
  double retval = 0;
  for(auto fun: this->_immune_response){
    retval += fun(x);
  }
  return retval;
}

double Person::get_infection_level(int x){
  double retval = 0;
  for(auto fun: this->_virus_response){
    retval += fun(x);
  }
  return retval;
}

std::uniform_real_distribution<> dis(0, 1);
/*
std::normal_distribution<> vdismean(7, 3);
std::normal_distribution<> vdiswidth(2, 1);
std::normal_distribution<> vdisscale(0.1, 0.01);
*/

//std::vector<Person *> Populat



Person* Population::generate(){


  Person *person = new Person();
  person->age = this->get_age();
  person->sex = this->get_sex();
  person->ethnicity = this->get_ethnicity();
  person->imd = this->get_imd();
  person->bmi = this->get_bmi();


  for(auto c : this->comorbidities){
    if(dis(this->gen) < c.second->get_prevelance(person) ){
      person->comorbidities[c.first] = c.second;
    }
  }

  return person;


  /*

  //simulate outcomes after infections

  for(auto date: person->infection_dates){
    //simulate 28 days from infection
    double mean = vdismean(this->gen);
    double scale = vdisscale(this->gen);
    double width = vdiswidth(this->gen);
    //double vmax = scale*normal(date+mean,date+mean,width);


    int vdate = ((person->vaccine_dates.size()>0) ?person->vaccine_dates.at(0) : -1);
    for(int i=date; i<date+28; i++){
      double p = person->get_immune_response(i);

      //virus level
      scale = 1;
      double v = scale*normal(i,date + mean, width);


      double pbad = v*pow(p+1,-5);//-0.2);//0.5);

      if(i==date && vdate<date && vdate>0){
      //   std::cout << " "<< p << " " << v << " " << pbad << std::endl;
      }

      double rand = dis(this->gen);
      if (rand < pbad){
        person->outcome_dates.push_back(i);
        //hospitalised, so dont have it recorded again...
        break;
      }

    }
  }

  return person;
   */
}

Person Population::test(){
  std::normal_distribution<> vaccine_dis(90,20);
  Person* p = this->generate();

  int days = int(0);
  int nvaccines = 5;
  for(int i=0; i<nvaccines; i++){
    //random not vaccinated with this dose
    if(dis(this->gen) < 0.1) break;
    days += int(vaccine_dis(this->gen));
    p->vaccine_dates.push_back(days);
  }

  p->create_immune_response();
  p->create_infection_response();

  return *p;
}

