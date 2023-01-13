#include "population.h"
#include <map>
#include <typeinfo>
#include <cmath>

Population::Population(){
  std::random_device rd;
  this->gen = std::mt19937_64(rd());

  //create a pandemic
  this->pandemic = new Pandemic();

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


double lognormal(double x, double mean, double stddev)
{
  return (1 / (x * stddev * sqrt(2*M_PI))) * exp(-pow((log(x) - mean), 2) / (2*pow(stddev, 2)));
}

double normal(double x, double mu, double sigma){
  return 1.0 / (sigma * sqrt(2.0 * M_PI)) * exp(-(pow((x - mu)/sigma, 2)/2.0));
}



void Person::set_immune_response(){
  int nvaccine = 0;

  for(auto start: this->vaccine_dates){
     nvaccine++;

     double scale = 300*pow(nvaccine*pow(nvaccine + exp(-2*nvaccine),-1),3);
     double width = 1;//1./scale;//(nvaccine+1)*0.3;
     double s = 2;

     double m_age = pow((1+this->age)/50.,-0.3);//-0.5);
     double m_bmi = pow((1+this->bmi)/30.,-0.1);//-0.7);

     double m_c = 1;
     for(auto const& [name, comorbidity]: this->comorbidities){
       m_c *= comorbidity->get_immune_influence(this);
     }

     //std::cout << nvaccine << " " << scale << " " << m_age << " " << m_bmi << " " << m_c <<std::endl;
     scale *= m_age*m_bmi*m_c;
     //''scale *= m_c;
     //scale = m_c;//pow(1+this->comorbidities.size(),-1);

     auto res = [scale,start,width,s](int x){
         double _x = (x-start)/100.;
         double part1 = scale*lognormal(_x,log(width),log(s));
         if(isnan(part1)) part1 = 0;
         //part1 = 0;

         double part2 = (scale*0.8)*(_x/(_x + 0.1));
         if (x<start) part2 = 0;
         return double(part1 + part2);
        };
     this->_immune_response.push_back(res);
  }

  //width = 0.5;
  //s = 2;
  std::uniform_real_distribution<> sdis(0, 10);
  std::random_device rd;
  std::mt19937_64 gen(rd());
  double scale = sdis(gen);
  double width = 0.5;
  double s = 2;
  for(auto start: this->infection_dates){
    auto res = [scale,start,width,s](int x){
      double _x = (x-start)/100.;
      double part1 = scale*lognormal(_x,log(width),log(s));
      if(isnan(part1)) part1 = 0;

      double part2 = (scale*0.8)*(_x/(_x + 0.1));
      if (x<start) part2 = 0;

      return double(part1 + part2);
    };
    this->_immune_response.push_back(res);
  }

}

double Person::get_immune_response(int x){
  double retval = 0;
  for(auto fun: this->_immune_response){
    retval += fun(x);
  }
  return retval;
}

std::uniform_real_distribution<> dis(0, 1);
std::normal_distribution<> vaccine_dis(80, 20);
std::normal_distribution<> vdismean(7, 3);
std::normal_distribution<> vdiswidth(2, 1);
std::normal_distribution<> vdisscale(0.1, 0.01);

Person* Population::generate(){

  //2 years;
  int ndays = 365*2;

  Person *person = new Person();
  person->age = this->get_age();
  person->sex = this->get_sex();
  person->imd = this->get_imd();
  person->bmi = this->get_bmi();


  for(auto const& [name, comorbidity] : this->comorbidities){
    if(dis(this->gen) < comorbidity->get_prevelance(person) ){
      person->comorbidities[name] = comorbidity;
    }
  }

  //simulate infections
  for(int i=0; i<ndays; i++){
    double p = this->pandemic->get_p_infection(i);
    if(dis(this->gen) < p){
      person->infection_dates.push_back(i);
      i+=50;//dont get infected for another 50 days
    }
  }

  // max 3 vaccines
  //start 100 days into the pandemic

  int nrisks = person->get_nrisks();
  double p_vaccine = pow((person->age+1)/100.,-0.3);
  //std::cout << p_vaccine << std::endl;
  if(nrisks>0){
    p_vaccine *= pow(nrisks/5.,-0.3);
  }
  //std::cout << p_vaccine << std::endl;
  int days = int(100*p_vaccine);
  //days = 100;

  //if(days>1000){
  //std::cout << days << " " << person->age << " " << nrisks << std::endl;
  //  }
  int nvaccines = 4;
  for(int i=0; i<nvaccines; i++){
    //random not vaccinated with this dose
    if(dis(this->gen) < 0.01) break;

    days += int(vaccine_dis(this->gen));
    //std::cout << i << " " << days << " " << p_vaccine << std::endl;
    person->vaccine_dates.push_back(days);
  }

  person->set_immune_response();

  //simulate outcomes after infections

  for(auto date: person->infection_dates){
    //simulate 28 days from infection
    double mean = vdismean(this->gen);
    double scale = vdisscale(this->gen);
    double width = vdiswidth(this->gen);
    //double vmax = scale*normal(date+mean,date+mean,width);


    for(int i=date; i<date+28; i++){
      double p = person->get_immune_response(i);

      //virus level
      scale = 1;
      double v = scale*normal(i,date + mean, width);

      double pbad = v*pow(p+1,-2);//0.5);
      double rand = dis(this->gen);
      if (rand < pbad){
        person->outcome_dates.push_back(i);
        //hospitalised, so dont have it recorded again...
        break;
      }

    }
  }

  return person;
}

Person Population::test(){
  Person* p = this->generate();
  return *p;
}

