// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// generate_ace
Rcpp::DataFrame generate_ace(double n);
RcppExport SEXP _seave_generate_ace(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_ace(n));
    return rcpp_result_gen;
END_RCPP
}
// truth_ace
Rcpp::DataFrame truth_ace(int np);
RcppExport SEXP _seave_truth_ace(SEXP npSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type np(npSEXP);
    rcpp_result_gen = Rcpp::wrap(truth_ace(np));
    return rcpp_result_gen;
END_RCPP
}
// generate_ages
Rcpp::NumericVector generate_ages(double n);
RcppExport SEXP _seave_generate_ages(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_ages(n));
    return rcpp_result_gen;
END_RCPP
}
// generate_bmi
Rcpp::NumericVector generate_bmi(double n);
RcppExport SEXP _seave_generate_bmi(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_bmi(n));
    return rcpp_result_gen;
END_RCPP
}
// generate_risks
Rcpp::NumericVector generate_risks(double n);
RcppExport SEXP _seave_generate_risks(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_risks(n));
    return rcpp_result_gen;
END_RCPP
}
// generate_dataframe
Rcpp::List generate_dataframe(double n);
RcppExport SEXP _seave_generate_dataframe(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_dataframe(n));
    return rcpp_result_gen;
END_RCPP
}
// get_true_response
Rcpp::List get_true_response();
RcppExport SEXP _seave_get_true_response() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_true_response());
    return rcpp_result_gen;
END_RCPP
}
// generate_serology
Rcpp::DataFrame generate_serology(double n);
RcppExport SEXP _seave_generate_serology(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_serology(n));
    return rcpp_result_gen;
END_RCPP
}
// run_pandemic
std::vector<double> run_pandemic(int n);
RcppExport SEXP _seave_run_pandemic(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(run_pandemic(n));
    return rcpp_result_gen;
END_RCPP
}
// generate_simple_vaccine_effectiveness
Rcpp::DataFrame generate_simple_vaccine_effectiveness(double n);
RcppExport SEXP _seave_generate_simple_vaccine_effectiveness(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_simple_vaccine_effectiveness(n));
    return rcpp_result_gen;
END_RCPP
}
// truth_generate
Rcpp::DataFrame truth_generate();
RcppExport SEXP _seave_truth_generate() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(truth_generate());
    return rcpp_result_gen;
END_RCPP
}
// my_pandemic_generate
Rcpp::DataFrame my_pandemic_generate(double n);
RcppExport SEXP _seave_my_pandemic_generate(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(my_pandemic_generate(n));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_RcppSeaveEx();
RcppExport SEXP _rcpp_module_boot_RcppSeavePandemic();
RcppExport SEXP _rcpp_module_boot_RcppSeavePerson();
RcppExport SEXP _rcpp_module_boot_RcppSeavePopulation();

static const R_CallMethodDef CallEntries[] = {
    {"_seave_generate_ace", (DL_FUNC) &_seave_generate_ace, 1},
    {"_seave_truth_ace", (DL_FUNC) &_seave_truth_ace, 1},
    {"_seave_generate_ages", (DL_FUNC) &_seave_generate_ages, 1},
    {"_seave_generate_bmi", (DL_FUNC) &_seave_generate_bmi, 1},
    {"_seave_generate_risks", (DL_FUNC) &_seave_generate_risks, 1},
    {"_seave_generate_dataframe", (DL_FUNC) &_seave_generate_dataframe, 1},
    {"_seave_get_true_response", (DL_FUNC) &_seave_get_true_response, 0},
    {"_seave_generate_serology", (DL_FUNC) &_seave_generate_serology, 1},
    {"_seave_run_pandemic", (DL_FUNC) &_seave_run_pandemic, 1},
    {"_seave_generate_simple_vaccine_effectiveness", (DL_FUNC) &_seave_generate_simple_vaccine_effectiveness, 1},
    {"_seave_truth_generate", (DL_FUNC) &_seave_truth_generate, 0},
    {"_seave_my_pandemic_generate", (DL_FUNC) &_seave_my_pandemic_generate, 1},
    {"_rcpp_module_boot_RcppSeaveEx", (DL_FUNC) &_rcpp_module_boot_RcppSeaveEx, 0},
    {"_rcpp_module_boot_RcppSeavePandemic", (DL_FUNC) &_rcpp_module_boot_RcppSeavePandemic, 0},
    {"_rcpp_module_boot_RcppSeavePerson", (DL_FUNC) &_rcpp_module_boot_RcppSeavePerson, 0},
    {"_rcpp_module_boot_RcppSeavePopulation", (DL_FUNC) &_rcpp_module_boot_RcppSeavePopulation, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_seave(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
