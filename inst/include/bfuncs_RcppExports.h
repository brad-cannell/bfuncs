// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_bfuncs_RCPPEXPORTS_H_GEN_
#define RCPP_bfuncs_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace bfuncs {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("bfuncs", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("bfuncs", "_bfuncs_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in bfuncs");
            }
        }
    }

    inline NumericVector na_locf(NumericVector x) {
        typedef SEXP(*Ptr_na_locf)(SEXP);
        static Ptr_na_locf p_na_locf = NULL;
        if (p_na_locf == NULL) {
            validateSignature("NumericVector(*na_locf)(NumericVector)");
            p_na_locf = (Ptr_na_locf)R_GetCCallable("bfuncs", "_bfuncs_na_locf");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_na_locf(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

}

#endif // RCPP_bfuncs_RCPPEXPORTS_H_GEN_
