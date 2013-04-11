#include <gsl/gsl_cdf.h>

#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value ml_gsl_cdf_hypergeometric_P(value k, value n1, value n2, value t)
{
    CAMLparam4(k, n1, n2, t);
    CAMLreturn(copy_double(gsl_cdf_hypergeometric_P(
                               Int_val(k),
                               Int_val(n1),
                               Int_val(n2),
                               Int_val(t))));
}
