#include <gsl/gsl_cdf.h>
#include <gsl/gsl_combination.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
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

static void combi_of_val(gsl_combination *c, value vc)
{
    c->n = Int_val(Field(vc, 0));
    c->k = Int_val(Field(vc, 1));
    c->data = Data_bigarray_val(Field(vc, 2));
}

CAMLprim value ml_gsl_combination_init_first(value vc)
{
    gsl_combination c;
    combi_of_val(&c, vc);
    gsl_combination_init_first(&c);
    return Val_unit;
}

CAMLprim value ml_gsl_combination_init_last(value vc)
{
    gsl_combination c;
    combi_of_val(&c, vc);
    gsl_combination_init_last(&c);
    return Val_unit;
}

CAMLprim value ml_gsl_combination_valid(value vc)
{
    int r;
    gsl_combination c;
    combi_of_val(&c, vc);
    r = gsl_combination_valid(&c);
    return Val_not(Val_bool(r));
}

CAMLprim value ml_gsl_combination_next(value vc)
{
    gsl_combination c;
    combi_of_val(&c, vc);
    gsl_combination_next(&c);
    return Val_unit;
}

CAMLprim value ml_gsl_combination_prev(value vc)
{
    gsl_combination c;
    combi_of_val(&c, vc);
    gsl_combination_prev(&c);
    return Val_unit;
}
