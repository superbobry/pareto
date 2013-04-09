#include <math.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value _erfc(value v_float)
{
    CAMLparam1(v_float);
    const double v = Double_val(v_float);
    CAMLlocal1(result);
    result = caml_copy_double(erfc(v));
    CAMLreturn(result);
}
