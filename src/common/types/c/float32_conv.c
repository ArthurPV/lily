#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "float32.h"
#include "float64.h"
#include "float80.h"
#include "float128.h"

CAMLprim value float32_of_int(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32((float32)Long_val(x)));
}

CAMLprim value float32_of_float(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32((float32)Double_val(x)));
}

CAMLprim value float32_of_float64(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32((float32)Float64_val(x)));
}

CAMLprim value float32_of_float80(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32((float32)Float80_val(x)));
}

CAMLprim value float32_of_float128(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32((float32)Float128_val(x)));
}
