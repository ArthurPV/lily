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

CAMLprim value int_of_float32(value x) {
	CAMLparam1(x);
	CAMLreturn(Val_long((intnat)Float32_val(x)));
}

CAMLprim value int_of_float64(value x) {
	CAMLparam1(x);
	CAMLreturn(Val_long((intnat)Float64_val(x)));
}

CAMLprim value int_of_float80(value x) {
	CAMLparam1(x);
	CAMLreturn(Val_long((intnat)Float80_val(x)));
}

CAMLprim value int_of_float128(value x) {
	CAMLparam1(x);
	CAMLreturn(Val_long((intnat)Float128_val(x)));
}
