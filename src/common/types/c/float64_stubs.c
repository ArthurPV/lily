#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/custom.h>

#include "float64.h"

static int float64_cmp(value x, value y) {
	float64 f1 = Float64_val(x);
	float64 f2 = Float64_val(y);
	return (f1 > f2) - (f1 < f2);
}

static intnat float64_hash(value x) {
	return Float64_val(x);
}

struct custom_operations float64_ops = {
	"float64",
	float64_cmp,
	float64_hash,
	custom_compare_ext_default,
	custom_finalize_default,
};

CAMLprim value copy_float64(float64 f) {
	CAMLparam0();
	value res = caml_alloc_custom(&float64_ops, F64_SIZE, 0, 1);
	Float64_val(res) = f;
	CAMLreturn(res);
}

CAMLprim value float64_add(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float64(Float64_val(x) + Float64_val(y)));
}

CAMLprim value float64_sub(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float64(Float64_val(x) - Float64_val(y)));
}

CAMLprim value float64_mul(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float64(Float64_val(x) * Float64_val(y)));
}

CAMLprim value float64_div(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float64(Float64_val(x) / Float64_val(y)));
}

CAMLprim value float64_neg(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float64(MAX_FLOAT64 - Float64_val(x) + 1));
}

CAMLprim value float64_max_float(void) {
	CAMLparam0();
	CAMLreturn(copy_float64(MAX_FLOAT64));
}

CAMLprim value float64_init_custom_ops(void) {
	CAMLparam0();
	caml_register_custom_operations(&float64_ops);
	CAMLreturn(Val_unit);
}
