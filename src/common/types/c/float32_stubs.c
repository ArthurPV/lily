#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/custom.h>

#include "float32.h"

static int float32_cmp(value x, value y) {
	float32 f1 = Float32_val(x);
	float32 f2 = Float32_val(y);
	return (f1 > f2) - (f1 < f2);
}

static intnat float32_hash(value x) {
	return Float32_val(x);
}

struct custom_operations float32_ops = {
	"float32",
	float32_cmp,
	float32_hash,
	custom_compare_ext_default,
	custom_finalize_default,
};

CAMLprim value copy_float32(float32 f) {
	CAMLparam0();
	value res = caml_alloc_custom(&float32_ops, F32_SIZE, 0, 1);
	Float32_val(res) = f;
	CAMLreturn(res);
}

CAMLprim value float32_add(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float32(Float32_val(x) + Float32_val(y)));
}

CAMLprim value float32_sub(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float32(Float32_val(x) - Float32_val(y)));
}

CAMLprim value float32_mul(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float32(Float32_val(x) * Float32_val(y)));
}

CAMLprim value float32_div(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float32(Float32_val(x) / Float32_val(y)));
}

CAMLprim value float32_neg(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float32(MAX_FLOAT32 - Float32_val(x) + 1));
}

CAMLprim value float32_max_float(void) {
	CAMLparam0();
	CAMLreturn(copy_float32(MAX_FLOAT32));
}

CAMLprim value float32_init_custom_ops(void) {
	CAMLparam0();
	caml_register_custom_operations(&float32_ops);
	CAMLreturn(Val_unit);
}
