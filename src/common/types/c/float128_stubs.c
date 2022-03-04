#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/custom.h>
#include <sys/types.h>

#include "float128.h"

static int float128_cmp(value x, value y) {
	float128 f1 = Float128_val(x);
	float128 f2 = Float128_val(y);
	return (f1 > f2) - (f1 < f2);
}

static intnat float128_hash(value x) {
	return Float128_val(x);
}

struct custom_operations float128_ops = {
	"float128",
	float128_cmp,
	float128_hash,
	custom_compare_ext_default,
	custom_finalize_default,
};

CAMLprim value copy_float128(float128 f) {
	CAMLparam0();
	value res = caml_alloc_custom(&float128_ops, F128_SIZE, 0, 1);
	Float128_val(res) = f;
	CAMLreturn(res);
}

CAMLprim value float128_add(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float128(Float128_val(x) + Float128_val(y)));
}

CAMLprim value float128_sub(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float128(Float128_val(x) - Float128_val(y)));
}

CAMLprim value float128_mul(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float128(Float128_val(x) * Float128_val(y)));
}

CAMLprim value float128_div(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float128(Float128_val(x) / Float128_val(y)));
}

CAMLprim value float128_neg(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float128(MAX_FLOAT128 - Float128_val(x) + 1));
}

CAMLprim value float128_max_float(void) {
	CAMLparam0();
	CAMLreturn(copy_float128(MAX_FLOAT128));
}

CAMLprim value float128_init_custom_ops(void) {
	CAMLparam0();
	caml_register_custom_operations(&float128_ops);
	CAMLreturn(Val_unit);
}
