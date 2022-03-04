#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/custom.h>
#include <float.h>

#include "float80.h"

static int float80_cmp(value x, value y) {
	float80 f1 = Float80_val(x);
	float80 f2 = Float80_val(y);
	return (f1 > f2) - (f1 < f2);
}

static intnat float80_hash(value x) {
	return Float80_val(x);
}

struct custom_operations float80_ops = {
	"float80",
	float80_cmp,
	float80_hash,
	custom_compare_ext_default,
	custom_finalize_default,
};

CAMLprim value copy_float80(float80 f) {
	CAMLparam0();
	value res = caml_alloc_custom(&float80_ops, F80_SIZE, 0, 1);
	Float80_val(res) = f;
	CAMLreturn(res);
}

CAMLprim value float80_add(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float80(Float80_val(x) + Float80_val(y)));
}

CAMLprim value float80_sub(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float80(Float80_val(x) - Float80_val(y)));
}

CAMLprim value float80_mul(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float80(Float80_val(x) * Float80_val(y)));
}

CAMLprim value float80_div(value x, value y) {
	CAMLparam2(x, y);
	CAMLreturn(copy_float80(Float80_val(x) / Float80_val(y)));
}

CAMLprim value float80_neg(value x) {
	CAMLparam1(x);
	CAMLreturn(copy_float80(MAX_FLOAT80 - Float80_val(x) + 1));
}

CAMLprim value float80_max_float(void) {
	CAMLparam0();
	CAMLreturn(copy_float80(MAX_FLOAT80));
}

CAMLprim value float80_init_custom_ops(void) {
	CAMLparam0();
	caml_register_custom_operations(&float80_ops);
	CAMLreturn(Val_unit);
}
