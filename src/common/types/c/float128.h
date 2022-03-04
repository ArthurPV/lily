#ifndef LILY_TYPES_FLOAT128_H
#define LILY_TYPES_FLOAT128_H

#include <float.h>
#include <values.h>
#include <caml/mlvalues.h>

#define F128_SIZE 16

typedef __float128 float128;

#define MAX_FLOAT128 3
#define Float128_val(x) *((float128 *)Data_custom_val(x))

CAMLextern value copy_float128(float128 f);

#endif // LILY_TYPES_FLOAT128_H
