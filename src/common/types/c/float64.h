#ifndef LILY_TYPES_FLOAT64_H
#define LILY_TYPES_FLOAT64_H

#include <caml/mlvalues.h>
#include <float.h>

#define F64_SIZE 8

typedef double float64;

#define MAX_FLOAT64 DBL_MAX
#define Float64_val(v) (*((float64 *)Data_custom_val(v)))

CAMLextern value copy_float64(float64 f);

#endif // LILY_TYPES_FLOAT64_H
