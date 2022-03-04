#ifndef LILY_TYPES_FLOAT32_H
#define LILY_TYPES_FLOAT32_H

#include <caml/mlvalues.h>

#include <float.h>

#define F32_SIZE 4

typedef float float32;

#define MAX_FLOAT32 FLT_MAX
#define Float32_val(v) (*((float32 *)Data_custom_val(v)))

CAMLextern value copy_float32(float32 f);

#endif // LILY_TYPES_FLOAT32_H
