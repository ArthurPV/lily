#ifndef LILY_TYPES_FLOAT128_H
#define LILY_TYPES_FLOAT128_H

#include <float.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/custom.h>

#define F128_SIZE 16

typedef __float128 float128;

typedef struct {
   struct custom_operations *ops;
   float128 v;  
} float128_s;

#define Float128_val(x) *((float128 *)Data_custom_val(x))

CAMLextern value copy_float128(float128 f);

#endif // LILY_TYPES_FLOAT128_H
