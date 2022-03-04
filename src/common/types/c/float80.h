#ifndef LILY_TYPES_FLOAT80_H
#define LILY_TYPES_FLOAT80_H

#include <caml/mlvalues.h>

#define F80_SIZE 10

typedef long double float80;

typedef struct {
	struct custom_operations *ops;
	float80 v;
} float80_s;

#define MAX_FLOAT80 LDBL_MAX
#define Float80_val(x) *((float80 *)Data_custom_val(x))

CAMLextern value copy_float80(float80 f);

#endif // LILY_TYPES_FLOAT80_H
