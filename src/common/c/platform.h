#ifndef LILY_PLATFORM_H
#define LILY_PLATFORM_H

#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value get_os(value unit);
CAMLprim value get_arch(value unit);

#endif // LILY_PLATFORM_H
