void f() __attribute__((x));

#define F2() void f2() __attribute__((x2))

/* From GNU C stdio.h */
extern int snprintf (char *__restrict __s, unsigned long int __maxlen,
             const char *__restrict __format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

F2();
