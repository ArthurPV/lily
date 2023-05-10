/*
 * MIT License
 *
 * Copyright (c) 2022-2023 ArthurPV
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef LILY_CORE_LILY_CHECKED_DATA_TYPE_H
#define LILY_CORE_LILY_CHECKED_DATA_TYPE_H

#include <base/macros.h>
#include <base/new.h>
#include <base/string.h>
#include <base/vec.h>

#include <core/shared/location.h>

typedef struct LilyCheckedScope LilyCheckedScope;

enum LilyCheckedDataTypeKind
{
    LILY_CHECKED_DATA_TYPE_KIND_ANY,
    LILY_CHECKED_DATA_TYPE_KIND_ARRAY,
    LILY_CHECKED_DATA_TYPE_KIND_BOOL,
    LILY_CHECKED_DATA_TYPE_KIND_BYTE,
    LILY_CHECKED_DATA_TYPE_KIND_BYTES,
    LILY_CHECKED_DATA_TYPE_KIND_CHAR,
    LILY_CHECKED_DATA_TYPE_KIND_CUSTOM,
    LILY_CHECKED_DATA_TYPE_KIND_EXCEPTION,
    LILY_CHECKED_DATA_TYPE_KIND_FLOAT32,
    LILY_CHECKED_DATA_TYPE_KIND_FLOAT64,
    LILY_CHECKED_DATA_TYPE_KIND_INT16,
    LILY_CHECKED_DATA_TYPE_KIND_INT32,
    LILY_CHECKED_DATA_TYPE_KIND_INT64,
    LILY_CHECKED_DATA_TYPE_KIND_INT8,
    LILY_CHECKED_DATA_TYPE_KIND_ISIZE,
    LILY_CHECKED_DATA_TYPE_KIND_LAMBDA,
    LILY_CHECKED_DATA_TYPE_KIND_LIST,
    LILY_CHECKED_DATA_TYPE_KIND_MUT,
    LILY_CHECKED_DATA_TYPE_KIND_NEVER,
    LILY_CHECKED_DATA_TYPE_KIND_OPTIONAL,
    LILY_CHECKED_DATA_TYPE_KIND_PTR,
    LILY_CHECKED_DATA_TYPE_KIND_REF,
    LILY_CHECKED_DATA_TYPE_KIND_STR,
    LILY_CHECKED_DATA_TYPE_KIND_TRACE,
    LILY_CHECKED_DATA_TYPE_KIND_TUPLE,
    LILY_CHECKED_DATA_TYPE_KIND_UINT16,
    LILY_CHECKED_DATA_TYPE_KIND_UINT32,
    LILY_CHECKED_DATA_TYPE_KIND_UINT64,
    LILY_CHECKED_DATA_TYPE_KIND_UINT8,
    LILY_CHECKED_DATA_TYPE_KIND_UNIT,
    LILY_CHECKED_DATA_TYPE_KIND_UNKNOWN,
    LILY_CHECKED_DATA_TYPE_KIND_USIZE,
};

typedef struct LilyCheckedDataType LilyCheckedDataType;

typedef struct LilyCheckedDataTypeLambda
{
    Vec *params; // Vec<LilyCheckedDataType*>*?
    LilyCheckedDataType *return_type;
} LilyCheckedDataTypeLambda;

/**
 *
 * @brief Construct LilyCheckedDataTypeLambda type.
 */
inline CONSTRUCTOR(LilyCheckedDataTypeLambda,
                   LilyCheckedDataTypeLambda,
                   Vec *params,
                   LilyCheckedDataType *return_type)
{
    return (LilyCheckedDataTypeLambda){ .params = params,
                                        .return_type = return_type };
}

/**
 *
 * @brief Convert LilyCheckedDataTypeLambda in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeLambda,
               const LilyCheckedDataTypeLambda *self);
#endif

enum LilyCheckedDataTypeArrayKind
{
    LILY_CHECKED_DATA_TYPE_ARRAY_KIND_DYNAMIC,
    LILY_CHECKED_DATA_TYPE_ARRAY_KIND_MULTI_POINTERS,
    LILY_CHECKED_DATA_TYPE_ARRAY_KIND_SIZED,
    LILY_CHECKED_DATA_TYPE_ARRAY_KIND_UNDETERMINED,
};

typedef struct LilyCheckedDataTypeArray
{
    enum LilyCheckedDataTypeArrayKind kind;
    LilyCheckedDataType *data_type;
    union
    {
        Usize size;
    };
} LilyCheckedDataTypeArray;

/**
 *
 * @brief Construct LilyCheckedDataTypeArray type.
 */
inline CONSTRUCTOR(LilyCheckedDataTypeArray,
                   LilyCheckedDataTypeArray,
                   enum LilyCheckedDataTypeArrayKind kind,
                   LilyCheckedDataType *data_type)
{
    return (LilyCheckedDataTypeArray){ .kind = kind, .data_type = data_type };
}

/**
 *
 * @brief Construct LilyCheckedDataTypeArray type
 * (LILY_CHECKED_DATA_TYPE_ARRAY_KIND_SIZED).
 */
inline VARIANT_CONSTRUCTOR(LilyCheckedDataTypeArray,
                           LilyCheckedDataTypeArray,
                           sized,
                           LilyCheckedDataType *data_type,
                           Usize size)
{
    return (LilyCheckedDataTypeArray){
        .kind = LILY_CHECKED_DATA_TYPE_ARRAY_KIND_SIZED,
        .data_type = data_type,
        .size = size
    };
}

/**
 *
 * @brief Convert LilyCheckedDataTypeArrayKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeArrayKind,
               enum LilyCheckedDataTypeArrayKind self);
#endif

/**
 *
 * @brief Convert LilyCheckedDataTypeArray in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeArray,
               const LilyCheckedDataTypeArray *self);
#endif

enum LilyCheckedDataTypeCustomKind
{
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_CLASS,
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_ENUM,
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_ENUM_OBJECT,
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_RECORD,
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_RECORD_OBJECT,
    LILY_CHECKED_DATA_TYPE_CUSTOM_KIND_TRAIT,
};

/**
 *
 * @brief Convert LilyCheckedDataTypeCustomKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeCustomKind,
               enum LilyCheckedDataTypeCustomKind self);
#endif

typedef struct LilyCheckedDataTypeCustom
{
    String *name;  // String* (&)
    Vec *generics; // Vec<LilyCheckedDataType*>*?
    enum LilyCheckedDataTypeCustomKind kind;
} LilyCheckedDataTypeCustom;

/**
 *
 * @brief Construct LilyCheckedDataTypeCustom type.
 */
inline CONSTRUCTOR(LilyCheckedDataTypeCustom,
                   LilyCheckedDataTypeCustom,
                   String *name,
                   Vec *generics,
                   enum LilyCheckedDataTypeCustomKind kind)
{
    return (LilyCheckedDataTypeCustom){ .name = name,
                                        .generics = generics,
                                        .kind = kind };
}

/**
 *
 * @brief Convert LilyCheckedDataTypeCustom in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeCustom,
               const LilyCheckedDataTypeCustom *self);
#endif

struct LilyCheckedDataType
{
    enum LilyCheckedDataTypeKind kind;
    const Location *location; // const Location*? (&)
    union
    {
        LilyCheckedDataTypeArray array;
        LilyCheckedDataTypeCustom custom;
        LilyCheckedDataType *exception;
        LilyCheckedDataTypeLambda lambda;
        LilyCheckedDataType *list;
        LilyCheckedDataType *mut;
        LilyCheckedDataType *optional;
        LilyCheckedDataType *ptr;
        LilyCheckedDataType *ref;
        LilyCheckedDataType *trace;
        Vec *tuple; // Vec<LilyCheckedDataType*>*
    };
};

/**
 *
 * @brief Construct LilyCheckedDataType type.
 */
CONSTRUCTOR(LilyCheckedDataType *,
            LilyCheckedDataType,
            enum LilyCheckedDataTypeKind kind,
            const Location *location);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_ARRAY).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    array,
                    const Location *location,
                    LilyCheckedDataTypeArray array);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_CUSTOM).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    custom,
                    const Location *location,
                    LilyCheckedDataTypeCustom custom);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_EXCEPTION).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    exception,
                    const Location *location,
                    LilyCheckedDataType *exception);
/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_LAMBDA).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    lambda,
                    const Location *location,
                    LilyCheckedDataTypeLambda lambda);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_LIST).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    list,
                    const Location *location,
                    LilyCheckedDataType *list);

/**
 *
 * @brief Construct LilyCheckedDataType type (LILY_CHECKED_DATA_TYPE_KIND_MUT).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    mut,
                    const Location *location,
                    LilyCheckedDataType *mut);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_OPTIONAL).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    optional,
                    const Location *location,
                    LilyCheckedDataType *optional);

/**
 *
 * @brief Construct LilyCheckedDataType type (LILY_CHECKED_DATA_TYPE_KIND_PTR).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    ptr,
                    const Location *location,
                    LilyCheckedDataType *ptr);

/**
 *
 * @brief Construct LilyCheckedDataType type (LILY_CHECKED_DATA_TYPE_KIND_REF).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    ref,
                    const Location *location,
                    LilyCheckedDataType *ref);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_TRACE).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    trace,
                    const Location *location,
                    LilyCheckedDataType *trace);

/**
 *
 * @brief Construct LilyCheckedDataType type
 * (LILY_CHECKED_DATA_TYPE_KIND_TUPLE).
 */
VARIANT_CONSTRUCTOR(LilyCheckedDataType *,
                    LilyCheckedDataType,
                    tuple,
                    const Location *location,
                    Vec *tuple);

/**
 *
 * @brief Convert LilyCheckedDataTypeKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDataTypeKind,
               enum LilyCheckedDataTypeKind self);
#endif

/**
 *
 * @brief Convert LilyCheckedDataType in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyCheckedDataType, const LilyCheckedDataType *self);
#endif

/**
 *
 * @brief Print debug LilyCheckedDataType struct.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
void
IMPL_FOR_DEBUG(debug, LilyCheckedDataType, const LilyCheckedDataType *self);
#endif

/**
 *
 * @brief Returns true if the both data types are equal otherwise returns false.
 */
bool
eq__LilyCheckedDataType(const LilyCheckedDataType *self,
                        const LilyCheckedDataType *other);

/**
 *
 * @brief Clone LilyCheckedDataType type.
 */
LilyCheckedDataType *
clone__LilyCheckedDataType(LilyCheckedDataType *self);

/**
 *
 * @brief Free LilyCheckedDataType.
 */
DESTRUCTOR(LilyCheckedDataType, LilyCheckedDataType *self);

#endif // LILY_CORE_LILY_CHECKED_DATA_TYPE_H
