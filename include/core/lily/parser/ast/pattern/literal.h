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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, PATTERNESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef LILY_CORE_LILY_AST_PATTERN_LITERAL_H
#define LILY_CORE_LILY_AST_PATTERN_LITERAL_H

#include <base/alloc.h>
#include <base/macros.h>
#include <base/new.h>
#include <base/string.h>
#include <base/types.h>

enum LilyAstPatternLiteralKind
{
    LILY_AST_PATTERN_LITERAL_KIND_BOOL,
    LILY_AST_PATTERN_LITERAL_KIND_BYTE,
    LILY_AST_PATTERN_LITERAL_KIND_BYTES,
    LILY_AST_PATTERN_LITERAL_KIND_CHAR,
    LILY_AST_PATTERN_LITERAL_KIND_CSTR,
    LILY_AST_PATTERN_LITERAL_KIND_FLOAT32,
    LILY_AST_PATTERN_LITERAL_KIND_FLOAT64,
    LILY_AST_PATTERN_LITERAL_KIND_INT32,
    LILY_AST_PATTERN_LITERAL_KIND_INT64,
    LILY_AST_PATTERN_LITERAL_KIND_NIL,
    LILY_AST_PATTERN_LITERAL_KIND_NONE,
    LILY_AST_PATTERN_LITERAL_KIND_STR,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT32,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT64,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT8,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT16,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT32,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT64,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_ISIZE,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT8,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT16,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT32,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT64,
    LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_USIZE,
    LILY_AST_PATTERN_LITERAL_KIND_UINT32,
    LILY_AST_PATTERN_LITERAL_KIND_UINT64,
    LILY_AST_PATTERN_LITERAL_KIND_UNDEF,
    LILY_AST_PATTERN_LITERAL_KIND_UNIT,
};

/**
 *
 * @brief Convert LilyAstPatternLiteralKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyAstPatternLiteralKind,
               enum LilyAstPatternLiteralKind self);
#endif

typedef struct LilyAstPatternLiteral
{
    enum LilyAstPatternLiteralKind kind;
    union
    {
        bool bool_;
        Uint8 byte;
        Uint8 *bytes;
        char char_;
        char *cstr;
        Float32 float32;
        Float64 float64;
        Int32 int32;
        Int64 int64;
        String *str;
        Float32 suffix_float32;
        Float64 suffix_float64;
        Int8 suffix_int8;
        Int16 suffix_int16;
        Int32 suffix_int32;
        Int64 suffix_int64;
        Isize suffix_isize;
        Uint8 suffix_uint8;
        Uint16 suffix_uint16;
        Uint32 suffix_uint32;
        Uint64 suffix_uint64;
        Usize suffix_usize;
        Uint32 uint32;
        Uint64 uint64;
    };
} LilyAstPatternLiteral;

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_BOOL).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           bool_,
                           bool bool_)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_BOOL,
                                    .bool_ = bool_ };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_BYTE).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           byte,
                           Uint8 byte)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_BYTE,
                                    .byte = byte };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_BYTES).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           bytes,
                           Uint8 *bytes)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_BYTES,
                                    .bytes = bytes };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_CHAR).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           char,
                           char char_)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_CHAR,
                                    .char_ = char_ };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_CSTR).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           cstr,
                           char *cstr)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_CSTR,
                                    .cstr = cstr };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_FLOAT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           float32,
                           Float32 float32)
{
    return (LilyAstPatternLiteral){ .kind =
                                      LILY_AST_PATTERN_LITERAL_KIND_FLOAT32,
                                    .float32 = float32 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_FLOAT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           float64,
                           Float64 float64)
{
    return (LilyAstPatternLiteral){ .kind =
                                      LILY_AST_PATTERN_LITERAL_KIND_FLOAT64,
                                    .float64 = float64 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_INT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           int32,
                           Int32 int32)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_INT32,
                                    .int32 = int32 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_INT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           int64,
                           Int64 int64)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_INT64,
                                    .int64 = int64 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_STR).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           str,
                           String *str)
{
    return (LilyAstPatternLiteral){ .kind = LILY_AST_PATTERN_LITERAL_KIND_STR,
                                    .str = str };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_float32,
                           Float32 suffix_float32)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT32,
        .suffix_float32 = suffix_float32
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_float64,
                           Float64 suffix_float64)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_FLOAT64,
        .suffix_float64 = suffix_float64
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT8).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_int8,
                           Int8 suffix_int8)
{
    return (LilyAstPatternLiteral){ .kind =
                                      LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT8,
                                    .suffix_int8 = suffix_int8 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT16).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_int16,
                           Int16 suffix_int16)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT16,
        .suffix_int16 = suffix_int16
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_int32,
                           Int32 suffix_int32)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT32,
        .suffix_int32 = suffix_int32
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_int64,
                           Int64 suffix_int64)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_INT64,
        .suffix_int64 = suffix_int64
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_ISIZE).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_isize,
                           Isize suffix_isize)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_ISIZE,
        .suffix_isize = suffix_isize
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT8).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_uint8,
                           Uint8 suffix_uint8)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT8,
        .suffix_uint8 = suffix_uint8
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT16).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_uint16,
                           Uint16 suffix_uint16)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT16,
        .suffix_uint16 = suffix_uint16
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_uint32,
                           Uint32 suffix_uint32)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT32,
        .suffix_uint32 = suffix_uint32
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_uint64,
                           Uint64 suffix_uint64)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT64,
        .suffix_uint64 = suffix_uint64
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_USIZE).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           suffix_usize,
                           Usize suffix_usize)
{
    return (LilyAstPatternLiteral){
        .kind = LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_USIZE,
        .suffix_usize = suffix_usize
    };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT32).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           uint32,
                           Uint32 uint32)
{
    return (LilyAstPatternLiteral){ .kind =
                                      LILY_AST_PATTERN_LITERAL_KIND_UINT32,
                                    .uint32 = uint32 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type
 * (LILY_AST_PATTERN_LITERAL_KIND_SUFFIX_UINT64).
 */
inline VARIANT_CONSTRUCTOR(LilyAstPatternLiteral,
                           LilyAstPatternLiteral,
                           uint64,
                           Uint64 uint64)
{
    return (LilyAstPatternLiteral){ .kind =
                                      LILY_AST_PATTERN_LITERAL_KIND_UINT64,
                                    .uint64 = uint64 };
}

/**
 *
 * @brief Construct LilyAstPatternLiteral type.
 */
inline CONSTRUCTOR(LilyAstPatternLiteral,
                   LilyAstPatternLiteral,
                   enum LilyAstPatternLiteralKind kind)
{
    return (LilyAstPatternLiteral){ .kind = kind };
}

/**
 *
 * @brief Convert LilyAstPatternLiteral in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyAstPatternLiteral,
               const LilyAstPatternLiteral *self);
#endif

/**
 *
 * @brief Free LilyAstPatternLiteral type (LILY_AST_PATTERN_LITERAL_KIND_STR).
 */
inline VARIANT_DESTRUCTOR(LilyAstPatternLiteral,
                          str,
                          const LilyAstPatternLiteral *self)
{
    FREE_MOVE(self->str, FREE(String, self->str));
}

/**
 *
 * @brief Free LilyAstPatternLiteral type.
 */
inline DESTRUCTOR(LilyAstPatternLiteral, const LilyAstPatternLiteral *self)
{
    switch (self->kind) {
        case LILY_AST_PATTERN_LITERAL_KIND_BYTES:
            lily_free(self->bytes);
            break;
        case LILY_AST_PATTERN_LITERAL_KIND_CSTR:
            lily_free(self->cstr);
            break;
        case LILY_AST_PATTERN_LITERAL_KIND_STR:
            FREE_VARIANT(LilyAstPatternLiteral, str, self);
            break;
        default:
            break;
    }
}

#endif // LILY_CORE_LILY_AST_PATTERN_LITERAL_H
