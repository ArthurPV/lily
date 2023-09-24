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

#ifndef LILY_CORE_LILY_DIAGNOSTIC_ERROR_H
#define LILY_CORE_LILY_DIAGNOSTIC_ERROR_H

#include <base/macros.h>

enum LilyErrorKind
{
    LILY_ERROR_KIND_UNEXPECTED_TOKEN,
    LILY_ERROR_KIND_UNCLOSED_CHAR_LITERAL,
    LILY_ERROR_KIND_INVALID_ESCAPE,
    LILY_ERROR_KIND_UNCLOSED_COMMENT_BLOCK,
    LILY_ERROR_KIND_INVALID_CHAR_LITERAL,
    LILY_ERROR_KIND_UNCLOSED_STRING_LITERAL,
    LILY_ERROR_KIND_INT8_OUT_OF_RANGE,
    LILY_ERROR_KIND_INT16_OUT_OF_RANGE,
    LILY_ERROR_KIND_INT32_OUT_OF_RANGE,
    LILY_ERROR_KIND_INT64_OUT_OF_RANGE,
    LILY_ERROR_KIND_UINT8_OUT_OF_RANGE,
    LILY_ERROR_KIND_UINT16_OUT_OF_RANGE,
    LILY_ERROR_KIND_UINT32_OUT_OF_RANGE,
    LILY_ERROR_KIND_UINT64_OUT_OF_RANGE,
    LILY_ERROR_KIND_ISIZE_OUT_OF_RANGE,
    LILY_ERROR_KIND_USIZE_OUT_OF_RANGE,
    LILY_ERROR_KIND_INVALID_LITERAL_SUFFIX,
    LILY_ERROR_KIND_INVALID_HEXADECIMAL_LITERAL,
    LILY_ERROR_KIND_INVALID_OCTAL_LITERAL,
    LILY_ERROR_KIND_INVALID_BIN_LITERAL,
    LILY_ERROR_KIND_INVALID_FLOAT_LITERAL,
    LILY_ERROR_KIND_MISMATCHED_CLOSING_DELIMITER,
    LILY_ERROR_KIND_UNEXPECTED_CHARACTER,
    LILY_ERROR_KIND_EXPECTED_IMPORT_VALUE,
    LILY_ERROR_KIND_EXPECTED_IDENTIFIER,
    LILY_ERROR_KIND_DUPLICATE_PACKAGE_DECLARATION,
    LILY_ERROR_KIND_PACKAGE_NAME_ALREADY_DEFINED,
    LILY_ERROR_KIND_BAD_IMPORT_VALUE,
    LILY_ERROR_KIND_UNKNOWN_IMPORT_AT_FLAG,
    LILY_ERROR_KIND_UNEXPECTED_CHARACTER_IN_IMPORT_VALUE,
    LILY_ERROR_KIND_NAME_CONFLICT,
    LILY_ERROR_KIND_EOF_NOT_EXPECTED,
    LILY_ERROR_KIND_EXPECTED_MODULE_IDENTIFIER,
    LILY_ERROR_KIND_EXPECTED_FUN_IDENTIFIER,
    LILY_ERROR_KIND_EXPECTED_TOKEN,
    LILY_ERROR_KIND_UNEXPECTED_TOKEN_IN_FUNCTION_BODY,
    LILY_ERROR_KIND_BAD_KIND_OF_TYPE,
    LILY_ERROR_KIND_IMPL_IS_ALREADY_DEFINED,
    LILY_ERROR_KIND_INHERIT_IS_ALREADY_DEFINED,
    LILY_ERROR_KIND_BAD_KIND_OF_OBJECT,
    LILY_ERROR_KIND_INHERIT_IS_NOT_EXPECTED,
    LILY_ERROR_KIND_IMPL_IS_NOT_EXPECTED,
    LILY_ERROR_KIND_EXPECTED_DATA_TYPE,
    LILY_ERROR_KIND_SET_IS_DUPLICATE,
    LILY_ERROR_KIND_GET_IS_DUPLICATE,
    LILY_ERROR_KIND_EXPECTED_EXPRESSION,
    LILY_ERROR_KIND_MISS_ONE_OR_MANY_EXPRESSIONS,
    LILY_ERROR_KIND_MISS_ONE_OR_MANY_IDENTIFIERS,
    LILY_ERROR_KIND_EXPECTED_ONE_OR_MANY_CHARACTERS,
    LILY_ERROR_KIND_FEATURE_NOT_YET_SUPPORTED,
    LILY_ERROR_KIND_EXPECTED_ASM_PARAM,
    LILY_ERROR_KIND_EXPECTED_COMPTIME_STRING_LITERAL,
    LILY_ERROR_KIND_EXPECTED_ONLY_ONE_EXPRESSION,
    LILY_ERROR_KIND_VARIABLE_DECLARATION_IS_NOT_EXPECTED,
    LILY_ERROR_KIND_EXPECTED_ONLY_ONE_PATTERN,
    LILY_ERROR_KIND_EXPECTED_ONLY_ONE_DATA_TYPE,
    LILY_ERROR_KIND_EXPECTED_ONLY_ONE_GENERIC_PARAM,
    LILY_ERROR_KIND_UNKNOWN_FROM_VALUE_IN_LIB,
    LILY_ERROR_KIND_MACRO_IS_NOT_FOUND,
    LILY_ERROR_KIND_MACRO_DO_NOTHING,
    LILY_ERROR_KIND_MACRO_EXPAND_MISS_FEW_PARAMS,
    LILY_ERROR_KIND_MACRO_EXPAND_HAVE_TOO_MANY_PARAMS,
    LILY_ERROR_KIND_EXPECTED_IDENTIFIER_DOLLAR,
    LILY_ERROR_KIND_UNKNOWN_MACRO_DATA_TYPE,
    LILY_ERROR_KIND_EXPECTED_MACRO_DATA_TYPE,
    LILY_ERROR_KIND_MACRO_EXPECTED_ID,
    LILY_ERROR_KIND_MACRO_EXPECTED_DT,
    LILY_ERROR_KIND_MACRO_EXPECTED_TK,
    LILY_ERROR_KIND_MACRO_EXPECTED_TKS,
    LILY_ERROR_KIND_MACRO_EXPECTED_STMT,
    LILY_ERROR_KIND_MACRO_EXPECTED_EXPR,
    LILY_ERROR_KIND_MACRO_EXPECTED_PATH,
    LILY_ERROR_KIND_MACRO_EXPECTED_PATT,
    LILY_ERROR_KIND_MACRO_EXPECTED_BLOCK,
    LILY_ERROR_KIND_MACRO_DUPLICATE_PARAM,
    LILY_ERROR_KIND_MACRO_IDENTIFIER_NOT_FOUND,
    LILY_ERROR_KIND_EXPECTED_PATTERN,
    LILY_ERROR_KIND_DUPLICATE_CONSTANT,
    LILY_ERROR_KIND_DUPLICATE_ERROR,
    LILY_ERROR_KIND_DUPLICATE_FUN,
    LILY_ERROR_KIND_DUPLICATE_MODULE,
    LILY_ERROR_KIND_DUPLICATE_CLASS,
    LILY_ERROR_KIND_DUPLICATE_ENUM_OBJECT,
    LILY_ERROR_KIND_DUPLICATE_RECORD_OBJECT,
    LILY_ERROR_KIND_DUPLICATE_TRAIT,
    LILY_ERROR_KIND_DUPLICATE_ALIAS,
    LILY_ERROR_KIND_DUPLICATE_ENUM,
    LILY_ERROR_KIND_DUPLICATE_RECORD,
    LILY_ERROR_KIND_DUPLICATE_PARAM_NAME,
    LILY_ERROR_KIND_DUPLICATE_VARIABLE,
    LILY_ERROR_KIND_BREAK_IS_NOT_EXPECTED_IN_THIS_CONTEXT,
    LILY_ERROR_KIND_NEXT_IS_NOT_EXPECTED_IN_THIS_CONTEXT,
    LILY_ERROR_KIND_DATA_TYPE_DONT_MATCH,
    LILY_ERROR_KIND_PATH_IS_NOT_EXPECTED_AFTER_SYS_IMPORT_FLAG,
    LILY_ERROR_KIND_IDENTIFIER_NOT_FOUND,
    LILY_ERROR_KIND_VALUE_HAS_BEEN_MOVED,
    LILY_ERROR_KIND_CANNOT_USE_ANY_IN_SAFE_MODE,
    LILY_ERROR_KIND_CANNOT_CAST_TO_ANY_IN_SAFE_MODE,
    LILY_ERROR_KIND_BAD_LITERAL_CAST,
    LILY_ERROR_KIND_UNKNOWN_CAST,
    LILY_ERROR_KIND_EXPECTED_MAIN_FUNCTION,
    LILY_ERROR_KIND_EXPECTED_MUTABLE_VARIABLE,
    LILY_ERROR_KIND_EXPECTED_BOOLEAN_EXPRESSION,
    LILY_ERROR_KIND_FUNCTION_IS_NOT_FOUND,
    LILY_ERROR_KIND_DUPLICATE_FIELD,
    LILY_ERROR_KIND_UNKNOWN_TYPE,
    LILY_ERROR_KIND_FIELD_IS_NOT_FOUND,
    LILY_ERROR_KIND_BAD_SYS_FUNCTION,
    LILY_ERROR_KIND_IMPORT_SYS_REQUIRED,
    LILY_ERROR_KIND_TOO_MANY_ITEMS_IN_MACRO_EXPAND,
    LILY_ERROR_KIND_DUPLICATE_VARIANT,
    LILY_ERROR_KIND_IMPORT_BUILTIN_REQUIRED,
    LILY_ERROR_KIND_BAD_BUILTIN_FUNCTION,
    LILY_ERROR_KIND_DATA_TYPE_NOT_FOUND,
    LILY_ERROR_KIND_EXPECTED_DATA_TYPE_IS_NOT_GUARANTEED,
    LILY_ERROR_KIND_CALL_NOT_EXPECTED_IN_THIS_CONTEXT,
    LILY_ERROR_KIND_NUMBER_OF_PARAMS_MISMATCHED,
    LILY_ERROR_KIND_TOO_MANY_PARAMS,
    LILY_ERROR_KIND_DEFAULT_PARAM_IS_NOT_EXPECTED,
    LILY_ERROR_KIND_THERE_IS_NO_FIELD_IN_TRAIT,
    LILY_ERROR_KIND_EXPECTED_CUSTOM_DATA_TYPE,
    LILY_ERROR_KIND_EXPECTED_OBJECT_DECL_AS_PARENT,
    LILY_ERROR_KIND_EXPECTED_METHOD_AS_PARENT,
    LILY_ERROR_KIND_THIS_KIND_OF_DATA_TYPE_IS_NOT_EXPECTED,
    LILY_ERROR_KIND_MAIN_FUNCTION_IS_NOT_CALLABLE,
    LILY_ERROR_KIND_IMPOSSIBLE_TO_GET_RETURN_DATA_TYPE,
    LILY_ERROR_KIND_COMPTIME_CAST_OVERFLOW,
    LILY_ERROR_KIND_THIS_DATA_TYPE_CANNOT_BE_DROPPED,
    LILY_ERROR_KIND_VALUE_HAS_BEEN_DROPPED,
    LILY_ERROR_KIND_THIS_KIND_OF_VALUE_IS_NOT_ALLOWED_TO_BE_DROP,
    LILY_ERROR_KIND_THIS_KIND_OF_EXPR_IS_NOT_ALLOWED_TO_BE_DROP,
    LILY_ERROR_KIND_ERROR_DECL_NOT_FOUND,
    LILY_ERROR_KIND_DATA_TYPE_DONT_MATCH_WITH_INFER_DATA_TYPE,
    LILY_ERROR_KIND_GENERIC_PARAMS_ARE_NOT_EXPECTED_IN_MAIN_FUNCTION,
    LILY_ERROR_KIND_NO_EXPLICIT_PARAMS_ARE_EXPECTED_IN_MAIN_FUNCTION,
    LILY_ERROR_KIND_OPERATOR_CANNOT_HAVE_COMPILER_DEFINED_DATA_TYPE_AS_PARAMETER,
    LILY_ERROR_KIND_THIS_RETURN_DATA_TYPE_IS_NOT_EXPECTED_FOR_A_MAIN_FUNCTION,
    LILY_ERROR_KIND_OPERATOR_MUST_HAVE_RETURN_DATA_TYPE,
    LILY_ERROR_KIND_MAIN_FUNCTION_CANNOT_BE_RECURSIVE,
    LILY_ERROR_KIND_OPERATOR_IS_NOT_VALID,
    LILY_ERROR_KIND_DUPLICATE_OPERATOR,
    LILY_ERROR_KIND_INFINITE_DATA_TYPE,
    LILY_ERROR_KIND_TUPLES_HAVE_NOT_SAME_SIZE,
    LILY_ERROR_KIND_EXPECTED_FUN_CALL,
    LILY_ERROR_KIND_UNEXPECTED_CLOSE,
    LILY_ERROR_KIND_SELF_IMPORT,
    LILY_ERROR_KIND_RECURSIVE_IMPORT,
    LILY_ERROR_KIND_EXPECTED_ERROR_DATA_TYPE,
    LILY_ERROR_KIND_UNEXPECTED_CALL_EXPR,
    LILY_ERROR_KIND_UNEXPECTED_WILDCARD,
    LILY_ERROR_KIND_UNEXPECTED_PATH,
    LILY_ERROR_KIND_EXPECTED_PATH,
    LILY_ERROR_KIND_RESTRICTED_CHARACTER_ON_IDENTIFIER_STRING
};

typedef struct LilyError
{
    enum LilyErrorKind kind;
    union
    {
        char *unexpected_token;
    };
} LilyError;

/**
 *
 * @brief Construct LilyError.
 */
inline CONSTRUCTOR(LilyError, LilyError, enum LilyErrorKind kind)
{
    return (LilyError){ .kind = kind };
}

/**
 *
 * @brief Construct LilyError (LILY_ERROR_KIND_UNEXPECTED_TOKEN).
 */
inline VARIANT_CONSTRUCTOR(LilyError,
                           LilyError,
                           unexpected_token,
                           char *unexpected_token)
{
    return (LilyError){ .kind = LILY_ERROR_KIND_UNEXPECTED_TOKEN,
                        .unexpected_token = unexpected_token };
}

/**
 *
 * @brief Convert LilyError in msg.
 */
char *
to_msg__LilyError(const LilyError *self);

/**
 *
 * @brief Convert LilyError in code.
 */
char *
to_code__LilyError(const LilyError *self);

/**
 *
 * @brief Convert LilyError in str.
 */
char *
to_string__LilyError(const LilyError *self);

#endif // LILY_CORE_LILY_DIAGNOSTIC_ERROR_H
