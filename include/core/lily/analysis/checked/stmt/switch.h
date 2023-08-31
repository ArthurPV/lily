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

#ifndef LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_SWITCH_H
#define LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_SWITCH_H

#include <base/alloc.h>
#include <base/macros.h>
#include <base/string.h>
#include <base/types.h>
#include <base/vec.h>

#include <core/lily/analysis/checked/expr.h>

typedef struct LilyCheckedBodyFunItem LilyCheckedBodyFunItem;

enum LilyCheckedStmtSwitchCaseValueKind
{
    LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT,
    LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT,
    LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT,
    LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE,
    LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION
};

/**
 *
 * @brief Convert LilyCheckedStmtSwitchCaseValueKind in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCaseValueKind,
               enum LilyCheckedStmtSwitchCaseValueKind self);
#endif

typedef struct LilyCheckedStmtSwitchCaseValue
{
    enum LilyCheckedStmtSwitchCaseValueKind kind;
    union
    {
        Int64 int_;
        Float64 float_;
        Uint64 uint;
        Vec *union_; // Vec<LilyCheckedStmtSwitchCaseValue*>*
    };
} LilyCheckedStmtSwitchCaseValue;

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCaseValue type
 * (LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT).
 */
VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    int,
                    Int64 int_);

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCaseValue type
 * (LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT).
 */
VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    float,
                    Float64 float_);

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCaseValue type
 * (LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT).
 */
VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    uint,
                    Uint64 uint);

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCaseValue type
 * (LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE).
 */
VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    else);

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCaseValue type
 * (LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION).
 */
VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    union,
                    Vec *union_);

/**
 *
 * @brief Check if the both values are equal.
 */
bool
eq__LilyCheckedStmtSwitchCaseValue(const LilyCheckedStmtSwitchCaseValue *self,
                                   const LilyCheckedStmtSwitchCaseValue *other);

/**
 *
 * @brief Convert LilyCheckedStmtSwitchCaseValue in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCaseValue,
               const LilyCheckedStmtSwitchCaseValue *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtSwitchCaseValue type.
 */
DESTRUCTOR(LilyCheckedStmtSwitchCaseValue,
           LilyCheckedStmtSwitchCaseValue *self);

typedef struct LilyCheckedStmtSwitchSubCase
{
    LilyCheckedExpr *cond; // LilyCheckedExpr*?
    LilyCheckedBodyFunItem *body_item;
} LilyCheckedStmtSwitchSubCase;

/**
 *
 * @brief Construct LilyCheckedStmtSwitchSubCase type.
 */
CONSTRUCTOR(LilyCheckedStmtSwitchSubCase *,
            LilyCheckedStmtSwitchSubCase,
            LilyCheckedExpr *cond,
            LilyCheckedBodyFunItem *body_item);

/**
 *
 * @brief Convert LilyCheckedStmtSwitchSubCase in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchSubCase,
               const LilyCheckedStmtSwitchSubCase *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtSwitchSubCase type.
 */
DESTRUCTOR(LilyCheckedStmtSwitchSubCase, LilyCheckedStmtSwitchSubCase *self);

// <case_value>:
// 		if <sub_case.cond> do
// 			<sub_case.body_item>
// 			break;
// 		end
// 		... (else case)
// 		<sub_case.body_item>
// 		break;
typedef struct LilyCheckedStmtSwitchCase
{
    LilyCheckedStmtSwitchCaseValue *case_value;
    Vec *sub_cases; // Vec<LilyCheckedStmtSwitchSubCase*>*
} LilyCheckedStmtSwitchCase;

/**
 *
 * @brief Construct LilyCheckedStmtSwitchCase type.
 */
CONSTRUCTOR(LilyCheckedStmtSwitchCase *,
            LilyCheckedStmtSwitchCase,
            LilyCheckedStmtSwitchCaseValue *case_value,
            LilyCheckedExpr *cond,
            LilyCheckedBodyFunItem *body_item);

/**
 *
 * @brief Convert LilyCheckedStmtSwitchCase in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCase,
               const LilyCheckedStmtSwitchCase *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtSwitchCase type.
 */
DESTRUCTOR(LilyCheckedStmtSwitchCase, LilyCheckedStmtSwitchCase *self);

typedef struct LilyCheckedStmtSwitch
{
    LilyCheckedExpr *switched_expr;
    Vec *cases; // Vec<LilyCheckedStmtSwitchCase*>*
} LilyCheckedStmtSwitch;

/**
 *
 * @brief Construct LilyCheckedStmtSwitch type.
 */
inline CONSTRUCTOR(LilyCheckedStmtSwitch,
                   LilyCheckedStmtSwitch,
                   LilyCheckedExpr *switched_expr,
                   Vec *cases)
{
    return (LilyCheckedStmtSwitch){ .switched_expr = switched_expr,
                                    .cases = cases };
}

/**
 *
 * @brief Add case to switch statement.
 */
void
add_case__LilyCheckedStmtSwitch(const LilyCheckedStmtSwitch *self,
                           LilyCheckedStmtSwitchCaseValue *case_value,
                           LilyCheckedExpr *cond,
                           LilyCheckedBodyFunItem *body_item);

/**
 *
 * @brief Convert LilyCheckedStmtSwitch in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitch,
               const LilyCheckedStmtSwitch *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtSwitch type.
 */
DESTRUCTOR(LilyCheckedStmtSwitch, const LilyCheckedStmtSwitch *self);

#endif // LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_SWITCH_H
