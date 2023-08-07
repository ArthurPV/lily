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

#ifndef LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_MATCH_H
#define LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_MATCH_H

#include <core/lily/analysis/checked/expr.h>
#include <core/lily/analysis/checked/pattern.h>

#include <base/ordered_hash_map.h>

typedef struct LilyCheckedBodyFunItem LilyCheckedBodyFunItem;

typedef struct LilyCheckedStmtMatchCase
{
    OrderedHashMap
      *captured_variables; // OrderedHashMap<LilyCheckedCapturedVariable*>*
    LilyCheckedPattern *pattern;
    LilyCheckedExpr *cond; // LilyCheckedExpr*?
    LilyCheckedBodyFunItem *body_item;
} LilyCheckedStmtMatchCase;

/**
 *
 * @brief Construct LilyCheckedStmtMatchCase type.
 */
CONSTRUCTOR(LilyCheckedStmtMatchCase *,
            LilyCheckedStmtMatchCase,
            OrderedHashMap *captured_variables,
            LilyCheckedPattern *pattern,
            LilyCheckedExpr *cond,
            LilyCheckedBodyFunItem *body_item);

/**
 *
 * @brief Convert LilyCheckedStmtMatchCase in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtMatchCase,
               const LilyCheckedStmtMatchCase *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtMatchCase type.
 */
DESTRUCTOR(LilyCheckedStmtMatchCase, LilyCheckedStmtMatchCase *self);

typedef struct LilyCheckedStmtMatch
{
    LilyCheckedExpr *expr;
    Vec *cases; // Vec<LilyCheckedStmtMatchCase*>*
    bool use_switch;
} LilyCheckedStmtMatch;

/**
 *
 * @brief Construct LilyCheckedStmtMatch type.
 */
inline CONSTRUCTOR(LilyCheckedStmtMatch,
                   LilyCheckedStmtMatch,
                   LilyCheckedExpr *expr,
                   Vec *cases,
                   bool use_switch)
{
    return (LilyCheckedStmtMatch){ .expr = expr,
                                   .cases = cases,
                                   .use_switch = use_switch };
}

/**
 *
 * @brief Convert LilyCheckedStmtMatch in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtMatch,
               const LilyCheckedStmtMatch *self);
#endif

/**
 *
 * @brief Free LilyCheckedStmtMatch type.
 */
DESTRUCTOR(LilyCheckedStmtMatch, const LilyCheckedStmtMatch *self);

#endif // LILY_CORE_LILY_ANALYSIS_CHECKED_STMT_MATCH_H
