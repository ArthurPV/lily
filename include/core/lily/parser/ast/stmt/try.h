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

#ifndef LILY_CORE_LILY_AST_STMT_TRY_H
#define LILY_CORE_LILY_AST_STMT_TRY_H

#include <base/macros.h>

#include <core/lily/parser/ast/expr.h>

#ifdef ENV_DEBUG
#include <base/string.h>
#endif

typedef struct LilyAstStmtTry
{
    Vec *try_body;           // Vec<LilyAstBodyFunItem*>*
    LilyAstExpr *catch_expr; // LilyAstExpr*?
    Vec *catch_body;         // Vec<LilyAstBodyFunItem*>*?
} LilyAstStmtTry;

/**
 *
 * @brief Construct LilyAstStmtTry type.
 */
inline CONSTRUCTOR(LilyAstStmtTry,
                   LilyAstStmtTry,
                   Vec *try_body,
                   LilyAstExpr *catch_expr,
                   Vec *catch_body)
{
    return (LilyAstStmtTry){ .try_body = try_body,
                             .catch_expr = catch_expr,
                             .catch_body = catch_body };
}

/**
 *
 * @brief Convert LilyAstStmtTry in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyAstStmtTry, const LilyAstStmtTry *self);
#endif

/**
 *
 * @brief Free LilyAstStmtTry type.
 */
DESTRUCTOR(LilyAstStmtTry, const LilyAstStmtTry *self);

#endif // LILY_CORE_LILY_AST_STMT_TRY_H
