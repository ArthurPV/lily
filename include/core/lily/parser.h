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

#ifndef LILY_CORE_LILY_PARSER_H
#define LILY_CORE_LILY_PARSER_H

#include <base/macros.h>
#include <base/new.h>
#include <base/types.h>
#include <base/vec.h>

#include <core/lily/ast/data_type.h>
#include <core/lily/ast/expr.h>
#include <core/lily/preparser.h>

typedef struct LilyPackage LilyPackage;
typedef struct LilyParser LilyParser;

typedef struct LilyParseBlock
{
    Vec *tokens; // Vec<LilyToken*>*
    LilyToken *current;
    LilyToken *previous;
    const File *file;
    Usize *count_error;
    Usize *count_warning;
    Usize position;
} LilyParseBlock;

/**
 *
 * @brief Construct LilyParseBlock
 */
CONSTRUCTOR(LilyParseBlock, LilyParseBlock, LilyParser *parser, Vec *tokens);

struct LilyParser
{
    Vec *decls; // Vec<LilyAstDecl*>*
    LilyPackage *package;
    LilyPreparserDecl *current;
    Usize position;
};

/**
 *
 * @brief Construct LilyParser type.
 */
inline CONSTRUCTOR(LilyParser, LilyParser, LilyPackage *package)
{
    return (LilyParser){
        .decls = NEW(Vec), .package = package, .current = NULL, .position = 0
    };
}

/**
 *
 * @brief Parse data type.
 * @note This function is only used in test.
 */
TEST(LilyAstDataType *, parse_data_type, LilyParseBlock *self);

/**
 *
 * @brief Parse expression.
 * @note This function is only used in test.
 */
TEST(LilyAstExpr *, parse_expr, LilyParseBlock *self);

/**
 *
 * @brief Run the parser.
 */
void
run__LilyParser(LilyParser *self);

/**
 *
 * @brief Free LilyParser type.
 */
DESTRUCTOR(LilyParser, const LilyParser *self);

#endif // LILY_CORE_LILY_PARSER_H
