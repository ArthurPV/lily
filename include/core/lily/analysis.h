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

#ifndef LILY_CORE_LILY_ANALYSIS_H
#define LILY_CORE_LILY_ANALYSIS_H

#include <base/vec.h>

#include <core/lily/ast.h>
#include <core/lily/checked.h>
#include <core/lily/parser.h>

typedef struct LilyPackage LilyPackage;

typedef struct LilyAnalysis
{
    Vec *decls; // Vec<LilyCheckedDecl*>*
    LilyPackage *package;
    LilyPackage *root_package;
    LilyAstDecl *current;
    const LilyParser *parser;
    Usize position;
} LilyAnalysis;

/**
 *
 * @brief Construct LilyAnalysis type.
 */
CONSTRUCTOR(LilyAnalysis,
            LilyAnalysis,
            LilyPackage *package,
            LilyPackage *root_package,
            const LilyParser *parser);

/**
 *
 * @brief Free LilyAnalysis type.
 */
DESTRUCTOR(LilyAnalysis, const LilyAnalysis *self);

#endif // LILY_CORE_LILY_ANALYSIS_H
