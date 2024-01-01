/*
 * MIT License
 *
 * Copyright (c) 2022-2024 ArthurPV
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

#ifndef LILY_CORE_CC_CI_PARSER_H
#define LILY_CORE_CC_CI_PARSER_H

#include <core/cc/ci/scanner.h>

typedef struct CIResultFile CIResultFile;

typedef struct CIParser
{
    CIResultFile *file;       // CIResultFile* (&)
    const CIScanner *scanner; // CIScanner* (&)
    Usize *count_error;       // Usize* (&)
    Usize position;
    CIToken *current;  // CIToken* (&)
    CIToken *previous; // CIToken* (&)
} CIParser;

/**
 *
 * @brief Construct CIParser type.
 */
CONSTRUCTOR(CIParser, CIParser, CIResultFile *file, const CIScanner *scanner);

/**
 *
 * @brief Run the parser.
 */
void
run__CIParser(CIParser *self);

#endif // LILY_CORE_CC_CI_PARSER_H