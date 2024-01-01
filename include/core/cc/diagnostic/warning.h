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

#ifndef LILY_CORE_CC_DIAGNOSTIC_WARNING_H
#define LILY_CORE_CC_DIAGNOSTIC_WARNING_H

#include <base/macros.h>

enum CcWarningKind
{
    CC_WARNING_KIND_UNUSED_PAREN
};

typedef struct CcWarning
{
    enum CcWarningKind kind;
    // union
    // {};
} CcWarning;

/**
 *
 * @brief Construct CppWarning.
 */
inline CONSTRUCTOR(CcWarning, CcWarning, enum CcWarningKind kind)
{
    return (CcWarning){ .kind = kind };
}

/**
 *
 * @brief Convert CcWarning in msg.
 */
char *
to_msg__CcWarning(const CcWarning *self);

/**
 *
 * @brief Convert CcWarning in code.
 */
char *
to_code__CcWarning(const CcWarning *self);

/**
 *
 * @brief Convert CcWarning in str.
 */
char *
to_string__CcWarning(const CcWarning *self);

#endif // LILY_CORE_CC_DIAGNOSTIC_WARNING_H
