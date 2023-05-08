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

#ifndef LILY_CORE_LILY_CHECKED_PATTERN_RECORD_CALL_H
#define LILY_CORE_LILY_CHECKED_PATTERN_RECORD_CALL_H

#include <base/string.h>
#include <base/macros.h>
#include <base/vec.h>

#include <core/lily/checked/expr.h>

typedef struct LilyCheckedPattern LilyCheckedPattern;

// <name> = <pattern>
// <pattern>
typedef struct LilyCheckedPatternRecordField
{
    String *name; // String*? (&)
    LilyCheckedPattern *pattern;
} LilyCheckedPatternRecordField;

/**
 *
 * @brief Construct LilyCheckedPatternRecordField type.
 */
CONSTRUCTOR(LilyCheckedPatternRecordField *,
            LilyCheckedPatternRecordField,
            String *name,
            LilyCheckedPattern *pattern);

/**
 *
 * @brief Convert LilyCheckedPatternRecordField in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedPatternRecordField,
               const LilyCheckedPatternRecordField *self);
#endif

/**
 *
 * @brief Free LilyCheckedPatternRecordField type.
 */
DESTRUCTOR(LilyCheckedPatternRecordField, LilyCheckedPatternRecordField *self);

typedef struct LilyCheckedPatternRecordCall
{
    LilyCheckedExpr *id; // LilyCheckedExpr*?
    Vec *fields;     // Vec<LilyCheckedPatternRecordField*>*
} LilyCheckedPatternRecordCall;

/**
 *
 * @brief Construct LilyCheckedPatternRecordCall type.
 */
inline CONSTRUCTOR(LilyCheckedPatternRecordCall,
                   LilyCheckedPatternRecordCall,
                   LilyCheckedExpr *id,
                   Vec *fields)
{
    return (LilyCheckedPatternRecordCall){ .id = id, .fields = fields };
}

/**
 *
 * @brief Convert LilyCheckedPatternRecordCall in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedPatternRecordCall,
               const LilyCheckedPatternRecordCall *self);
#endif

/**
 *
 * @brief Free LilyCheckedPatternRecordCall type.
 */
DESTRUCTOR(LilyCheckedPatternRecordCall, const LilyCheckedPatternRecordCall *self);

#endif // LILY_CORE_LILY_CHECKED_PATTERN_RECORD_CALL_H
