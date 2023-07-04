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

#ifndef LILY_CORE_LILY_ANALYSIS_CHECKED_OPERATOR_H
#define LILY_CORE_LILY_ANALYSIS_CHECKED_OPERATOR_H

#include <base/macros.h>
#include <base/string.h>
#include <base/vec.h>

#include <core/lily/analysis/checked/data_type.h>

#define DEFAULT_OPERATORS_COUNT 663

typedef struct LilyCheckedOperator
{
    String *name;   // String* (&)
                    // [params, return_data_type]
    Vec *signature; // Vec<LilyCheckedDataType* (&)>* (&)
    Usize ref_count;
} LilyCheckedOperator;

/**
 *
 * @brief Construct LilyCheckedOperator type.
 */
CONSTRUCTOR(LilyCheckedOperator *,
            LilyCheckedOperator,
            String *name,
            Vec *signature);

/**
 *
 * @brief Reference LilyCheckedOperator type.
 */
inline LilyCheckedOperator *
ref__LilyCheckedOperator(LilyCheckedOperator *self)
{
    ++self->ref_count;
    return self;
}

/**
 *
 * @brief Valid operator name.
 */
bool
valid_operator__LilyCheckedOperator(String *name);

/**
 *
 * @brief Load default operators.
 */
LilyCheckedOperator *
load_default_operators__LilyCheckedOperator();

/**
 *
 * @brief Convert LilyCheckedOperator in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyCheckedOperator, const LilyCheckedOperator *self);
#endif

/**
 *
 * @brief Free LilyCheckedOperator type.
 */
inline DESTRUCTOR(LilyCheckedOperator, LilyCheckedOperator *self)
{
    if (self->ref_count > 0) {
        --self->ref_count;

        return;
    }

    lily_free(self);
}

#endif // LILY_CORE_LILY_ANALYSIS_CHECKED_OPERATOR_H
