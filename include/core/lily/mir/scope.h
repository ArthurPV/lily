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

#ifndef LILY_CORE_LILY_MIR_SCOPE_H
#define LILY_CORE_LILY_MIR_SCOPE_H

#include <base/vec.h>

#include <core/lily/checked/data_type.h>

typedef struct LilyMirScopeParam
{
    LilyCheckedDataType *data_type; // LilyCheckedDataType* (&)
} LilyMirScopeParam;

/**
 *
 * @brief Construct LilyMirScopeParam type.
 */
CONSTRUCTOR(LilyMirScopeParam *,
            LilyMirScopeParam,
            LilyCheckedDataType *data_type);

/**
 *
 * @brief Free LilyMirScopeParam type.
 */
inline DESTRUCTOR(LilyMirScopeParam, LilyMirScopeParam *self)
{
    lily_free(self);
}

typedef struct LilyMirScopeVar
{
    String *name;                   // String* (&)
    LilyCheckedDataType *data_type; // LilyCheckedDataType* (&)
} LilyMirScopeVar;

/**
 *
 * @brief Construct LilyMirScopeVar type.
 */
CONSTRUCTOR(LilyMirScopeVar *,
            LilyMirScopeVar,
            String *name,
            LilyCheckedDataType *data_type);

/**
 *
 * @brief Free LilyMirScopeVar type.
 */
inline DESTRUCTOR(LilyMirScopeVar, LilyMirScopeVar *self)
{
    lily_free(self);
}

typedef struct LilyMirScope
{
    Vec *loads;  // Vec<LilyMirInstructionFunLoad*>*
    Vec *params; // Vec<LilyMirScopeParam*>*
    Vec *vars;   // Vec<LilyMirScopeVar*>*
    struct LilyMirScope *parent;
} LilyMirScope;

/**
 *
 * @brief Construct LilyMirScope type.
 */
inline CONSTRUCTOR(LilyMirScope, LilyMirScope, LilyMirScope *parent)
{
    return (LilyMirScope){ .loads = NEW(Vec),
                           .params = NEW(Vec),
                           .vars = NEW(Vec),
                           .parent = parent };
}

/**
 *
 * @brief Free LilyMirScope type.
 */
DESTRUCTOR(LilyMirScope, const LilyMirScope *self);

#endif // LILY_CORE_LILY_MIR_SCOPE_H