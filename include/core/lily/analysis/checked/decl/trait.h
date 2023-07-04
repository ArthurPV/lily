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

#ifndef LILY_CORE_LILY_CHECKED_DECL_TRAIT_H
#define LILY_CORE_LILY_CHECKED_DECL_TRAIT_H

#include <base/macros.h>
#include <base/string.h>
#include <base/vec.h>

#include <core/lily/analysis/checked/body/trait.h>
#include <core/lily/analysis/checked/generic_param.h>
#include <core/lily/analysis/checked/inherit_param.h>
#include <core/lily/analysis/checked/scope.h>
#include <core/lily/shared/visibility.h>

typedef struct LilyCheckedDeclTrait
{
    String *name; // String* (&)
    String *global_name;
    Vec *generic_params; // Vec<LilyCheckedGenericParam*>*?
    Vec *inherit_params; // Vec<LilyCheckedInheritParam*>*?
    Vec *body;           // Vec<LilyCheckedBodyTraitItem*>*
    LilyCheckedScope *scope;
    enum LilyVisibility visibility;
    bool is_checked;
} LilyCheckedDeclTrait;

/**
 *
 * @brief Construct LilyCheckedDeclTrait type.
 */
inline CONSTRUCTOR(LilyCheckedDeclTrait,
                   LilyCheckedDeclTrait,
                   String *name,
                   String *global_name,
                   Vec *generic_params,
                   Vec *inherit_params,
                   Vec *body,
                   LilyCheckedScope *scope,
                   enum LilyVisibility visibility)
{
    return (LilyCheckedDeclTrait){ .name = name,
                                   .global_name = global_name,
                                   .generic_params = generic_params,
                                   .inherit_params = inherit_params,
                                   .body = body,
                                   .scope = scope,
                                   .visibility = visibility,
                                   .is_checked = false };
}

/**
 *
 * @brief Convert LilyCheckedDeclTrait in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedDeclTrait,
               const LilyCheckedDeclTrait *self);
#endif

/**
 *
 * @brief Free LilyCheckedDeclTrait type.
 */
DESTRUCTOR(LilyCheckedDeclTrait, const LilyCheckedDeclTrait *self);

#endif // LILY_CORE_LILY_CHECKED_DECL_TRAIT_H
