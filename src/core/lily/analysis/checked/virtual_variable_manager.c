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

#include <base/string.h>

#include <core/lily/analysis/checked/virtual_variable_manager.h>

String *
generate_name__LilyCheckedVirtualVariableManager(
  const LilyCheckedVirtualVariableManager *self)
{
    String *name = format__String("{s}{zu}", self->base_name, self->names->len);

    push__Vec(self->names, name);

    return name;
}

LilyCheckedStmt
generate_variable__LilyCheckedVirtualVariableManager(
  const LilyCheckedVirtualVariableManager *self,
  Usize id,
  LilyCheckedScope *scope,
  LilyCheckedExpr *expr)
{
    String *name = generate_name__LilyCheckedVirtualVariableManager(self);

    add_variable__LilyCheckedScope(
      scope, NEW(LilyCheckedScopeContainerVariable, name, id));

    return NEW_VARIANT(LilyCheckedStmt,
                       variable,
                       expr->location,
                       NULL,
                       NEW(LilyCheckedStmtVariable,
                           name,
                           ref__LilyCheckedDataType(expr->data_type),
                           expr,
                           false));
}

DESTRUCTOR(LilyCheckedVirtualVariableManager,
           const LilyCheckedVirtualVariableManager *self)
{
    FREE_BUFFER_ITEMS(self->names->buffer, self->names->len, String);
    FREE(Vec, self->names);
}