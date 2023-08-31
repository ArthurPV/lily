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

#include <core/lily/analysis/checked/body/fun.h>
#include <core/lily/analysis/checked/parent.h>
#include <core/lily/analysis/checked/stmt/switch.h>

#include <stdio.h>
#include <stdlib.h>

#ifdef ENV_DEBUG
#include <base/format.h>
#endif

static VARIANT_DESTRUCTOR(LilyCheckedStmtSwitchCaseValue,
                          union,
                          LilyCheckedStmtSwitchCaseValue *self);

VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    int,
                    Int64 int_)
{
    LilyCheckedStmtSwitchCaseValue *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCaseValue));

    self->kind = LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT;
    self->int_ = int_;

    return self;
}

VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    float,
                    Float64 float_)
{
    LilyCheckedStmtSwitchCaseValue *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCaseValue));

    self->kind = LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT;
    self->float_ = float_;

    return self;
}

VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    uint,
                    Uint64 uint)
{
    LilyCheckedStmtSwitchCaseValue *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCaseValue));

    self->kind = LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT;
    self->uint = uint;

    return self;
}

VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    else)
{
    LilyCheckedStmtSwitchCaseValue *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCaseValue));

    self->kind = LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE;

    return self;
}

VARIANT_CONSTRUCTOR(LilyCheckedStmtSwitchCaseValue *,
                    LilyCheckedStmtSwitchCaseValue,
                    union,
                    Vec *union_)
{
    LilyCheckedStmtSwitchCaseValue *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCaseValue));

    self->kind = LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION;
    self->union_ = union_;

    return self;
}

#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCaseValueKind,
               enum LilyCheckedStmtSwitchCaseValueKind self)
{
    switch (self) {
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT:
            return "LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT";
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT:
            return "LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT";
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT:
            return "LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT";
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE:
            return "LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE";
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION:
            return "LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION";
        default:
            UNREACHABLE("unknown variant");
    }
}
#endif

bool
eq__LilyCheckedStmtSwitchCaseValue(const LilyCheckedStmtSwitchCaseValue *self,
                                   const LilyCheckedStmtSwitchCaseValue *other)
{
    if (self->kind != other->kind) {
        if (self->kind == LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE ||
            other->kind == LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE) {
            return false;
        } else if (self->kind ==
                     LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION ||
                   other->kind ==
                     LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION) {
            if (self->kind == LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION) {
                for (Usize i = 0; i < self->union_->len; ++i) {
                    if (eq__LilyCheckedStmtSwitchCaseValue(
                          get__Vec(self->union_, i), other)) {
                        return true;
                    }
                }

                return false;
            }

            for (Usize i = 0; i < other->union_->len; ++i) {
                if (eq__LilyCheckedStmtSwitchCaseValue(
                      get__Vec(other->union_, i), self)) {
                    return true;
                }
            }

            return false;
        }

        UNREACHABLE("value must be the same");
    }

    switch (self->kind) {
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT:
            return self->int_ == other->int_;
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT:
            return self->float_ == other->float_;
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT:
            return self->uint == other->uint;
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE:
            return true;
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION:
            if (self->union_->len != other->union_->len) {
                return false;
            }

            for (Usize i = 0; i < self->union_->len; ++i) {
                if (!eq__LilyCheckedStmtSwitchCaseValue(
                      get__Vec(self->union_, i), get__Vec(other->union_, i))) {
                    return false;
                }
            }

            return true;
        default:
            UNREACHABLE("unknown variant");
    }
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCaseValue,
               const LilyCheckedStmtSwitchCaseValue *self)
{
    switch (self->kind) {
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_INT:
            return format__String(
              "LilyCheckedStmtSwitchCase{{ kind = {s}, int_ = {d} }",
              to_string__Debug__LilyCheckedStmtSwitchCaseValueKind(self->kind),
              self->int_);
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_FLOAT:
            return format__String(
              "LilyCheckedStmtSwitchCase{{ kind = {s}, float_ = {f} }",
              to_string__Debug__LilyCheckedStmtSwitchCaseValueKind(self->kind),
              self->float_);
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UINT:
            return format__String(
              "LilyCheckedStmtSwitchCase{{ kind = {s}, float_ = {u} }",
              to_string__Debug__LilyCheckedStmtSwitchCaseValueKind(self->kind),
              self->uint);
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_ELSE:
            return format__String(
              "LilyCheckedStmtSwitchCase{{ kind = {s} }",
              to_string__Debug__LilyCheckedStmtSwitchCaseValueKind(self->kind));
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION: {
            String *res = format__String(
              "LilyCheckedStmtSwitchCase{{ kind = {s}, union_ =",
              to_string__Debug__LilyCheckedStmtSwitchCaseValueKind(self->kind));

            DEBUG_VEC_STRING(self->union_, res, LilyCheckedStmtSwitchCaseValue);

            push_str__String(res, " }");

            return res;
        }
        default:
            UNREACHABLE("unknown variant");
    }
}
#endif

VARIANT_DESTRUCTOR(LilyCheckedStmtSwitchCaseValue,
                   union,
                   LilyCheckedStmtSwitchCaseValue *self)
{
    FREE_BUFFER_ITEMS(
      self->union_->buffer, self->union_->len, LilyCheckedStmtSwitchCaseValue);
    FREE(Vec, self->union_);
    lily_free(self);
}

DESTRUCTOR(LilyCheckedStmtSwitchCaseValue, LilyCheckedStmtSwitchCaseValue *self)
{
    switch (self->kind) {
        case LILY_CHECKED_STMT_SWITCH_CASE_VALUE_KIND_UNION:
            FREE_VARIANT(LilyCheckedStmtSwitchCaseValue, union, self);
            break;
        default:
            lily_free(self);
    }
}

CONSTRUCTOR(LilyCheckedStmtSwitchSubCase *,
            LilyCheckedStmtSwitchSubCase,
            LilyCheckedExpr *cond,
            LilyCheckedBodyFunItem *body_item)
{
    LilyCheckedStmtSwitchSubCase *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchSubCase));

    self->cond = cond;
    self->body_item = body_item;

    return self;
}

#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchSubCase,
               const LilyCheckedStmtSwitchSubCase *self)
{
    return format(
      "LilyCheckedStmtSwitchSubCase{{ cond = {Sr}, body_item = {Sr} }",
      self->cond ? to_string__Debug__LilyCheckedExpr(self->cond)
                 : from__String("NULL"),
      to_string__Debug__LilyCheckedBodyFunItem(self->body_item));
}
#endif

DESTRUCTOR(LilyCheckedStmtSwitchSubCase, LilyCheckedStmtSwitchSubCase *self)
{
    if (self->cond) {
        FREE(LilyCheckedExpr, self->cond);
    }

    FREE(LilyCheckedBodyFunItem, self->body_item);
    lily_free(self);
}

CONSTRUCTOR(LilyCheckedStmtSwitchCase *,
            LilyCheckedStmtSwitchCase,
            LilyCheckedStmtSwitchCaseValue *case_value,
            LilyCheckedExpr *cond,
            LilyCheckedBodyFunItem *body_item)
{
    LilyCheckedStmtSwitchCase *self =
      lily_malloc(sizeof(LilyCheckedStmtSwitchCase));

    self->case_value = case_value;
    self->sub_cases =
      init__Vec(1, NEW(LilyCheckedStmtSwitchSubCase, cond, body_item));

    return self;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitchCase,
               const LilyCheckedStmtSwitchCase *self)
{
    String *res = format__String(
      "LilyCheckedStmtSwitchCase{{ case_value = {Sr}",
      to_string__Debug__LilyCheckedStmtSwitchCaseValue(self->case_value));

    push_str__String(res, ", sub_cases =");

    DEBUG_VEC_STR(self->sub_cases, res, LilyCheckedStmtSwitchSubCase);

    push_str__String(res, " }");

    return res;
}
#endif

DESTRUCTOR(LilyCheckedStmtSwitchCase, LilyCheckedStmtSwitchCase *self)
{
    FREE(LilyCheckedStmtSwitchCaseValue, self->case_value);
    FREE_BUFFER_ITEMS(self->sub_cases->buffer,
                      self->sub_cases->len,
                      LilyCheckedStmtSwitchSubCase);
    FREE(Vec, self->sub_cases);
    lily_free(self);
}

void
add_case__LilyCheckedStmtSwitch(const LilyCheckedStmtSwitch *self,
                                LilyCheckedStmtSwitchCaseValue *case_value,
                                LilyCheckedExpr *cond,
                                LilyCheckedBodyFunItem *body_item)
{
    for (Usize i = 0; i < self->cases->len; ++i) {
        LilyCheckedStmtSwitchCase *case_ = get__Vec(self->cases, i);

        if (eq__LilyCheckedStmtSwitchCaseValue(case_value, case_->case_value)) {
            FREE(LilyCheckedStmtSwitchCaseValue, case_value);

            push__Vec(case_->sub_cases,
                      NEW(LilyCheckedStmtSwitchSubCase, cond, body_item));

            return;
        }
    }

    push__Vec(self->cases,
              NEW(LilyCheckedStmtSwitchCase, case_value, cond, body_item));
}

int
look_for_case_error__LilyCheckedStmtSwitch(
  const LilyCheckedStmtSwitch *self,
  LilyCheckedStmtSwitchCaseValue *case_value,
  LilyCheckedExpr *cond)
{
    for (Usize i = 0; i < self->cases->len; ++i) {
        LilyCheckedStmtSwitchCase *case_ = get__Vec(self->cases, i);

        if (!eq__LilyCheckedStmtSwitchCaseValue(case_->case_value,
                                                case_value)) {
            continue;
        }

        for (Usize j = 0; j < case_->sub_cases->len; ++j) {
            LilyCheckedStmtSwitchSubCase *sub_case =
              get__Vec(case_->sub_cases, j);

            if (sub_case->cond && cond) {
                if (eq__LilyCheckedExpr(sub_case->cond, cond)) {
                    return 2;
                }
            } else if (!sub_case->cond && !cond) {
                return 2;
            }

            // Check for unused case
            if (j + 1 == case_->sub_cases->len && cond && !sub_case->cond) {
                return 1;
            }
        }

        break;
    }

    return 0;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedStmtSwitch,
               const LilyCheckedStmtSwitch *self)
{
    String *res =
      format__String("LilyCheckedStmtSwitch{{ switched_expr = {Sr}, cases =",
                     to_string__Debug__LilyCheckedExpr(self->switched_expr));

    DEBUG_VEC_STRING(self->cases, res, LilyCheckedStmtSwitchCase);

    push_str__String(res, " }");

    return res;
}
#endif

DESTRUCTOR(LilyCheckedStmtSwitch, const LilyCheckedStmtSwitch *self)
{
    FREE(LilyCheckedExpr, self->switched_expr);
    FREE_BUFFER_ITEMS(
      self->cases->buffer, self->cases->len, LilyCheckedStmtSwitchCase);
    FREE(Vec, self->cases);
}
