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

#include <base/alloc.h>

#include <core/lily/ast/decl/method.h>

#include <stdio.h>
#include <stdlib.h>

#ifdef ENV_DEBUG
#include <base/format.h>
#endif

// Free LilyAstDeclMethodParam type (LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT).
static VARIANT_DESTRUCTOR(LilyAstDeclMethodParam,
                          default,
                          LilyAstDeclMethodParam *self);

// Free LilyAstDeclFunParam type (LILY_AST_DECL_METHOD_PARAM_KIND_NORMAL).
static VARIANT_DESTRUCTOR(LilyAstDeclMethodParam,
                          normal,
                          LilyAstDeclMethodParam *self);

#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               LilyAstDeclMethodParamKind,
               enum LilyAstDeclMethodParamKind self)
{
    switch (self) {
        case LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT";
        case LILY_AST_DECL_METHOD_PARAM_KIND_NORMAL:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_NORMAL";
        case LILY_AST_DECL_METHOD_PARAM_KIND_MUT_SELF:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_MUT_SELF";
        case LILY_AST_DECL_METHOD_PARAM_KIND_REF_MUT_SELF:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_REF_MUT_SELF";
        case LILY_AST_DECL_METHOD_PARAM_KIND_REF_SELF:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_REF_SELF";
        case LILY_AST_DECL_METHOD_PARAM_KIND_SELF:
            return "LILY_AST_DECL_METHOD_PARAM_KIND_SELF";
        default:
            UNREACHABLE("unknown variant");
    }
}
#endif

VARIANT_CONSTRUCTOR(LilyAstDeclMethodParam *,
                    LilyAstDeclMethodParam,
                    default,
                    String *name,
                    LilyAstDataType *data_type,
                    Location location,
                    LilyAstExpr *default_)
{
    LilyAstDeclMethodParam *self = lily_malloc(sizeof(LilyAstDeclMethodParam));

    self->name = name;
    self->data_type = data_type;
    self->kind = LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT;
    self->location = location;
    self->default_ = default_;

    return self;
}

VARIANT_CONSTRUCTOR(LilyAstDeclMethodParam *,
                    LilyAstDeclMethodParam,
                    normal,
                    String *name,
                    LilyAstDataType *data_type,
                    Location location)
{
    LilyAstDeclMethodParam *self = lily_malloc(sizeof(LilyAstDeclMethodParam));

    self->name = name;
    self->data_type = data_type;
    self->kind = LILY_AST_DECL_METHOD_PARAM_KIND_NORMAL;
    self->location = location;

    return self;
}

CONSTRUCTOR(LilyAstDeclMethodParam *,
            LilyAstDeclMethodParam,
            enum LilyAstDeclMethodParamKind kind,
            Location location)
{
    LilyAstDeclMethodParam *self = lily_malloc(sizeof(LilyAstDeclMethodParam));

    self->name = NULL;
    self->data_type = NULL;
    self->kind = kind;
    self->location = location;

    return self;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyAstDeclMethodParam,
               const LilyAstDeclMethodParam *self)
{
    String *res = from__String("LilyAstDeclMethodParam{{ name = ");

    switch (self->kind) {
        case LILY_AST_DECL_METHOD_PARAM_KIND_SELF:
        case LILY_AST_DECL_METHOD_PARAM_KIND_MUT_SELF:
            push_str__String(res, "NULL");
            break;
        default:
            append__String(res, self->name);
            break;
    }

    push_str__String(res, ", data_type =");

    if (self->data_type) {
        String *s = to_string__Debug__LilyAstDataType(self->data_type);

        APPEND_AND_FREE(res, s);
    } else {
        push_str__String(res, " NULL");
    }

    {
        char *s =
          format(", kind = {s}, location = {sa}",
                 to_string__Debug__LilyAstDeclMethodParamKind(self->kind),
                 to_string__Debug__Location(&self->location));

        PUSH_STR_AND_FREE(res, s);
    }

    switch (self->kind) {
        case LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT: {
            String *s = to_string__Debug__LilyAstExpr(self->default_);

            push_str__String(res, ", expr = ");
            APPEND_AND_FREE(res, s);

            break;
        }
        default:
            break;
    }

    push_str__String(res, " }");

    return res;
}
#endif

VARIANT_DESTRUCTOR(LilyAstDeclMethodParam,
                   default,
                   LilyAstDeclMethodParam *self)
{
    FREE_MOVE(self->name, FREE(String, self->name));

    if (self->data_type) {
        FREE(LilyAstDataType, self->data_type);
    }

    FREE(LilyAstExpr, self->default_);
    lily_free(self);
}

VARIANT_DESTRUCTOR(LilyAstDeclMethodParam, normal, LilyAstDeclMethodParam *self)
{
    FREE_MOVE(self->name, FREE(String, self->name));

    if (self->data_type) {
        FREE(LilyAstDataType, self->data_type);
    }

    lily_free(self);
}

DESTRUCTOR(LilyAstDeclMethodParam, LilyAstDeclMethodParam *self)
{
    switch (self->kind) {
        case LILY_AST_DECL_METHOD_PARAM_KIND_DEFAULT:
            FREE_VARIANT(LilyAstDeclMethodParam, default, self);
            break;
        case LILY_AST_DECL_METHOD_PARAM_KIND_NORMAL:
            FREE_VARIANT(LilyAstDeclMethodParam, normal, self);
            break;
        case LILY_AST_DECL_METHOD_PARAM_KIND_REF_MUT_SELF:
        case LILY_AST_DECL_METHOD_PARAM_KIND_MUT_SELF:
        case LILY_AST_DECL_METHOD_PARAM_KIND_SELF:
        case LILY_AST_DECL_METHOD_PARAM_KIND_REF_SELF:
            lily_free(self);
            break;
        default:
            UNREACHABLE("unknown variant");
    }
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyAstDeclMethod, const LilyAstDeclMethod *self)
{
    String *res = format__String(
      "LilyAstDeclMethod{{ name = {S}, object_impl = ", self->name);

    if (self->object_impl) {
        append__String(res, self->object_impl);
    } else {
        push_str__String(res, "NULL");
    }

    push_str__String(res, ", generic_params =");

    if (self->generic_params) {
        DEBUG_VEC_STRING(self->generic_params, res, LilyAstGenericParam);
    } else {
        push_str__String(res, " NULL");
    }

    push_str__String(res, ", params =");

    if (self->params) {
        DEBUG_VEC_STRING(self->params, res, LilyAstDeclMethodParam);
    } else {
        push_str__String(res, " NULL");
    }

    push_str__String(res, ", return_data_type = ");

    if (self->return_data_type) {
        String *s = to_string__Debug__LilyAstDataType(self->return_data_type);

        APPEND_AND_FREE(res, s);
    } else {
        push_str__String(res, "NULL");
    }

    push_str__String(res, ", body =");
    DEBUG_VEC_STRING(self->body, res, LilyAstBodyFunItem);

    push_str__String(res, ", req =");

    if (self->req) {
        DEBUG_VEC_STRING(self->req, res, LilyAstExpr);
    } else {
        push_str__String(res, " NULL");
    }

    push_str__String(res, ", when =");

    if (self->when) {
        DEBUG_VEC_STRING(self->when, res, LilyAstExpr);
    } else {
        push_str__String(res, " NULL");
    }

    {
        char *s = format(", visibility = {s}, is_async = {b}, is_operator = "
                         "{b}, req_is_comptime = {b}, when_is_comptime = {b} }",
                         to_string__Debug__LilyVisibility(self->visibility),
                         self->is_async,
                         self->is_operator,
                         self->req_is_comptime,
                         self->when_is_comptime);

        PUSH_STR_AND_FREE(res, s);
    }

    return res;
}
#endif

DESTRUCTOR(LilyAstDeclMethod, const LilyAstDeclMethod *self)
{
    FREE_MOVE(self->name, FREE(String, self->name));
    FREE_MOVE(self->object_impl, FREE(String, self->object_impl));

    if (self->generic_params) {
        FREE_BUFFER_ITEMS(self->generic_params->buffer,
                          self->generic_params->len,
                          LilyAstGenericParam);
        FREE(Vec, self->generic_params);
    }

    if (self->params) {
        FREE_BUFFER_ITEMS(
          self->params->buffer, self->params->len, LilyAstDeclMethodParam);
        FREE(Vec, self->params);
    }

    if (self->return_data_type) {
        FREE(LilyAstDataType, self->return_data_type);
    }

    FREE_BUFFER_ITEMS(self->body->buffer, self->body->len, LilyAstBodyFunItem);
    FREE(Vec, self->body);

    if (self->req) {
        FREE_BUFFER_ITEMS(self->req->buffer, self->req->len, LilyAstExpr);
        FREE(Vec, self->req);
    }

    if (self->when) {
        FREE_BUFFER_ITEMS(self->when->buffer, self->when->len, LilyAstExpr);
        FREE(Vec, self->when);
    }
}
