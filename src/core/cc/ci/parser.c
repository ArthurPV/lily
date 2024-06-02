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

// TODO: add a typecheck.

#include <base/assert.h>
#include <base/atof.h>
#include <base/atoi.h>
#include <base/format.h>

#include <core/cc/ci/diagnostic/error.h>
#include <core/cc/ci/include.h>
#include <core/cc/ci/parser.h>
#include <core/cc/ci/result.h>
#include <core/shared/diagnostic.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

struct CIParserContext
{
    const CIDecl *current_decl; // CIDecl*? (&)
    const CIExpr *current_expr; // CIExpr*? (&)
    const CIStmt *current_stmt; // CIStmt*? (&)
};

static inline CONSTRUCTOR(struct CIParserContext, CIParserContext);

static inline void
add_macro__CIParser(CIParser *self, CIParserMacroCall *macro_call);

static inline void
init_function_to_visit_waiting_list__CIParser(CIParser *self, String *name);

static inline void
init_typedef_to_visit_waiting_list__CIParser(CIParser *self, String *name);

static inline void
init_union_to_visit_waiting_list__CIParser(CIParser *self, String *name);

static inline void
init_struct_to_visit_waiting_list__CIParser(CIParser *self, String *name);

static void
add_function_to_visit_waiting_list__CIParser(CIParser *self,
                                             String *name,
                                             CIGenericParams *generic_params);

static void
add_typedef_to_visit_waiting_list__CIParser(CIParser *self,
                                            String *name,
                                            CIGenericParams *generic_params);

static void
add_union_to_visit_waiting_list__CIParser(CIParser *self,
                                          String *name,
                                          CIGenericParams *generic_params);

static void
add_struct_to_visit_waiting_list__CIParser(CIParser *self,
                                           String *name,
                                           CIGenericParams *generic_params);

static void
add_item_to_visit_waiting_list__CIParser(CIParser *self,
                                         enum CIDeclKind kind,
                                         String *name,
                                         CIGenericParams *generic_params);

/// @return CIParserVisitWaitingListItem*? (&)
static inline CIParserVisitWaitingListItem *
get_function_from_visit_waiting_list__CIParser(const CIParser *self,
                                               const String *name);

/// @return CIParserVisitWaitingListItem*? (&)
static inline CIParserVisitWaitingListItem *
get_typedef_from_visit_waiting_list__CIParser(const CIParser *self,
                                              const String *name);

/// @return CIParserVisitWaitingListItem*? (&)
static inline CIParserVisitWaitingListItem *
get_union_from_visit_waiting_list__CIParser(const CIParser *self,
                                            const String *name);

/// @return CIParserVisitWaitingListItem*? (&)
static inline CIParserVisitWaitingListItem *
get_struct_from_visit_waiting_list__CIParser(const CIParser *self,
                                             const String *name);

/// @brief Checks if the corresponding function name is recursive.
/// @note The corresponding function must exist.
static inline bool
is_recursive_function__CIParser(const CIParser *self, const String *name);

static inline bool
is_recursive_typedef__CIParser(const CIParser *self, const String *name);

/// @brief Checks if the corresponding union name is recursive.
/// @note The corresponding union must exist.
static inline bool
is_recursive_union__CIParser(const CIParser *self, const String *name);

/// @brief Checks if the corresponding struct name is recursive.
/// @note The corresponding struct must exist.
static inline bool
is_recursive_struct__CIParser(const CIParser *self, const String *name);

static String *
generate_name_error__CIParser();

/// @brief lhs + rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_add_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial);

/// @brief lhs - rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_sub_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial);

/// @brief lhs * rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_mul_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial);

/// @brief lhs / rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_div_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial);

/// @brief lhs % rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_mod_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial);

/// @brief lhs & rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_and_expr__CIParser(CIParser *self,
                               CIExpr *lhs,
                               CIExpr *rhs,
                               bool is_partial);

/// @brief lhs | rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_or_expr__CIParser(CIParser *self,
                              CIExpr *lhs,
                              CIExpr *rhs,
                              bool is_partial);

/// @brief lhs ^ rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_xor_expr__CIParser(CIParser *self,
                               CIExpr *lhs,
                               CIExpr *rhs,
                               bool is_partial);

/// @brief lhs << rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_lshift_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial);

/// @brief lhs >> rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_rshift_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial);

/// @brief lhs && rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_logical_and_expr__CIParser(CIParser *self,
                                   CIExpr *lhs,
                                   CIExpr *rhs,
                                   bool is_partial);

/// @brief lhs || rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_logical_or_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial);

/// @brief lhs == rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_eq_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief lhs != rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_ne_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief lhs < rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_lt_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief lhs <= rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_le_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief lhs > rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_gt_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief lhs >= rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_ge_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial);

/// @brief Resolve: ++rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_pre_increment_expr__CIParser(CIParser *self,
                                     CIExpr *rhs,
                                     bool is_partial);

/// @brief Resolve: --rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_pre_decrement_expr__CIParser(CIParser *self,
                                     CIExpr *rhs,
                                     bool is_partial);

/// @brief Resolve: rhs++
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_post_increment_expr__CIParser(CIParser *self,
                                      CIExpr *rhs,
                                      bool is_partial);

/// @brief Resolve: rhs--
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_post_decrement_expr__CIParser(CIParser *self,
                                      CIExpr *rhs,
                                      bool is_partial);

/// @brief Resolve: +rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_positive_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve: -rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_negative_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve: ~rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_bit_not_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve: !rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_not_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve: *rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_dereference__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve: &rhs
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_ref__CIParser(CIParser *self, CIExpr *rhs, bool is_partial);

/// @brief Resolve unary expresssion
static CIExpr *
resolve_unary_expr__CIParser(CIParser *self, CIExpr *expr, bool is_partial);

/// @brief Resolve (can be partially) expression at compile time.
/// @param is_partial This makes it possible to have expressions that are not
/// resolvable at compile time.
static CIExpr *
resolve_expr__CIParser(CIParser *self, CIExpr *expr, bool is_partial);

static void
resolve_function_visit_waiting_list__CIParser(CIParser *self);

static void
resolve_typedef_visit_waiting_list__CIParser(CIParser *self);

static void
resolve_union_visit_waiting_list__CIParser(CIParser *self);

static void
resolve_struct_visit_waiting_list__CIParser(CIParser *self);

static void
resolve_visit_waiting_list__CIParser(CIParser *self);

/// @brief Move to next conditional preprocessor.
static void
next_conditional_preprocessor__CIParser(CIParser *self, CIToken **next_token);

/// @brief Determine if the expression is true.
static bool
check_if_resolved_expr_is_true__CIParser(CIParser *self, CIExpr *expr);

/// @brief Determine which branch of conditional preprocessor to jump in.
/// @return the next token after a preprocessor conditional, if its condition is
/// met, otherwise returns NULL.
static CIToken *
select_conditional_preprocessor__CIParser(CIParser *self, CIToken *next_token);

static void
parse_target__CIParser(CIParser *self,
                       CIToken **current_token,
                       CITokens *content,
                       enum CITokenKind target);

static void
parse_tokens_in_macro_call_param__CIParser(CIParser *self,
                                           CIToken **current_token,
                                           CITokens *content);

static CIParserMacroCallParam *
parse_macro_call_param__CIParser(CIParser *self, CIToken **current_token);

static void
parse_macro_call_params__CIParser(CIParser *self, CIToken **current_token);

static CIToken *
jump_in_macro_call__CIParser(CIParser *self, CIToken *next_token);

/// @brief Jump into token block such as macro call, conditional preprocessor,
/// etc.
/// @return the token where we are after the jump.
static CIToken *
jump_in_token_block__CIParser(CIParser *self, CIToken *next_token);

static void
set_current_token__CIParser(CIParser *self, CIToken *next_token);

/// @brief Check that the token is available in accordance with the standard.
/// @note This function can modify the token type.
static void
check_standard__CIParser(CIParser *self, CIToken *token);

/// @brief Advance to one token on the current iterator.
static void
next_token__CIParser(CIParser *self);

static bool
next_token_must_continue_to_iter__CIParser(CIParser *self, CIToken *next_token);

static inline void
init__CIParser(CIParser *self);

static inline void
set_current_scope__CIParser(CIParser *self);

/// @brief Peek token at position + n.
static CIToken *
peek_token__CIParser(CIParser *self, Usize n);

/// @brief Check if the current token has the same kind than passed in
/// parameter.
static bool
expect__CIParser(CIParser *self, enum CITokenKind kind, bool emit_error);

/// @brief Check if the current token has the same type as the current element
/// in the type list.
static bool
expect_many__CIParser(CIParser *self, Usize n, ...);

/// @brief Check if the current token has the same kind than an element in the
/// list of kind (only one element needs to match for this function not to emit
/// an error).
static void
expect_with_list__CIParser(CIParser *self, Usize n, ...);

/// @brief Advance to n token(s).
static void
jump__CIParser(CIParser *self, Usize n);

/// @brief Check if the passed token is a data type.
static bool
token_is_data_type__CIParser(CIParser *self, const CIToken *token);

/// @brief Check if the current token can be a data type.
static inline bool
is_data_type__CIParser(CIParser *self);

static CIDataType *
substitute_and_generate_from_data_type__CIParser(
  CIParser *self,
  CIDataType *data_type,
  CIGenericParams *generic_params,
  CIGenericParams *called_generic_params);

/// @return the generated fields.
static Vec *
visit_struct_or_union__CIParser(CIParser *self,
                                const CIDecl *decl,
                                CIGenericParams *called_generic_params);

/// @return the generated data type.
/// e.g. typedef <data_type> <name>;
static CIDataType *
visit_typedef__CIParser(CIParser *self,
                        const CIDecl *decl,
                        CIGenericParams *called_generic_params);

static void
visit_data_type__CIParser(CIParser *self,
                          const CIDataType *data_type,
                          CIGenericParams *called_generic_params);

static void
visit_function_decl__CIParser(CIParser *self,
                              const CIDecl *decl,
                              CIGenericParams *called_generic_params);

static void
visit_function_expr__CIParser(CIParser *self,
                              const CIExpr *expr,
                              CIGenericParams *called_generic_params);

static void
visit_function_stmt__CIParser(CIParser *self,
                              const CIStmt *stmt,
                              CIGenericParams *called_generic_params);

static void
visit_function__CIParser(CIParser *self,
                         const CIDecl *function_decl,
                         CIGenericParams *called_generic_params);

/// @brief Generaste function gen.
static void
generate_function_gen__CIParser(CIParser *self,
                                String *function_name,
                                CIGenericParams *called_generic_params);

/// @brief Generate struct, union or typedef gen.
static void
generate_type_gen__CIParser(CIParser *self,
                            enum CIDeclKind kind,
                            String *name,
                            CIGenericParams *called_generic_params,
                            bool(const CIParser *, const String *),
                            CIDecl *(*search_decl)(const CIResultFile *,
                                                   const String *));

static inline void
generate_struct_gen__CIParser(CIParser *self,
                              String *struct_name,
                              CIGenericParams *called_generic_params);

static inline void
generate_union_gen__CIParser(CIParser *self,
                             String *union_name,
                             CIGenericParams *called_generic_params);

static inline void
generate_typedef_gen__CIParser(CIParser *self,
                               String *typedef_name_ref,
                               CIGenericParams *called_generic_params);

/// @brief Parse post data type, like pointer, ...
static CIDataType *
parse_post_data_type__CIParser(CIParser *self, CIDataType *data_type);

/// @brief Parse enum variants.
/// @return Vec<CIDeclEnumVariant*>*
static Vec *
parse_enum_variants__CIParser(CIParser *self);

/// @brief Parse enum declaration.
static CIDecl *
parse_enum__CIParser(CIParser *self, int storage_class_flag, String *name);

/// @brief Parse function params.
/// @return Vec<CIDeclFunctionParam*>*?
static Vec *
parse_function_params__CIParser(CIParser *self);

/// @brief Parse generic params.
/// @return Vec<CIDataType*>*
/// @param is_call Check whether this is a generic params call.
/// @example
///
/// struct Vec[@T] { // => generic params, so `is_call` is false
/// 	@T *buffer;
/// 	Usize len;
/// 	Usize capacity;
/// };
///
/// // ...
///
/// Vec.[@T] // => generic params call, so `is_call` is true
static CIGenericParams *
parse_generic_params__CIParser(CIParser *self, bool is_call);

/// @brief Parser function call.
static CIExpr *
parse_function_call__CIParser(CIParser *self,
                              String *identifier,
                              CIGenericParams *generic_params);

/// @brief Parse literal expression.
/// @brief @return CIExpr*?
static CIExpr *
parse_literal_expr__CIParser(CIParser *self);

/// @brief Parse primary expression.
/// @return CIExpr*?
static CIExpr *
parse_primary_expr__CIParser(CIParser *self);

static CIExpr *
parse_struct_call__CIParser(CIParser *self);

static CIExpr *
parse_array__CIParser(CIParser *self);

static CIExpr *
parse_binary_expr__CIParser(CIParser *self, CIExpr *expr);

/// @example x++, x[0], ...
static CIExpr *
parse_post_expr__CIParser(CIParser *self, CIExpr *expr);

static CIDeclFunctionItem *
parse_case__CIParser(CIParser *self);

static CIDeclFunctionItem *
parse_do_while_stmt__CIParser(CIParser *self, bool in_switch);

static CIDeclFunctionItem *
parse_for_stmt__CIParser(CIParser *self, bool in_switch);

static CIStmtIfBranch *
parse_if_branch__CIParser(CIParser *self, bool in_loop, bool in_switch);

static CIDeclFunctionItem *
parse_if_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch);

static CIDeclFunctionItem *
parse_while_stmt__CIParser(CIParser *self, bool in_switch);

static CIDeclFunctionItem *
parse_switch_stmt__CIParser(CIParser *self, bool in_loop);

static CIDeclFunctionItem *
parse_block_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch);

/// @brief Parse statement.
/// @return CIDeclFunctionItem*?
static CIDeclFunctionItem *
parse_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch);

/// @brief Parse function body.
/// @return Vec<CIDeclFunctionItem*>*
static Vec *
parse_function_body__CIParser(CIParser *self, bool in_loop, bool in_switch);

static CIDeclFunctionItem *
parse_function_body_item__CIParser(CIParser *self,
                                   bool in_loop,
                                   bool in_switch);

/// @brief Parse function declaration.
static CIDecl *
parse_function__CIParser(CIParser *self,
                         int storage_class_flag,
                         CIDataType *return_data_type,
                         String *name,
                         CIGenericParams *generic_params);

/// @brief Parse fields.
static Vec *
parse_fields__CIParser(CIParser *self);

/// @brief Parse struct or union fields.
static Vec *
parse_struct_or_union_fields__CIParser(CIParser *self);

/// @brief Add struct or union to scope.
/// @return If this function returns false, this means that the struct or union
/// declaration passed is not added to the scope.
static bool
add_struct_or_union__CIParser(CIParser *self, CIDecl *decl);

/// @brief Parse struct declaration.
static CIDecl *
parse_struct__CIParser(CIParser *self,
                       int storage_class_flag,
                       String *name,
                       CIGenericParams *generic_params);

/// @brief Add struct to scope.
static inline bool
add_struct__CIParser(CIParser *self, CIDecl *struct_decl);

/// @brief Parse typedef declaration.
static CIDecl *
parse_typedef__CIParser(CIParser *self);

/// @brief Parse union declaration.
static CIDecl *
parse_union__CIParser(CIParser *self,
                      int storage_class_flag,
                      String *name,
                      CIGenericParams *generic_params);

/// @brief Add union to scope.
static inline bool
add_union__CIParser(CIParser *self, CIDecl *union_decl);

/// @brief Parse variable declaration.
static CIDecl *
parse_variable__CIParser(CIParser *self,
                         int storage_class_flag,
                         CIDataType *data_type,
                         String *name,
                         bool no_expr,
                         bool is_local);

/// @brief Parse list of variable declaration.
static CIDecl *
parse_variable_list__CIParser(CIParser *self,
                              CIDecl *current_var,
                              int storage_class_flag,
                              CIDataType *data_type,
                              bool in_function_body);

/// @brief Resolve `#embed` preprocessor.
static void
resolve_preprocessor_embed__CIParser(CIParser *self,
                                     CIToken *preprocessor_embed_token);

/// @brief Resolve `#error` preprocessor.
static void
resolve_preprocessor_error__CIParser(CIParser *self,
                                     CIToken *preprocessor_error_token);

/// @brief Resolve `#include` preprocessor.
static void
resolve_preprocessor_include__CIParser(CIParser *self,
                                       CIToken *preprocessor_include_token);

/// @brief Resolve `#line` preprocessor.
static inline bool
resolve_preprocessor_line__CIParser(CIParser *self,
                                    CIToken *preprocessor_line_token);

/// @brief Resolve `#pragma` preprocessor.
static bool
resolve_preprocessor_pragma__CIParser(CIParser *self,
                                      CIToken *preprocessor_pragma_token);

/// @brief Resolve `#undef` preprocessor.
static inline void
resolve_preprocessor_undef__CIParser(CIParser *self,
                                     CIToken *preprocessor_undef_token);

/// @brief Resolve `#warning` preprocessor.
static void
resolve_preprocessor_warning__CIParser(CIParser *self,
                                       CIToken *preprocessor_warning_token);

/// @brief Resolve preprocessor (not conditional preprocessor).
/// @return Return true if a preprocessor has been resolved, otherwise return
/// false.
static bool
resolve_preprocessor__CIParser(CIParser *self, CIToken *next_token);

/// @brief Parse declaration.
static CIDecl *
parse_decl__CIParser(CIParser *self, bool in_function_body);

/// @brief Add declaration to scope.
/// @return If the function fails to add a declaration, it returns false.
static const CIDecl *
add_decl_to_scope__CIParser(CIParser *self,
                            CIDecl **decl_ref,
                            bool in_function_body,
                            bool must_free);

/// @brief Parse storage class specifier.
/// @return true if the token is a storage class specifier, false otherwise.
static bool
parse_storage_class_specifier__CIParser(CIParser *self,
                                        int *storage_class_flag);

/// @brief Parse many storage class specifiers.
static void
parse_storage_class_specifiers__CIParser(CIParser *self,
                                         int *storage_class_flag);

// NOTE: If the program is multi-threaded, you'll need to adapt these variables
// to multi-threading.
// If a name (of a variable, a function, etc.) is expected by the compiler, but
// is not here, it will generate a name and therefore increment the
// `name_error_count` and store all `name_error` in the `names_error` vector, as
// declaration strucutures (CIDeclVariable, CIDeclFunction, ...), are not
// supposed to own the name (in other words not to be able to free it).
static Vec *names_error = NULL; // Vec<String*>*
static Usize name_error_count = 0;
// The `data_type_as_expression` variable is used to keep the `DataType`
// pointer, when in `parse_decl__CIParser`, when `in_function_body` is true is
// that the following (current) token is not a `=`, `;` or `(`. Then, in the
// `parse_expr__CIParser` function, if `data_type_as_expression` is not NULL,
// the data type is returned as an expression.
static CIDataType *data_type_as_expression = NULL;

static int storage_class_flag = CI_STORAGE_CLASS_NONE;

static struct CIParserContext ctx;

// The `in_label` variable checks whether you're in a label and emits an error
// if there's a variable declaration in a label (also in case and default
// statetement).
static bool in_label = false;

#define ENABLE_IN_LABEL() in_label = true;
#define DISABLE_IN_LABEL() \
    if (in_label) {        \
        in_label = false;  \
    }

// Represent the `scope` of the current block being analyzed.
static CIScope *current_scope = NULL; // CIScope*?

// Represent the `body` of the current block being analyzed.
static Vec *current_body = NULL; // Vec<CIDeclFunctionItem*>*?

// Allow initialization.
//
// e.g.
//
// Array: { 3, 2, 1, ... }
//
// Struct call: { .a = 1, .b = 2, ... } or { 1, 2, ... }
static bool allow_initialization = false;

#define ENABLE_ALLOW_INITIALIZATION() allow_initialization = true;
#define DISABLE_ALLOW_INITIALIZATION() allow_initialization = false;

static bool resolve_visit_waiting_list = false;

#define ENABLE_RESOLVE_VISIT_WAITING_LIST() resolve_visit_waiting_list = true;
#define DISABLE_RESOLVE_VISIT_WAITING_LIST() resolve_visit_waiting_list = false;

static const CIFeature *tokens_feature = NULL; // CIFeature* (&)

#define HAS_REACH_EOF(token) (token->kind == CI_TOKEN_KIND_EOF)
#define HAS_REACH_EOT(token) (token->kind == CI_TOKEN_KIND_EOT)

CONSTRUCTOR(struct CIParserContext, CIParserContext)
{
    return (struct CIParserContext){ .current_decl = NULL,
                                     .current_expr = NULL,
                                     .current_stmt = NULL };
}

void
add_macro__CIParser(CIParser *self, CIParserMacroCall *macro_call)
{
    ASSERT(macro_call);

    push__Stack(self->macros_call, macro_call);
}

#define INIT_X_TO_WAIT_TO_VISIT(hm) \
    insert__HashMap(                \
      hm, name->buffer, NEW(CIParserVisitWaitingListItem, name, NULL));

void
init_function_to_visit_waiting_list__CIParser(CIParser *self, String *name)
{
    INIT_X_TO_WAIT_TO_VISIT(self->visit_waiting_list.functions);
}

void
init_typedef_to_visit_waiting_list__CIParser(CIParser *self, String *name)
{
    INIT_X_TO_WAIT_TO_VISIT(self->visit_waiting_list.typedefs);
}

void
init_union_to_visit_waiting_list__CIParser(CIParser *self, String *name)
{
    INIT_X_TO_WAIT_TO_VISIT(self->visit_waiting_list.unions);
}

void
init_struct_to_visit_waiting_list__CIParser(CIParser *self, String *name)
{
    INIT_X_TO_WAIT_TO_VISIT(self->visit_waiting_list.structs);
}

#define ADD_X_TO_VISIT_WAITING_LIST(hm)                                      \
    CIParserVisitWaitingListItem *item = get__HashMap(hm, name->buffer);     \
                                                                             \
    /* NOTE: Normally, it should already have been created, before the first \
    signature can be sent to the visit queue. */                             \
    ASSERT(item);                                                            \
                                                                             \
    if (item->generic_params_list) {                                         \
        for (Usize i = 0; i < item->generic_params_list->len; ++i) {         \
            if (eq__CIGenericParams(get__Vec(item->generic_params_list, i),  \
                                    generic_params)) {                       \
                return;                                                      \
            }                                                                \
        }                                                                    \
    } else {                                                                 \
        item->generic_params_list = NEW(Vec);                                \
    }                                                                        \
                                                                             \
    push__Vec(item->generic_params_list, generic_params);

void
add_function_to_visit_waiting_list__CIParser(CIParser *self,
                                             String *name,
                                             CIGenericParams *generic_params)
{
    ADD_X_TO_VISIT_WAITING_LIST(self->visit_waiting_list.functions);
}

void
add_typedef_to_visit_waiting_list__CIParser(CIParser *self,
                                            String *name,
                                            CIGenericParams *generic_params)
{
    ADD_X_TO_VISIT_WAITING_LIST(self->visit_waiting_list.typedefs);
}

void
add_union_to_visit_waiting_list__CIParser(CIParser *self,
                                          String *name,
                                          CIGenericParams *generic_params)
{
    ADD_X_TO_VISIT_WAITING_LIST(self->visit_waiting_list.unions);
}

void
add_struct_to_visit_waiting_list__CIParser(CIParser *self,
                                           String *name,
                                           CIGenericParams *generic_params)
{
    ADD_X_TO_VISIT_WAITING_LIST(self->visit_waiting_list.structs);
}

void
add_item_to_visit_waiting_list__CIParser(CIParser *self,
                                         enum CIDeclKind kind,
                                         String *name,
                                         CIGenericParams *generic_params)
{
    switch (kind) {
        case CI_DECL_KIND_FUNCTION:
            add_function_to_visit_waiting_list__CIParser(
              self, name, generic_params);

            break;
        case CI_DECL_KIND_TYPEDEF:
            add_typedef_to_visit_waiting_list__CIParser(
              self, name, generic_params);

            break;
        case CI_DECL_KIND_UNION:
            add_union_to_visit_waiting_list__CIParser(
              self, name, generic_params);

            break;
        case CI_DECL_KIND_STRUCT:
            add_struct_to_visit_waiting_list__CIParser(
              self, name, generic_params);

            break;
        default:
            UNREACHABLE("this kind of variant is not expected to add to visit "
                        "waiting lists");
    }
}

CIParserVisitWaitingListItem *
get_function_from_visit_waiting_list__CIParser(const CIParser *self,
                                               const String *name)
{
    return get__HashMap(self->visit_waiting_list.functions, name->buffer);
}

CIParserVisitWaitingListItem *
get_typedef_from_visit_waiting_list__CIParser(const CIParser *self,
                                              const String *name)
{
    return get__HashMap(self->visit_waiting_list.typedefs, name->buffer);
}

CIParserVisitWaitingListItem *
get_union_from_visit_waiting_list__CIParser(const CIParser *self,
                                            const String *name)
{
    return get__HashMap(self->visit_waiting_list.unions, name->buffer);
}

CIParserVisitWaitingListItem *
get_struct_from_visit_waiting_list__CIParser(const CIParser *self,
                                             const String *name)
{
    return get__HashMap(self->visit_waiting_list.structs, name->buffer);
}

bool
is_recursive_function__CIParser(const CIParser *self, const String *name)
{
    ASSERT(!search_function__CIResultFile(self->file, name));

    return get_function_from_visit_waiting_list__CIParser(self, name);
}

bool
is_recursive_typedef__CIParser(const CIParser *self, const String *name)
{
    // NOTE: A typedef cannot be recursive.
    return false;
}

bool
is_recursive_union__CIParser(const CIParser *self, const String *name)
{
    ASSERT(!search_union__CIResultFile(self->file, name));

    return get_union_from_visit_waiting_list__CIParser(self, name);
}

bool
is_recursive_struct__CIParser(const CIParser *self, const String *name)
{
    ASSERT(!search_struct__CIResultFile(self->file, name));

    return get_struct_from_visit_waiting_list__CIParser(self, name);
}

CONSTRUCTOR(CIParserVisitWaitingListItem *,
            CIParserVisitWaitingListItem,
            String *name,
            Vec *generic_params_list)
{
    CIParserVisitWaitingListItem *self =
      lily_malloc(sizeof(CIParserVisitWaitingListItem));

    self->name = name;
    self->generic_params_list = generic_params_list;

    return self;
}

DESTRUCTOR(CIParserVisitWaitingListItem, CIParserVisitWaitingListItem *self)
{
    if (self->generic_params_list) {
        FREE(Vec, self->generic_params_list);
    }

    lily_free(self);
}

DESTRUCTOR(CIParserVisitWaitingList, const CIParserVisitWaitingList *self)
{
    FREE_HASHMAP_VALUES(self->functions, CIParserVisitWaitingListItem);
    FREE(HashMap, self->functions);
    FREE_HASHMAP_VALUES(self->typedefs, CIParserVisitWaitingListItem);
    FREE(HashMap, self->typedefs);
    FREE_HASHMAP_VALUES(self->unions, CIParserVisitWaitingListItem);
    FREE(HashMap, self->unions);
    FREE_HASHMAP_VALUES(self->structs, CIParserVisitWaitingListItem);
    FREE(HashMap, self->structs);
}

CONSTRUCTOR(CIParserMacroCallParam *, CIParserMacroCallParam, CITokens content)
{
    CIParserMacroCallParam *self = lily_malloc(sizeof(CIParserMacroCallParam));

    self->content = content;
    self->next = NULL;

    return self;
}

DESTRUCTOR(CIParserMacroCallParam, CIParserMacroCallParam *self)
{
    // Restore the real next token(s) by removing EOT.
    CIToken *eot_token =
      remove_when_match__CITokens(&self->content, self->content.last);

    ASSERT(eot_token);
    ASSERT(eot_token->kind == CI_TOKEN_KIND_EOT);

    FREE(CIToken, eot_token);
    lily_free(self);
}

void
add__CIParserMacroCallParams(CIParserMacroCallParams *self,
                             CIParserMacroCallParam *param)
{
    if (!self->first) {
        self->first = param;
        self->last = param;

        return;
    }

    self->last->next = param;
    self->last = self->last->next;
    ++self->len;
}

CIParserMacroCallParam *
get__CIParserMacroCallParams(const CIParserMacroCallParams *self, Usize index)
{
    CIParserMacroCallParam *param = self->first;

    for (Usize i = 0; i < index; ++i) {
        param = param->next;
    }

    ASSERT(param);

    return param;
}

DESTRUCTOR(CIParserMacroCallParams, const CIParserMacroCallParams *self)
{
    CIParserMacroCallParam *current = self->first;

    while (current) {
        CIParserMacroCallParam *prev = current;

        current = current->next;

        FREE(CIParserMacroCallParam, prev);
    }
}

CONSTRUCTOR(CIParserMacroCall *, CIParserMacroCall)
{
    CIParserMacroCall *self = lily_malloc(sizeof(CIParserMacroCall));

    self->params = NEW(CIParserMacroCallParams);
    self->is_empty = false;

    return self;
}

VARIANT_CONSTRUCTOR(CIParserMacroCall *, CIParserMacroCall, is_empty)
{
    CIParserMacroCall *self = lily_malloc(sizeof(CIParserMacroCall));

    // self->params = <undefined>
    self->is_empty = true;

    return self;
}

DESTRUCTOR(CIParserMacroCall, CIParserMacroCall *self)
{
    if (!self->is_empty) {
        FREE(CIParserMacroCallParams, &self->params);
    }

    lily_free(self);
}

CONSTRUCTOR(CIParser, CIParser, CIResultFile *file, const CIScanner *scanner)
{
    if (!names_error) {
        names_error = NEW(Vec);
    }

    if (!tokens_feature) {
        tokens_feature = get_tokens_feature__CIScanner();
    }

    ctx = NEW(CIParserContext);

    return (CIParser){ .file = file,
                       .scanner = scanner,
                       .count_error = &file->file_analysis->count_error,
                       .count_warning = &file->file_analysis->count_warning,
                       .tokens = &scanner->tokens,
                       .current_token = scanner->tokens.first,
                       .previous_token = scanner->tokens.first,
                       .macros_call =
                         NEW(Stack, CI_PARSER_MACROS_CALL_MAX_SIZE),
                       .visit_waiting_list = NEW(CIParserVisitWaitingList) };
}

CIParser
from_tokens__CIParser(CIResultFile *file, const CITokens *content)
{
    return (CIParser){ .file = file,
                       .scanner = NULL,
                       .count_error = &file->file_analysis->count_error,
                       .count_warning = &file->file_analysis->count_warning,
                       .tokens = content,
                       .current_token = NULL,
                       .previous_token = NULL,
                       .macros_call =
                         NEW(Stack, CI_PARSER_MACROS_CALL_MAX_SIZE),
                       .visit_waiting_list = NEW(CIParserVisitWaitingList) };
}

String *
generate_name_error__CIParser()
{
    ASSERT(names_error);

    String *name_error = format__String("%__error__{zu}", name_error_count++);

    push__Vec(names_error, name_error);

    return name_error;
}

// +, -, *, /
#define RESOLVE_BASIC_BINARY_ARITHMETIC_EXPR(op, binary_kind)                 \
    ASSERT(lhs &&rhs);                                                        \
                                                                              \
    switch (lhs->kind + rhs->kind) {                                          \
        case CI_EXPR_KIND_LITERAL + CI_EXPR_KIND_LITERAL: {                   \
            switch (lhs->literal.kind) {                                      \
                case CI_EXPR_LITERAL_KIND_BOOL:                               \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                signed_int,                                   \
                                lhs->literal.bool_ op rhs->literal.bool_));   \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                signed_int,                                   \
                                lhs->literal.bool_ op rhs->literal.char_));   \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                float,                                        \
                                lhs->literal.bool_ op rhs->literal.float_));  \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.bool_ op               \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.bool_ op               \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_CHAR:                               \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                signed_int,                                   \
                                lhs->literal.char_ op rhs->literal.bool_));   \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                signed_int,                                   \
                                lhs->literal.char_ op rhs->literal.char_));   \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                float,                                        \
                                lhs->literal.char_ op rhs->literal.float_));  \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.char_ op               \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.char_ op               \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_FLOAT:                              \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                float,                                        \
                                lhs->literal.float_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                float,                                        \
                                lhs->literal.float_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                float,                                        \
                                lhs->literal.float_ op rhs->literal.float_)); \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          float,                              \
                                          lhs->literal.float_ op              \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          float,                              \
                                          lhs->literal.float_ op              \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:                         \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.bool_));             \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.char_));             \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          float,                              \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.float_));            \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          signed_int,                         \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_STRING:                             \
                    FAILED("this operation is unsure at compile-time");       \
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:                       \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.bool_));             \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.char_));             \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          float,                              \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.float_));            \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          unsigned_int,                       \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
            }                                                                 \
        }                                                                     \
        default:                                                              \
            if (!is_partial) {                                                \
                FAILED(                                                       \
                  "these expressions are not resolvable at compile-time");    \
            }                                                                 \
    }                                                                         \
    return NEW_VARIANT(                                                       \
      CIExpr,                                                                 \
      binary,                                                                 \
      NEW(CIExprBinary, binary_kind, ref__CIExpr(lhs), ref__CIExpr(rhs)));

CIExpr *
resolve_add_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial)
{
    RESOLVE_BASIC_BINARY_ARITHMETIC_EXPR(+, CI_EXPR_BINARY_KIND_ADD);
}

CIExpr *
resolve_sub_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial)
{
    RESOLVE_BASIC_BINARY_ARITHMETIC_EXPR(-, CI_EXPR_BINARY_KIND_SUB);
}

CIExpr *
resolve_mul_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial)
{
    RESOLVE_BASIC_BINARY_ARITHMETIC_EXPR(*, CI_EXPR_BINARY_KIND_MUL);
}

CIExpr *
resolve_div_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial)
{
    RESOLVE_BASIC_BINARY_ARITHMETIC_EXPR(/, CI_EXPR_BINARY_KIND_DIV);
}

// &, |, ^, <<, >>
// Also %
#define RESOLVE_BASIC_BINARY_BIT_EXPR(op, binary_kind)                       \
    ASSERT(lhs &&rhs);                                                       \
                                                                             \
    switch (lhs->kind + rhs->kind) {                                         \
        case CI_EXPR_KIND_LITERAL + CI_EXPR_KIND_LITERAL: {                  \
            switch (lhs->literal.kind) {                                     \
                case CI_EXPR_LITERAL_KIND_BOOL:                              \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                signed_int,                                  \
                                lhs->literal.bool_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                signed_int,                                  \
                                lhs->literal.bool_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          signed_int,                        \
                                          lhs->literal.bool_ op              \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.bool_ op              \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_CHAR:                              \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                signed_int,                                  \
                                lhs->literal.char_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                signed_int,                                  \
                                lhs->literal.char_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          signed_int,                        \
                                          lhs->literal.char_ op              \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.char_ op              \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_FLOAT:                             \
                    FAILED("cannot perform this operation with no-castable " \
                           "to int types");                                  \
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:                        \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          signed_int,                        \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.bool_));            \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          signed_int,                        \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.char_));            \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          signed_int,                        \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_STRING:                            \
                    FAILED("this operation is unsure at compile-time");      \
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:                      \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.bool_));            \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.char_));            \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          unsigned_int,                      \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
            }                                                                \
        }                                                                    \
        default:                                                             \
            if (!is_partial) {                                               \
                FAILED(                                                      \
                  "these expressions are not resolvable at compile-time");   \
            }                                                                \
    }                                                                        \
    return NEW_VARIANT(                                                      \
      CIExpr,                                                                \
      binary,                                                                \
      NEW(CIExprBinary, binary_kind, ref__CIExpr(lhs), ref__CIExpr(rhs)));

CIExpr *
resolve_mod_expr__CIParser(CIParser *self,
                           CIExpr *lhs,
                           CIExpr *rhs,
                           bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(%, CI_EXPR_BINARY_KIND_MOD);
}

CIExpr *
resolve_bit_and_expr__CIParser(CIParser *self,
                               CIExpr *lhs,
                               CIExpr *rhs,
                               bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(&, CI_EXPR_BINARY_KIND_BIT_AND);
}

CIExpr *
resolve_bit_or_expr__CIParser(CIParser *self,
                              CIExpr *lhs,
                              CIExpr *rhs,
                              bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(|, CI_EXPR_BINARY_KIND_BIT_OR);
}

CIExpr *
resolve_bit_xor_expr__CIParser(CIParser *self,
                               CIExpr *lhs,
                               CIExpr *rhs,
                               bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(^, CI_EXPR_BINARY_KIND_BIT_XOR);
}

CIExpr *
resolve_bit_lshift_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(<<, CI_EXPR_BINARY_KIND_BIT_LSHIFT);
}

CIExpr *
resolve_bit_rshift_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial)
{
    RESOLVE_BASIC_BINARY_BIT_EXPR(>>, CI_EXPR_BINARY_KIND_BIT_RSHIFT);
}

// &&, ||
#define RESOLVE_BASIC_BINARY_LOGICAL_EXPR(op, binary_kind)                   \
    ASSERT(lhs &&rhs);                                                       \
                                                                             \
    switch (lhs->kind + rhs->kind) {                                         \
        case CI_EXPR_KIND_LITERAL + CI_EXPR_KIND_LITERAL: {                  \
            switch (lhs->literal.kind) {                                     \
                case CI_EXPR_LITERAL_KIND_BOOL:                              \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                bool,                                        \
                                lhs->literal.bool_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                bool,                                        \
                                lhs->literal.bool_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.bool_ op              \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.bool_ op              \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_CHAR:                              \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                bool,                                        \
                                lhs->literal.char_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(                                   \
                                CIExprLiteral,                               \
                                bool,                                        \
                                lhs->literal.char_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.char_ op              \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.char_ op              \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_FLOAT:                             \
                    FAILED("cannot perform this operation with no-castable " \
                           "to int types");                                  \
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:                        \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.bool_));            \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.char_));            \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.signed_int op         \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
                case CI_EXPR_LITERAL_KIND_STRING:                            \
                    FAILED("this operation is unsure at compile-time");      \
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:                      \
                    switch (rhs->literal.kind) {                             \
                        case CI_EXPR_LITERAL_KIND_BOOL:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.bool_));            \
                        case CI_EXPR_LITERAL_KIND_CHAR:                      \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.char_));            \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                     \
                            FAILED("cannot perform this operation with "     \
                                   "no-castable to int types");              \
                        case CI_EXPR_LITERAL_KIND_STRING:                    \
                            FAILED(                                          \
                              "this operation is unsure at compile-time");   \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.signed_int));       \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:              \
                            return NEW_VARIANT(                              \
                              CIExpr,                                        \
                              literal,                                       \
                              NEW_VARIANT(CIExprLiteral,                     \
                                          bool,                              \
                                          lhs->literal.unsigned_int op       \
                                            rhs->literal.unsigned_int));     \
                        default:                                             \
                            UNREACHABLE("unknown variant");                  \
                    }                                                        \
            }                                                                \
        }                                                                    \
        default:                                                             \
            if (!is_partial) {                                               \
                FAILED(                                                      \
                  "these expressions are not resolvable at compile-time");   \
            }                                                                \
    }                                                                        \
    return NEW_VARIANT(                                                      \
      CIExpr,                                                                \
      binary,                                                                \
      NEW(CIExprBinary, binary_kind, ref__CIExpr(lhs), ref__CIExpr(rhs)));

CIExpr *
resolve_logical_and_expr__CIParser(CIParser *self,
                                   CIExpr *lhs,
                                   CIExpr *rhs,
                                   bool is_partial)
{
    RESOLVE_BASIC_BINARY_LOGICAL_EXPR(&&, CI_EXPR_BINARY_KIND_AND);
}

CIExpr *
resolve_logical_or_expr__CIParser(CIParser *self,
                                  CIExpr *lhs,
                                  CIExpr *rhs,
                                  bool is_partial)
{
    RESOLVE_BASIC_BINARY_LOGICAL_EXPR(||, CI_EXPR_BINARY_KIND_OR);
}

#define RESOLVE_BASIC_BINARY_COMPARISON_EXPR(op, binary_kind)                 \
    ASSERT(lhs &&rhs);                                                        \
                                                                              \
    switch (lhs->kind + rhs->kind) {                                          \
        case CI_EXPR_KIND_LITERAL + CI_EXPR_KIND_LITERAL: {                   \
            switch (lhs->literal.kind) {                                      \
                case CI_EXPR_LITERAL_KIND_BOOL:                               \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.bool_ op rhs->literal.bool_));   \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.bool_ op rhs->literal.char_));   \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.bool_ op rhs->literal.float_));  \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.bool_ op               \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.bool_ op               \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_CHAR:                               \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.char_ op rhs->literal.bool_));   \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.char_ op rhs->literal.char_));   \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.char_ op rhs->literal.float_));  \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.char_ op               \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.char_ op               \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_FLOAT:                              \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.float_ op rhs->literal.bool_));  \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.float_ op rhs->literal.char_));  \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(                                    \
                                CIExprLiteral,                                \
                                bool,                                         \
                                lhs->literal.float_ op rhs->literal.float_)); \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.float_ op              \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.float_ op              \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:                         \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.bool_));             \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.char_));             \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.float_));            \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.signed_int op          \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
                case CI_EXPR_LITERAL_KIND_STRING:                             \
                    FAILED("this operation is unsure at compile-time");       \
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:                       \
                    switch (rhs->literal.kind) {                              \
                        case CI_EXPR_LITERAL_KIND_BOOL:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.bool_));             \
                        case CI_EXPR_LITERAL_KIND_CHAR:                       \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.char_));             \
                        case CI_EXPR_LITERAL_KIND_FLOAT:                      \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.float_));            \
                        case CI_EXPR_LITERAL_KIND_STRING:                     \
                            FAILED(                                           \
                              "this operation is unsure at compile-time");    \
                        case CI_EXPR_LITERAL_KIND_SIGNED_INT:                 \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.signed_int));        \
                        case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:               \
                            return NEW_VARIANT(                               \
                              CIExpr,                                         \
                              literal,                                        \
                              NEW_VARIANT(CIExprLiteral,                      \
                                          bool,                               \
                                          lhs->literal.unsigned_int op        \
                                            rhs->literal.unsigned_int));      \
                        default:                                              \
                            UNREACHABLE("unknown variant");                   \
                    }                                                         \
            }                                                                 \
        }                                                                     \
        default:                                                              \
            if (!is_partial) {                                                \
                FAILED("these expressions are not resolvable at "             \
                       "compile-time");                                       \
            }                                                                 \
    }                                                                         \
    return NEW_VARIANT(                                                       \
      CIExpr,                                                                 \
      binary,                                                                 \
      NEW(CIExprBinary, binary_kind, ref__CIExpr(lhs), ref__CIExpr(rhs)));

CIExpr *
resolve_eq_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(==, CI_EXPR_BINARY_KIND_EQ);
}

CIExpr *
resolve_ne_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(!=, CI_EXPR_BINARY_KIND_NE);
}

CIExpr *
resolve_lt_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(<, CI_EXPR_BINARY_KIND_LESS);
}

CIExpr *
resolve_le_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(<=, CI_EXPR_BINARY_KIND_LESS_EQ);
}

CIExpr *
resolve_gt_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(>, CI_EXPR_BINARY_KIND_GREATER);
}

CIExpr *
resolve_ge_expr__CIParser(CIParser *self,
                          CIExpr *lhs,
                          CIExpr *rhs,
                          bool is_partial)
{
    RESOLVE_BASIC_BINARY_COMPARISON_EXPR(>=, CI_EXPR_BINARY_KIND_GREATER_EQ);
}

CIExpr *
resolve_pre_increment_expr__CIParser(CIParser *self,
                                     CIExpr *rhs,
                                     bool is_partial)
{
    TODO("resolve ++rhs");
}

CIExpr *
resolve_pre_decrement_expr__CIParser(CIParser *self,
                                     CIExpr *rhs,
                                     bool is_partial)
{
    TODO("resolve --rhs");
}

CIExpr *
resolve_post_increment_expr__CIParser(CIParser *self,
                                      CIExpr *rhs,
                                      bool is_partial)
{
    TODO("resolve rhs++");
}

CIExpr *
resolve_post_decrement_expr__CIParser(CIParser *self,
                                      CIExpr *rhs,
                                      bool is_partial)
{
    TODO("resolve rhs--");
}

CIExpr *
resolve_positive_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    TODO("resolve +rhs");
}

CIExpr *
resolve_negative_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    TODO("resolve -rhs");
}

CIExpr *
resolve_bit_not_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    TODO("resolve ~rhs");
}

CIExpr *
resolve_not_expr__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    ASSERT(rhs);

    switch (rhs->kind) {
        case CI_EXPR_KIND_LITERAL:
            switch (rhs->literal.kind) {
                case CI_EXPR_LITERAL_KIND_BOOL:
                    return NEW_VARIANT(
                      CIExpr,
                      literal,
                      NEW_VARIANT(CIExprLiteral, bool, !rhs->literal.bool_));
                case CI_EXPR_LITERAL_KIND_CHAR:
                    return NEW_VARIANT(
                      CIExpr,
                      literal,
                      NEW_VARIANT(CIExprLiteral, char, !rhs->literal.char_));
                case CI_EXPR_LITERAL_KIND_FLOAT:
                    return NEW_VARIANT(
                      CIExpr,
                      literal,
                      NEW_VARIANT(CIExprLiteral, float, !rhs->literal.float_));
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:
                    return NEW_VARIANT(CIExpr,
                                       literal,
                                       NEW_VARIANT(CIExprLiteral,
                                                   signed_int,
                                                   !rhs->literal.signed_int));
                case CI_EXPR_LITERAL_KIND_STRING:
                    goto default_expr_case;
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:
                    return NEW_VARIANT(CIExpr,
                                       literal,
                                       NEW_VARIANT(CIExprLiteral,
                                                   unsigned_int,
                                                   !rhs->literal.unsigned_int));
                default:
                    UNREACHABLE(
                      "this kind of literal is not expected with `!`");
            }
        default:
        default_expr_case:
            if (!is_partial) {
                FAILED("this expression are not resolvable at compile-time");
            }
    }

    return NEW_VARIANT(
      CIExpr,
      unary,
      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_NOT, ref__CIExpr(rhs)));
}

CIExpr *
resolve_dereference__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    // The referencing operator doesn't seem to be resolvable at the moment.
    return NEW_VARIANT(
      CIExpr,
      unary,
      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_DEREFERENCE, ref__CIExpr(rhs)));
}

CIExpr *
resolve_ref__CIParser(CIParser *self, CIExpr *rhs, bool is_partial)
{
    TODO("resolve &rhs");
}

CIExpr *
resolve_unary_expr__CIParser(CIParser *self, CIExpr *expr, bool is_partial)
{
    CIExpr *rhs = resolve_expr__CIParser(self, expr->unary.expr, is_partial);
    CIExpr *res = NULL;

    switch (expr->unary.kind) {
        case CI_EXPR_UNARY_KIND_PRE_INCREMENT:
            res = resolve_pre_increment_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_PRE_DECREMENT:
            res = resolve_pre_decrement_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_POST_INCREMENT:
            res = resolve_post_increment_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_POST_DECREMENT:
            res = resolve_post_decrement_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_POSITIVE:
            res = resolve_positive_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_NEGATIVE:
            res = resolve_negative_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_BIT_NOT:
            res = resolve_bit_not_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_NOT:
            res = resolve_not_expr__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_DEREFERENCE:
            res = resolve_dereference__CIParser(self, rhs, is_partial);

            break;
        case CI_EXPR_UNARY_KIND_REF:
            res = resolve_ref__CIParser(self, rhs, is_partial);

            break;
        default:
            UNREACHABLE("unknown variant");
    }

    FREE(CIExpr, rhs);

    return res;
}

CIExpr *
resolve_expr__CIParser(CIParser *self, CIExpr *expr, bool is_partial)
{
    ASSERT(expr);

    switch (expr->kind) {
        case CI_EXPR_KIND_ALIGNOF:
            TODO("resolve alignof");
        case CI_EXPR_KIND_ARRAY_ACCESS:
            TODO("array access");
        case CI_EXPR_KIND_BINARY: {
            switch (expr->binary.kind) {
                case CI_EXPR_BINARY_KIND_ASSIGN:
                case CI_EXPR_BINARY_KIND_ASSIGN_ADD:
                case CI_EXPR_BINARY_KIND_ASSIGN_SUB:
                case CI_EXPR_BINARY_KIND_ASSIGN_MUL:
                case CI_EXPR_BINARY_KIND_ASSIGN_DIV:
                case CI_EXPR_BINARY_KIND_ASSIGN_MOD:
                case CI_EXPR_BINARY_KIND_ASSIGN_BIT_AND:
                case CI_EXPR_BINARY_KIND_ASSIGN_BIT_OR:
                case CI_EXPR_BINARY_KIND_ASSIGN_XOR:
                case CI_EXPR_BINARY_KIND_ASSIGN_BIT_LSHIFT:
                case CI_EXPR_BINARY_KIND_ASSIGN_BIT_RSHIFT:
                case CI_EXPR_BINARY_KIND_DOT:
                case CI_EXPR_BINARY_KIND_ARROW:
                    if (!is_partial) {
                        FAILED("cannot resolve assigns binary expression");
                    }

                    return ref__CIExpr(expr);
                default:
                    break;
            }

            CIExpr *lhs =
              resolve_expr__CIParser(self, expr->binary.left, is_partial);

            switch (expr->binary.kind) {
                case CI_EXPR_BINARY_KIND_AND:
                    if (!check_if_resolved_expr_is_true__CIParser(self, lhs)) {
                        return lhs;
                    }

                    break;
                case CI_EXPR_BINARY_KIND_OR:
                    if (check_if_resolved_expr_is_true__CIParser(self, lhs)) {
                        return lhs;
                    }

                    break;
                default:
                    break;
            }

            CIExpr *rhs =
              resolve_expr__CIParser(self, expr->binary.right, is_partial);
            CIExpr *res = NULL;

            switch (expr->binary.kind) {
                case CI_EXPR_BINARY_KIND_ADD:
                    res =
                      resolve_add_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_SUB:
                    res =
                      resolve_sub_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_MUL:
                    res =
                      resolve_mul_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_DIV:
                    res =
                      resolve_div_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_MOD:
                    res =
                      resolve_mod_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_BIT_AND:
                    res = resolve_bit_and_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_BIT_OR:
                    res =
                      resolve_bit_or_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_BIT_XOR:
                    res = resolve_bit_xor_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_BIT_LSHIFT:
                    res = resolve_bit_lshift_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_BIT_RSHIFT:
                    res = resolve_bit_rshift_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_AND:
                    res = resolve_logical_and_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_OR:
                    res = resolve_logical_or_expr__CIParser(
                      self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_EQ:
                    res = resolve_eq_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_NE:
                    res = resolve_ne_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_LESS:
                    res = resolve_lt_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_LESS_EQ:
                    res = resolve_le_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_GREATER:
                    res = resolve_gt_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                case CI_EXPR_BINARY_KIND_GREATER_EQ:
                    res = resolve_ge_expr__CIParser(self, lhs, rhs, is_partial);

                    break;
                default:
                    UNREACHABLE("unknown binary kind");
            }

            FREE(CIExpr, lhs);
            FREE(CIExpr, rhs);

            return res;
        }
        case CI_EXPR_KIND_GROUPING:
            return resolve_expr__CIParser(self, expr->grouping, is_partial);
        case CI_EXPR_KIND_IDENTIFIER:
            TODO("resolve identifier");
        case CI_EXPR_KIND_LITERAL:
            return ref__CIExpr(expr);
        case CI_EXPR_KIND_NULLPTR:
            return ref__CIExpr(expr);
        case CI_EXPR_KIND_SIZEOF:
            TODO("resolve sizeof");
        case CI_EXPR_KIND_STRUCT_CALL:
            TODO("resolve struct");
        case CI_EXPR_KIND_TERNARY: {
            CIExpr *resolved_cond =
              resolve_expr__CIParser(self, expr->ternary.cond, is_partial);

            if (check_if_resolved_expr_is_true__CIParser(self, resolved_cond)) {
                return resolve_expr__CIParser(
                  self, expr->ternary.if_, is_partial);
            }

            return resolve_expr__CIParser(
              self, expr->ternary.else_, is_partial);
        }
        case CI_EXPR_KIND_UNARY:
            return resolve_unary_expr__CIParser(self, expr, is_partial);
        default:
            UNREACHABLE("unknown variant");
    }
}

#define RESOLVE_VISIT_WAITING_LIST(hm, gen)                                    \
    HashMapIter iter = NEW(HashMapIter, hm);                                   \
    CIParserVisitWaitingListItem *item = NULL;                                 \
                                                                               \
    while ((item = next__HashMapIter(&iter))) {                                \
        if (item->generic_params_list) {                                       \
            for (Usize i = 0; i < item->generic_params_list->len; ++i) {       \
                gen(self, item->name, get__Vec(item->generic_params_list, i)); \
            }                                                                  \
        }                                                                      \
    }

void
resolve_function_visit_waiting_list__CIParser(CIParser *self)
{
    RESOLVE_VISIT_WAITING_LIST(self->visit_waiting_list.functions,
                               generate_function_gen__CIParser);
}

void
resolve_typedef_visit_waiting_list__CIParser(CIParser *self)
{
    RESOLVE_VISIT_WAITING_LIST(self->visit_waiting_list.typedefs,
                               generate_typedef_gen__CIParser);
}

void
resolve_union_visit_waiting_list__CIParser(CIParser *self)
{
    RESOLVE_VISIT_WAITING_LIST(self->visit_waiting_list.unions,
                               generate_union_gen__CIParser);
}

void
resolve_struct_visit_waiting_list__CIParser(CIParser *self)
{
    RESOLVE_VISIT_WAITING_LIST(self->visit_waiting_list.structs,
                               generate_struct_gen__CIParser);
}

void
resolve_visit_waiting_list__CIParser(CIParser *self)
{
    ENABLE_RESOLVE_VISIT_WAITING_LIST();

    resolve_function_visit_waiting_list__CIParser(self);
    resolve_typedef_visit_waiting_list__CIParser(self);
    resolve_union_visit_waiting_list__CIParser(self);
    resolve_struct_visit_waiting_list__CIParser(self);

    DISABLE_RESOLVE_VISIT_WAITING_LIST();
}

void
next_conditional_preprocessor__CIParser(CIParser *self, CIToken **next_token)
{
    CIToken *current = *next_token;

    // Move onto the next conditional preprocessor.
    while (current->kind != CI_TOKEN_KIND_EOT) {
        *next_token = (*next_token)->next;
        current = *next_token;

        check_standard__CIParser(self, current);

        if (is_conditional_preprocessor__CITokenKind(current->kind) &&
            current->kind != CI_TOKEN_KIND_PREPROCESSOR_IF &&
            current->kind != CI_TOKEN_KIND_PREPROCESSOR_IFDEF &&
            current->kind != CI_TOKEN_KIND_PREPROCESSOR_IFNDEF) {
            return;
        }
    }
}

bool
check_if_resolved_expr_is_true__CIParser(CIParser *self, CIExpr *expr)
{
    ASSERT(expr);

    switch (expr->kind) {
        case CI_EXPR_KIND_LITERAL:
            switch (expr->literal.kind) {
                case CI_EXPR_LITERAL_KIND_BOOL:
                    return expr->literal.bool_;
                case CI_EXPR_LITERAL_KIND_CHAR:
                    return expr->literal.char_;
                case CI_EXPR_LITERAL_KIND_FLOAT:
                    return expr->literal.float_;
                case CI_EXPR_LITERAL_KIND_SIGNED_INT:
                    return expr->literal.signed_int;
                case CI_EXPR_LITERAL_KIND_STRING:
                    return expr->literal.string;
                case CI_EXPR_LITERAL_KIND_UNSIGNED_INT:
                    return expr->literal.unsigned_int;
                default:
                    UNREACHABLE("unknown variant");
            }
        default:
            return false;
    }
}

CIToken *
select_conditional_preprocessor__CIParser(CIParser *self, CIToken *next_token)
{
#define PREPROCESSOR_PARSE_EXPR(n)                          \
    CIParser expr_parser =                                  \
      from_tokens__CIParser(self->file, preprocessor_cond); \
                                                            \
    init__CIParser(&expr_parser);                           \
                                                            \
    CIExpr *n = parse_expr__CIParser(&expr_parser);         \
                                                            \
    if (!HAS_REACH_EOF(expr_parser.current_token)) {        \
        FAILED("expected only one expression");             \
    }                                                       \
                                                            \
    free_from_tokens_case__CIParser(&expr_parser);

#define SELECT_IF_CONDITIONAL_PREPROCESSOR(k)                                \
    {                                                                        \
        CIToken *preprocessor = next_token;                                  \
        CITokens *preprocessor_cond = &preprocessor->preprocessor_##k.cond;  \
                                                                             \
        PREPROCESSOR_PARSE_EXPR(cond);                                       \
                                                                             \
        CIExpr *resolved_cond = resolve_expr__CIParser(self, cond, false);   \
                                                                             \
        FREE(CIExpr, cond);                                                  \
                                                                             \
        next_token = preprocessor->preprocessor_##k.content.first;           \
                                                                             \
        if (check_if_resolved_expr_is_true__CIParser(self, resolved_cond)) { \
            FREE(CIExpr, resolved_cond);                                     \
                                                                             \
            return next_token;                                               \
        }                                                                    \
                                                                             \
        FREE(CIExpr, resolved_cond);                                         \
                                                                             \
        if (!HAS_REACH_EOT(next_token)) {                                    \
            next_conditional_preprocessor__CIParser(self, &next_token);      \
        }                                                                    \
                                                                             \
        return select_conditional_preprocessor__CIParser(self, next_token);  \
    }

// The `inverse` parameter is used for `#ifndef`, `#elifndef` cases, to
// invert the condition.
#define SELECT_IFDEF_CONDITIONAL_PREPROCESSOR(k, reverse)                   \
    {                                                                       \
        const CIResultDefine *is_def = get_define__CIResultFile(            \
          self->file, next_token->preprocessor_##k.identifier);             \
                                                                            \
        next_token = next_token->preprocessor_##k.content.first;            \
                                                                            \
        if ((is_def && !reverse) || (!is_def && reverse)) {                 \
            return next_token;                                              \
        }                                                                   \
                                                                            \
        if (!HAS_REACH_EOT(next_token)) {                                   \
            next_conditional_preprocessor__CIParser(self, &next_token);     \
        }                                                                   \
                                                                            \
        return select_conditional_preprocessor__CIParser(self, next_token); \
    }

    if (next_token) {
        switch (next_token->kind) {
            case CI_TOKEN_KIND_PREPROCESSOR_IF:
                SELECT_IF_CONDITIONAL_PREPROCESSOR(if);
            case CI_TOKEN_KIND_PREPROCESSOR_IFDEF:
                SELECT_IFDEF_CONDITIONAL_PREPROCESSOR(ifdef, false);
            case CI_TOKEN_KIND_PREPROCESSOR_IFNDEF:
                SELECT_IFDEF_CONDITIONAL_PREPROCESSOR(ifndef, true);
            case CI_TOKEN_KIND_PREPROCESSOR_ELIF:
                SELECT_IF_CONDITIONAL_PREPROCESSOR(elif);
            case CI_TOKEN_KIND_PREPROCESSOR_ELIFDEF:
                SELECT_IFDEF_CONDITIONAL_PREPROCESSOR(elifdef, false);
            case CI_TOKEN_KIND_PREPROCESSOR_ELIFNDEF:
                SELECT_IFDEF_CONDITIONAL_PREPROCESSOR(elifndef, true);
            case CI_TOKEN_KIND_PREPROCESSOR_ELSE:
                return next_token->preprocessor_else.content.first;
            default:
                return NULL;
        }
    }

    return NULL;

#undef PREPROCESSOR_PARSE_EXPR
#undef SELECT_IF_CONDITIONAL_PREPROCESSOR
#undef SELECT_IFDEF_CONDITIONAL_PREPROCESSOR
}

void
parse_target__CIParser(CIParser *self,
                       CIToken **current_token,
                       CITokens *content,
                       enum CITokenKind target)
{
#define NEXT(ct) ct = ct->kind != CI_TOKEN_KIND_EOF ? ct->next : ct
#define PEEK(ct) ct->next
#define EXPECT(ct, k)               \
    if (ct->kind != k) {            \
        FAILED("unexpected token"); \
    }
#define CURRENT(ct) ct

    NEXT((*current_token));

loop:
    add__CITokens(content, CURRENT((*current_token)));

    switch (CURRENT((*current_token))->kind) {
        case CI_TOKEN_KIND_LPAREN:
            parse_target__CIParser(
              self, current_token, content, CI_TOKEN_KIND_LPAREN);

            goto loop;
        case CI_TOKEN_KIND_LBRACE:
            parse_target__CIParser(
              self, current_token, content, CI_TOKEN_KIND_LBRACE);

            goto loop;
        case CI_TOKEN_KIND_LHOOK:
            parse_target__CIParser(
              self, current_token, content, CI_TOKEN_KIND_LHOOK);

            goto loop;
        case CI_TOKEN_KIND_RPAREN:
        case CI_TOKEN_KIND_RBRACE:
        case CI_TOKEN_KIND_RHOOK: {
            static enum CITokenKind
              match_target_with_current[CI_TOKEN_KIND_MAX] = {
                  [CI_TOKEN_KIND_LPAREN] = CI_TOKEN_KIND_RPAREN,
                  [CI_TOKEN_KIND_LBRACE] = CI_TOKEN_KIND_RBRACE,
                  [CI_TOKEN_KIND_LHOOK] = CI_TOKEN_KIND_RHOOK
              };

            if (match_target_with_current[target] ==
                CURRENT((*current_token))->kind) {
                NEXT((*current_token));

                return;
            } else {
                FAILED("`)`, `}` or `]` is not expected");
            }

            break;
        }
        default:
            break;
    }

    NEXT((*current_token));

    goto loop;
}

void
parse_tokens_in_macro_call_param__CIParser(CIParser *self,
                                           CIToken **current_token,
                                           CITokens *content)
{

    while (CURRENT((*current_token))->kind != CI_TOKEN_KIND_COMMA &&
           CURRENT((*current_token))->kind != CI_TOKEN_KIND_RPAREN &&
           CURRENT((*current_token))->kind != CI_TOKEN_KIND_EOF) {
        add__CITokens(content, CURRENT((*current_token)));

        if (CURRENT((*current_token))->kind == CI_TOKEN_KIND_LPAREN ||
            CURRENT((*current_token))->kind == CI_TOKEN_KIND_LBRACE ||
            CURRENT((*current_token))->kind == CI_TOKEN_KIND_LHOOK) {
            parse_target__CIParser(
              self, current_token, content, CURRENT((*current_token))->kind);
        } else {
            NEXT((*current_token));
        }
    }
}

CIParserMacroCallParam *
parse_macro_call_param__CIParser(CIParser *self, CIToken **current_token)
{
    CITokens content = NEW(CITokens);

    parse_tokens_in_macro_call_param__CIParser(self, current_token, &content);
    // NOTE: In practice, it makes no difference that we don't configure the
    // location on this token (CI_TOKEN_KIND_EOT), because it's not used by the
    // parser and is only used as a transition token.
    ASSERT(insert_after_match__CITokens(
      &content,
      content.last,
      NEW_VARIANT(CIToken,
                  eot,
                  default__Location(""),
                  CI_TOKEN_EOT_CONTEXT_MACRO_PARAM)));

    return NEW(CIParserMacroCallParam, content);
}

void
parse_macro_call_params__CIParser(CIParser *self, CIToken **current_token)
{
    NEXT((*current_token)); // skip `(`

    CIParserMacroCall *macro_call = NEW(CIParserMacroCall);

    while (CURRENT((*current_token))->kind != CI_TOKEN_KIND_RPAREN &&
           CURRENT((*current_token))->kind != CI_TOKEN_KIND_EOF) {

        CIParserMacroCallParam *param =
          parse_macro_call_param__CIParser(self, current_token);

        if (CURRENT((*current_token))->kind != CI_TOKEN_KIND_RPAREN) {
            EXPECT((*current_token), CI_TOKEN_KIND_COMMA);
            NEXT((*current_token));
        }

        add__CIParserMacroCallParams(&macro_call->params, param);
    }

    EXPECT((*current_token), CI_TOKEN_KIND_RPAREN);

    push__Stack(self->macros_call, macro_call);
}

CIToken *
jump_in_macro_call__CIParser(CIParser *self, CIToken *next_token)
{
    const CIResultDefine *define =
      get_define__CIResultFile(self->file, next_token->identifier);

    if (define) {
        CIToken *peeked = PEEK(next_token);

        if (peeked) {
            switch (peeked->kind) {
                case CI_TOKEN_KIND_LPAREN:
                    NEXT(next_token); // skip maco identifier
                    parse_macro_call_params__CIParser(self, &next_token);

                    break;
                default:
                    // Push empty macro call on the stack.
                    push__Stack(self->macros_call,
                                NEW_VARIANT(CIParserMacroCall, is_empty));
            }
        }

        ASSERT(define->define->tokens.last->kind == CI_TOKEN_KIND_EOT);
        ASSERT(define->define->tokens.last->eot.ctx ==
               CI_TOKEN_EOT_CONTEXT_MACRO_CALL);

        // NOTE: We add the next token after the macro call,
        // but it's okay to add it like that, because it's
        // after a transition token and if the macro call is
        // called several times the next token after EOT
        // will be updated.
        define->define->tokens.last->next = next_token->next;

        return define->define->tokens.first;
    }

    return NULL;

#undef NEXT
#undef PEEK
#undef EXPECT
#undef CURRENT
}

CIToken *
jump_in_token_block__CIParser(CIParser *self, CIToken *next_token)
{
    ASSERT(next_token);

    switch (next_token->kind) {
        case CI_TOKEN_KIND_PREPROCESSOR_ENDIF:
            UNREACHABLE("#endif is not expected at this point");
        case CI_TOKEN_KIND_PREPROCESSOR_DEFINE:
            add_define__CIResultFile(
              self->file,
              NEW(CIResultDefine,
                  &next_token->preprocessor_define,
                  NEW(CIFileID, self->file->entity.id, self->file->kind)));

            break;
        case CI_TOKEN_KIND_MACRO_PARAM: {
            CIParserMacroCall *current_macro_call =
              peek__Stack(self->macros_call);
            CIParserMacroCallParam *param = get__CIParserMacroCallParams(
              &current_macro_call->params, next_token->macro_param);

            ASSERT(param->content.last->kind == CI_TOKEN_KIND_EOT);
            ASSERT(param->content.last->eot.ctx ==
                   CI_TOKEN_EOT_CONTEXT_MACRO_PARAM);

            // NOTE: Save the `param->content.last->next`, to able to restore
            // after reach EOT.
            param->content.last->eot.macro_param = next_token;

            return param->content.first;
        }
        case CI_TOKEN_KIND_IDENTIFIER:
            return jump_in_macro_call__CIParser(self, next_token);
        case CI_TOKEN_KIND_HASHTAG_HASHTAG: {
            CIToken *pre_id = self->current_token;

            switch (pre_id->kind) {
                case CI_TOKEN_KIND_IDENTIFIER:
                    break;
                default:
                    FAILED("expected identifier");
            }

            next_token = next_token->next;

            ASSERT(next_token);

        loop:
            switch (next_token->kind) {
                case CI_TOKEN_KIND_IDENTIFIER: {
                    CIToken *post_id = next_token;
                    String *merged_id = format__String(
                      "{S}{S}", pre_id->identifier, post_id->identifier);
                    Location merged_location =
                      NEW(Location,
                          next_token->location.filename,
                          next_token->location.start_line,
                          post_id->location.end_line,
                          next_token->location.start_column,
                          post_id->location.end_column,
                          next_token->location.start_position,
                          post_id->location.end_position);
                    CIToken *merged_token = NEW_VARIANT(
                      CIToken, identifier, merged_location, merged_id);
                    CIToken *eot = NEW_VARIANT(
                      CIToken,
                      eot,
                      default__Location(next_token->location.filename),
                      CI_TOKEN_EOT_CONTEXT_MERGED_ID);

                    merged_token->next = eot;
                    // NOTE: We save the merged token pointer in EOT,
                    // because we need to free it after use.
                    eot->eot.merged_token = merged_token;
                    eot->next = next_token;

                    return merged_token;
                }
                case CI_TOKEN_KIND_LITERAL_CONSTANT_INT:
                    TODO("merge digit");
                case CI_TOKEN_KIND_MACRO_PARAM:
                    next_token =
                      jump_in_token_block__CIParser(self, next_token);

                    goto loop;
                default:
                    FAILED("expected `pre_id`##`post_id`");
            }
        }
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC__:
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_VERSION__:
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_HOSTED__: {
            char *standard_predefined_macro_s[CI_TOKEN_KIND_MAX] = {
                [CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC__] = "__STDC__",
                [CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_VERSION__] =
                  "__STDC_VERSION__",
                [CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_HOSTED__] =
                  "__STDC_HOSTED__",
            };

            const CIResultDefine *def = get_define_from_str__CIResultFile(
              self->file, standard_predefined_macro_s[next_token->kind]);

            if (def) {
                add_macro__CIParser(self, NEW(CIParserMacroCall));

                def->define->tokens.last->next = next_token->next;

                return def->define->tokens.first;
            }

            UNREACHABLE("standard predefined macro cannot be undefined");
        }
        case CI_TOKEN_KIND_PREPROCESSOR_IF:
        case CI_TOKEN_KIND_PREPROCESSOR_IFDEF:
        case CI_TOKEN_KIND_PREPROCESSOR_IFNDEF:
            return select_conditional_preprocessor__CIParser(self, next_token);
        default:
            return NULL;
    }

    return NULL;
}

void
set_current_token__CIParser(CIParser *self, CIToken *next_token)
{
    self->previous_token =
      self->current_token ? self->current_token : next_token;
    self->current_token = next_token;
}

void
init__CIParser(CIParser *self)
{
    ASSERT(self->tokens->first);

    check_standard__CIParser(self, self->tokens->first);

    return next_token__CIParser(self);
}

void
set_current_scope__CIParser(CIParser *self)
{
    current_scope = self->file->scope_base;
}

bool
next_token_must_continue_to_iter__CIParser(CIParser *self, CIToken *next_token)
{
    switch (next_token->kind) {
        case CI_TOKEN_KIND_EOT:
        case CI_TOKEN_KIND_HASHTAG_HASHTAG:
        case CI_TOKEN_KIND_MACRO_PARAM:
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC__:
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_VERSION__:
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___STDC_HOSTED__:
            return true;
        case CI_TOKEN_KIND_IDENTIFIER:
            return get_define__CIResultFile(self->file,
                                            next_token->identifier) ||
                   next_token->next->kind == CI_TOKEN_KIND_HASHTAG_HASHTAG;
        default:
            if (is_preprocessor__CITokenKind(next_token->kind)) {
                return true;
            }

            return false;
    }
}

void
check_standard__CIParser(CIParser *self, CIToken *token)
{
    ASSERT(token->kind < CI_TOKEN_KIND_MAX);

    Location location_error = clone__Location(&token->location);
    const CIFeature *feature = &tokens_feature[token->kind];

    CHECK_STANDARD_SINCE(
      self->file->entity.result->config->standard, feature->since, {
          enum CIErrorKind error_kind;

          switch (feature->since) {
              case CI_STANDARD_NONE:
                  return;
              case CI_STANDARD_KR:
                  UNREACHABLE("since: no error with K&R standard");
              case CI_STANDARD_89:
                  error_kind = CI_ERROR_KIND_REQUIRED_C89_OR_LATER;

                  break;
              case CI_STANDARD_95:
                  error_kind = CI_ERROR_KIND_REQUIRED_C95_OR_LATER;

                  break;
              case CI_STANDARD_99:
                  error_kind = CI_ERROR_KIND_REQUIRED_C99_OR_LATER;

                  break;
              case CI_STANDARD_11:
                  error_kind = CI_ERROR_KIND_REQUIRED_C11_OR_LATER;

                  break;
              case CI_STANDARD_17:
                  error_kind = CI_ERROR_KIND_REQUIRED_C17_OR_LATER;

                  break;
              case CI_STANDARD_23:
                  error_kind = CI_ERROR_KIND_REQUIRED_C23_OR_LATER;

                  break;
              default:
                  UNREACHABLE("unknown standard");
          }

          emit__Diagnostic(NEW_VARIANT(Diagnostic,
                                       simple_ci_error,
                                       &self->file->file_input,
                                       &location_error,
                                       NEW(CIError, error_kind),
                                       NULL,
                                       NULL,
                                       NULL),
                           self->count_error);
      });

    CHECK_STANDARD_UNTIL(
      self->file->entity.result->config->standard, feature->until, {
          String *note = NULL;

          switch (feature->until) {
              case CI_STANDARD_NONE:
                  return;
              case CI_STANDARD_KR:
                  UNREACHABLE("since: no error with K&R standard");
              case CI_STANDARD_89:
                  note =
                    from__String("this feature is no longer available in C89");

                  break;
              case CI_STANDARD_95:
                  note =
                    from__String("this feature is no longer available in C95");

                  break;
              case CI_STANDARD_99:
                  note =
                    from__String("this feature is no longer available in C99");

                  break;
              case CI_STANDARD_11:
                  note =
                    from__String("this feature is no longer available in C11");

                  break;
              case CI_STANDARD_17:
                  note =
                    from__String("this feature is no longer available in C17");

                  break;
              case CI_STANDARD_23:
                  note =
                    from__String("this feature is no longer available in C23");

                  break;
              default:
                  UNREACHABLE("unknown standard");
          }

          emit_note__Diagnostic(NEW_VARIANT(Diagnostic,
                                            simple_ci_note,
                                            &self->file->file_input,
                                            &location_error,
                                            note,
                                            NULL,
                                            NULL,
                                            NULL));
      });

#undef CHECK_STANDARD_SINCE
#undef CHECK_STANDARD_UNTIL
}

void
next_token__CIParser(CIParser *self)
{
    CIToken *next_token = self->current_token;

loop:
    if (next_token) {
        check_standard__CIParser(self, next_token);

        switch (next_token->kind) {
            case CI_TOKEN_KIND_EOF:
                return set_current_token__CIParser(self, next_token);
            case CI_TOKEN_KIND_EOT:
                switch (next_token->eot.ctx) {
                    case CI_TOKEN_EOT_CONTEXT_INCLUDE:
                        // Restore EOF.
                        next_token->kind = CI_TOKEN_KIND_EOF;

                        break;
                    case CI_TOKEN_EOT_CONTEXT_MACRO_CALL: {
                        CIParserMacroCall *macro_call =
                          pop__Stack(self->macros_call);

                        FREE(CIParserMacroCall, macro_call);

                        break;
                    }
                    case CI_TOKEN_EOT_CONTEXT_MACRO_PARAM:
                        // NOTE: Restore the saved token in EOT.
                        if (next_token->eot.macro_param) {
                            next_token = next_token->eot.macro_param;
                            next_token->eot.macro_param = NULL;
                        }

                        break;
                    case CI_TOKEN_EOT_CONTEXT_MERGED_ID: {
                        CIToken *eot = next_token;

                        next_token = next_token->next;

                        ASSERT(eot->eot.merged_token);

                        FREE(CIToken, eot->eot.merged_token);
                        FREE(CIToken, eot);

                        break;
                    }
                    case CI_TOKEN_EOT_CONTEXT_OTHER:
                        break;
                    default:
                        UNREACHABLE("unknown variant");
                }

                break;
            default:
                break;
        }
    }

    next_token = next_token ? next_token->next : self->tokens->first;

check: {
    CIToken *original_token = next_token;

    // Resolve preprocessor like #error, #warning, #embed, #include, ...
    // (not conditional preprocessor).
    //
    // If we know that a preprocessor has been solved (when
    // `resolve_preprocessor__CIParser(self)` == true), we want to move one
    // token forward again, to skip it.
    if (resolve_preprocessor__CIParser(self, next_token)) {
        goto loop;
    }

    // Jump in token block such as #if, #ifdef, macro call, ...
    {
        CIToken *res = jump_in_token_block__CIParser(self, next_token);

        if (res) {
            next_token = res;
        }
    }

    if (next_token_must_continue_to_iter__CIParser(self, next_token)) {
        if (original_token != next_token &&
            next_token->kind != CI_TOKEN_KIND_EOF) {
            check_standard__CIParser(self, next_token);

            goto check;
        } else if (next_token->next->kind == CI_TOKEN_KIND_HASHTAG_HASHTAG) {
            set_current_token__CIParser(self, next_token);
        }

        goto loop;
    }

    set_current_token__CIParser(self, next_token);
}
}

CIToken *
peek_token__CIParser(CIParser *self, Usize n)
{
    CIToken *original_previous_token = self->previous_token;
    CIToken *original_current_token = self->current_token;

    while (n--) {
        next_token__CIParser(self);
    }

    CIToken *peeked = self->current_token;

    self->previous_token = original_previous_token;
    self->current_token = original_current_token;

    return peeked;
}

bool
expect__CIParser(CIParser *self, enum CITokenKind kind, bool emit_error)
{
    if (self->current_token->kind == kind) {
        next_token__CIParser(self);

        return true;
    }

    if (emit_error) {
        FAILED("expected: ...");
    }

    return false;
}

bool
expect_many__CIParser(CIParser *self, Usize n, ...)
{
    ASSERT(n > 0);

    va_list vl;

    va_start(vl);

    while (n--) {
        if (!expect__CIParser(self, va_arg(vl, enum CITokenKind), true)) {
            return false;
        }
    }

    va_end(vl);

    return true;
}

void
expect_with_list__CIParser(CIParser *self, Usize n, ...)
{
    ASSERT(n > 0);

    va_list vl;

    va_start(vl);

    while (n--) {
        if (expect__CIParser(self, va_arg(vl, enum CITokenKind), false)) {
            return;
        }
    }

    va_end(vl);

    FAILED("expected ...");
}

void
jump__CIParser(CIParser *self, Usize n)
{
    while (n--) {
        next_token__CIParser(self);
    }
}

bool
token_is_data_type__CIParser(CIParser *self, const CIToken *token)
{
    switch (token->kind) {
        case CI_TOKEN_KIND_IDENTIFIER:
            return search_typedef__CIResultFile(self->file, token->identifier);
        case CI_TOKEN_KIND_AT: // TODO: check if the next token is an identifier
                               // (not needed for the moment)
        case CI_TOKEN_KIND_KEYWORD_BOOL:
        case CI_TOKEN_KIND_KEYWORD_CHAR:
        case CI_TOKEN_KIND_KEYWORD_CONST:
        case CI_TOKEN_KIND_KEYWORD_DOUBLE:
        case CI_TOKEN_KIND_KEYWORD_DOUBLE__COMPLEX:
        case CI_TOKEN_KIND_KEYWORD_DOUBLE__IMAGINARY:
        case CI_TOKEN_KIND_KEYWORD_ENUM:
        case CI_TOKEN_KIND_KEYWORD_FLOAT:
        case CI_TOKEN_KIND_KEYWORD_FLOAT__COMPLEX:
        case CI_TOKEN_KIND_KEYWORD_FLOAT__IMAGINARY:
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE:
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE__COMPLEX:
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE__IMAGINARY:
        case CI_TOKEN_KIND_KEYWORD_LONG_INT:
        case CI_TOKEN_KIND_KEYWORD_INT:
        case CI_TOKEN_KIND_KEYWORD_SHORT_INT:
        case CI_TOKEN_KIND_KEYWORD_SIGNED_CHAR:
        case CI_TOKEN_KIND_KEYWORD_STRUCT:
        case CI_TOKEN_KIND_KEYWORD_UNION:
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_CHAR:
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_INT:
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_LONG_INT:
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_SHORT_INT:
        case CI_TOKEN_KIND_KEYWORD_VOID:
        case CI_TOKEN_KIND_KEYWORD_VOLATILE:
        case CI_TOKEN_KIND_KEYWORD__BOOL:
        case CI_TOKEN_KIND_KEYWORD__DECIMAL128:
        case CI_TOKEN_KIND_KEYWORD__DECIMAL32:
        case CI_TOKEN_KIND_KEYWORD__DECIMAL64:
            return true;
        default:
            return false;
    }
}

bool
is_data_type__CIParser(CIParser *self)
{
    return token_is_data_type__CIParser(self, self->current_token);
}

CIDataType *
substitute_generic__CIParser(const String *generic_name,
                             CIGenericParams *generic_params,
                             CIGenericParams *called_generic_params)
{
    Usize id = 0;

    for (Usize i = 0; i < generic_params->params->len; ++i) {
        CIDataType *generic_param = get__Vec(generic_params->params, i);

        switch (generic_param->kind) {
            case CI_DATA_TYPE_KIND_GENERIC:
                if (!strcmp(generic_param->generic->buffer,
                            generic_name->buffer)) {
                    id = i;
                    goto exit_loop;
                }

                break;
            default:
                FAILED("expected only generic data type for the moment");
        }
    }

    FAILED("generic param name not found");

    return NULL;

exit_loop:
    return get__Vec(called_generic_params->params, id);
}

CIDataType *
substitute_data_type__CIParser(CIDataType *data_type,
                               CIGenericParams *generic_params,
                               CIGenericParams *called_generic_params)
{
    switch (data_type->kind) {
        case CI_DATA_TYPE_KIND_ARRAY: {
            CIDataType *subs = substitute_data_type__CIParser(
              data_type, generic_params, called_generic_params);

            if (subs) {
                switch (data_type->array.kind) {
                    case CI_DATA_TYPE_ARRAY_KIND_SIZED:
                        return NEW_VARIANT(CIDataType,
                                           array,
                                           NEW_VARIANT(CIDataTypeArray,
                                                       sized,
                                                       subs,
                                                       data_type->array.name,
                                                       data_type->array.size));
                    case CI_DATA_TYPE_ARRAY_KIND_NONE:
                        return NEW_VARIANT(CIDataType,
                                           array,
                                           NEW_VARIANT(CIDataTypeArray,
                                                       none,
                                                       subs,
                                                       data_type->array.name));
                    default:
                        UNREACHABLE("unknown variant");
                }
            }

            return ref__CIDataType(data_type);
        }
        case CI_DATA_TYPE_KIND__ATOMIC: {
#define SUBSTITUTE_DATA_TYPE_WITH_MAX_ONE_GENERIC(data_type_name)        \
    CIDataType *subs = substitute_data_type__CIParser(                   \
      data_type->data_type_name, generic_params, called_generic_params); \
                                                                         \
    if (subs) {                                                          \
        return NEW_VARIANT(CIDataType, data_type_name, subs);            \
    }                                                                    \
                                                                         \
    return ref__CIDataType(data_type);

            SUBSTITUTE_DATA_TYPE_WITH_MAX_ONE_GENERIC(_atomic);
        }
        case CI_DATA_TYPE_KIND_FUNCTION: {
            Vec *subs_params = NEW(Vec);

            for (Usize i = 0; i < data_type->function.params->len; ++i) {
                CIDataType *subs_param = substitute_data_type__CIParser(
                  get__Vec(data_type->function.params, i),
                  generic_params,
                  called_generic_params);

                if (subs_param) {
                    push__Vec(subs_params, subs_param);
                }
            }

            CIDataType *subs_return_data_type = substitute_data_type__CIParser(
              data_type->function.return_data_type,
              generic_params,
              called_generic_params);

            if (subs_return_data_type) {
                return ref__CIDataType(data_type);
            } else {
                FREE_BUFFER_ITEMS(
                  subs_params->buffer, subs_params->len, CIDataType);
                FREE(Vec, subs_params);
            }

            return NEW_VARIANT(CIDataType,
                               function,
                               NEW(CIDataTypeFunction,
                                   data_type->function.name,
                                   subs_params,
                                   subs_return_data_type));
        }
        case CI_DATA_TYPE_KIND_GENERIC: {
            CIDataType *res = substitute_generic__CIParser(
              data_type->generic, generic_params, called_generic_params);

            if (res) {
                return ref__CIDataType(res);
            }

            return NULL;
        }
        case CI_DATA_TYPE_KIND_PRE_CONST: {
            SUBSTITUTE_DATA_TYPE_WITH_MAX_ONE_GENERIC(pre_const);
        }
        case CI_DATA_TYPE_KIND_POST_CONST: {
            SUBSTITUTE_DATA_TYPE_WITH_MAX_ONE_GENERIC(post_const);
        }
        case CI_DATA_TYPE_KIND_PTR: {
            SUBSTITUTE_DATA_TYPE_WITH_MAX_ONE_GENERIC(ptr);
        }
        case CI_DATA_TYPE_KIND_STRUCT: {
#define SUBSTITUTE_GENERIC_PARAMS(decl_name)                            \
    Vec *subs_params = NULL;                                            \
                                                                        \
    if (data_type->decl_name.generic_params) {                          \
        subs_params = NEW(Vec);                                         \
                                                                        \
        for (Usize i = 0;                                               \
             i < data_type->decl_name.generic_params->params->len;      \
             ++i) {                                                     \
            CIDataType *subs_param = substitute_data_type__CIParser(    \
              get__Vec(data_type->decl_name.generic_params->params, i), \
              generic_params,                                           \
              called_generic_params);                                   \
                                                                        \
            if (subs_param) {                                           \
                push__Vec(subs_params, subs_param);                     \
            }                                                           \
        }                                                               \
    }

#define SUBSTITUTE_STRUCT_OR_UNION(decl_name, decl_ty, variant)                \
    if (data_type->decl_name.generic_params || data_type->decl_name.fields) {  \
        SUBSTITUTE_GENERIC_PARAMS(decl_name);                                  \
                                                                               \
        Vec *subs_fields = NULL;                                               \
                                                                               \
        if (data_type->decl_name.fields) {                                     \
            subs_fields = NEW(Vec);                                            \
                                                                               \
            for (Usize i = 0; i < data_type->decl_name.fields->len; ++i) {     \
                CIDeclStructField *field =                                     \
                  get__Vec(data_type->decl_name.fields, i);                    \
                CIDataType *subs_field_dt = substitute_data_type__CIParser(    \
                  field->data_type, generic_params, called_generic_params);    \
                                                                               \
                push__Vec(subs_fields,                                         \
                          NEW(CIDeclStructField, field->name, subs_field_dt)); \
            }                                                                  \
        }                                                                      \
                                                                               \
        return NEW_VARIANT(                                                    \
          CIDataType,                                                          \
          variant,                                                             \
          NEW(decl_ty,                                                         \
              data_type->decl_name.name,                                       \
              subs_params ? NEW(CIGenericParams, subs_params) : NULL,          \
              subs_fields));                                                   \
    }                                                                          \
                                                                               \
    return ref__CIDataType(data_type);

            SUBSTITUTE_STRUCT_OR_UNION(struct_, CIDataTypeStruct, struct);
        }
        case CI_DATA_TYPE_KIND_TYPEDEF: {
            if (data_type->typedef_.generic_params) {
                SUBSTITUTE_GENERIC_PARAMS(typedef_);

                return NEW_VARIANT(CIDataType,
                                   typedef,
                                   NEW(CIDataTypeTypedef,
                                       data_type->typedef_.name,
                                       NEW(CIGenericParams, subs_params)));
            }

            return ref__CIDataType(data_type);
        }
        case CI_DATA_TYPE_KIND_UNION: {
            SUBSTITUTE_STRUCT_OR_UNION(union_, CIDataTypeUnion, union);
        }
        default:
            return ref__CIDataType(data_type);
    }
}

CIDataType *
substitute_and_generate_from_data_type__CIParser(
  CIParser *self,
  CIDataType *data_type,
  CIGenericParams *generic_params,
  CIGenericParams *called_generic_params)
{
    CIDataType *subs_data_type = substitute_data_type__CIParser(
      data_type, generic_params, called_generic_params);

    switch (subs_data_type->kind) {
        case CI_DATA_TYPE_KIND_STRUCT:
            if (subs_data_type->struct_.name) {
                generate_struct_gen__CIParser(
                  self,
                  subs_data_type->struct_.name,
                  subs_data_type->struct_.generic_params);
            }

            break;
        case CI_DATA_TYPE_KIND_UNION:
            if (subs_data_type->union_.name) {
                generate_union_gen__CIParser(
                  self,
                  subs_data_type->union_.name,
                  subs_data_type->union_.generic_params);
            }

            break;
        case CI_DATA_TYPE_KIND_TYPEDEF:
            generate_typedef_gen__CIParser(
              self,
              subs_data_type->typedef_.name,
              subs_data_type->typedef_.generic_params);

            break;
        default:
            break;
    }

    return subs_data_type;
}

Vec *
visit_struct_or_union__CIParser(CIParser *self,
                                const CIDecl *decl,
                                CIGenericParams *called_generic_params)
{
    if (has_generic__CIDecl(decl)) {
        const Vec *fields = get_fields__CIDecl(decl);

        if (!fields) {
            return NULL;
        }

        CIGenericParams *generic_params = get_generic_params__CIDecl(decl);
        Vec *gen_fields = NEW(Vec);

        for (Usize i = 0; i < fields->len; ++i) {
            const CIDeclStructField *field = get__Vec(fields, i);
            CIDataType *subs_data_type =
              substitute_and_generate_from_data_type__CIParser(
                self, field->data_type, generic_params, called_generic_params);

            if (subs_data_type) {
                push__Vec(gen_fields,
                          NEW(CIDeclStructField, field->name, subs_data_type));
            }
        }

        return gen_fields;
    }

    return NULL;
}

CIDataType *
visit_typedef__CIParser(CIParser *self,
                        const CIDecl *decl,
                        CIGenericParams *called_generic_params)
{
    ASSERT(decl->kind == CI_DECL_KIND_TYPEDEF);

    return substitute_and_generate_from_data_type__CIParser(
      self,
      decl->typedef_.data_type,
      decl->typedef_.generic_params,
      called_generic_params);
}

void
visit_data_type__CIParser(CIParser *self,
                          const CIDataType *data_type,
                          CIGenericParams *called_generic_params)
{
    switch (data_type->kind) {
        case CI_DATA_TYPE_KIND_GENERIC:
            break;
        case CI_DATA_TYPE_KIND_STRUCT:
            break;
        case CI_DATA_TYPE_KIND_UNION:
            break;
        default:
            break;
    }
}

void
visit_function_decl__CIParser(CIParser *self,
                              const CIDecl *decl,
                              CIGenericParams *called_generic_params)
{
    switch (decl->kind) {
        case CI_DECL_KIND_VARIABLE:
            break;
        default:
            break;
    }
}

void
visit_function_expr__CIParser(CIParser *self,
                              const CIExpr *expr,
                              CIGenericParams *called_generic_params)
{
    switch (expr->kind) {
        case CI_EXPR_KIND_ALIGNOF:
            visit_function_expr__CIParser(
              self, expr->alignof_, called_generic_params);

            break;
        case CI_EXPR_KIND_ARRAY_ACCESS:
            visit_function_expr__CIParser(
              self, expr->array_access.array, called_generic_params);
            visit_function_expr__CIParser(
              self, expr->array_access.access, called_generic_params);

            break;
        case CI_EXPR_KIND_BINARY:
            visit_function_expr__CIParser(
              self, expr->binary.left, called_generic_params);
            visit_function_expr__CIParser(
              self, expr->binary.right, called_generic_params);

            break;
        case CI_EXPR_KIND_CAST:
            visit_function_expr__CIParser(
              self, expr->cast.expr, called_generic_params);
            visit_data_type__CIParser(
              self, expr->cast.data_type, called_generic_params);

            break;
        case CI_EXPR_KIND_DATA_TYPE:
            visit_data_type__CIParser(
              self, expr->data_type, called_generic_params);

            break;
        case CI_EXPR_KIND_FUNCTION_CALL:
            TODO("visit function call");

            break;
        case CI_EXPR_KIND_GROUPING:
            visit_function_expr__CIParser(
              self, expr->grouping, called_generic_params);

            break;
        case CI_EXPR_KIND_IDENTIFIER:
            TODO("visit identifier");

            break;
        case CI_EXPR_KIND_LITERAL:
        case CI_EXPR_KIND_STRUCT_CALL:
            break;
        case CI_EXPR_KIND_SIZEOF:
            visit_function_expr__CIParser(
              self, expr->sizeof_, called_generic_params);

            break;
        case CI_EXPR_KIND_TERNARY:
            visit_function_expr__CIParser(
              self, expr->ternary.cond, called_generic_params);
            visit_function_expr__CIParser(
              self, expr->ternary.if_, called_generic_params);
            visit_function_expr__CIParser(
              self, expr->ternary.else_, called_generic_params);

            break;
        case CI_EXPR_KIND_UNARY:
            visit_function_expr__CIParser(
              self, expr->unary.expr, called_generic_params);

            break;
        default:
            UNREACHABLE("unknown variant");
    }
}

void
visit_function_stmt__CIParser(CIParser *self,
                              const CIStmt *stmt,
                              CIGenericParams *called_generic_params)
{
}

void
visit_function__CIParser(CIParser *self,
                         const CIDecl *function_decl,
                         CIGenericParams *called_generic_params)
{
    for (Usize i = 0; i < function_decl->function.body->len; ++i) {
        CIDeclFunctionItem *item = get__Vec(function_decl->function.body, i);

        switch (item->kind) {
            case CI_DECL_FUNCTION_ITEM_KIND_DECL:
                visit_function_decl__CIParser(
                  self, item->decl, called_generic_params);

                break;
            case CI_DECL_FUNCTION_ITEM_KIND_EXPR:
                visit_function_expr__CIParser(
                  self, item->expr, called_generic_params);

                break;
            case CI_DECL_FUNCTION_ITEM_KIND_STMT:
                visit_function_stmt__CIParser(
                  self, &item->stmt, called_generic_params);

                break;
            default:
                UNREACHABLE("unknown variant");
        }
    }
}

void
generate_function_gen__CIParser(CIParser *self,
                                String *function_name,
                                CIGenericParams *called_generic_params)
{
    const CIDecl *function_decl =
      search_function__CIResultFile(self->file, function_name);

    if (!function_decl) {
        if (is_recursive_function__CIParser(self, function_name)) {
            TODO("function is recursive");
        }

        FAILED("unknown function, impossible to call unknown function");
    }

    // Generate gen function declaration
    if (called_generic_params &&
        !is_generic_params_contains_generic__CIDecl(called_generic_params)) {
        if (function_decl->is_prototype) {
            add_item_to_visit_waiting_list__CIParser(
              self,
              function_decl->kind,
              function_decl->function.name,
              called_generic_params);
        } else {
            String *serialized_called_function_name =
              serialize_name__CIDeclFunction(&function_decl->function,
                                             called_generic_params);
            const CIDecl *function_gen = search_function__CIResultFile(
              self->file, serialized_called_function_name);

            if (!function_gen) {
                CIDataType *subs_return_data_type =
                  substitute_data_type__CIParser(
                    function_decl->function.return_data_type,
                    function_decl->function.generic_params,
                    called_generic_params);

                visit_function__CIParser(
                  self, function_decl, called_generic_params);

                CIDecl *function_gen_decl =
                  NEW_VARIANT(CIDecl,
                              function_gen,
                              (CIDecl *)function_decl,
                              ref__CIGenericParams(called_generic_params),
                              serialized_called_function_name,
                              subs_return_data_type ? subs_return_data_type
                                                    : ref__CIDataType(
                                                        function_decl->function
                                                          .return_data_type) /* Return a ref data type, when the substituted data type is NULL, to avoid an optional data type in the `return_data_type` field. */);

                add_decl_to_scope__CIParser(
                  self, &function_gen_decl, false, true);
            }
        }
    }
}

void
generate_type_gen__CIParser(CIParser *self,
                            enum CIDeclKind kind,
                            String *name,
                            CIGenericParams *called_generic_params,
                            bool (*is_recursive)(const CIParser *,
                                                 const String *),
                            CIDecl *(*search_decl)(const CIResultFile *,
                                                   const String *))
{
    CIDecl *decl = search_decl(self->file, name);

    if (!decl) {
        if (is_recursive(self, name)) {
            if (called_generic_params) {
                add_item_to_visit_waiting_list__CIParser(
                  self, kind, name, called_generic_params);
            }

            return;
        }

        FAILED("struct, typedef or union not found");
    }

    ASSERT(kind == decl->kind);

    if (called_generic_params &&
        !is_generic_params_contains_generic__CIDecl(called_generic_params)) {
        if (decl->is_prototype && !is_typedef__CIDecl(decl) &&
            !resolve_visit_waiting_list) {
            add_item_to_visit_waiting_list__CIParser(
              self, decl->kind, get_name__CIDecl(decl), called_generic_params);
        } else {
            String *serialized_called_decl_name = NULL;

            switch (decl->kind) {
                case CI_DECL_KIND_STRUCT:
                    serialized_called_decl_name = serialize_name__CIDeclStruct(
                      &decl->struct_, called_generic_params);

                    break;
                case CI_DECL_KIND_TYPEDEF:
                    serialized_called_decl_name = serialize_name__CIDeclTypedef(
                      &decl->typedef_, called_generic_params);

                    break;
                case CI_DECL_KIND_UNION:
                    serialized_called_decl_name = serialize_name__CIDeclUnion(
                      &decl->union_, called_generic_params);

                    break;
                default:
                    UNREACHABLE("this kind of variant is not exepcted");
            }

            CIDecl *decl_gen =
              search_decl(self->file, serialized_called_decl_name);

            if (!decl_gen) {
                switch (decl->kind) {
                    case CI_DECL_KIND_STRUCT:
                        decl_gen = NEW_VARIANT(
                          CIDecl,
                          struct_gen,
                          decl,
                          ref__CIGenericParams(called_generic_params),
                          serialized_called_decl_name,
                          visit_struct_or_union__CIParser(
                            self, decl, called_generic_params));

                        break;
                    case CI_DECL_KIND_TYPEDEF:
                        decl_gen = NEW_VARIANT(
                          CIDecl,
                          typedef_gen,
                          decl,
                          ref__CIGenericParams(called_generic_params),
                          serialized_called_decl_name,
                          visit_typedef__CIParser(
                            self, decl, called_generic_params));

                        break;
                    case CI_DECL_KIND_UNION:
                        decl_gen = NEW_VARIANT(
                          CIDecl,
                          union_gen,
                          decl,
                          ref__CIGenericParams(called_generic_params),
                          serialized_called_decl_name,
                          visit_struct_or_union__CIParser(
                            self, decl, called_generic_params));

                        break;
                    default:
                        UNREACHABLE("this kind of variant is not expected");
                }

                add_decl_to_scope__CIParser(self, &decl_gen, false, true);
            } else {
                FREE(String, serialized_called_decl_name);
            }
        }
    }
}

void
generate_struct_gen__CIParser(CIParser *self,
                              String *struct_name_ref,
                              CIGenericParams *called_generic_params)
{
    generate_type_gen__CIParser(self,
                                CI_DECL_KIND_STRUCT,
                                struct_name_ref,
                                called_generic_params,
                                &is_recursive_struct__CIParser,
                                &search_struct__CIResultFile);
}

void
generate_union_gen__CIParser(CIParser *self,
                             String *union_name_ref,
                             CIGenericParams *called_generic_params)
{
    generate_type_gen__CIParser(self,
                                CI_DECL_KIND_UNION,
                                union_name_ref,
                                called_generic_params,
                                &is_recursive_union__CIParser,
                                &search_union__CIResultFile);
}

void
generate_typedef_gen__CIParser(CIParser *self,
                               String *typedef_name_ref,
                               CIGenericParams *called_generic_params)
{
    generate_type_gen__CIParser(self,
                                CI_DECL_KIND_TYPEDEF,
                                typedef_name_ref,
                                called_generic_params,
                                &is_recursive_typedef__CIParser,
                                &search_typedef__CIResultFile);
}

CIDataType *
parse_post_data_type__CIParser(CIParser *self, CIDataType *data_type)
{
loop:
    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LPAREN:
            TODO("function data type");
        default:
            if (self->current_token->kind == CI_TOKEN_KIND_STAR ||
                self->current_token->kind == CI_TOKEN_KIND_KEYWORD_CONST) {
                while (true) {
                    switch (self->current_token->kind) {
                        case CI_TOKEN_KIND_STAR:
                            data_type = NEW_VARIANT(CIDataType, ptr, data_type);

                            break;
                        case CI_TOKEN_KIND_KEYWORD_CONST:
                            data_type =
                              NEW_VARIANT(CIDataType, post_const, data_type);

                            break;
                        default:
                            goto exit;
                    }

                    next_token__CIParser(self);
                }

                goto loop;
            }
    }

exit:
    return data_type;
}

CIDataType *
parse_data_type__CIParser(CIParser *self)
{
    CIDataType *res = NULL;

    next_token__CIParser(self);

    switch (self->previous_token->kind) {
        case CI_TOKEN_KIND_IDENTIFIER: {
            String *name = self->previous_token->identifier;
            CIGenericParams *generic_params =
              parse_generic_params__CIParser(self, true);

            res = NEW_VARIANT(CIDataType,
                              typedef,
                              NEW(CIDataTypeTypedef, name, generic_params));

            generate_typedef_gen__CIParser(self, name, generic_params);

            break;
        }
        case CI_TOKEN_KIND_AT: {
            String *generic = NULL;

            if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
                generic = self->previous_token->identifier;
            } else {
                generic = generate_name_error__CIParser();
            }

            res = NEW_VARIANT(CIDataType, generic, generic);

            break;
        }
        case CI_TOKEN_KIND_KEYWORD_BOOL:
        case CI_TOKEN_KIND_KEYWORD__BOOL:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_BOOL);

            break;
        case CI_TOKEN_KIND_KEYWORD_CHAR:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_CHAR);

            break;
        case CI_TOKEN_KIND_KEYWORD_CONST:
            res = NEW_VARIANT(
              CIDataType, pre_const, parse_data_type__CIParser(self));

            break;
        case CI_TOKEN_KIND_KEYWORD_DOUBLE:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_DOUBLE);

            break;
        case CI_TOKEN_KIND_KEYWORD_DOUBLE__COMPLEX:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_DOUBLE__COMPLEX);

            break;
        case CI_TOKEN_KIND_KEYWORD_DOUBLE__IMAGINARY:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_DOUBLE__IMAGINARY);

            break;
        case CI_TOKEN_KIND_KEYWORD_ENUM: {
            String *name = NULL; // String* (&)

            // enum <name> ...;
            // enum <name> { ... } ...;
            if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
                name = self->previous_token->identifier;
            } else {
                name = generate_name_error__CIParser();
            }

            res = NEW_VARIANT(CIDataType, enum, name);

            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_LBRACE:
                case CI_TOKEN_KIND_SEMICOLON: {
                    CIDecl *enum_decl =
                      parse_enum__CIParser(self, storage_class_flag, name);

                    if (add_enum__CIResultFile(self->file, enum_decl)) {
                        FREE(CIDecl, enum_decl);

                        FAILED("duplicate enum declaration");
                    }

                    break;
                }
                default:
                    break;
            }

            break;
        }
        case CI_TOKEN_KIND_KEYWORD_FLOAT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_FLOAT);

            break;
        case CI_TOKEN_KIND_KEYWORD_FLOAT__COMPLEX:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_FLOAT__COMPLEX);

            break;
        case CI_TOKEN_KIND_KEYWORD_FLOAT__IMAGINARY:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_FLOAT__IMAGINARY);

            break;
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_LONG_DOUBLE);

            break;
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE__COMPLEX:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_LONG_DOUBLE__COMPLEX);

            break;
        case CI_TOKEN_KIND_KEYWORD_LONG_DOUBLE__IMAGINARY:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_LONG_DOUBLE__IMAGINARY);

            break;
        case CI_TOKEN_KIND_KEYWORD_LONG_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_LONG_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_SHORT_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_SHORT_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_SIGNED_CHAR:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_SIGNED_CHAR);

            break;
        case CI_TOKEN_KIND_KEYWORD_STRUCT:
        case CI_TOKEN_KIND_KEYWORD_UNION: {
            enum CITokenKind previous_token_kind = self->previous_token->kind;
            String *name = NULL; // String* (&)

            // struct <name><generic_params, ...> ...;
            // struct <name><generic_params, ...> { ... } ...;
            // struct { ... } ...;
            // NOTE: A union or struct can be anonymous.
            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_IDENTIFIER:
                    name = self->current_token->identifier;
                    next_token__CIParser(self);

                    break;
                default:
                    break;
            }

            bool generic_params_is_call = false;

            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_DOT:
                    generic_params_is_call = true;

                    break;
                default:
                    break;
            }

            CIGenericParams *generic_params = parse_generic_params__CIParser(
              self, generic_params_is_call); // CIGenericParams*?

            switch (previous_token_kind) {
                case CI_TOKEN_KIND_KEYWORD_STRUCT:
                    res = NEW_VARIANT(
                      CIDataType,
                      struct,
                      NEW(CIDataTypeStruct, name, generic_params, NULL));

                    break;
                case CI_TOKEN_KIND_KEYWORD_UNION:
                    res = NEW_VARIANT(
                      CIDataType,
                      union,
                      NEW(CIDataTypeUnion, name, generic_params, NULL));

                    break;
                default:
                    UNREACHABLE("unknown variant");
            }

            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_LBRACE:
                case CI_TOKEN_KIND_SEMICOLON:
                    if (generic_params_is_call) {
                        FAILED("`.` is not expected before generic params");
                    }

                    switch (previous_token_kind) {
                        case CI_TOKEN_KIND_KEYWORD_STRUCT: {
                            CIDecl *struct_decl = parse_struct__CIParser(
                              self,
                              storage_class_flag,
                              name,
                              generic_params
                                ? ref__CIGenericParams(generic_params)
                                : NULL);

                            if (!add_struct__CIParser(self, struct_decl)) {
                                break;
                            }

                            if (struct_decl->struct_.fields &&
                                !struct_decl->struct_.name) {
                                res->struct_.fields =
                                  clone_fields__CIDeclStructField(
                                    struct_decl->struct_.fields);
                                FREE(CIDecl, struct_decl);
                            }

                            break;
                        }
                        case CI_TOKEN_KIND_KEYWORD_UNION: {
                            CIDecl *union_decl = parse_union__CIParser(
                              self,
                              storage_class_flag,
                              name,
                              generic_params
                                ? ref__CIGenericParams(generic_params)
                                : NULL);

                            if (!add_union__CIParser(self, union_decl)) {
                                break;
                            }

                            if (union_decl->union_.fields &&
                                !union_decl->union_.name) {
                                res->union_.fields =
                                  clone_fields__CIDeclStructField(
                                    union_decl->union_.fields);
                                FREE(CIDecl, union_decl);
                            }

                            break;
                        }
                        default:
                            UNREACHABLE("unknown variant");
                    }

                    break;
                default:
                    switch (previous_token_kind) {
                        case CI_TOKEN_KIND_KEYWORD_STRUCT:
                            generate_struct_gen__CIParser(
                              self, name, generic_params);

                            break;
                        case CI_TOKEN_KIND_KEYWORD_UNION:
                            generate_union_gen__CIParser(
                              self, name, generic_params);

                            break;
                        default:
                            UNREACHABLE("this situation is impossible");
                    }
            }

            break;
        }
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_CHAR:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_UNSIGNED_CHAR);

            break;
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_UNSIGNED_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_LONG_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_UNSIGNED_LONG_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_UNSIGNED_SHORT_INT:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_UNSIGNED_SHORT_INT);

            break;
        case CI_TOKEN_KIND_KEYWORD_VOID:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND_VOID);

            break;
        case CI_TOKEN_KIND_KEYWORD_VOLATILE:
            TODO("volatile");
        case CI_TOKEN_KIND_KEYWORD__DECIMAL128:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND__DECIMAL128);

            break;
        case CI_TOKEN_KIND_KEYWORD__DECIMAL32:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND__DECIMAL32);

            break;
        case CI_TOKEN_KIND_KEYWORD__DECIMAL64:
            res = NEW(CIDataType, CI_DATA_TYPE_KIND__DECIMAL64);

            break;
        default:
            FAILED("expected data type");
    }

    return parse_post_data_type__CIParser(self, res);
}

Vec *
parse_enum_variants__CIParser(CIParser *self)
{
    Vec *variants = NEW(Vec); // Vec<CIDeclEnumVariant*>*

    while (self->current_token->kind != CI_TOKEN_KIND_RBRACE &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        String *name = NULL; // String* (&)

        if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
            name = self->previous_token->identifier;
        } else {
            name = generate_name_error__CIParser();
        }

        switch (self->current_token->kind) {
            case CI_TOKEN_KIND_EQ:
                next_token__CIParser(self);

                TODO("parse expression");

                break;
            default:
                push__Vec(variants,
                          NEW_VARIANT(CIDeclEnumVariant, default, name));
        }

        if (self->current_token->kind != CI_TOKEN_KIND_RBRACE) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }
    }

    expect__CIParser(self, CI_TOKEN_KIND_RBRACE, true);

    if (variants->len == 0) {
        FAILED("expected one or many variants");
    }

    return variants;
}

CIDecl *
parse_enum__CIParser(CIParser *self, int storage_class_flag, String *name)
{
    ASSERT(self->current_token->kind == CI_TOKEN_KIND_LBRACE ||
           self->current_token->kind == CI_TOKEN_KIND_SEMICOLON);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            break;
        default:
            return NEW_VARIANT(CIDecl,
                               enum,
                               storage_class_flag,
                               true,
                               NEW(CIDeclEnum, name, NULL));
    }

    return NEW_VARIANT(
      CIDecl,
      enum,
      storage_class_flag,
      false,
      NEW(CIDeclEnum, name, parse_enum_variants__CIParser(self)));
}

Vec *
parse_function_params__CIParser(CIParser *self)
{
    next_token__CIParser(self); // skip `(`

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_RPAREN:
            next_token__CIParser(self);

            return NULL;
        default:
            break;
    }

    Vec *params = NEW(Vec); // Vec<CIDeclFunctionParam*>*

    while (self->current_token->kind != CI_TOKEN_KIND_RPAREN &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIDataType *data_type = parse_data_type__CIParser(self);
        String *name = NULL; // String*? (&)

        // TODO: ... parameter
        if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, false)) {
            name = self->previous_token->identifier;
        }

        push__Vec(params, NEW(CIDeclFunctionParam, name, data_type));

        if (self->current_token->kind != CI_TOKEN_KIND_RPAREN) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }
    }

    if (self->current_token->kind == CI_TOKEN_KIND_EOF) {
        FAILED("hit EOF");
    } else {
        next_token__CIParser(self); // skip `}`
    }

    return params;
}

CIGenericParams *
parse_generic_params__CIParser(CIParser *self, bool is_call)
{
    Usize is_data_type_jump = 0;

    if (is_call) {
        switch (self->current_token->kind) {
            case CI_TOKEN_KIND_DOT: {
                CIToken *peeked = peek_token__CIParser(self, 1);

                switch (peeked->kind) {
                    case CI_TOKEN_KIND_LHOOK:
                        break;
                    default:
                        return NULL;
                }

                is_data_type_jump = 2;

                break;
            }
            default:
                return NULL;
        }
    } else {
        switch (self->current_token->kind) {
            case CI_TOKEN_KIND_LHOOK:
                is_data_type_jump = 1;

                break;
            default:
                return NULL;
        }
    }

    {
        CIToken *peeked = peek_token__CIParser(self, is_data_type_jump);

        if (token_is_data_type__CIParser(self, peeked)) {
            jump__CIParser(self, is_data_type_jump);
        } else {
            return NULL;
        }
    }

    Vec *params = NEW(Vec); // Vec<CIDataType*>*

    while (self->current_token->kind != CI_TOKEN_KIND_RHOOK &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIDataType *data_type = parse_data_type__CIParser(self);

        if (data_type) {
            push__Vec(params, data_type);
        }

        if (self->current_token->kind != CI_TOKEN_KIND_RHOOK) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }
    }

    if (params->len == 0) {
        FAILED("expected at least one generic param");
    }

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_RHOOK:
            next_token__CIParser(self);

            break;
        case CI_TOKEN_KIND_EOF:
            FAILED("unexpected EOF");
        default:
            UNREACHABLE("expected `]` or `EOF`");
    }

    return params ? NEW(CIGenericParams, params) : NULL;
}

CIExpr *
parse_function_call__CIParser(CIParser *self,
                              String *identifier,
                              CIGenericParams *generic_params)
{
    next_token__CIParser(self); // skip `(`

    Vec *params = NEW(Vec); // Vec<CIExpr*>*

    while (self->current_token->kind != CI_TOKEN_KIND_RPAREN &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIExpr *param = parse_expr__CIParser(self);

        if (param) {
            push__Vec(params, param);
        }

        if (self->current_token->kind != CI_TOKEN_KIND_RPAREN) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }
    }

    expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);
    generate_function_gen__CIParser(self, identifier, generic_params);

    return NEW_VARIANT(
      CIExpr,
      function_call,
      NEW(CIExprFunctionCall, identifier, params, generic_params));
}

CIExpr *
parse_literal_expr__CIParser(CIParser *self)
{
    switch (self->previous_token->kind) {
        case CI_TOKEN_KIND_KEYWORD_TRUE:
            return NEW_VARIANT(
              CIExpr, literal, NEW_VARIANT(CIExprLiteral, bool, true));
        case CI_TOKEN_KIND_KEYWORD_FALSE:
            return NEW_VARIANT(
              CIExpr, literal, NEW_VARIANT(CIExprLiteral, bool, false));
        case CI_TOKEN_KIND_LITERAL_CONSTANT_INT:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_OCTAL:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_HEX:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_BIN: {
            String *int_s = NULL;
            int base = 10;

            switch (self->previous_token->kind) {
                case CI_TOKEN_KIND_LITERAL_CONSTANT_INT:
                    int_s = self->previous_token->literal_constant_int.value;

                    break;
                case CI_TOKEN_KIND_LITERAL_CONSTANT_OCTAL:
                    int_s = self->previous_token->literal_constant_octal.value;
                    base = 8;

                    break;
                case CI_TOKEN_KIND_LITERAL_CONSTANT_HEX:
                    int_s = self->previous_token->literal_constant_hex.value;
                    base = 16;

                    break;
                case CI_TOKEN_KIND_LITERAL_CONSTANT_BIN:
                    int_s = self->previous_token->literal_constant_bin.value;
                    base = 2;

                    break;
                default:
                    UNREACHABLE("this kind of integer is not expected");
            }

            Optional *res_op = atoi_safe__Isize(int_s->buffer, base);

            if (is_none__Optional(res_op)) {
                FREE(Optional, res_op);

                FAILED("bad integer");
            }

            Isize res = (Isize)(Uptr)get__Optional(res_op);

            FREE(Optional, res_op);

            return NEW_VARIANT(
              CIExpr, literal, NEW_VARIANT(CIExprLiteral, signed_int, res));
        }
        case CI_TOKEN_KIND_LITERAL_CONSTANT_FLOAT:
            // TODO: Check if the float is overflow/underflow.
            // Update `base/atof.h` header.
            return NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(
                CIExprLiteral,
                float,
                atof__Float64(
                  self->previous_token->literal_constant_float.value->buffer)));
        case CI_TOKEN_KIND_LITERAL_CONSTANT_CHARACTER:
            return NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(CIExprLiteral,
                          char,
                          self->previous_token->literal_constant_character));
        case CI_TOKEN_KIND_LITERAL_CONSTANT_STRING:
            return NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(CIExprLiteral,
                          string,
                          self->previous_token->literal_constant_string));
        default:
            UNREACHABLE("unexpected token");
    }
}

CIExpr *
parse_struct_call__CIParser(CIParser *self)
{
    Vec *fields = NEW(Vec);

    next_token__CIParser(self); // skip `{`

    while (self->current_token->kind != CI_TOKEN_KIND_RBRACE &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        Vec *path = NEW(Vec);

        while (self->current_token->kind == CI_TOKEN_KIND_DOT) {
            next_token__CIParser(self);

            if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
                push__Vec(path, self->previous_token->identifier);
            }
        }

        expect__CIParser(self, CI_TOKEN_KIND_EQ, true);

        CIExpr *value = parse_expr__CIParser(self);

        if (self->current_token->kind != CI_TOKEN_KIND_RBRACE) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }

        if (!value) {
            FREE(Vec, path);

            continue;
        }

        push__Vec(fields, NEW(CIExprStructFieldCall, path, value));
    }

    expect__CIParser(self, CI_TOKEN_KIND_RBRACE, true);

    return NEW_VARIANT(CIExpr, struct_call, NEW(CIExprStructCall, fields));
}

CIExpr *
parse_array__CIParser(CIParser *self)
{
    next_token__CIParser(self); // skip `{`

    Vec *array = NEW(Vec);

    while (self->current_token->kind != CI_TOKEN_KIND_RBRACE &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIExpr *expr = parse_expr__CIParser(self);

        if (expr) {
            push__Vec(array, expr);
        }

        if (self->current_token->kind != CI_TOKEN_KIND_RBRACE) {
            expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
        }
    }

    expect__CIParser(self, CI_TOKEN_KIND_RBRACE, true);

    return NEW_VARIANT(CIExpr, array, NEW(CIExprArray, array));
}

CIExpr *
parse_primary_expr__CIParser(CIParser *self)
{
    if (data_type_as_expression) {
        CIDataType *data_type = data_type_as_expression;

        data_type_as_expression = NULL;

        return NEW_VARIANT(CIExpr, data_type, data_type);
    } else if (is_data_type__CIParser(self)) {
        CIDataType *data_type = parse_data_type__CIParser(self);

        return NEW_VARIANT(CIExpr, data_type, data_type);
    }

    next_token__CIParser(self);

    CIExpr *res = NULL;

    switch (self->previous_token->kind) {
        case CI_TOKEN_KIND_KEYWORD_ALIGNOF: {
            CIExpr *alignof_expr = parse_expr__CIParser(self);

            if (alignof_expr) {
                res = NEW_VARIANT(CIExpr, alignof, alignof_expr);
                break;
            }

            return NULL;
        }
        case CI_TOKEN_KIND_LPAREN: {
            if (is_data_type__CIParser(self)) {
                CIDataType *data_type = parse_data_type__CIParser(self);

                expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);

                CIExpr *expr = parse_expr__CIParser(self);

                if (!data_type || !expr) {
                    if (data_type) {
                        FREE(CIDataType, data_type);
                    } else if (expr) {
                        FREE(CIExpr, expr);
                    }

                    return NULL;
                }

                res =
                  NEW_VARIANT(CIExpr, cast, NEW(CIExprCast, data_type, expr));

                break;
            }

            CIExpr *expr = parse_expr__CIParser(self);

            expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);

            res = NEW_VARIANT(CIExpr, grouping, expr);

            break;
        }
        case CI_TOKEN_KIND_HASHTAG:
            TODO("done Stringification");
        case CI_TOKEN_KIND_KEYWORD_SIZEOF: {
            bool has_open_paren = false;

            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_LPAREN:
                    has_open_paren = true;

                    next_token__CIParser(self);

                    break;
                default:
                    break;
            }

            CIExpr *sizeof_expr = parse_expr__CIParser(self);

            if (has_open_paren) {
                expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);
            }

            if (sizeof_expr) {
                res = NEW_VARIANT(CIExpr, sizeof, sizeof_expr);

                break;
            }

            return NULL;
        }
        case CI_TOKEN_KIND_IDENTIFIER: {
            String *identifier = self->previous_token->identifier;
            CIGenericParams *generic_params =
              parse_generic_params__CIParser(self, true); // CIGenericParams*?

            switch (self->current_token->kind) {
                case CI_TOKEN_KIND_LPAREN:
                    res = parse_function_call__CIParser(
                      self, identifier, generic_params);

                    break;
                case CI_TOKEN_KIND_LBRACE:
                    TODO("parse struct call");
                default:
                    res = NEW_VARIANT(CIExpr, identifier, identifier);
            }

            break;
        }
        case CI_TOKEN_KIND_KEYWORD_TRUE:
        case CI_TOKEN_KIND_KEYWORD_FALSE:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_INT:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_FLOAT:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_OCTAL:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_HEX:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_BIN:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_CHARACTER:
        case CI_TOKEN_KIND_LITERAL_CONSTANT_STRING:
            res = parse_literal_expr__CIParser(self);

            break;
        case CI_TOKEN_KIND_BANG:
        case CI_TOKEN_KIND_AMPERSAND:
        case CI_TOKEN_KIND_MINUS:
        case CI_TOKEN_KIND_PLUS:
        case CI_TOKEN_KIND_WAVE:
        case CI_TOKEN_KIND_STAR:
        case CI_TOKEN_KIND_PLUS_PLUS:
        case CI_TOKEN_KIND_MINUS_MINUS: {
            enum CITokenKind unary_token_kind = self->previous_token->kind;
            CIExpr *expr = parse_primary_expr__CIParser(self);

            if (!expr) {
                return NULL;
            }

            switch (unary_token_kind) {
                case CI_TOKEN_KIND_BANG:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_NOT, expr));

                    break;
                case CI_TOKEN_KIND_AMPERSAND:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_REF, expr));

                    break;
                case CI_TOKEN_KIND_MINUS:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_NEGATIVE, expr));

                    break;
                case CI_TOKEN_KIND_PLUS:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_POSITIVE, expr));

                    break;
                case CI_TOKEN_KIND_WAVE:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_BIT_NOT, expr));

                    break;
                case CI_TOKEN_KIND_STAR:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_DEREFERENCE, expr));

                    break;
                case CI_TOKEN_KIND_PLUS_PLUS:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_PRE_INCREMENT, expr));

                    break;
                case CI_TOKEN_KIND_MINUS_MINUS:
                    res = NEW_VARIANT(
                      CIExpr,
                      unary,
                      NEW(CIExprUnary, CI_EXPR_UNARY_KIND_PRE_DECREMENT, expr));

                    break;
                default:
                    UNREACHABLE("unknown variant");
            }

            break;
        }
        case CI_TOKEN_KIND_MACRO_DEFINED: {
            const CIResultDefine *is_def = get_define__CIResultFile(
              self->file, self->previous_token->macro_defined);

            if (is_def) {
                res = NEW_VARIANT(
                  CIExpr, literal, NEW_VARIANT(CIExprLiteral, bool, true));

                break;
            }

            res = NEW_VARIANT(
              CIExpr, literal, NEW_VARIANT(CIExprLiteral, bool, false));

            break;
        }
        case CI_TOKEN_KIND_KEYWORD_NULLPTR:
            return NEW(CIExpr, CI_EXPR_KIND_NULLPTR);
        case CI_TOKEN_KIND_BUILTIN_MACRO___HAS_FEATURE: {
            bool has_feature = false;

            switch (self->previous_token->has_feature) {
                case CI_EXTENSIONS_HAS_FEATURE_C_ALIGNAS:
                case CI_EXTENSIONS_HAS_FEATURE_C_ALIGNOF:
                case CI_EXTENSIONS_HAS_FEATURE_C_ATOMIC:
                case CI_EXTENSIONS_HAS_FEATURE_C_GENERIC_SELECTIONS:
                case CI_EXTENSIONS_HAS_FEATURE_C_GENERIC_SELECTION_WITH_CONTROLLING_TYPE:
                case CI_EXTENSIONS_HAS_FEATURE_C_STATIC_ASSERT:
                case CI_EXTENSIONS_HAS_FEATURE_C_THREAD_LOCAL:
                    if (self->file->entity.result->config->standard ==
                        CI_STANDARD_11) {
                        has_feature = true;
                    }

                    break;
                case CI_EXTENSIONS_HAS_FEATURE_ADDRESS_SANITIZER:
                    if (self->file->entity.result->config->compiler.kind ==
                        CI_COMPILER_KIND_CLANG) {
                        has_feature = true;
                    }

                    break;
                case CI_EXTENSIONS_HAS_FEATURE_MODULES:
                    break;
                default:
                    UNREACHABLE("unknown variant");
            }

            res = NEW_VARIANT(
              CIExpr, literal, NEW_VARIANT(CIExprLiteral, bool, has_feature));

            break;
        }
        // NOTE: Standard predefined macro cannot be redefined outside of
        // builtin file.
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___DATE__:
            res = NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(
                CIExprLiteral,
                string,
                self->previous_token->standard_predefined_macro___date__));

            break;
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___FILE__:
            res = NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(
                CIExprLiteral,
                string,
                self->previous_token->standard_predefined_macro___file__));

            break;
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___LINE__:
            res = NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(
                CIExprLiteral,
                unsigned_int,
                self->previous_token->standard_predefined_macro___line__));

            break;
        case CI_TOKEN_KIND_STANDARD_PREDEFINED_MACRO___TIME__:
            res = NEW_VARIANT(
              CIExpr,
              literal,
              NEW_VARIANT(
                CIExprLiteral,
                string,
                self->previous_token->standard_predefined_macro___time__));

            break;
        default:
            FAILED("unexpected token");
    }

    return parse_post_expr__CIParser(self, res);
}

CIExpr *
parse_binary_expr__CIParser(CIParser *self, CIExpr *expr)
{
    Vec *stack = NEW(Vec); // Vec<CIExpr* | enum CIExprBinaryKind*>*
    Usize last_precedence = to_precedence__CIExpr(expr);

    push__Vec(stack, expr);

    while (self->current_token->kind == CI_TOKEN_KIND_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_PLUS_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_MINUS_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_STAR_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_SLASH_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_PERCENTAGE_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_AMPERSAND_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_BAR_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_HAT_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_LSHIFT_LSHIFT_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_RSHIFT_RSHIFT_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_PLUS ||
           self->current_token->kind == CI_TOKEN_KIND_MINUS ||
           self->current_token->kind == CI_TOKEN_KIND_STAR ||
           self->current_token->kind == CI_TOKEN_KIND_SLASH ||
           self->current_token->kind == CI_TOKEN_KIND_PERCENTAGE ||
           self->current_token->kind == CI_TOKEN_KIND_AMPERSAND ||
           self->current_token->kind == CI_TOKEN_KIND_BAR ||
           self->current_token->kind == CI_TOKEN_KIND_HAT ||
           self->current_token->kind == CI_TOKEN_KIND_LSHIFT_LSHIFT ||
           self->current_token->kind == CI_TOKEN_KIND_RSHIFT_RSHIFT ||
           self->current_token->kind == CI_TOKEN_KIND_AMPERSAND_AMPERSAND ||
           self->current_token->kind == CI_TOKEN_KIND_BAR_BAR ||
           self->current_token->kind == CI_TOKEN_KIND_EQ_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_BANG_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_LSHIFT ||
           self->current_token->kind == CI_TOKEN_KIND_RSHIFT ||
           self->current_token->kind == CI_TOKEN_KIND_LSHIFT_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_RSHIFT_EQ ||
           self->current_token->kind == CI_TOKEN_KIND_DOT ||
           self->current_token->kind == CI_TOKEN_KIND_ARROW) {
        enum CIExprBinaryKind op =
          from_token__CIExprBinaryKind(self->current_token);
        Usize precedence = to_precedence__CIExprBinaryKind(op);

        next_token__CIParser(self);

        CIExpr *right = parse_primary_expr__CIParser(self);

        if (!right) {
            for (Usize i = 0; i < stack->len; i += 2) {
                FREE(CIExpr, get__Vec(stack, i));
            }

            FREE(Vec, stack);

            return NULL;
        }

        while (precedence <= last_precedence && stack->len > 1) {
            CIExpr *top_right = pop__Vec(stack);
            enum CIExprBinaryKind top_op =
              (enum CIExprBinaryKind)(Uptr)pop__Vec(stack);

            last_precedence = to_precedence__CIExprBinaryKind(top_op);

            if (last_precedence < precedence) {
                push__Vec(stack, (int *)top_op);
                push__Vec(stack, top_right);

                break;
            }

            CIExpr *top_left = pop__Vec(stack);

            push__Vec(
              stack,
              NEW_VARIANT(CIExpr,
                          binary,
                          NEW(CIExprBinary, top_op, top_left, top_right)));
        }

        push__Vec(stack, (int *)op);
        push__Vec(stack, right);

        last_precedence = precedence;
    }

    while (stack->len > 1) {
        CIExpr *rhs = pop__Vec(stack);
        enum CIExprBinaryKind op = (enum CIExprBinaryKind)(Uptr)pop__Vec(stack);
        CIExpr *lhs = pop__Vec(stack);

        push__Vec(stack,
                  NEW_VARIANT(CIExpr, binary, NEW(CIExprBinary, op, lhs, rhs)));
    }

    CIExpr *res = pop__Vec(stack);

    FREE(Vec, stack);

    return res;
}

CIExpr *
parse_post_expr__CIParser(CIParser *self, CIExpr *expr)
{
    if (!expr) {
        return NULL;
    }

loop:
    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_PLUS_PLUS:
            next_token__CIParser(self);

            expr = NEW_VARIANT(
              CIExpr,
              unary,
              NEW(CIExprUnary, CI_EXPR_UNARY_KIND_POST_INCREMENT, expr));

            goto loop;
        case CI_TOKEN_KIND_MINUS_MINUS:
            next_token__CIParser(self);

            expr = NEW_VARIANT(
              CIExpr,
              unary,
              NEW(CIExprUnary, CI_EXPR_UNARY_KIND_POST_DECREMENT, expr));

            goto loop;
        // Parse array access
        case CI_TOKEN_KIND_LHOOK: {
            next_token__CIParser(self);

            CIExpr *access = parse_expr__CIParser(self);

            expect__CIParser(self, CI_TOKEN_KIND_RHOOK, true);

            expr = NEW_VARIANT(
              CIExpr, array_access, NEW(CIExprArrayAccess, expr, access));

            goto loop;
        }
        default:
            return expr;
    }
}

CIExpr *
parse_expr__CIParser(CIParser *self)
{
    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE: {
            if (!allow_initialization) {
                FAILED("cannot declare array, struct call or union call "
                       "outside of variable initialization");
            }

            CIToken *peeked = peek_token__CIParser(self, 1);

            if (peeked && peeked->kind == CI_TOKEN_KIND_DOT) {
                return parse_struct_call__CIParser(self);
            } else {
                return parse_array__CIParser(self);
            }

            break;
        }
        default:
            break;
    }

    CIExpr *expr = parse_primary_expr__CIParser(self);

    if (!expr) {
        return NULL;
    }

    expr = parse_post_expr__CIParser(self, expr);

loop:
    switch (self->current_token->kind) {
        // Parse binary expression
        case CI_TOKEN_KIND_EQ:
        case CI_TOKEN_KIND_PLUS_EQ:
        case CI_TOKEN_KIND_MINUS_EQ:
        case CI_TOKEN_KIND_STAR_EQ:
        case CI_TOKEN_KIND_SLASH_EQ:
        case CI_TOKEN_KIND_PERCENTAGE_EQ:
        case CI_TOKEN_KIND_AMPERSAND_EQ:
        case CI_TOKEN_KIND_BAR_EQ:
        case CI_TOKEN_KIND_HAT_EQ:
        case CI_TOKEN_KIND_LSHIFT_LSHIFT_EQ:
        case CI_TOKEN_KIND_RSHIFT_RSHIFT_EQ:
        case CI_TOKEN_KIND_PLUS:
        case CI_TOKEN_KIND_MINUS:
        case CI_TOKEN_KIND_STAR:
        case CI_TOKEN_KIND_SLASH:
        case CI_TOKEN_KIND_PERCENTAGE:
        case CI_TOKEN_KIND_AMPERSAND:
        case CI_TOKEN_KIND_BAR:
        case CI_TOKEN_KIND_HAT:
        case CI_TOKEN_KIND_LSHIFT_LSHIFT:
        case CI_TOKEN_KIND_RSHIFT_RSHIFT:
        case CI_TOKEN_KIND_AMPERSAND_AMPERSAND:
        case CI_TOKEN_KIND_BAR_BAR:
        case CI_TOKEN_KIND_EQ_EQ:
        case CI_TOKEN_KIND_BANG_EQ:
        case CI_TOKEN_KIND_LSHIFT:
        case CI_TOKEN_KIND_RSHIFT:
        case CI_TOKEN_KIND_LSHIFT_EQ:
        case CI_TOKEN_KIND_RSHIFT_EQ:
        case CI_TOKEN_KIND_DOT:
        case CI_TOKEN_KIND_ARROW:
            expr = parse_binary_expr__CIParser(self, expr);

            goto loop;
        case CI_TOKEN_KIND_INTERROGATION:
            next_token__CIParser(self); // skip `?`

            CIExpr *expr_if = parse_expr__CIParser(self);

            if (!expr_if) {
                return expr;
            }

            expect__CIParser(self, CI_TOKEN_KIND_COLON, true);

            CIExpr *expr_else = parse_expr__CIParser(self);

            if (!expr_else) {
                FREE(CIExpr, expr_if);

                return expr;
            }

            return NEW_VARIANT(
              CIExpr, ternary, NEW(CIExprTernary, expr, expr_if, expr_else));
        default:
            return parse_post_expr__CIParser(self, expr);
    }
}

CIDeclFunctionItem *
parse_do_while_stmt__CIParser(CIParser *self, bool in_switch)
{
    Vec *body = NULL;

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            body = parse_function_body__CIParser(self, true, in_switch);

            break;
        default: {
            CIDeclFunctionItem *item =
              parse_function_body_item__CIParser(self, true, in_switch);

            if (item) {
                body = init__Vec(1, item);
            } else {
                body = NEW(Vec);
            }
        }
    }

    expect_many__CIParser(
      self, 2, CI_TOKEN_KIND_KEYWORD_WHILE, CI_TOKEN_KIND_LPAREN);

    CIExpr *cond = parse_expr__CIParser(self);

    if (!cond) {
        return NULL;
    }

    expect_many__CIParser(
      self, 2, CI_TOKEN_KIND_RPAREN, CI_TOKEN_KIND_SEMICOLON);

    return NEW_VARIANT(
      CIDeclFunctionItem,
      stmt,
      NEW_VARIANT(CIStmt, do_while, NEW(CIStmtDoWhile, body, cond)));
}

CIDeclFunctionItem *
parse_case__CIParser(CIParser *self)
{
    CIExpr *expr = parse_expr__CIParser(self);

    if (!expr) {
        return NULL;
    }

    expect__CIParser(self, CI_TOKEN_KIND_COLON, true);

    ENABLE_IN_LABEL();

    return NEW_VARIANT(CIDeclFunctionItem,
                       stmt,
                       NEW_VARIANT(CIStmt, case, NEW(CIStmtSwitchCase, expr)));
}

CIDeclFunctionItem *
parse_for_stmt__CIParser(CIParser *self, bool in_switch)
{
    CIDeclFunctionItem *init_clause = NULL;
    CIExpr *expr1 = NULL;
    Vec *exprs2 = NULL;
    Vec *body = NULL;

    expect__CIParser(self, CI_TOKEN_KIND_LPAREN, true);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_SEMICOLON:
            next_token__CIParser(self);
            break;
        default:
            init_clause =
              parse_function_body_item__CIParser(self, false, false);

            if (init_clause) {
                if (!is_for_init_clause__CIDeclFunctionItem(init_clause)) {
                    FAILED("expected valid for init-clause");
                }
            }
    }

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_SEMICOLON:
            next_token__CIParser(self);
            break;
        default:
            expr1 = parse_expr__CIParser(self);
            expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);
    }

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_RPAREN:
            next_token__CIParser(self);
            break;
        default:
            exprs2 = NEW(Vec);

            do {
                CIExpr *expr = parse_expr__CIParser(self);

                if (expr) {
                    push__Vec(exprs2, expr);
                }

                if (self->current_token->kind != CI_TOKEN_KIND_RPAREN) {
                    expect__CIParser(self, CI_TOKEN_KIND_COMMA, true);
                }
            } while (self->current_token->kind != CI_TOKEN_KIND_RPAREN &&
                     self->current_token->kind != CI_TOKEN_KIND_EOF);

            expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);
    }

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            body = parse_function_body__CIParser(self, true, in_switch);

            break;
        default: {
            CIDeclFunctionItem *item =
              parse_function_body_item__CIParser(self, true, in_switch);

            if (item) {
                body = init__Vec(1, item);
            } else {
                body = NEW(Vec);
            }
        }
    }

    return NEW_VARIANT(CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, for, NEW(CIStmtFor, body, init_clause, expr1, exprs2)));
}

CIStmtIfBranch *
parse_if_branch__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    expect__CIParser(self, CI_TOKEN_KIND_LPAREN, true);

    CIExpr *cond = parse_expr__CIParser(self);
    Vec *body = NULL;

    if (!cond) {
        FAILED("expected if condition");
    }

    expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            body = parse_function_body__CIParser(self, in_loop, in_switch);

            break;
        default: {
            CIDeclFunctionItem *item =
              parse_function_body_item__CIParser(self, in_loop, in_switch);

            if (item) {
                body = init__Vec(1, item);
            } else {
                body = NEW(Vec);
            }
        }
    }

    return NEW(CIStmtIfBranch, cond, body);
}

CIDeclFunctionItem *
parse_if_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    CIStmtIfBranch *if_ = parse_if_branch__CIParser(self, in_loop, in_switch);
    Vec *else_ifs = NULL;
    Vec *else_ = NULL;

    if (!if_) {
        return NULL;
    }

    if (self->current_token->kind == CI_TOKEN_KIND_KEYWORD_ELSE_IF) {
        else_ifs = NEW(Vec);
    }

    for (; self->current_token->kind == CI_TOKEN_KIND_KEYWORD_ELSE_IF;) {
        next_token__CIParser(self);

        CIStmtIfBranch *else_if =
          parse_if_branch__CIParser(self, in_loop, in_switch);

        if (else_if) {
            push__Vec(else_ifs, else_if);
        }
    }

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_KEYWORD_ELSE:
            next_token__CIParser(self);

            if (expect__CIParser(self, CI_TOKEN_KIND_LBRACE, false)) {
                else_ = parse_function_body__CIParser(self, in_loop, in_switch);
            } else {
                CIDeclFunctionItem *item =
                  parse_function_body_item__CIParser(self, in_loop, false);

                if (item) {
                    else_ = init__Vec(1, item);
                } else {
                    else_ = NEW(Vec);
                }
            }

            break;
        default:
            break;
    }

    return NEW_VARIANT(
      CIDeclFunctionItem,
      stmt,
      NEW_VARIANT(CIStmt, if, NEW(CIStmtIf, if_, else_ifs, else_)));
}

CIDeclFunctionItem *
parse_while_stmt__CIParser(CIParser *self, bool in_switch)
{
    expect__CIParser(self, CI_TOKEN_KIND_LPAREN, true);

    CIExpr *cond = parse_expr__CIParser(self);
    Vec *body = NULL;

    if (!cond) {
        return NULL;
    }

    expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            body = parse_function_body__CIParser(self, true, in_switch);

            break;
        default: {
            CIDeclFunctionItem *item =
              parse_function_body_item__CIParser(self, true, in_switch);

            if (item) {
                body = NEW(Vec);

                push__Vec(body, item);
            }
        }
    }

    return NEW_VARIANT(
      CIDeclFunctionItem,
      stmt,
      NEW_VARIANT(CIStmt, while, NEW(CIStmtWhile, cond, body)));
}

CIDeclFunctionItem *
parse_switch_stmt__CIParser(CIParser *self, bool in_loop)
{
    expect__CIParser(self, CI_TOKEN_KIND_LPAREN, true);

    CIExpr *expr = parse_expr__CIParser(self);

    if (!expr) {
        return NULL;
    }

    expect__CIParser(self, CI_TOKEN_KIND_RPAREN, true);

    Vec *body = NULL;

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);

            body = parse_function_body__CIParser(self, in_loop, true);

            break;
        default: {
            CIDeclFunctionItem *item =
              parse_function_body_item__CIParser(self, in_loop, true);

            if (item) {
                body = init__Vec(1, item);
            } else {
                body = NEW(Vec);
            }
        }
    }

    return NEW_VARIANT(
      CIDeclFunctionItem,
      stmt,
      NEW_VARIANT(CIStmt, switch, NEW(CIStmtSwitch, expr, body)));
}

CIDeclFunctionItem *
parse_block_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    Vec *body = parse_function_body__CIParser(self, in_loop, in_switch);

    return NEW_VARIANT(CIDeclFunctionItem,
                       stmt,
                       NEW_VARIANT(CIStmt, block, NEW(CIStmtBlock, body)));
}

CIDeclFunctionItem *
parse_stmt__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    next_token__CIParser(self);

    switch (self->previous_token->kind) {
        case CI_TOKEN_KIND_KEYWORD_BREAK:
            expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

            if (in_loop || in_switch) {
                return NEW_VARIANT(
                  CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, break));
            } else {
                FAILED("break is not expected outside of a loop");
            }
        case CI_TOKEN_KIND_KEYWORD_CASE:
            if (in_switch) {
                return parse_case__CIParser(self);
            } else {
                FAILED("case is not expected outside of a switch");
            }
        case CI_TOKEN_KIND_KEYWORD_CONTINUE:
            expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

            if (in_loop) {
                return NEW_VARIANT(
                  CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, continue));
            } else {
                FAILED("continue is not expected outside of a loop");
            }
        case CI_TOKEN_KIND_KEYWORD_DEFAULT:
            expect__CIParser(self, CI_TOKEN_KIND_COLON, true);

            ENABLE_IN_LABEL();

            if (in_switch) {
                return NEW_VARIANT(
                  CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, default));
            } else {
                FAILED("default is not expected outside of a switch");
            }
        case CI_TOKEN_KIND_KEYWORD_DO:
            return parse_do_while_stmt__CIParser(self, in_switch);
        case CI_TOKEN_KIND_KEYWORD_FOR:
            return parse_for_stmt__CIParser(self, in_switch);
        case CI_TOKEN_KIND_KEYWORD_GOTO: {
            String *label_identifier = NULL;

            if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
                label_identifier = self->previous_token->identifier;
            } else {
                label_identifier = generate_name_error__CIParser();
            }

            return NEW_VARIANT(CIDeclFunctionItem,
                               stmt,
                               NEW_VARIANT(CIStmt, goto, label_identifier));
        }
        case CI_TOKEN_KIND_IDENTIFIER: {
            String *identifier = self->previous_token->identifier;

            ASSERT(expect__CIParser(self, CI_TOKEN_KIND_COLON, false));

            return NEW_VARIANT(
              CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, label, identifier));
        }
        case CI_TOKEN_KIND_KEYWORD_IF:
            return parse_if_stmt__CIParser(self, in_loop, in_switch);
        case CI_TOKEN_KIND_KEYWORD_RETURN: {
            ENABLE_ALLOW_INITIALIZATION();

            CIExpr *expr = parse_expr__CIParser(self);

            DISABLE_ALLOW_INITIALIZATION();

            if (expr) {
                expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

                return NEW_VARIANT(
                  CIDeclFunctionItem, stmt, NEW_VARIANT(CIStmt, return, expr));
            }

            FAILED("expected expression");
        }
        case CI_TOKEN_KIND_KEYWORD_SWITCH:
            return parse_switch_stmt__CIParser(self, in_loop);
        case CI_TOKEN_KIND_KEYWORD_WHILE:
            return parse_while_stmt__CIParser(self, in_switch);
        case CI_TOKEN_KIND_LBRACE:
            return parse_block_stmt__CIParser(self, in_loop, in_switch);
        default:
            UNREACHABLE("not expected token");
    }
}

CIDeclFunctionItem *
parse_function_body_item__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_IDENTIFIER: {
            CIToken *peeked = peek_token__CIParser(self, 1);

            if (peeked && peeked->kind == CI_TOKEN_KIND_COLON) {
                goto parse_stmt;
            }

            goto default_case;
        }
        case CI_TOKEN_KIND_KEYWORD_BREAK:
        case CI_TOKEN_KIND_KEYWORD_CASE:
        case CI_TOKEN_KIND_KEYWORD_CONTINUE:
        case CI_TOKEN_KIND_KEYWORD_DEFAULT:
        case CI_TOKEN_KIND_KEYWORD_DO:
        case CI_TOKEN_KIND_KEYWORD_FOR:
        case CI_TOKEN_KIND_KEYWORD_GOTO:
        case CI_TOKEN_KIND_KEYWORD_IF:
        case CI_TOKEN_KIND_KEYWORD_RETURN:
        case CI_TOKEN_KIND_KEYWORD_SWITCH:
        case CI_TOKEN_KIND_KEYWORD_WHILE:
        case CI_TOKEN_KIND_LBRACE:
        parse_stmt: {
            DISABLE_IN_LABEL();

            return parse_stmt__CIParser(self, in_loop, in_switch);
        }
        case CI_TOKEN_KIND_SEMICOLON:
            next_token__CIParser(self);

            return NULL;
        default:
        default_case: {
            if (is_data_type__CIParser(self)) {
                CIDecl *decl = parse_decl__CIParser(self, true);

                DISABLE_IN_LABEL();

                if (decl) {
                    return NEW_VARIANT(CIDeclFunctionItem, decl, decl);
                } else if (!data_type_as_expression) {
                    return NULL;
                }
            }

            DISABLE_IN_LABEL();

            CIDeclFunctionItem *item =
              NEW_VARIANT(CIDeclFunctionItem, expr, parse_expr__CIParser(self));

            expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

            return item;
        }
    }
}

Vec *
parse_function_body__CIParser(CIParser *self, bool in_loop, bool in_switch)
{
    ASSERT(current_scope);

    // Save parent values.
    CIScope *parent_scope = current_scope;
    Vec *parent_body = current_body;

    current_scope =
      add_scope__CIResultFile(self->file, current_scope->scope_id, true);
    current_body = NEW(Vec);

    while (self->current_token->kind != CI_TOKEN_KIND_RBRACE &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIDeclFunctionItem *item =
          parse_function_body_item__CIParser(self, in_loop, in_switch);

        if (item) {
            push__Vec(current_body, item);
        }
    }

    Vec *res_body = current_body;

    // Restore parent values as current.
    current_scope = parent_scope;
    current_body = parent_body;

    DISABLE_IN_LABEL();

    expect__CIParser(self, CI_TOKEN_KIND_RBRACE, true);

    return res_body;
}

CIDecl *
parse_function__CIParser(CIParser *self,
                         int storage_class_flag,
                         CIDataType *return_data_type,
                         String *name,
                         CIGenericParams *generic_params)
{
    init_function_to_visit_waiting_list__CIParser(self, name);

    Vec *params =
      parse_function_params__CIParser(self); // Vec<CIDeclFunctionParam*>*?

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_SEMICOLON:
            next_token__CIParser(self);

            return NEW_VARIANT(CIDecl,
                               function,
                               storage_class_flag,
                               true,
                               NEW(CIDeclFunction,
                                   name,
                                   return_data_type,
                                   generic_params,
                                   params,
                                   NULL));
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);

            return NEW_VARIANT(
              CIDecl,
              function,
              storage_class_flag,
              false,
              NEW(CIDeclFunction,
                  name,
                  return_data_type,
                  generic_params,
                  params,
                  parse_function_body__CIParser(self, false, false)));
        default:
            FAILED("expected `;` or `{`");
    }
}

Vec *
parse_fields__CIParser(CIParser *self)
{
    Vec *fields = NEW(Vec); // Vec<CIDeclStructField*>*

    while (self->current_token->kind != CI_TOKEN_KIND_RBRACE &&
           self->current_token->kind != CI_TOKEN_KIND_EOF) {
        CIDataType *data_type = parse_data_type__CIParser(self);
        String *name = NULL;

        switch (data_type->kind) {
            case CI_DATA_TYPE_KIND_STRUCT:
            case CI_DATA_TYPE_KIND_UNION:
                if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, false)) {
                    name = self->previous_token->identifier;
                }

                break;
            default:
                if (expect__CIParser(self, CI_TOKEN_KIND_IDENTIFIER, true)) {
                    name = self->previous_token->identifier;
                } else {
                    name = generate_name_error__CIParser();
                }
        }

        expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

        // TODO: parse bits set

        push__Vec(fields, NEW(CIDeclStructField, name, data_type));
    }

    expect__CIParser(self, CI_TOKEN_KIND_RBRACE, true);

    return fields;
}

Vec *
parse_struct_or_union_fields__CIParser(CIParser *self)
{
    ASSERT(self->current_token->kind == CI_TOKEN_KIND_LBRACE ||
           self->current_token->kind == CI_TOKEN_KIND_SEMICOLON);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_LBRACE:
            next_token__CIParser(self);
            break;
        default:
            return NULL;
    }

    return parse_fields__CIParser(self);
}

bool
add_struct_or_union__CIParser(CIParser *self, CIDecl *decl)
{
    String *name = get_name__CIDecl(decl);

    if (name) {
        CIDecl *already_defined =
          (CIDecl *)add_decl_to_scope__CIParser(self, &decl, false, false);

        if (already_defined) {
            if (already_defined->is_prototype) {
                // Verify if the `decl` is a prototype.
                if (decl->is_prototype) {
                    // Verify if the prototypes match.
                    if (!match_prototype__CIDecl(decl, already_defined)) {
                        FAILED("unmatch redefinition of prototype");
                    }

                    FREE(CIDecl, decl);

                    return false;
                }

                already_defined->is_prototype = true;

                switch (already_defined->kind) {
                    case CI_DECL_KIND_STRUCT:
                        already_defined->struct_.fields = decl->struct_.fields;

                        break;
                    case CI_DECL_KIND_UNION:
                        already_defined->union_.fields = decl->union_.fields;

                        break;
                    default:
                        UNREACHABLE("expected struct or union");
                }

                free_as_prototype__CIDecl(decl);

                return false;
            } else {
                FREE(CIDecl, decl);

                FAILED("struct or union name is already defined");
            }
        }
    }

    return true;
}

CIDecl *
parse_struct__CIParser(CIParser *self,
                       int storage_class_flag,
                       String *name,
                       CIGenericParams *generic_params)
{
    if (name) {
        init_struct_to_visit_waiting_list__CIParser(self, name);
    }

    Vec *fields = parse_struct_or_union_fields__CIParser(self);

    return NEW_VARIANT(CIDecl,
                       struct,
                       storage_class_flag,
                       fields ? false : true,
                       NEW(CIDeclStruct, name, generic_params, fields));
}

bool
add_struct__CIParser(CIParser *self, CIDecl *struct_decl)
{
    return add_struct_or_union__CIParser(self, struct_decl);
}

CIDecl *
parse_typedef__CIParser(CIParser *self)
{
    // Disable `typedef`, as we no longer need it.
    storage_class_flag &= ~CI_STORAGE_CLASS_TYPEDEF;

    if (storage_class_flag != CI_STORAGE_CLASS_NONE) {
        FAILED(
          "cannot combine other storage class specifier(s) with `typedef`");
    }

    CIDataType *data_type = parse_data_type__CIParser(self);
    String *typedef_name = NULL;

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_IDENTIFIER:
            typedef_name = self->current_token->identifier;
            next_token__CIParser(self);

            break;
        default:
            FAILED("expected typedef identifier");
    }

    init_typedef_to_visit_waiting_list__CIParser(self, typedef_name);

    CIGenericParams *generic_params =
      parse_generic_params__CIParser(self, false);

    {
        const CIGenericParams *data_type_generic_params =
          get_generic_params__CIDataType(data_type);

        if (!eq_op__CIGenericParams(generic_params, data_type_generic_params)) {
            FAILED("expected same generic params in typedef declaration");
        }
    }

    expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

    return NEW_VARIANT(
      CIDecl,
      typedef,
      NEW(CIDeclTypedef, typedef_name, generic_params, data_type));
}

CIDecl *
parse_union__CIParser(CIParser *self,
                      int storage_class_flag,
                      String *name,
                      CIGenericParams *generic_params)
{
    if (name) {
        init_union_to_visit_waiting_list__CIParser(self, name);
    }

    Vec *fields = parse_struct_or_union_fields__CIParser(self);

    return NEW_VARIANT(CIDecl,
                       union,
                       storage_class_flag,
                       fields ? false : true,
                       NEW(CIDeclUnion, name, generic_params, fields));
}

bool
add_union__CIParser(CIParser *self, CIDecl *union_decl)
{
    return add_struct_or_union__CIParser(self, union_decl);
}

CIDecl *
parse_variable__CIParser(CIParser *self,
                         int storage_class_flag,
                         CIDataType *data_type,
                         String *name,
                         bool no_expr,
                         bool is_local)
{
    if (in_label) {
        FAILED("Don't accept variable declaration in label");
    }

    if (self->current_token->kind != CI_TOKEN_KIND_COMMA) {
        next_token__CIParser(self); // skip `=` or `;`
    }

    if (no_expr) {
        return NEW_VARIANT(
          CIDecl,
          variable,
          storage_class_flag,
          false,
          NEW(CIDeclVariable, data_type, name, NULL, is_local));
    }

    ENABLE_ALLOW_INITIALIZATION();

    CIExpr *expr = parse_expr__CIParser(self);

    DISABLE_ALLOW_INITIALIZATION();

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_SEMICOLON:
            next_token__CIParser(self);
            break;
        case CI_TOKEN_KIND_COMMA:
            break;
        default:
            FAILED("expected `;`");
    }

    return NEW_VARIANT(CIDecl,
                       variable,
                       storage_class_flag,
                       false,
                       NEW(CIDeclVariable, data_type, name, expr, is_local));
}

CIDecl *
parse_variable_list__CIParser(CIParser *self,
                              CIDecl *current_var,
                              int storage_class_flag,
                              CIDataType *data_type,
                              bool in_function_body)
{
    // Parse list of variable.
    // first_variable, <var2>, <var3>;
    while (self->current_token->kind == CI_TOKEN_KIND_COMMA &&
           self->previous_token->kind != CI_TOKEN_KIND_SEMICOLON) {
        if (current_var) {
            add_decl_to_scope__CIParser(
              self, &current_var, in_function_body, true);

            // Push to the current body of the function the current
            // variable.
            if (in_function_body) {
                ASSERT(current_body);

                push__Vec(current_body,
                          NEW_VARIANT(CIDeclFunctionItem, decl, current_var));
            }
        }

        next_token__CIParser(self); // skip `,`

        CIDataType *cloned_data_type =
          parse_post_data_type__CIParser(self, clone__CIDataType(data_type));
        String *name = NULL;

        switch (self->current_token->kind) {
            case CI_TOKEN_KIND_IDENTIFIER:
                name = self->current_token->identifier;
                next_token__CIParser(self);

                break;
            default:
                FAILED("expected identifier");
        }

        switch (self->current_token->kind) {
            case CI_TOKEN_KIND_EQ:
                current_var = parse_variable__CIParser(self,
                                                       storage_class_flag,
                                                       cloned_data_type,
                                                       name,
                                                       false,
                                                       in_function_body);

                break;
            case CI_TOKEN_KIND_COMMA:
            case CI_TOKEN_KIND_SEMICOLON:
                current_var = parse_variable__CIParser(self,
                                                       storage_class_flag,
                                                       cloned_data_type,
                                                       name,
                                                       true,
                                                       in_function_body);

                break;
            default:
                FAILED("expected `=`, `*` or `;`");
        }
    }

    return current_var;
}

void
resolve_preprocessor_embed__CIParser(CIParser *self,
                                     CIToken *preprocessor_embed_token)
{
    TODO("resolve #embed preprocessor");
}

void
resolve_preprocessor_error__CIParser(CIParser *self,
                                     CIToken *preprocessor_error_token)
{
    emit__Diagnostic(
      NEW_VARIANT(
        Diagnostic,
        simple_ci_error,
        self->file->scanner.base.source.file,
        &preprocessor_error_token->location,
        NEW_VARIANT(CIError,
                    preprocessor_error,
                    preprocessor_error_token->preprocessor_error->buffer),
        NULL,
        NULL,
        NULL),
      self->count_error);
}

void
resolve_preprocessor_include__CIParser(CIParser *self,
                                       CIToken *preprocessor_include_token)
{
    const Vec *include_dirs = get_include_dirs__CIInclude();

    for (Usize i = 0; i < include_dirs->len; ++i) {
        const String *include_dir = get__Vec(include_dirs, i);
        // include_dir + '/' + preprocessor_include.value
        char *full_include_path =
          format("{S}/{S}",
                 include_dir,
                 preprocessor_include_token->preprocessor_include.value);

        if (exists__File(full_include_path)) {
            CIResultFile *header =
              add_and_run_header__CIResult(self->file->entity.result,
                                           self->file,
                                           full_include_path,
                                           self->file->entity.result->config);

            // NOTE: The next token is the first token in the header, and the
            // token following the last token in the header is the current next
            // token (token after the preprocessor include).
            //
            // Before:
            //
            // ... -> INCLUDE
            //
            // After:
            //
            // ... -> INCLUDE -> HEADER_FIRST -> ... -> HEADER_LAST (EOT (old
            // EOF)) -> INCLUDE
            header->scanner.tokens.last->kind = CI_TOKEN_KIND_EOT;
            header->scanner.tokens.last->eot.ctx = CI_TOKEN_EOT_CONTEXT_INCLUDE;
            header->scanner.tokens.last->next =
              preprocessor_include_token->next;
            preprocessor_include_token->next = header->scanner.tokens.first;

            lily_free(full_include_path);

            goto exit;
        } else {
            continue;
        }
    }

    FAILED("the include file is not found");

exit:
}

bool
resolve_preprocessor_line__CIParser(CIParser *self,
                                    CIToken *preprocessor_line_token)
{
    // NOTE: We cannot resolve the `#line` preprocessor in the case of a
    // transpiler.
    return false;
}

bool
resolve_preprocessor_pragma__CIParser(CIParser *self,
                                      CIToken *preprocessor_pragma_token)
{
    // NOTE: In the case of a `#pragma`, we can only solve `#pragma once`, but
    // all other #pragma cases are not really possible to solve in the case of a
    // transpiler.
    TODO("resolve #pragma preprocessor");
}

void
resolve_preprocessor_undef__CIParser(CIParser *self,
                                     CIToken *preprocessor_undef_token)
{
    undef_define__CIResultFile(self->file,
                               preprocessor_undef_token->preprocessor_undef);
}

void
resolve_preprocessor_warning__CIParser(CIParser *self,
                                       CIToken *preprocessor_warning_token)
{
    emit_warning__Diagnostic(
      NEW_VARIANT(
        Diagnostic,
        simple_ci_warning,
        self->file->scanner.base.source.file,
        &preprocessor_warning_token->location,
        NEW_VARIANT(CIWarning,
                    preprocessor_warning,
                    preprocessor_warning_token->preprocessor_warning->buffer),
        NULL,
        NULL,
        NULL),
      NULL,
      self->count_warning);
}

bool
resolve_preprocessor__CIParser(CIParser *self, CIToken *next_token)
{
    switch (next_token->kind) {
        case CI_TOKEN_KIND_PREPROCESSOR_EMBED:
            resolve_preprocessor_embed__CIParser(self, next_token);

            break;
        case CI_TOKEN_KIND_PREPROCESSOR_ERROR:
            resolve_preprocessor_error__CIParser(self, next_token);

            break;
        case CI_TOKEN_KIND_PREPROCESSOR_INCLUDE:
            resolve_preprocessor_include__CIParser(self, next_token);

            break;
        case CI_TOKEN_KIND_PREPROCESSOR_LINE:
            return resolve_preprocessor_line__CIParser(self, next_token);
        case CI_TOKEN_KIND_PREPROCESSOR_PRAGMA:
            return resolve_preprocessor_pragma__CIParser(self, next_token);
        case CI_TOKEN_KIND_PREPROCESSOR_UNDEF:
            resolve_preprocessor_undef__CIParser(self, next_token);

            break;
        case CI_TOKEN_KIND_PREPROCESSOR_WARNING:
            resolve_preprocessor_warning__CIParser(self, next_token);

            break;
        default:
            return false;
    }

    return true;
}

CIDecl *
parse_decl__CIParser(CIParser *self, bool in_function_body)
{
    storage_class_flag = CI_STORAGE_CLASS_NONE;

    parse_storage_class_specifiers__CIParser(self, &storage_class_flag);

    CIDecl *res = NULL;

    // Parse typedef declaration.
    if (storage_class_flag & CI_STORAGE_CLASS_TYPEDEF) {
        res = parse_typedef__CIParser(self);

        goto exit;
    }

    CIDataType *data_type = parse_data_type__CIParser(self);

    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_IDENTIFIER: {
            String *name = self->current_token->identifier;

            next_token__CIParser(self);

            CIGenericParams *generic_params =
              parse_generic_params__CIParser(self, false); // CIGenericParams*?

            switch (self->current_token->kind) {
                // Parse first variable
                case CI_TOKEN_KIND_EQ:
                    if (generic_params) {
                        goto no_generic_params_expected;
                    }

                    res = parse_variable__CIParser(self,
                                                   storage_class_flag,
                                                   data_type,
                                                   name,
                                                   false,
                                                   in_function_body);

                    break;
                case CI_TOKEN_KIND_COMMA:
                case CI_TOKEN_KIND_SEMICOLON:
                    if (generic_params) {
                    no_generic_params_expected: {
                        FAILED("no generic params are expected in variable "
                               "declaration");
                    }
                    }

                    res = parse_variable__CIParser(self,
                                                   storage_class_flag,
                                                   data_type,
                                                   name,
                                                   true,
                                                   in_function_body);

                    break;
                case CI_TOKEN_KIND_LPAREN:
                    res = parse_function__CIParser(self,
                                                   storage_class_flag,
                                                   data_type,
                                                   name,
                                                   generic_params);

                    break;
                default:
                    if (!in_function_body) {
                        FAILED("expected `=`, `;`, `(`");
                    }

                    data_type_as_expression = data_type;

                    return NULL;
            }

            res = parse_variable_list__CIParser(
              self, res, storage_class_flag, data_type, in_function_body);

            break;
        }
        default:
            expect__CIParser(self, CI_TOKEN_KIND_SEMICOLON, true);

            FREE(CIDataType, data_type);

            return NULL;
    }

exit:
    if (res) {
        add_decl_to_scope__CIParser(self, &res, in_function_body, true);
    }

    return res;
}

const CIDecl *
add_decl_to_scope__CIParser(CIParser *self,
                            CIDecl **decl_ref,
                            bool in_function_body,
                            bool must_free)
{
    CIDecl *decl = *decl_ref;
    CIDecl *res = NULL;

    ASSERT(decl);

    switch (decl->kind) {
        case CI_DECL_KIND_ENUM:
            if ((res = (CIDecl *)add_enum__CIResultFile(self->file, decl))) {
                FAILED("enum is already defined");

                goto free;
            }

            goto exit;
        case CI_DECL_KIND_FUNCTION:
        case CI_DECL_KIND_FUNCTION_GEN:
            if ((res =
                   (CIDecl *)add_function__CIResultFile(self->file, decl))) {
                FAILED("function is already defined");

                goto free;
            }

            goto exit;
        case CI_DECL_KIND_STRUCT:
        case CI_DECL_KIND_STRUCT_GEN:
            if ((res = (CIDecl *)add_struct__CIResultFile(self->file, decl))) {
                FAILED("struct is already defined");

                goto free;
            }

            goto exit;
        case CI_DECL_KIND_TYPEDEF:
        case CI_DECL_KIND_TYPEDEF_GEN:
            if ((res = (CIDecl *)add_typedef__CIResultFile(self->file, decl))) {
                FAILED("typedef is already defined");

                goto free;
            }

            goto exit;
        case CI_DECL_KIND_UNION:
        case CI_DECL_KIND_UNION_GEN:
            if ((res = (CIDecl *)add_union__CIResultFile(self->file, decl))) {
                FAILED("union is already defined");

                goto free;
            }

            goto exit;
        case CI_DECL_KIND_VARIABLE:
            if ((res = (CIDecl *)add_variable__CIResultFile(
                   self->file,
                   current_scope,
                   in_function_body ? ref__CIDecl(decl) : decl))) {
                FAILED("variable is already defined");

                goto free;
            }

            goto exit;
        default:
            UNREACHABLE("impossible situation");
    }

exit:
    return res;

free:
    if (must_free) {
        FREE(CIDecl, decl);
        *decl_ref = NULL;
    }

    return res;
}

bool
parse_storage_class_specifier__CIParser(CIParser *self, int *storage_class_flag)
{
    switch (self->current_token->kind) {
        case CI_TOKEN_KIND_KEYWORD_AUTO:
            *storage_class_flag |= CI_STORAGE_CLASS_AUTO;
            break;
        case CI_TOKEN_KIND_KEYWORD_CONSTEXPR:
            *storage_class_flag |= CI_STORAGE_CLASS_CONSTEXPR;
            break;
        case CI_TOKEN_KIND_KEYWORD_EXTERN:
            *storage_class_flag |= CI_STORAGE_CLASS_EXTERN;
            break;
        case CI_TOKEN_KIND_KEYWORD_INLINE:
            *storage_class_flag |= CI_STORAGE_CLASS_INLINE;
            break;
        case CI_TOKEN_KIND_KEYWORD_REGISTER:
            *storage_class_flag |= CI_STORAGE_CLASS_REGISTER;
            break;
        case CI_TOKEN_KIND_KEYWORD_STATIC:
            *storage_class_flag |= CI_STORAGE_CLASS_STATIC;
            break;
        case CI_TOKEN_KIND_KEYWORD_THREAD_LOCAL:
            *storage_class_flag |= CI_STORAGE_CLASS_THREAD_LOCAL;
            break;
        case CI_TOKEN_KIND_KEYWORD_TYPEDEF:
            *storage_class_flag |= CI_STORAGE_CLASS_TYPEDEF;
            break;
        default:
            return false;
    }

    return true;
}

void
parse_storage_class_specifiers__CIParser(CIParser *self,
                                         int *storage_class_flag)
{
    int old_storage_class_flag = *storage_class_flag;

    while (parse_storage_class_specifier__CIParser(self, storage_class_flag)) {
        next_token__CIParser(self);

        if (old_storage_class_flag == *storage_class_flag) {
            FAILED("warning: duplicate storage class specifier");
        }

        old_storage_class_flag = *storage_class_flag;
    }
}

void
run__CIParser(CIParser *self)
{
    // Set the current scope.
    set_current_scope__CIParser(self);
    // Initialize the parser.
    init__CIParser(self);

    while (self->current_token->kind != CI_TOKEN_KIND_EOF) {
        parse_decl__CIParser(self, false);
    }

    resolve_visit_waiting_list__CIParser(self);

#ifdef ENV_DEBUG
    // TODO: Print debug
#endif
}

void
free_from_tokens_case__CIParser(const CIParser *self)
{
    // FREE_STACK_ITEMS(self->macros_call, CIParserMacroCall);
    FREE(Stack, self->macros_call);
    FREE(CIParserVisitWaitingList, &self->visit_waiting_list);
}

DESTRUCTOR(CIParser, const CIParser *self)
{
    if (names_error) {
        FREE_BUFFER_ITEMS(names_error->buffer, names_error->len, String);
        FREE(Vec, names_error);

        names_error = NULL;
    }

    free_from_tokens_case__CIParser(self);
}
