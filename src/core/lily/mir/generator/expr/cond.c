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

#include <core/lily/mir/generator/expr.h>
#include <core/lily/mir/generator/expr/cond.h>

#define GENERATE_ASSIGN0_BLOCK()                                              \
    /* Generate assign to 0 block */                                          \
    /* assign0: */                                                            \
    LilyMirAddBlock(module, assign0_block);                                   \
    LilyMirSetBlockLimit(assign0_block->block.limit,                          \
                         assign0_block->block.limit->id);                     \
                                                                              \
    /* store val(i1) %.0, val(i1) 0 */                                        \
    LilyMirAddInst(                                                           \
      module,                                                                 \
      NEW_VARIANT(LilyMirInstruction,                                         \
                  store,                                                      \
                  NEW(LilyMirInstructionDestSrc,                              \
                      current_virtual_variable,                               \
                      NEW_VARIANT(LilyMirInstructionVal,                      \
                                  int,                                        \
                                  NEW(LilyMirDt, LILY_MIR_DT_KIND_I1),        \
                                  0))));                                      \
                                                                              \
    /* jmp exit_block */                                                      \
    LilyMirAddInst(module,                                                    \
                   NEW_VARIANT(LilyMirInstruction, jmp, &exit_block->block)); \
                                                                              \
    LilyMirPopBlock(module);

#define GENERATE_ASSIGN1_BLOCK()                                              \
    /* Generate assign to 1 block */                                          \
    /* assign1: */                                                            \
    LilyMirAddBlock(module, assign1_block);                                   \
    LilyMirSetBlockLimit(assign1_block->block.limit,                          \
                         assign1_block->block.limit->id);                     \
                                                                              \
    /* store val(i1) %.0, val(i1) 1 */                                        \
    LilyMirAddInst(                                                           \
      module,                                                                 \
      NEW_VARIANT(LilyMirInstruction,                                         \
                  store,                                                      \
                  NEW(LilyMirInstructionDestSrc,                              \
                      current_virtual_variable,                               \
                      NEW_VARIANT(LilyMirInstructionVal,                      \
                                  int,                                        \
                                  NEW(LilyMirDt, LILY_MIR_DT_KIND_I1),        \
                                  1))));                                      \
                                                                              \
    /* jmp exit_block */                                                      \
    LilyMirAddInst(module,                                                    \
                   NEW_VARIANT(LilyMirInstruction, jmp, &exit_block->block)); \
                                                                              \
    LilyMirPopBlock(module);

LilyMirInstruction *
generate_cond__LilyMir(LilyMirModule *module,
                       LilyCheckedSignatureFun *fun_signature,
                       LilyMirScope *scope,
                       LilyCheckedExpr *expr,
                       LilyMirInstructionVal *virtual_variable,
                       LilyMirInstruction *exit_block)
{
    // NOTE: maybe add optional type as agreate condition data type, ...
    ASSERT(expr->data_type->kind == LILY_CHECKED_DATA_TYPE_KIND_BOOL);

    switch (expr->kind) {
        case LILY_CHECKED_EXPR_KIND_BINARY:
            switch (expr->binary.kind) {
                case LILY_CHECKED_EXPR_BINARY_KIND_AND: {
                    // => cond and cond
                    //
                    // ; ...
                    // var .0 = alloc i1
                    // jmp first_cond
                    // first_cond:
                    //   %r.0 = <cond>
                    //   jmpcond val(i1) %r.0, second_cond, assign0
                    // second_cond:
                    //   %r.1 = <cond>
                    //   jmpcond val(i1) %r.1, assign1, assign0
                    // assign0:
                    //   store val(i1) %.0, val(i1) 0
                    //   jmp exit_block
                    // assign1:
                    //   store val(i1) %.0, val(i1) 1
                    //   jmp exit_block
                    // exit_block:
                    //   ; ...

                    // NOTE: make a partiacular attention to
                    // `current_virtual_variable`
                    // TODO: delete `jmp first_cond` and `first_cond` block (to
                    // improve performance)
                    LilyMirInstructionVal *current_virtual_variable =
                      virtual_variable
                        ? virtual_variable
                        : LilyMirBuildVirtualVariable(
                            module, NEW(LilyMirDt, LILY_MIR_DT_KIND_I1));
                    LilyMirInstruction *assign0_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *assign1_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *first_cond_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *second_cond_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));

                    // jmp first_cond
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmp,
                                               &first_cond_block->block));

                    LilyMirPopBlock(module);

                    GENERATE_ASSIGN0_BLOCK();
                    GENERATE_ASSIGN1_BLOCK();

                    // Generate first condition block
                    // first_cond:
                    LilyMirAddBlock(module, first_cond_block);

                    // %r.0 = <cond>
                    LilyMirInstruction *first_cond =
                      generate_cond__LilyMir(module,
                                             fun_signature,
                                             scope,
                                             expr->binary.left,
                                             current_virtual_variable,
                                             assign0_block);

                    ASSERT(first_cond);
                    ASSERT(first_cond->kind == LILY_MIR_INSTRUCTION_KIND_VAL);
                    ASSERT(!LilyMirHasFinalInstruction(module));

                    // jmpcond val(i1) %r.1, second_cond, assign0
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmpcond,
                                               NEW(LilyMirInstructionJmpCond,
                                                   first_cond->val,
                                                   &second_cond_block->block,
                                                   &assign0_block->block)));
                    LilyMirSetBlockLimit(first_cond_block->block.limit,
                                         LilyMirGetInsertBlock(module)->id);
                    LilyMirPopBlock(module);

                    // Generate second condition block
                    // second_cond:
                    LilyMirAddBlock(module, second_cond_block);

                    // %r.1 = <cond>
                    LilyMirInstruction *second_cond =
                      generate_cond__LilyMir(module,
                                             fun_signature,
                                             scope,
                                             expr->binary.right,
                                             current_virtual_variable,
                                             assign1_block);

                    ASSERT(second_cond);
                    ASSERT(second_cond->kind == LILY_MIR_INSTRUCTION_KIND_VAL);
                    ASSERT(!LilyMirHasFinalInstruction(module));

                    // jmpcond val(i1) %r.1, assign1, assign0
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmpcond,
                                               NEW(LilyMirInstructionJmpCond,
                                                   second_cond->val,
                                                   &assign1_block->block,
                                                   &assign0_block->block)));

                    LilyMirSetBlockLimit(second_cond_block->block.limit,
                                         LilyMirGetInsertBlock(module)->id);
                    LilyMirPopBlock(module);

                    LilyMirAddBlock(module, exit_block);

                    lily_free(first_cond);
                    lily_free(second_cond);

                    return NEW_VARIANT(
                      LilyMirInstruction, val, current_virtual_variable);
                }
                case LILY_CHECKED_EXPR_BINARY_KIND_OR: {
                    // => cond or cond
                    //
                    // ; ...
                    // var .0 = alloc i1
                    // jmp first_cond
                    // first_cond:
                    //   %r.0 = <cond>
                    //   jmpcond val(i1) %r.0, assign1, second_cond
                    // second_cond:
                    //   %r.1 = <cond>
                    //   jmpcond val(i1) %r.1, assign1, assign0
                    // assign0:
                    //   store val(i1) %.0, val(i1) 0
                    //   jmp exit_block
                    // assign1:
                    //   store val(i1) %.0, val(i1) 1
                    //   jmp exit_block
                    // exit_block:
                    //   ; ...

                    // NOTE: make a partiacular attention to
                    // `current_virtual_variable`
                    // TODO: delete `jmp first_cond` and `first_cond` block (to
                    // improve performance)
                    LilyMirInstructionVal *current_virtual_variable =
                      virtual_variable
                        ? virtual_variable
                        : LilyMirBuildVirtualVariable(
                            module, NEW(LilyMirDt, LILY_MIR_DT_KIND_I1));
                    LilyMirInstruction *assign0_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *assign1_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *first_cond_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));
                    LilyMirInstruction *second_cond_block =
                      LilyMirBuildBlock(module, NEW(LilyMirBlockLimit));

                    // jmp first_cond
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmp,
                                               &first_cond_block->block));

                    LilyMirPopBlock(module);

                    GENERATE_ASSIGN0_BLOCK();
                    GENERATE_ASSIGN1_BLOCK();

                    // Generate first condition block
                    // first_cond:
                    LilyMirAddBlock(module, first_cond_block);

                    // %r.0 = <cond>
                    LilyMirInstruction *first_cond =
                      generate_cond__LilyMir(module,
                                             fun_signature,
                                             scope,
                                             expr->binary.left,
                                             current_virtual_variable,
                                             assign0_block);

                    ASSERT(first_cond->kind == LILY_MIR_INSTRUCTION_KIND_VAL);
                    ASSERT(!LilyMirHasFinalInstruction(module));

                    // jmpcond val(i1) %r.1, second_cond, assign0
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmpcond,
                                               NEW(LilyMirInstructionJmpCond,
                                                   first_cond->val,
                                                   &assign1_block->block,
                                                   &second_cond_block->block)));
                    LilyMirSetBlockLimit(first_cond_block->block.limit,
                                         LilyMirGetInsertBlock(module)->id);
                    LilyMirPopBlock(module);

                    // Generate second condition block
                    // second_cond:
                    LilyMirAddBlock(module, second_cond_block);

                    // %r.1 = <cond>
                    LilyMirInstruction *second_cond =
                      generate_cond__LilyMir(module,
                                             fun_signature,
                                             scope,
                                             expr->binary.right,
                                             current_virtual_variable,
                                             assign1_block);

                    ASSERT(second_cond->kind == LILY_MIR_INSTRUCTION_KIND_VAL);
                    ASSERT(!LilyMirHasFinalInstruction(module));

                    // jmpcond val(i1) %r.1, assign1, assign0
                    LilyMirAddInst(module,
                                   NEW_VARIANT(LilyMirInstruction,
                                               jmpcond,
                                               NEW(LilyMirInstructionJmpCond,
                                                   second_cond->val,
                                                   &assign1_block->block,
                                                   &assign0_block->block)));

                    LilyMirSetBlockLimit(second_cond_block->block.limit,
                                         LilyMirGetInsertBlock(module)->id);
                    LilyMirPopBlock(module);

                    LilyMirAddBlock(module, exit_block);

                    lily_free(first_cond);
                    lily_free(second_cond);

                    return NEW_VARIANT(
                      LilyMirInstruction, val, current_virtual_variable);
                }
                default:
                    return generate_expr__LilyMir(
                      module, fun_signature, scope, expr, false);
            }
        default:
            return generate_expr__LilyMir(
              module, fun_signature, scope, expr, false);
    }
}
