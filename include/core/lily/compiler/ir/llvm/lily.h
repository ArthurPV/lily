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

#ifndef LILY_CORE_LILY_COMPILER_IR_LLVM_LILY_H
#define LILY_CORE_LILY_COMPILER_IR_LLVM_LILY_H

#include <core/lily/compiler/ir/llvm.h>
#include <core/lily/compiler/ir/llvm/pending.h>
#include <core/lily/compiler/ir/llvm/scope.h>

#include <core/lily/mir/instruction.h>

#include <llvm-c/Core.h>

LLVMValueRef
LilyLLVMGetNamedFunction(const LilyIrLlvm *Self,
                         const LilyIrLlvmPending *Pending,
                         const char *Name);

LLVMValueRef
LilyLLVMBuildAlloc(const LilyIrLlvm *Self,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstruction *Inst,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildAnd(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildBitCast(const LilyIrLlvm *Self,
                     const LilyIrLlvmScope *Scope,
                     const LilyIrLlvmPending *Pending,
                     const LilyMirInstructionVal *Val,
                     const LilyMirDt *DestDt,
                     const char *Name);

LLVMValueRef
LilyLLVMBuildNot(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *RHS,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildOr(const LilyIrLlvm *Self,
                const LilyIrLlvmScope *Scope,
                const LilyIrLlvmPending *Pending,
                const LilyMirInstructionVal *LHS,
                const LilyMirInstructionVal *RHS,
                const char *Name);

LLVMBasicBlockRef
LilyLLVMBuildBlock(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const Vec *Insts,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildBuiltinCall(const LilyIrLlvm *Self,
                         const LilyIrLlvmScope *Scope,
                         const LilyIrLlvmPending *Pending,
                         const Vec *Params,
                         const LilyMirDt *DT,
                         const char *BuiltinName,
                         const char *Name);

LLVMValueRef
LilyLLVMBuildCall(const LilyIrLlvm *Self,
                  const LilyIrLlvmScope *Scope,
                  const LilyIrLlvmPending *Pending,
                  const Vec *Params,
                  const LilyMirDt *DT,
                  const char *FnName,
                  const char *Name);

LLVMValueRef
LilyLLVMBuildAdd(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildCmpEq(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildCmpNe(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildCmpLe(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   bool IsSigned,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildCmpLt(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   bool IsSigned,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildCmpGe(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   bool IsSigned,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildCmpGt(const LilyIrLlvm *Self,
                   const LilyIrLlvmScope *Scope,
                   const LilyIrLlvmPending *Pending,
                   const LilyMirInstructionVal *LHS,
                   const LilyMirInstructionVal *RHS,
                   bool IsFloat,
                   bool IsSigned,
                   const char *Name);

LLVMValueRef
LilyLLVMBuildDiv(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 bool IsSigned,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildMul(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildNeg(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildRem(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 bool IsSigned,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildSub(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *LHS,
                 const LilyMirInstructionVal *RHS,
                 bool IsFloat,
                 const char *Name);

LLVMValueRef
LilyLLVMBuildGetArray(const LilyIrLlvm *Self,
                      const LilyIrLlvmScope *Scope,
                      const LilyIrLlvmPending *Pending,
                      const LilyMirInstructionVal *Val,
                      const LilyMirDt *DT,
                      const Vec *Indexes,
                      bool IsConst,
                      const char *Name);

LLVMValueRef
LilyLLVMBuildGetField(const LilyIrLlvm *Self,
                      const LilyIrLlvmScope *Scope,
                      const LilyIrLlvmPending *Pending,
                      const LilyMirInstructionVal *Val,
                      const LilyMirDt *DT,
                      const Vec *Indexes,
                      const char *Name);

LLVMValueRef
LilyLLVMBuildGetPtr(const LilyIrLlvm *Self,
                    const LilyIrLlvmScope *Scope,
                    const LilyIrLlvmPending *Pending,
                    const LilyMirInstructionVal *Val,
                    const LilyMirDt *DT,
                    const char *Name);

LLVMValueRef
LilyLLVMBuildVal(const LilyIrLlvm *Self,
                 const LilyIrLlvmScope *Scope,
                 const LilyIrLlvmPending *Pending,
                 const LilyMirInstructionVal *Val);

LLVMValueRef
LilyLLVMBuildInst(const LilyIrLlvm *Self,
                  const LilyIrLlvmScope *Scope,
                  const LilyIrLlvmPending *Pending,
                  const LilyMirInstruction *Inst,
                  const char *Name);

LLVMTypeRef
LilyLLVMGetType(const LilyIrLlvm *Self,
                const LilyIrLlvmPending *Pending,
                const LilyMirDt *DT);

#endif // LILY_CORE_LILY_COMPILER_IR_LLVM_LILY_H
