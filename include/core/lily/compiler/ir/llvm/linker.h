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

#ifndef LILY_CORE_LILY_COMPILER_IR_LLVM_LINKER_H
#define LILY_CORE_LILY_COMPILER_IR_LLVM_LINKER_H

#include <base/new.h>
#include <base/vec.h>

#include <core/lily/compiler/ir/llvm.h>

typedef struct LilyPackage LilyPackage;

typedef struct LilyIrLlvmLinker
{
    const LilyIrLlvm *llvm;
    Vec *args; // Vec<char*>*
} LilyIrLlvmLinker;

inline CONSTRUCTOR(LilyIrLlvmLinker, LilyIrLlvmLinker, const LilyIrLlvm *llvm)
{
    return (LilyIrLlvmLinker){ .llvm = llvm, .args = NEW(Vec) };
}

/**
 *
 * @brief Run the linker to compile the object files (in executable).
 */
void
compile_exe__LilyIrLlvmLinker(LilyPackage *self);

/**
 *
 * @brief Run the linker to compile the object files (in library).
 */
void
compile_lib__LilyIrLlvmLinker(LilyPackage *self);

/**
 *
 * @brief Free LilyIrLlvmLinker type.
 */
DESTRUCTOR(LilyIrLlvmLinker, const LilyIrLlvmLinker *self);

#endif // LILY_CORE_LILY_COMPILER_IR_LLVM_LINKER_H
