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

#ifndef LILY_CORE_LILY_COMPILER_PACKAGE_PACKAGE_H
#define LILY_CORE_LILY_COMPILER_PACKAGE_PACKAGE_H

#include <base/string.h>
#include <base/types.h>
#include <base/vec.h>

#include <cli/lilyc/config.h>

#include <core/lily/analysis/analysis.h>
#include <core/lily/analysis/checked/operator.h>
#include <core/lily/analysis/checked/operator_register.h>
#include <core/lily/compiler/ir.h>
#include <core/lily/compiler/linker/linker.h>
#include <core/lily/functions/builtin.h>
#include <core/lily/functions/sys.h>
#include <core/lily/mir/mir.h>
#include <core/lily/package/config.h>
#include <core/lily/package/library.h>
#include <core/lily/package/program.h>
#include <core/lily/parser/parser.h>
#include <core/lily/precompiler/precompiler.h>
#include <core/lily/preparser/preparser.h>
#include <core/lily/scanner/scanner.h>
#include <core/lily/shared/visibility.h>

typedef struct LilyPackage LilyPackage;
enum LilyPackageStatus;

#define LOG_VERBOSE(package, msg)                                  \
    if (package->config->verbose) {                                \
        printf("+ %s package %s\n",                                \
               package->global_name ? package->global_name->buffer \
                                    : "(not defined)",             \
               msg);                                               \
    }

#define SET_ROOT_PACKAGE_NAME(root_package)                              \
    if (root_package->preparser_info.package->name) {                    \
        root_package->name = root_package->preparser_info.package->name; \
        root_package->global_name = clone__String(root_package->name);   \
    } else {                                                             \
        root_package->name = from__String("main");                       \
        root_package->global_name = from__String("main");                \
    }

#define SET_ROOT_PACKAGE_IR(config, root_package)                             \
    if (config->cc_ir) {                                                      \
        /* TODO: add a linker for CC */                                       \
        root_package->compiler.ir = NEW_VARIANT(LilyIr, cc, NEW(LilyIrCc));   \
        root_package->compiler.linker = LILY_LINKER_KIND_CC;                  \
    } else if (config->cpp_ir) {                                              \
        /* TODO: add a linker for CPP */                                      \
        root_package->compiler.ir = NEW_VARIANT(LilyIr, cpp, NEW(LilyIrCpp)); \
        root_package->compiler.linker = LILY_LINKER_KIND_CPP;                 \
    } else if (config->js_ir) {                                               \
        self->compiler.ir = NEW_VARIANT(LilyIr, js, NEW(LilyIrJs));           \
    } else {                                                                  \
        root_package->compiler.ir = NEW_VARIANT(                              \
          LilyIr, llvm, NEW(LilyIrLlvm, root_package->global_name->buffer));  \
        root_package->compiler.linker = LILY_LINKER_KIND_LLVM;                \
    }

#define SET_ROOT_PACKAGE_PROGRAM(root_package, p, lib)                \
    /* Set the kind of program (static lib, dynamic lib, exe, ...) */ \
    root_package->program = p;                                        \
                                                                      \
    switch (root_package->program->kind) {                            \
        case LILY_PROGRAM_KIND_EXE:                                   \
            root_package->is_exe = true;                              \
                                                                      \
            break;                                                    \
        case LILY_PROGRAM_KIND_STATIC_LIB:                            \
            ASSERT(lib);                                              \
                                                                      \
            finish_set__LilyLibrary(                                  \
              lib,                                                    \
              root_package->name,                                     \
              (enum LilyArKind)root_package->compiler.linker);        \
                                                                      \
            root_package->is_lib = true;                              \
            root_package->is_static_lib = true;                       \
            lib->package = root_package;                              \
            root_package->compiler.lib = lib;                         \
                                                                      \
            break;                                                    \
        case LILY_PROGRAM_KIND_DYNAMIC_LIB:                           \
            ASSERT(lib);                                              \
                                                                      \
            finish_set__LilyLibrary(                                  \
              lib,                                                    \
              root_package->name,                                     \
              (enum LilyArKind)root_package->compiler.linker);        \
                                                                      \
            root_package->is_lib = true;                              \
            root_package->is_dynamic_lib = true;                      \
            lib->package = root_package;                              \
            root_package->compiler.lib = lib;                         \
                                                                      \
            break;                                                    \
        default:                                                      \
            UNREACHABLE("unknown variant");                           \
    }

#define LOAD_ROOT_PACKAGE_RESOURCES(root_package, p)                          \
    /* Load builtins and syss */                                              \
    root_package->builtins = p->resources.builtins;                           \
    root_package->syss = p->resources.syss;                                   \
                                                                              \
    /* Load default operators in operator register                            \
    In a normal case where we wanted to add an operator to the register, we'd \
    use `add_operator__LilyCheckedOperatorRegister`, but in this case it's    \
    guaranteed that no operator repeats itself, so to increase loading speed  \
    we'll add it directly to the vector without checking. */                  \
    for (Usize i = 0; i < DEFAULT_OPERATORS_COUNT; ++i) {                     \
        push__Vec(root_package->operator_register.operators,                  \
                  ref__LilyCheckedOperator(                                   \
                    root_package->program->resources.default_operators[i]));  \
    }

#define SET_ROOT_PACKAGE_USE_SWITCH(root_package)                       \
    /* Set `use_switch` on true for CC IR, CPP IR, JS IR or LLVM IR. */ \
    if (root_package->config->cc_ir || root_package->config->cpp_ir ||  \
        root_package->config->js_ir || root_package->config->llvm_ir) { \
        root_package->analysis.use_switch = true;                       \
    }

typedef struct LilyCompilerAdapter
{
    char *output_path;
    LilyIr ir;
    enum LilyLinkerKind linker;
    LilyLibrary *lib; // LilyLibrary*? (&)
} LilyCompilerAdapter;

/**
 *
 * @brief Construct LilyCompilerAdapter type.
 */
inline CONSTRUCTOR(LilyCompilerAdapter, LilyCompilerAdapter)
{
    return (LilyCompilerAdapter){
        .output_path = NULL,
        .lib = NULL,
    };
}

/**
 *
 * @brief Free LilyCompilerAdapter type.
 */
DESTRUCTOR(LilyCompilerAdapter, const LilyCompilerAdapter *self);

/**
 *
 * @brief Build all packages.
 * @param lib LilyLibrary*?
 * @return LilyPackage*?
 */
LilyPackage *
build__LilyPackage(const LilycConfig *config,
                   enum LilyVisibility visibility,
                   enum LilyPackageStatus status,
                   const char *default_path,
                   const LilyProgram *program,
                   LilyLibrary *lib);

/**
 *
 * @brief Build all packages of the library.
 * @return LilyLibrary*?
 */
LilyLibrary *
build_lib__LilyPackage(const LilycConfig *config,
                       enum LilyVisibility visibility,
                       enum LilyPackageStatus status,
                       const char *default_path,
                       const LilyProgram *program,
                       String *version,
                       String *url,
                       String *path);

/**
 *
 * @brief Compile package.
 * @note Begin by the main package.
 * @return LilyPackage*?
 */
LilyPackage *
compile__LilyPackage(const LilycConfig *config,
                     enum LilyVisibility visibility,
                     enum LilyPackageStatus status,
                     const char *default_path,
                     const LilyProgram *program);

/**
 *
 * @brief Compile library.
 * @note Begin by the main package.
 * @return LilyLibrary*?
 */
LilyLibrary *
compile_lib__LilyPackage(const LilycConfig *config,
                         enum LilyVisibility visibility,
                         enum LilyPackageStatus status,
                         const char *default_path,
                         const LilyProgram *program);

/**
 *
 * @brief Run until the scanner.
 */
void
run_scanner__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the preparser.
 */
void
run_preparser__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the precompiler.
 */
void
run_precompiler__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the parser.
 */
void
run_parser__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the analysis.
 */
void
run_analysis__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the MIR.
 */
void
run_mir__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Run until the IR.
 */
void
run_ir__LilyPackage(const LilycConfig *config);

/**
 *
 * @brief Free LilyPackage type.
 */
DESTRUCTOR(LilyPackage, LilyPackage *self);

#endif // LILY_CORE_LILY_COMPILER_PACKAGE_PACKAGE_H
