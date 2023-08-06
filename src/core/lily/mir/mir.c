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

#include <base/format.h>
#include <base/string.h>

#include <core/lily/analysis/checked/body/fun.h>
#include <core/lily/mir/generator/body.h>
#include <core/lily/mir/generator/expr.h>
#include <core/lily/mir/generator/stmt.h>
#include <core/lily/mir/mir.h>

#include <string.h>

DESTRUCTOR(LilyMirNameManager, const LilyMirNameManager *self)
{
    for (Usize i = 0; i < self->names->len; ++i) {
        lily_free(get__Vec(self->names, i));
    }

    FREE(Vec, self->names);
}

DESTRUCTOR(LilyMirCurrentFun, const LilyMirCurrentFun *self)
{
    FREE(LilyMirNameManager, &self->block_manager);
    FREE(LilyMirNameManager, &self->reg_manager);
}

void
LilyMirAddInst(LilyMirModule *Module, LilyMirInstruction *Inst)
{
    switch (Inst->kind) {
        case LILY_MIR_INSTRUCTION_KIND_CONST:
            push__Stack(Module->current,
                        NEW_VARIANT(LilyMirCurrent, const, Inst));
            break;
        case LILY_MIR_INSTRUCTION_KIND_FUN:
            push__Stack(
              Module->current,
              NEW_VARIANT(LilyMirCurrent, fun, NEW(LilyMirCurrentFun, Inst)));
            break;
        case LILY_MIR_INSTRUCTION_KIND_STRUCT:
            push__Stack(Module->current,
                        NEW_VARIANT(LilyMirCurrent, struct, Inst));
            break;
        default: {
            LilyMirCurrent *current = Module->current->top;

            if (!current) {
                ASSERT("cannot push this instrution in global");
            }

            switch (current->kind) {
                case LILY_MIR_CURRENT_KIND_FUN: {
                    LilyMirInstructionBlock *block =
                      current->fun.fun->fun.block_stack->top;

                    ASSERT(block);

                    push__Vec(block->insts, Inst);

                    break;
                }
                default:
                    ASSERT("cannot push this instruction in this instruction");
            }
        }
    }
}

VARIANT_CONSTRUCTOR(LilyMirCurrent *,
                    LilyMirCurrent,
                    const,
                    LilyMirInstruction *const_)
{
    LilyMirCurrent *self = lily_malloc(sizeof(LilyMirCurrent));

    self->kind = LILY_MIR_CURRENT_KIND_CONST;
    self->const_ = const_;

    return self;
}

VARIANT_CONSTRUCTOR(LilyMirCurrent *,
                    LilyMirCurrent,
                    fun,
                    LilyMirCurrentFun fun)
{
    LilyMirCurrent *self = lily_malloc(sizeof(LilyMirCurrent));

    self->kind = LILY_MIR_CURRENT_KIND_FUN;
    self->fun = fun;

    return self;
}

VARIANT_CONSTRUCTOR(LilyMirCurrent *,
                    LilyMirCurrent,
                    struct,
                    LilyMirInstruction *struct_)
{
    LilyMirCurrent *self = lily_malloc(sizeof(LilyMirCurrent));

    self->kind = LILY_MIR_CURRENT_KIND_STRUCT;
    self->struct_ = struct_;

    return self;
}

DESTRUCTOR(LilyMirCurrent, LilyMirCurrent *self)
{
    switch (self->kind) {
        case LILY_MIR_CURRENT_KIND_FUN:
            FREE(LilyMirCurrentFun, &self->fun);
            lily_free(self);
            break;
        default:
            lily_free(self);
    }
}

void
LilyMirPopCurrent(LilyMirModule *Module)
{
    if (Module->current->len == 0) {
        return;
    }

    LilyMirCurrent *current = pop__Stack(Module->current);

    switch (current->kind) {
        case LILY_MIR_CURRENT_KIND_CONST:
            insert__OrderedHashMap(Module->insts,
                                   (char *)current->const_->const_.name,
                                   current->const_);
            break;
        case LILY_MIR_CURRENT_KIND_FUN:
            insert__OrderedHashMap(Module->insts,
                                   (char *)current->fun.fun->fun.name,
                                   current->fun.fun);
            break;
        case LILY_MIR_CURRENT_KIND_STRUCT:
            insert__OrderedHashMap(Module->insts,
                                   (char *)current->struct_->struct_.name,
                                   current->struct_);
            break;
        default:
            UNREACHABLE("unknown variant");
    }

    FREE(LilyMirCurrent, current);
}

LilyMirScopeVar *
LilyMirScopeGetVar(const LilyMirScope *Scope, String *name)
{
    for (Usize i = 0; i < Scope->vars->len; ++i) {
        LilyMirScopeVar *var = get__Vec(Scope->vars, i);

        if (!strcmp(var->name->buffer, name->buffer)) {
            return var;
        }
    }

    if (Scope->parent) {
        return LilyMirScopeGetVar(Scope->parent, name);
    }

    UNREACHABLE("the analysis has a bug!!");
}

LilyMirScopeParam *
LilyMirScopeGetParam(const LilyMirModule *Module, Usize id)
{
    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    return get__Vec(current->fun.fun->fun.scope.params, id);
}

LilyCheckedDataType *
LilyMirGetCheckedDtFromExpr(const LilyMirModule *Module,
                            const LilyMirScope *Scope,
                            const LilyCheckedExpr *Expr)
{
    switch (Expr->kind) {
        case LILY_CHECKED_EXPR_KIND_CALL:
            switch (Expr->call.kind) {
                case LILY_CHECKED_EXPR_CALL_KIND_FUN_PARAM:
                    return LilyMirScopeGetParam(Module, Expr->call.fun_param)
                      ->data_type;
                case LILY_CHECKED_EXPR_CALL_KIND_VARIABLE:
                    return LilyMirScopeGetVar(Scope, Expr->call.global_name)
                      ->data_type;
                default:
                    return Expr->data_type;
            }
        default:
            return Expr->data_type;
    }
}

LilyMirInstruction *
LilyMirBuildReg(LilyMirModule *Module, LilyMirInstruction *Inst)
{
    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    char *name = LilyMirGenerateName(&current->fun.reg_manager);

    return NEW_VARIANT(LilyMirInstruction,
                       reg,
                       NEW(LilyMirInstructionReg, from__String(name), Inst));
}

LilyMirInstructionVal *
LilyMirBuildLoad(LilyMirModule *Module,
                 LilyMirInstructionVal *src,
                 LilyMirDt *dt,
                 String *value_name)
{
    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    LilyMirInstructionFunLoad *matched_load = NULL;

    for (Usize i = 0; i < current->fun.fun->fun.scope.loads->len; ++i) {
        LilyMirInstructionFunLoad *load =
          get__Vec(current->fun.fun->fun.scope.loads, i);

        if (!strcmp(load->value_name->buffer, value_name->buffer)) {
            matched_load = load;
        }
    }

    if (matched_load) {
        // assume: %<name> = <val_inst>
        FREE(LilyMirInstructionVal, src);
        FREE(LilyMirDt, dt);

        return NEW_VARIANT(
          LilyMirInstructionVal,
          reg,
          clone__LilyMirDt(matched_load->inst->reg.inst->load.dt),
          clone__String(matched_load->inst->reg.name));
    }

    char *name = LilyMirGenerateName(&current->fun.reg_manager);

    LilyMirInstruction *load_inst =
      NEW_VARIANT(LilyMirInstruction,
                  reg,
                  NEW(LilyMirInstructionReg,
                      from__String(name),
                      NEW_VARIANT(LilyMirInstruction,
                                  load,
                                  NEW(LilyMirInstructionLoad,
                                      NEW(LilyMirInstructionSrc, src),
                                      clone__LilyMirDt(dt)))));

    push__Vec(current->fun.fun->fun.scope.loads,
              NEW(LilyMirInstructionFunLoad,
                  value_name,
                  load_inst,
                  LilyMirGetBlockId(Module)));

    LilyMirAddInst(Module, load_inst);

    return NEW_VARIANT(LilyMirInstructionVal, reg, dt, from__String(name));
}

LilyMirInstruction *
LilyMirBuildVar(LilyMirModule *Module,
                char *name,
                LilyMirDt *dt,
                LilyMirInstruction *inst)
{
    LilyMirAddInst(
      Module,
      NEW_VARIANT(
        LilyMirInstruction,
        var,
        NEW(LilyMirInstructionVar, name, LilyMirBuildAlloc(Module, dt))));

    LilyMirInstruction *var = LilyMirBuildStore(
      NEW_VARIANT(LilyMirInstructionVal, var, clone__LilyMirDt(dt), name),
      inst->val);

    lily_free(inst);

    return var;
}

LilyMirInstruction *
LilyMirBuildCall(LilyMirModule *Module,
                 LilyMirDt *ReturnDt,
                 const char *Name,
                 Vec *Params)
{
    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    if (ReturnDt->kind == LILY_MIR_DT_KIND_UNIT) {
        return NEW_VARIANT(
          LilyMirInstruction,
          call,
          NEW(
            LilyMirInstructionCall, clone__LilyMirDt(ReturnDt), Name, Params));
    }

    char *name = LilyMirGenerateName(&current->fun.reg_manager);

    LilyMirInstruction *inst =
      NEW_VARIANT(LilyMirInstruction,
                  reg,
                  NEW(LilyMirInstructionReg,
                      from__String(name),
                      NEW_VARIANT(LilyMirInstruction,
                                  call,
                                  NEW(LilyMirInstructionCall,
                                      clone__LilyMirDt(ReturnDt),
                                      Name,
                                      Params))));

    LilyMirAddInst(Module, inst);

    return NEW_VARIANT(
      LilyMirInstruction,
      val,
      NEW_VARIANT(LilyMirInstructionVal, reg, ReturnDt, from__String(name)));
}

LilyMirDebugInfo *
LilyMirBuildDIFile(LilyMirModule *Module, String *filename, String *directory)
{
    LilyMirDebugInfo *file =
      NEW_VARIANT(LilyMirDebugInfo,
                  file,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoFile, filename, directory));

    for (Usize i = 0; i < Module->files->len; ++i) {
        LilyMirDebugInfo *item = get__Vec(Module->files, i);

        if (eq__LilyMirDebugInfo(item, file)) {
            FREE(LilyMirDebugInfo, file);

            return item;
        }
    }

    ++Module->debug_info_manager.count;

    push__Vec(Module->files, file);

    return file;
}

LilyMirDebugInfo *
LilyMirBuildDIBlock(LilyMirModule *Module,
                    const LilyMirDebugInfo *Scope,
                    const LilyMirDebugInfoFile *File,
                    Usize Line,
                    Usize Column)
{
    LilyMirDebugInfo *block_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  block,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoBlock, Scope, File, Line, Column));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        block_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDILocation(LilyMirModule *Module,
                       const LilyMirDebugInfo *Scope,
                       Usize Line,
                       Usize Column)
{
    LilyMirDebugInfo *location_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  location,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoLocation, Scope, Line, Column));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        location_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDISubProgram(LilyMirModule *Module,
                         const LilyMirDebugInfo *Scope,
                         const LilyMirDebugInfoFile *File,
                         Usize Line,
                         Usize Column)
{
    LilyMirDebugInfo *sub_program_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  sub_program,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoSubProgram, Scope, File, Line, Column));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        sub_program_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIEnumerator(LilyMirModule *Module,
                         const char *name,
                         Usize value,
                         bool is_unsigned)
{
    LilyMirDebugInfo *enumerator_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  enumerator,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoEnumerator, name, value, is_unsigned));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        enumerator_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIGlobalVariable(LilyMirModule *Module,
                             const LilyMirDebugInfo *Scope,
                             const LilyMirDebugInfoFile *File,
                             const char *Name,
                             const char *LinkageName,
                             bool IsLocal,
                             bool IsDefinition)
{
    LilyMirDebugInfo *global_variable_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  global_variable,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoGlobalVariable,
                      Scope,
                      File,
                      Name,
                      LinkageName,
                      IsLocal,
                      IsDefinition));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        global_variable_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDILocalVariable(LilyMirModule *Module,
                            const LilyMirDebugInfo *Scope,
                            const LilyMirDebugInfoFile *File,
                            const LilyMirDebugInfo *Type,
                            const char *Name,
                            Usize ArgCount,
                            Usize Line)
{
    LilyMirDebugInfo *local_variable_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  local_variable,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoLocalVariable,
                      Scope,
                      File,
                      Type,
                      Name,
                      ArgCount,
                      Line));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        local_variable_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIExpression(LilyMirModule *Module, LilyMirDebugInfo *Expression)
{
    LilyMirDebugInfo *expression_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  expression,
                  Module->debug_info_manager.count,
                  Expression);

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        expression_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIType(LilyMirModule *Module,
                   const char *Name,
                   Usize Size,
                   enum LilyMirDebugInfoEncoding Encoding)
{
    LilyMirDebugInfo *type_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  type,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoType, Name, Size, Encoding));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        type_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIDerivedType(LilyMirModule *Module,
                          const LilyMirDebugInfo *Scope,
                          const LilyMirDebugInfo *BaseType,
                          enum LilyMirDebugInfoTag Tag,
                          const char *Name,
                          Usize Size,
                          Usize Align,
                          Usize Offset)
{
    LilyMirDebugInfo *derived_type_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  derived_type,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoDerivedType,
                      Scope,
                      BaseType,
                      Tag,
                      Name,
                      Size,
                      Align,
                      Offset));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        derived_type_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDICompositeType(LilyMirModule *Module,
                            enum LilyMirDebugInfoTag Tag,
                            const char *Name,
                            Usize Size,
                            Usize Align,
                            LilyMirDebugInfo *Elements)
{
    LilyMirDebugInfo *composite_type_debug_info = NEW_VARIANT(
      LilyMirDebugInfo,
      comp_type,
      Module->debug_info_manager.count,
      NEW(LilyMirDebugInfoCompositeType, Tag, Name, Size, Align, Elements));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        composite_type_debug_info);
}

LilyMirDebugInfo *
LilyMirBuildDIElements(LilyMirModule *Module, Vec *items)
{
    LilyMirDebugInfo *elements_debug_info =
      NEW_VARIANT(LilyMirDebugInfo,
                  elements,
                  Module->debug_info_manager.count,
                  NEW(LilyMirDebugInfoElements, items));

    return add__LilyMirDebugInfoManager(&Module->debug_info_manager,
                                        elements_debug_info);
}

void
LilyMirDisposeModule(const LilyMirModule *Module)
{
    FREE_ORD_HASHMAP_VALUES(Module->insts, LilyMirInstruction);
    FREE(OrderedHashMap, Module->insts);
    FREE_STACK_ITEMS(Module->current, LilyMirCurrent);
    FREE(Stack, Module->current);
    FREE_BUFFER_ITEMS(
      Module->files->buffer, Module->files->len, LilyMirDebugInfoFile);
    FREE(Vec, Module->files);
    FREE(LilyMirDebugInfoManager, &Module->debug_info_manager);
}

void
LilyMirNextBlockAndClearScope(LilyMirModule *Module, LilyMirScope *Scope)
{
    ASSERT(Module->current->len > 0);

    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    pop__Stack(current->fun.fun->fun.block_stack);

    FREE(LilyMirScope, Scope);
}

LilyMirInstruction *
LilyMirBuildBlock(LilyMirModule *Module, LilyMirBlockLimit *limit)
{
    LilyMirCurrent *current = Module->current->top;

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    char *name = LilyMirGenerateName(&current->fun.block_manager);

    return NEW_VARIANT(LilyMirInstruction,
                       block,
                       NEW(LilyMirInstructionBlock,
                           from__String(name),
                           limit,
                           current->fun.fun->fun.block_count++));
}

void
LilyMirAddBlock(LilyMirModule *Module, LilyMirInstruction *Block)
{
    LilyMirCurrent *current = LilyMirGetCurrentOnTop(Module);

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

    push__Vec(current->fun.fun->fun.insts, Block);
    push__Stack(current->fun.fun->fun.block_stack, &Block->block);
}

LilyMirInstructionBlock *
LilyMirPopBlock(LilyMirModule *Module)
{
    LilyMirCurrent *current = LilyMirGetCurrentOnTop(Module);

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);
    ASSERT(current->fun.fun->fun.block_stack->len > 0);

    return pop__Stack(current->fun.fun->fun.block_stack);
}

LilyMirInstructionBlock *
LilyMirGetInsertBlock(LilyMirModule *Module)
{
    LilyMirCurrent *current = LilyMirGetCurrentOnTop(Module);

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);
    ASSERT(current->fun.fun->fun.block_stack->len > 0);

    return current->fun.fun->fun.block_stack->top;
}

bool
LilyMirEmptyBlock(LilyMirModule *Module)
{
    LilyMirCurrent *current = LilyMirGetCurrentOnTop(Module);

    ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);
    ASSERT(current->fun.fun->fun.block_stack->len > 0);

    return CAST(LilyMirInstructionBlock *,
                current->fun.fun->fun.block_stack->top)
             ->insts->len == 0;
}

char *
LilyMirGenerateName(LilyMirNameManager *NameManager)
{
    char *name = format("{s}{d}", NameManager->base_name, NameManager->count);

    push__Vec(NameManager->names, name);

    ++NameManager->count;

    return name;
}

LilyMirInstructionVal *
LilyMirGetValFromInst(LilyMirInstruction *inst)
{
    switch (inst->kind) {
        case LILY_MIR_INSTRUCTION_KIND_VAL:
            return inst->val;
        default:
            return NULL;
    }
}

bool
LilyMirValidTypesOfFun(LilyMirInstructionFun *Fun, LilyMirDt **types, Usize len)
{
    for (Usize i = 0; i < Fun->args->len; ++i) {
        LilyMirInstruction *arg = get__Vec(Fun->args, i);

        if (!eq__LilyMirDt(arg->arg.dt, types[i])) {
            return false;
        }
    }

    return eq__LilyMirDt(Fun->return_data_type, types[len - 1]);
}

const char *
LilyMirGetFunNameFromTypes(LilyMirModule *Module,
                           const char *BaseName,
                           LilyMirDt **Types,
                           Usize Len)
{
    {
        OrderedHashMapIter insts_iter = NEW(OrderedHashMapIter, Module->insts);
        LilyMirInstruction *current = NULL;

        while ((current = next__OrderedHashMapIter(&insts_iter))) {
            switch (current->kind) {
                case LILY_MIR_INSTRUCTION_KIND_FUN:
                    if (!strcmp(BaseName, current->fun.base_name) &&
                        LilyMirValidTypesOfFun(&current->fun, Types, Len)) {
                        return current->fun.name;
                    }

                    break;
                default:
                    continue;
            }
        }
    }

    // Search in the stack (for unpushed function to the insts field)
    if (Module->current->len > 0) {
        LilyMirCurrent *top = Module->current->top;

        if (top) {
            ASSERT(top->kind == LILY_MIR_CURRENT_KIND_FUN);

            if (!strcmp(BaseName, top->fun.fun->fun.base_name) &&
                LilyMirValidTypesOfFun(&top->fun.fun->fun, Types, Len)) {
                return top->fun.fun->fun.name;
            }
        }

        for (Usize i = 0; i < Module->current->len - 1; ++i) {
            LilyMirCurrent *current = Module->current->buffer[i];

            ASSERT(current->kind == LILY_MIR_CURRENT_KIND_FUN);

            if (!strcmp(BaseName, current->fun.fun->fun.base_name) &&
                LilyMirValidTypesOfFun(&current->fun.fun->fun, Types, Len)) {
                return current->fun.fun->fun.name;
            }
        }
    }

    UNREACHABLE("the analysis has a bug!!");
}

void
LilyMirAddFinalInstruction(LilyMirModule *Module,
                           LilyMirInstruction *exit_block)
{
    LilyMirCurrent *top = Module->current->top;

    ASSERT(top->kind == LILY_MIR_CURRENT_KIND_FUN);
    ASSERT(top->fun.fun->fun.insts->len > 0);

    LilyMirInstructionBlock *current_block = top->fun.fun->fun.block_stack->top;

    if (current_block->insts->len == 0) {
        return LilyMirAddInst(Module, LilyMirBuildJmp(Module, exit_block));
    }

    LilyMirInstruction *last_inst = last__Vec(current_block->insts);

    if (last_inst->kind != LILY_MIR_INSTRUCTION_KIND_RET &&
        last_inst->kind != LILY_MIR_INSTRUCTION_KIND_JMP &&
        last_inst->kind != LILY_MIR_INSTRUCTION_KIND_JMPCOND) {
        return LilyMirAddInst(Module, LilyMirBuildJmp(Module, exit_block));
    }
}

bool
LilyMirHasRetInstruction(LilyMirModule *Module)
{
    LilyMirCurrent *top = Module->current->top;

    ASSERT(top->kind == LILY_MIR_CURRENT_KIND_FUN);
    ASSERT(top->fun.fun->fun.insts->len > 0);

    LilyMirInstructionBlock *current_block = top->fun.fun->fun.block_stack->top;

    if (current_block->insts->len == 0) {
        return false;
    }

    LilyMirInstruction *last_inst = last__Vec(current_block->insts);

    if (last_inst->kind != LILY_MIR_INSTRUCTION_KIND_RET) {
        return false;
    }

    return true;
}

void
LilyMirBuildIfBranch(LilyMirModule *Module,
                     LilyCheckedSignatureFun *fun_signature,
                     LilyMirScope *scope,
                     const LilyCheckedStmtIfBranch *if_branch,
                     LilyMirInstruction *next_block,
                     LilyMirInstruction *exit_block)
{
    //   ; ...
    //   jmp cond
    // cond:
    //   %0 = <cond>
    //   jmpcond %0, if, next
    // if:
    //   ; ...
    //   jmp exit
    // next:
    //   ; ...
    // exit:
    //   ; ...

    // If the previous block is empty, no condition block will be created.
    LilyMirBlockLimit *block_limit = NEW(LilyMirBlockLimit);
    LilyMirInstruction *cond_block =
      LilyMirEmptyBlock(Module) ? NULL : LilyMirBuildBlock(Module, block_limit);

    // 1. Add jmp cond
    // 2. Add cond block
    if (cond_block) {
        LilyMirAddInst(Module, LilyMirBuildJmp(Module, cond_block));
        LilyMirPopBlock(Module);
        LilyMirAddBlock(Module, cond_block);
    }

    LilyMirInstruction *cond =
      generate_expr__LilyMir(Module, fun_signature, scope, if_branch->cond);
    LilyMirInstruction *if_block =
      LilyMirBuildBlock(Module, ref__LilyMirBlockLimit(block_limit));

    // 3. Add conditional jmp
    LilyMirAddInst(Module,
                   LilyMirBuildJmpCond(Module, cond, if_block, next_block));

    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, if_block);

    // 4. Generate the content of the body
    GENERATE_BODY(
      Module, fun_signature, scope, block_limit, NULL, NULL, if_branch->body);

    // 5. Add final instruction
    LilyMirSetBlockLimit(block_limit, LilyMirGetInsertBlock(Module)->id);
    LilyMirAddFinalInstruction(Module, exit_block);

    lily_free(cond);
}

void
LilyMirBuildElifBranch(LilyMirModule *Module,
                       LilyCheckedSignatureFun *fun_signature,
                       LilyMirScope *scope,
                       const LilyCheckedStmtIfBranch *elif_branch,
                       LilyMirInstruction *next_block,
                       LilyMirInstruction *current_block,
                       LilyMirInstruction *exit_block)
{
    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, current_block);

    LilyMirInstruction *cond =
      generate_expr__LilyMir(Module, fun_signature, scope, elif_branch->cond);
    LilyMirInstruction *elif_block = LilyMirBuildBlock(
      Module, ref__LilyMirBlockLimit(current_block->block.limit));

    // 1. Add conditional jmp
    LilyMirAddInst(Module,
                   LilyMirBuildJmpCond(Module, cond, elif_block, next_block));

    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, elif_block);

    // 2. Generate the content of the body
    GENERATE_BODY(Module,
                  fun_signature,
                  scope,
                  elif_block->block.limit,
                  NULL,
                  NULL,
                  elif_branch->body);

    // 3. Add final instruction
    LilyMirSetBlockLimit(elif_block->block.limit,
                         LilyMirGetInsertBlock(Module)->id);
    LilyMirAddFinalInstruction(Module, exit_block);

    lily_free(cond);
}

void
LilyMirBuildElseBranch(LilyMirModule *Module,
                       LilyCheckedSignatureFun *fun_signature,
                       LilyMirScope *scope,
                       const LilyCheckedStmtElseBranch *else_branch,
                       LilyMirInstruction *current_block,
                       LilyMirInstruction *exit_block)
{
    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, current_block);

    // 1. Generate the content of the body
    GENERATE_BODY(Module,
                  fun_signature,
                  scope,
                  current_block->block.limit,
                  NULL,
                  NULL,
                  else_branch->body);

    // 2. Add final instruction
    LilyMirSetBlockLimit(current_block->block.limit,
                         LilyMirGetInsertBlock(Module)->id);
    LilyMirAddFinalInstruction(Module, exit_block);
}

void
LilyMirBuildIf(LilyMirModule *Module,
               LilyCheckedSignatureFun *fun_signature,
               LilyMirScope *scope,
               LilyMirBlockLimit *parent_block_limit,
               const LilyCheckedStmtIf *if_stmt)
{
    LilyMirInstruction *exit_block =
      LilyMirBuildBlock(Module, ref__LilyMirBlockLimit(parent_block_limit));
    LilyMirInstruction *next_block =
      !if_stmt->elifs && !if_stmt->else_
        ? exit_block
        : LilyMirBuildBlock(Module, NEW(LilyMirBlockLimit));

    LilyMirBuildIfBranch(
      Module, fun_signature, scope, if_stmt->if_, next_block, exit_block);

    if (if_stmt->elifs) {
        for (Usize i = 0; i < if_stmt->elifs->len; ++i) {
            LilyMirInstruction *current_block = next_block;

            next_block = i + 1 == if_stmt->elifs->len && !if_stmt->else_
                           ? exit_block
                           : LilyMirBuildBlock(Module, NEW(LilyMirBlockLimit));

            LilyMirBuildElifBranch(Module,
                                   fun_signature,
                                   scope,
                                   get__Vec(if_stmt->elifs, i),
                                   next_block,
                                   current_block,
                                   exit_block);
        }
    }

    if (if_stmt->else_) {
        LilyMirBuildElseBranch(
          Module, fun_signature, scope, if_stmt->else_, next_block, exit_block);

        if (!LilyMirHasRetInstruction(Module)) {
            LilyMirPopBlock(Module);
            LilyMirAddBlock(Module, exit_block);
        } else {
            FREE(LilyMirInstruction, exit_block);
        }
    } else {
        LilyMirPopBlock(Module);
        LilyMirAddBlock(Module, exit_block);
    }
}

void
LilyMirBuildWhile(LilyMirModule *Module,
                  LilyCheckedSignatureFun *fun_signature,
                  LilyMirScope *scope,
                  LilyMirBlockLimit *parent_block_limit,
                  const LilyCheckedStmtWhile *while_stmt)
{
    //   ; ...
    //   jmp cond
    // cond:
    //   %0 = <cond>
    //   jmpcond %0, if, exit
    // while:
    //   ; ...
    //   jmp cond
    // exit:
    //   ; ...

    LilyMirBlockLimit *block_limit = NEW(LilyMirBlockLimit);
    LilyMirInstruction *cond_block =
      LilyMirEmptyBlock(Module)
        ? NULL
        : LilyMirBuildBlock(Module, ref__LilyMirBlockLimit(block_limit));
    LilyMirInstruction *while_block = LilyMirBuildBlock(Module, block_limit);
    LilyMirInstruction *exit_block =
      LilyMirBuildBlock(Module, ref__LilyMirBlockLimit(parent_block_limit));

    // 1. Add jmp cond
    // 2. Add cond block
    if (cond_block) {
        LilyMirAddInst(Module, LilyMirBuildJmp(Module, cond_block));
        LilyMirPopBlock(Module);
        LilyMirAddBlock(Module, cond_block);
    }

    LilyMirInstruction *cond =
      generate_expr__LilyMir(Module, fun_signature, scope, while_stmt->cond);

    // 3. Add conditional jmp
    LilyMirAddInst(Module,
                   LilyMirBuildJmpCond(Module, cond, while_block, exit_block));

    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, while_block);

    // 4. Generate the content of the body
    GENERATE_BODY(Module,
                  fun_signature,
                  scope,
                  block_limit,
                  exit_block,
                  cond_block,
                  while_stmt->body);

    // 5. Add final instruction
    LilyMirSetBlockLimit(block_limit, LilyMirGetInsertBlock(Module)->id);
    LilyMirAddFinalInstruction(Module, cond_block);

    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, exit_block);

    lily_free(cond);
}

void
LilyMirBuildBlockStmt(LilyMirModule *Module,
                      LilyCheckedSignatureFun *fun_signature,
                      LilyMirScope *scope,
                      LilyMirBlockLimit *parent_block_limit,
                      const LilyCheckedStmtBlock *block_stmt)
{
    //   ; ...
    //   jmp block
    // block:
    //   ; ...
    //   jmp exit
    // exit:
    //   ; ...

    LilyMirInstruction *block =
      LilyMirBuildBlock(Module, NEW(LilyMirBlockLimit));
    LilyMirInstruction *exit_block =
      LilyMirBuildBlock(Module, ref__LilyMirBlockLimit(parent_block_limit));

    LilyMirAddInst(Module, LilyMirBuildJmp(Module, block));
    LilyMirPopBlock(Module);
    LilyMirAddBlock(Module, block);

    GENERATE_BODY(Module,
                  fun_signature,
                  scope,
                  block->block.limit,
                  NULL,
                  NULL,
                  block_stmt->body);

    LilyMirSetBlockLimit(block->block.limit, LilyMirGetInsertBlock(Module)->id);

    if (LilyMirHasRetInstruction(Module)) {
        FREE(LilyMirInstruction, exit_block);
    } else {
        LilyMirAddFinalInstruction(Module, exit_block);
        LilyMirPopBlock(Module);
        LilyMirAddBlock(Module, exit_block);
    }
}

void
LilyMirBuildBreak(LilyMirModule *Module, LilyMirInstruction *exit_block)
{
    ASSERT(exit_block);
    LilyMirAddInst(Module, LilyMirBuildJmp(Module, exit_block));
}

void
LilyMirBuildNext(LilyMirModule *Module, LilyMirInstruction *next_block)
{
    ASSERT(next_block);
    LilyMirAddInst(Module, LilyMirBuildJmp(Module, next_block));
}
