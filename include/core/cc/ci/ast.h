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

#ifndef LILY_CORE_CC_CI_AST_H
#define LILY_CORE_CC_CI_AST_H

#include <base/alloc.h>
#include <base/hash_map.h>
#include <base/string.h>
#include <base/types.h>
#include <base/vec.h>

#define MAX_CI_EXPR_PRECEDENCE 100

typedef struct CIToken CIToken;
typedef struct CIExpr CIExpr;
typedef struct CIDataType CIDataType;
typedef struct CIDeclFunctionItem CIDeclFunctionItem;

typedef struct CIScopeID
{
    Usize id;
} CIScopeID;

/**
 *
 * @brief Construct CIScopeID type.
 */
CONSTRUCTOR(CIScopeID *, CIScopeID, Usize id);

/**
 *
 * @brief Free CIScopeID type.
 */
inline DESTRUCTOR(CIScopeID, CIScopeID *self)
{
    lily_free(self);
}

#define CI_FILE_ID_KIND_HEADER 0b0
#define CI_FILE_ID_KIND_SOURCE 0b1

typedef struct CIFileID
{
    Usize id;
    Uint8 kind : 1;
} CIFileID;

/**
 *
 * @brief Construct CIFileID type.
 */
inline CONSTRUCTOR(CIFileID, CIFileID, Usize id, Uint8 kind)
{
    return (CIFileID){ .id = id, .kind = kind };
}

typedef struct CIEnumID
{
    CIFileID file_id;
    Usize id;
} CIEnumID;

/**
 *
 * @brief Construct CIEnumID type.
 */
CONSTRUCTOR(CIEnumID *, CIEnumID, CIFileID file_id, Usize id);

/**
 *
 * @brief Free CIEnumID type.
 */
inline DESTRUCTOR(CIEnumID, CIEnumID *self)
{
    lily_free(self);
}

typedef struct CIFunctionID
{
    CIFileID file_id;
    Usize id;
} CIFunctionID;

/**
 *
 * @brief Construct CIFunctionID type.
 */
CONSTRUCTOR(CIFunctionID *, CIFunctionID, CIFileID file_id, Usize id);

/**
 *
 * @brief Free CIFunctionID type.
 */
inline DESTRUCTOR(CIFunctionID, CIFunctionID *self)
{
    lily_free(self);
}

typedef struct CIStructID
{
    CIFileID file_id;
    Usize id;
} CIStructID;

/**
 *
 * @brief Construct CIStructID type.
 */
CONSTRUCTOR(CIStructID *, CIStructID, CIFileID file_id, Usize id);

/**
 *
 * @brief Free CIStructID type.
 */
inline DESTRUCTOR(CIStructID, CIStructID *self)
{
    lily_free(self);
}

typedef struct CIUnionID
{
    CIFileID file_id;
    Usize id;
} CIUnionID;

/**
 *
 * @brief Construct CIUnionID type.
 */
CONSTRUCTOR(CIUnionID *, CIUnionID, CIFileID file_id, Usize id);

/**
 *
 * @brief Free CIUnionID type.
 */
inline DESTRUCTOR(CIUnionID, CIUnionID *self)
{
    lily_free(self);
}

typedef struct CIVariableID
{
    CIFileID file_id;
    CIScopeID scope_id;
    Usize id;
} CIVariableID;

/**
 *
 * @brief Construct CIVariableID type.
 */
CONSTRUCTOR(CIVariableID *,
            CIVariableID,
            CIFileID file_id,
            CIScopeID scope_id,
            Usize id);

/**
 *
 * @brief Free CIVariableID type.
 */
inline DESTRUCTOR(CIVariableID, CIVariableID *self)
{
    lily_free(self);
}

typedef struct CIScope
{
    CIScopeID *parent; // CIScopeID*? (&)
    CIScopeID *scope_id;
    bool is_block;
    HashMap *enums;     // HashMap<CIEnumID*>*
    HashMap *functions; // HashMap<CIFunctionID*>*
    HashMap *structs;   // HashMap<CIStructID*>*
    HashMap *unions;    // HashMap<CIUnionID*>*
    HashMap *variables; // HashMap<CIVariableID*>*
} CIScope;

/**
 *
 * @brief Construct CIScope type.
 */
CONSTRUCTOR(CIScope *, CIScope, CIScopeID *parent, Usize id, bool is_block);

#define ADD_IN_SCOPE(ty, hm) \
    return insert__HashMap(hm, name->buffer, NEW(ty, file_id, hm->len));

#define ADD_VARIABLE_IN_SCOPE(ty, scope_id, hm) \
    return insert__HashMap(                     \
      hm, name->buffer, NEW(ty, file_id, scope_id, hm->len));

/**
 *
 * @brief Add enum to the scope.
 * @return CIEnumID*? (&)
 */
inline const CIEnumID *
add_enum__CIScope(const CIScope *self, const String *name, CIFileID file_id)
{
    ADD_IN_SCOPE(CIEnumID, self->enums);
}

/**
 *
 * @brief Add function to the scope.
 * @return CIFunctionID*? (&)
 */
inline const CIFunctionID *
add_function__CIScope(const CIScope *self, const String *name, CIFileID file_id)
{
    ADD_IN_SCOPE(CIFunctionID, self->functions);
}

/**
 *
 * @brief Add struct to the scope.
 * @return CIStructID*? (&)
 */
inline const CIStructID *
add_struct__CIScope(const CIScope *self, const String *name, CIFileID file_id)
{
    ADD_IN_SCOPE(CIStructID, self->structs);
}

/**
 *
 * @brief Add union to the scope.
 * @return CIUnionID*? (&)
 */
inline const CIUnionID *
add_union__CIScope(const CIScope *self, const String *name, CIFileID file_id)
{
    ADD_IN_SCOPE(CIUnionID, self->unions);
}

/**
 *
 * @brief Add variable to the scope.
 * @return CIVariableID*? (&)
 */
inline const CIVariableID *
add_variable__CIScope(const CIScope *self,
                      const String *name,
                      CIScopeID scope_id,
                      CIFileID file_id)
{
    ADD_VARIABLE_IN_SCOPE(CIVariableID, scope_id, self->variables);
}

#define SEARCH_IN_SCOPE(hm) return get__HashMap(hm, name->buffer);

/**
 *
 * @brief Search enum to the scope.
 * @return CIEnumID*? (&)
 */
inline const CIEnumID *
search_enum__CIScope(const CIScope *self, const String *name)
{
    SEARCH_IN_SCOPE(self->enums);
}

/**
 *
 * @brief Search function to the scope.
 * @return CIFunctionID*? (&)
 */
inline const CIFunctionID *
search_function__CIScope(const CIScope *self, const String *name)
{
    SEARCH_IN_SCOPE(self->functions);
}

/**
 *
 * @brief Search struct to the scope.
 * @return CIStructID*? (&)
 */
inline const CIStructID *
search_struct__CIScope(const CIScope *self, const String *name)
{
    SEARCH_IN_SCOPE(self->structs);
}

/**
 *
 * @brief Search union to the scope.
 * @return CIUnionID*? (&)
 */
inline const CIUnionID *
search_union__CIScope(const CIScope *self, const String *name)
{
    SEARCH_IN_SCOPE(self->unions);
}

/**
 *
 * @brief Search variable to the scope.
 * @return CIVariableID*? (&)
 */
inline const CIVariableID *
search_variable__CIScope(const CIScope *self, const String *name)
{
    SEARCH_IN_SCOPE(self->variables);
}

/**
 *
 * @brief Free CIScope type.
 */
DESTRUCTOR(CIScope, CIScope *self);

typedef struct CIGenericParams
{
    Usize ref_count;
    Vec *params; // Vec<CIDataType*>*
} CIGenericParams;

/**
 *
 * @brief Construct CIGenericParams type.
 */
CONSTRUCTOR(CIGenericParams *, CIGenericParams, Vec *params);

/**
 *
 * @brief Increment `ref_count`.
 * @return CIGenericParams*
 */
inline CIGenericParams *
ref__CIGenericParams(CIGenericParams *self)
{
    ++self->ref_count;
    return self;
}

/**
 *
 * @brief Clone CIGenericParams type.
 */
CIGenericParams *
clone__CIGenericParams(const CIGenericParams *self);

/**
 *
 * @brief Convert CIGenericParams in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIGenericParams, const CIGenericParams *self);
#endif

/**
 *
 * @brief Free CIGenericParams type.
 */
DESTRUCTOR(CIGenericParams, CIGenericParams *self);

enum CIDataTypeKind
{
    CI_DATA_TYPE_KIND_ARRAY,
    CI_DATA_TYPE_KIND__ATOMIC,
    CI_DATA_TYPE_KIND_BOOL,
    CI_DATA_TYPE_KIND_CHAR,
    CI_DATA_TYPE_KIND_DOUBLE,
    CI_DATA_TYPE_KIND_DOUBLE__COMPLEX,
    CI_DATA_TYPE_KIND_DOUBLE__IMAGINARY,
    CI_DATA_TYPE_KIND__DECIMAL32,
    CI_DATA_TYPE_KIND__DECIMAL64,
    CI_DATA_TYPE_KIND__DECIMAL128,
    CI_DATA_TYPE_KIND_ENUM,
    CI_DATA_TYPE_KIND_FLOAT,
    CI_DATA_TYPE_KIND_FLOAT__COMPLEX,
    CI_DATA_TYPE_KIND_FLOAT__IMAGINARY,
    CI_DATA_TYPE_KIND_FUNCTION,
    CI_DATA_TYPE_KIND_GENERIC,
    CI_DATA_TYPE_KIND_INT,
    CI_DATA_TYPE_KIND_LONG_DOUBLE,
    CI_DATA_TYPE_KIND_LONG_DOUBLE__COMPLEX,
    CI_DATA_TYPE_KIND_LONG_DOUBLE__IMAGINARY,
    CI_DATA_TYPE_KIND_LONG_INT,
    CI_DATA_TYPE_KIND_LONG_LONG_INT,
    CI_DATA_TYPE_KIND_PRE_CONST,
    CI_DATA_TYPE_KIND_POST_CONST,
    CI_DATA_TYPE_KIND_PTR,
    CI_DATA_TYPE_KIND_SHORT_INT,
    CI_DATA_TYPE_KIND_SIGNED_CHAR,
    CI_DATA_TYPE_KIND_STRUCT,
    CI_DATA_TYPE_KIND_TYPEDEF,
    CI_DATA_TYPE_KIND_UNSIGNED_INT,
    CI_DATA_TYPE_KIND_UNSIGNED_CHAR,
    CI_DATA_TYPE_KIND_UNSIGNED_LONG_INT,
    CI_DATA_TYPE_KIND_UNSIGNED_LONG_LONG_INT,
    CI_DATA_TYPE_KIND_UNSIGNED_SHORT_INT,
    CI_DATA_TYPE_KIND_UNION,
    CI_DATA_TYPE_KIND_VOID,
};

/**
 *
 * @brief Convert CIDataTypeKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIDataTypeKind, enum CIDataTypeKind self);
#endif

enum CIDataTypeArrayKind
{
    CI_DATA_TYPE_ARRAY_KIND_SIZED,
    CI_DATA_TYPE_ARRAY_KIND_NONE,
};

/**
 *
 * @brief Convert CIDataTypeArrayKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIDataTypeArrayKind, enum CIDataTypeArrayKind self);
#endif

typedef struct CIDataTypeArray
{
    enum CIDataTypeArrayKind kind;
    struct CIDataType *data_type;
    String *name; // String*? (&)
    union
    {
        Usize size;
    };
} CIDataTypeArray;

/**
 *
 * @brief Construct CIDataTypeArray type (CI_DATA_TYPE_ARRAY_KIND_SIZED).
 */
inline VARIANT_CONSTRUCTOR(CIDataTypeArray,
                           CIDataTypeArray,
                           sized,
                           struct CIDataType *data_type,
                           String *name,
                           Usize size)
{
    return (CIDataTypeArray){ .kind = CI_DATA_TYPE_ARRAY_KIND_SIZED,
                              .data_type = data_type,
                              .name = name,
                              .size = size };
}

/**
 *
 * @brief Construct CIDataTypeArray type (CI_DATA_TYPE_ARRAY_KIND_NONE).
 */
inline VARIANT_CONSTRUCTOR(CIDataTypeArray,
                           CIDataTypeArray,
                           none,
                           struct CIDataType *data_type,
                           String *name)
{
    return (CIDataTypeArray){
        .kind = CI_DATA_TYPE_ARRAY_KIND_NONE,
        .data_type = data_type,
        .name = name,
    };
}

/**
 *
 * @brief Convert CIDataTypeArray in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDataTypeArray, const CIDataTypeArray *self);
#endif

/**
 *
 * @brief Free CIDataTypeArray type.
 */
DESTRUCTOR(CIDataTypeArray, const CIDataTypeArray *self);

typedef struct CIDataTypeFunction
{
    String *name; // String* (&)
    Vec *params;  // Vec<CIDataType*>*
    struct CIDataType *return_data_type;
} CIDataTypeFunction;

/**
 *
 * @brief Construct CIDataTypeFunction type.
 */
inline CONSTRUCTOR(CIDataTypeFunction,
                   CIDataTypeFunction,
                   String *name,
                   Vec *params,
                   struct CIDataType *return_data_type)
{
    return (CIDataTypeFunction){ .name = name,
                                 .params = params,
                                 .return_data_type = return_data_type };
}

/**
 *
 * @brief Convert CIDataTypeFunction in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDataTypeFunction, const CIDataTypeFunction *self);
#endif

/**
 *
 * @brief Free CIDataTypeFunction type.
 */
DESTRUCTOR(CIDataTypeFunction, const CIDataTypeFunction *self);

typedef struct CIDataTypeStruct
{
    String *name;                    // String* (&)
    CIGenericParams *generic_params; // CIGenericParams*?
} CIDataTypeStruct;

/**
 *
 * @brief Construct CIDataTypeStruct type.
 */
inline CONSTRUCTOR(CIDataTypeStruct,
                   CIDataTypeStruct,
                   String *name,
                   CIGenericParams *generic_params)
{
    return (CIDataTypeStruct){ .name = name, .generic_params = generic_params };
}

/**
 *
 * @brief Convert CIDataTypeStruct in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDataTypeStruct, const CIDataTypeStruct *self);
#endif

/**
 *
 * @brief Free CIDataTypeStruct type.
 */
DESTRUCTOR(CIDataTypeStruct, const CIDataTypeStruct *self);

typedef struct CIDataTypeUnion
{
    String *name;                    // String* (&)
    CIGenericParams *generic_params; // CIGenericParams*?
} CIDataTypeUnion;

/**
 *
 * @brief Construct CIDataTypeUnion type.
 */
inline CONSTRUCTOR(CIDataTypeUnion,
                   CIDataTypeUnion,
                   String *name,
                   CIGenericParams *generic_params)
{
    return (CIDataTypeUnion){ .name = name, .generic_params = generic_params };
}

/**
 *
 * @brief Convert CIDataTypeUnion in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDataTypeUnion, const CIDataTypeUnion *self);
#endif

/**
 *
 * @brief Free CIDataTypeUnion type.
 */
DESTRUCTOR(CIDataTypeUnion, const CIDataTypeUnion *self);

typedef struct CIDataType
{
    enum CIDataTypeKind kind;
    Usize ref_count;
    union
    {
        CIDataTypeArray array;
        struct CIDataType *_atomic;
        String *enum_; // String* (&)
        CIDataTypeFunction function;
        String *generic; // String* (&)
        struct CIDataType *pre_const;
        struct CIDataType *post_const;
        struct CIDataType *ptr;
        CIDataTypeStruct struct_;
        String *typedef_; // String* (&)
        CIDataTypeUnion union_;
    };
} CIDataType;

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_ARRAY).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, array, CIDataTypeArray array);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND__ATOMIC).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, _atomic, CIDataType *_atomic);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_ENUM).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, enum, String *enum_);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_FUNCTION).
 */
VARIANT_CONSTRUCTOR(CIDataType *,
                    CIDataType,
                    function,
                    CIDataTypeFunction function);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_GENERIC).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, generic, String *generic);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_PRE_CONST).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, pre_const, CIDataType *pre_const);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_POST_CONST).
 */
VARIANT_CONSTRUCTOR(CIDataType *,
                    CIDataType,
                    post_const,
                    CIDataType *post_const);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_PTR).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, ptr, CIDataType *ptr);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_STRUCT).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, struct, CIDataTypeStruct struct_);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_TYPEDEF).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, typedef, String *typedef_);

/**
 *
 * @brief Construct CIDataType type (CI_DATA_TYPE_KIND_UNION).
 */
VARIANT_CONSTRUCTOR(CIDataType *, CIDataType, union, CIDataTypeUnion union_);

/**
 *
 * @brief Construct CIDataType type.
 */
CONSTRUCTOR(CIDataType *, CIDataType, enum CIDataTypeKind kind);

/**
 *
 * @brief Increment `ref_count`.
 */
inline CIDataType *
ref__CIDataType(CIDataType *self)
{
    ++self->ref_count;
    return self;
}

/**
 *
 * @brief Clone a CIDataType type.
 */
CIDataType *
clone__CIDataType(const CIDataType *self);

/**
 *
 * @brief Serialize CIDataType.
 * @param self const CIDataType* (&)
 */
void
serialize__CIDataType(const CIDataType *self, String *buffer);

/**
 *
 * @brief Serialize Vec<CIDataType*>*.
 * @param data_types const Vec<CIDataType*>* (&)
 */
void
serialize_vec__CIDataType(const Vec *data_types, String *buffer);

/**
 *
 * @brief Check if the both data types are equal.
 */
bool
eq__CIDataType(const CIDataType *self, const CIDataType *other);

/**
 *
 * @brief Check if the data type is an integer (signed or unsigned).
 */
bool
is_integer__CIDataType(const CIDataType *self);

/**
 *
 * @brief Get pointer data type.
 */
CIDataType *
get_ptr__CIDataType(const CIDataType *self);

/**
 *
 * @brief Convert CIDataType in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDataType, const CIDataType *self);
#endif

/**
 *
 * @brief Free CIDataType type.
 */
DESTRUCTOR(CIDataType, CIDataType *self);

enum CIStorageClass
{
    CI_STORAGE_CLASS_NONE = 0,
    CI_STORAGE_CLASS_AUTO = 1 << 0,
    CI_STORAGE_CLASS_CONSTEXPR = 1 << 1,
    CI_STORAGE_CLASS_EXTERN = 1 << 2,
    CI_STORAGE_CLASS_INLINE = 1 << 3,
    CI_STORAGE_CLASS_REGISTER = 1 << 4,
    CI_STORAGE_CLASS_STATIC = 1 << 5,
    CI_STORAGE_CLASS_THREAD_LOCAL = 1 << 6,
    CI_STORAGE_CLASS_TYPEDEF = 1 << 7
};

/**
 *
 * @brief Convert CIStorageClass in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIStorageClass, int storage_class_flag);
#endif

/**
 *
 * @brief Convert CIStorageClass in string.
 */
char *
to_string__CIStorageClass(int storage_class_flag);

enum CIDeclKind
{
    CI_DECL_KIND_ENUM = 1 << 0,
    CI_DECL_KIND_FUNCTION = 1 << 1,
    CI_DECL_KIND_STRUCT = 1 << 2,
    CI_DECL_KIND_UNION = 1 << 3,
    CI_DECL_KIND_VARIABLE = 1 << 4,
#define CI_DECL_KIND_GEN (1 << 5)
    CI_DECL_KIND_FUNCTION_GEN = CI_DECL_KIND_FUNCTION | CI_DECL_KIND_GEN,
    CI_DECL_KIND_STRUCT_GEN = CI_DECL_KIND_STRUCT | CI_DECL_KIND_GEN,
    CI_DECL_KIND_UNION_GEN = CI_DECL_KIND_UNION | CI_DECL_KIND_GEN,
};

/**
 *
 * @brief Convert CIDeclKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIDeclKind, enum CIDeclKind self);
#endif

enum CIDeclEnumVariantKind
{
    CI_DECL_ENUM_VARIANT_KIND_DEFAULT,
    CI_DECL_ENUM_VARIANT_KIND_CUSTOM
};

/**
 *
 * @brief Convert CIDeclEnumVariantKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               CIDeclEnumVariantKind,
               enum CIDeclEnumVariantKind self);
#endif

typedef struct CIDeclEnumVariant
{
    enum CIDeclEnumVariantKind kind;
    String *name; // String* (&)
    union
    {
        Isize value;
    };
} CIDeclEnumVariant;

/**
 *
 * @brief Construct CIDeclEnumVariant type (CI_DECL_ENUM_VARIANT_KIND_CUSTOM).
 */
VARIANT_CONSTRUCTOR(CIDeclEnumVariant *,
                    CIDeclEnumVariant,
                    custom,
                    String *name,
                    Isize value);

/**
 *
 * @brief Construct CIDeclEnumVariant type (CI_DECL_ENUM_VARIANT_KIND_DEFAULT).
 */
VARIANT_CONSTRUCTOR(CIDeclEnumVariant *,
                    CIDeclEnumVariant,
                    default,
                    String *name);

/**
 *
 * @brief Convert CIDeclEnumVariant in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclEnumVariant, const CIDeclEnumVariant *self);
#endif

/**
 *
 * @brief Free CIDeclEnumVariant type.
 */
inline DESTRUCTOR(CIDeclEnumVariant, CIDeclEnumVariant *self)
{
    lily_free(self);
}

typedef struct CIDeclEnum
{
    String *name;  // String* (&)
    Vec *variants; // Vec<CIDeclEnumVariant*>*?
} CIDeclEnum;

/**
 *
 * @brief Construct CIDeclEnum type.
 */
inline CONSTRUCTOR(CIDeclEnum, CIDeclEnum, String *name, Vec *variants)
{
    return (CIDeclEnum){ .name = name, .variants = variants };
}

/**
 *
 * @brief Convert CIDeclEnum in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclEnum, const CIDeclEnum *self);
#endif

/**
 *
 * @brief Free CIDeclEnum type.
 */
DESTRUCTOR(CIDeclEnum, const CIDeclEnum *self);

typedef struct CIDeclFunctionParam
{
    String *name; // String*? (&)
    CIDataType *data_type;
} CIDeclFunctionParam;

/**
 *
 * @brief Construct CIDeclFunctionParam type.
 */
CONSTRUCTOR(CIDeclFunctionParam *,
            CIDeclFunctionParam,
            String *name,
            CIDataType *data_type);

/**
 *
 * @brief Convert CIDeclFunctionParam in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclFunctionParam, const CIDeclFunctionParam *self);
#endif

/**
 *
 * @brief Free CIDeclFunctionParam type.
 */
DESTRUCTOR(CIDeclFunctionParam, CIDeclFunctionParam *self);

// TODO: Perhaps create a field scope (to have a local scope) in the
// CIDeclFunction.
typedef struct CIDeclFunction
{
    String *name; // String* (&)
    CIDataType *return_data_type;
    CIGenericParams *generic_params; // CIGenericParams*?
    Vec *params;                     // Vec<CIDeclFunctionParam*>*?
    Vec *body;                       // Vec<CIDeclFunctionItem*>*?
} CIDeclFunction;

/**
 *
 * @brief Construct CIDeclFunction type.
 */
inline CONSTRUCTOR(CIDeclFunction,
                   CIDeclFunction,
                   String *name,
                   CIDataType *return_data_type,
                   CIGenericParams *generic_params,
                   Vec *params,
                   Vec *body)
{
    return (CIDeclFunction){ .name = name,
                             .return_data_type = return_data_type,
                             .generic_params = generic_params,
                             .params = params,
                             .body = body };
}

/**
 *
 * @brief Serialize function name.
 */
String *
serialize_name__CIDeclFunction(const CIDeclFunction *self,
                               const CIGenericParams *called_generic_params);

/**
 *
 * @brief Convert CIDeclFunction in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclFunction, const CIDeclFunction *self);
#endif

/**
 *
 * @brief Free CIDeclFunction type.
 */
DESTRUCTOR(CIDeclFunction, const CIDeclFunction *self);

typedef struct CIDeclFunctionGen
{
    const CIDeclFunction *function; // const CIDeclFunction* (&)
    String *name;
    CIGenericParams *called_generic_params; // CIGenericParams*
} CIDeclFunctionGen;

/**
 *
 * @brief Construct CIDeclFunctionGen type.
 */
inline CONSTRUCTOR(CIDeclFunctionGen,
                   CIDeclFunctionGen,
                   const CIDeclFunction *function,
                   String *name,
                   CIGenericParams *called_generic_params)
{
    return (CIDeclFunctionGen){ .function = function,
                                .name = name,
                                .called_generic_params =
                                  called_generic_params };
}

/**
 *
 * @brief Check if the gen function has generic.
 */
bool
has_generic__CIDeclFunctionGen(const CIDeclFunctionGen *self);

/**
 *
 * @brief Convert CIDeclFunctionGen in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclFunctionGen, const CIDeclFunctionGen *self);
#endif

/**
 *
 * @brief Free CIDeclFunctionGen type.
 */
DESTRUCTOR(CIDeclFunctionGen, const CIDeclFunctionGen *self);

typedef struct CIDeclStructField
{
    String *name; // String* (&)
    CIDataType *data_type;
} CIDeclStructField;

/**
 *
 * @brief Construct CIDeclStructField type.
 */
CONSTRUCTOR(CIDeclStructField *,
            CIDeclStructField,
            String *name,
            CIDataType *data_type);

/**
 *
 * @brief Convert CIDeclStructField in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclStructField, const CIDeclStructField *self);
#endif

/**
 *
 * @brief Free CIDeclStructField type.
 */
DESTRUCTOR(CIDeclStructField, CIDeclStructField *self);

typedef struct CIDeclStruct
{
    String *name;                    // String* (&)
    CIGenericParams *generic_params; // CIGenericParams*?
    Vec *fields;                     // Vec<CIDeclStructField*>*?
} CIDeclStruct;

/**
 *
 * @brief Construct CIDeclStruct type.
 */
inline CONSTRUCTOR(CIDeclStruct,
                   CIDeclStruct,
                   String *name,
                   CIGenericParams *generic_params,
                   Vec *fields)
{
    return (CIDeclStruct){ .name = name,
                           .generic_params = generic_params,
                           .fields = fields };
}

/**
 *
 * @brief Serialize struct name.
 */
String *
serialize_name__CIDeclStruct(const CIDeclStruct *self,
                             const CIGenericParams *called_generic_params);

/**
 *
 * @brief Convert CIDeclStruct in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclStruct, const CIDeclStruct *self);
#endif

/**
 *
 * @brief Free CIDeclStruct type.
 */
DESTRUCTOR(CIDeclStruct, const CIDeclStruct *self);

typedef struct CIDeclStructGen
{
    const CIDeclStruct *struct_; // const CIDeclStruct* (&)
    String *name;
    CIGenericParams *called_generic_params; // CIGenericParams*
} CIDeclStructGen;

/**
 *
 * @brief Construct CIDeclStructGen type.
 */
inline CONSTRUCTOR(CIDeclStructGen,
                   CIDeclStructGen,
                   const CIDeclStruct *struct_,
                   String *name,
                   CIGenericParams *called_generic_params)
{
    return (CIDeclStructGen){ .struct_ = struct_,
                              .name = name,
                              .called_generic_params = called_generic_params };
}

/**
 *
 * @brief Check if the gen struct has generic.
 */
bool
has_generic__CIDeclStructGen(const CIDeclStructGen *self);

/**
 *
 * @brief Convert CIDeclStructGen in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclStructGen, const CIDeclStructGen *self);
#endif

/**
 *
 * @brief Free CIDeclStructGen type.
 */
DESTRUCTOR(CIDeclStructGen, const CIDeclStructGen *self);

typedef struct CIDeclUnion
{
    String *name;                    // String* (&)
    CIGenericParams *generic_params; // CIGenericParams*?
    Vec *fields;                     // Vec<CIDeclStructField*>*?
} CIDeclUnion;

/**
 *
 * @brief Construct CIDeclUnion type.
 */
inline CONSTRUCTOR(CIDeclUnion,
                   CIDeclUnion,
                   String *name,
                   CIGenericParams *generic_params,
                   Vec *fields)
{
    return (CIDeclUnion){ .name = name,
                          .generic_params = generic_params,
                          .fields = fields };
}

/**
 *
 * @brief Serialize union name.
 */
String *
serialize_name__CIDeclUnion(const CIDeclUnion *self,
                            const CIGenericParams *called_generic_params);

/**
 *
 * @brief Convert CIDeclUnion in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclUnion, const CIDeclUnion *self);
#endif

/**
 *
 * @brief Free CIDeclUnion type.
 */
DESTRUCTOR(CIDeclUnion, const CIDeclUnion *self);

typedef struct CIDeclUnionGen

{
    const CIDeclUnion *union_; // const CIDeclFunction* (&)
    String *name;
    CIGenericParams *called_generic_params; // CIGenericParams*
} CIDeclUnionGen;

/**
 *
 * @brief Construct CIDeclUnionGen type.
 */
inline CONSTRUCTOR(CIDeclUnionGen,
                   CIDeclUnionGen,
                   const CIDeclUnion *union_,
                   String *name,
                   CIGenericParams *called_generic_params)
{
    return (CIDeclUnionGen){ .union_ = union_,
                             .name = name,
                             .called_generic_params = called_generic_params };
}

/**
 *
 * @brief Check if the gen union has generic.
 */
bool
has_generic__CIDeclUnionGen(const CIDeclUnionGen *self);

/**
 *
 * @brief Convert CIDeclUnionGen in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclUnionGen, const CIDeclUnionGen *self);
#endif

/**
 *
 * @brief Free CIDeclUnionGen type.
 */
DESTRUCTOR(CIDeclUnionGen, const CIDeclUnionGen *self);

typedef struct CIDeclVariable
{
    CIDataType *data_type;
    String *name; // String* (&)
    CIExpr *expr; // CIExpr*?
    bool is_local;
} CIDeclVariable;

/**
 *
 * @brief Construct CIDeclVariable type.
 */
inline CONSTRUCTOR(CIDeclVariable,
                   CIDeclVariable,
                   CIDataType *data_type,
                   String *name,
                   CIExpr *expr,
                   bool is_local)
{
    return (CIDeclVariable){
        .data_type = data_type, .name = name, .expr = expr, .is_local = is_local
    };
}

/**
 *
 * @brief Convert CIDeclVariable in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclVariable, const CIDeclVariable *self);
#endif

/**
 *
 * @brief Free CIDeclVariable type.
 */
DESTRUCTOR(CIDeclVariable, const CIDeclVariable *self);

typedef struct CIDecl
{
    enum CIDeclKind kind;
    int storage_class_flag;
    bool is_prototype;
    Usize ref_count;
    String
      *typedef_name; // String*? (&) | String*? (only for gen(s) declaration)
    union
    {
        CIDeclEnum enum_;
        CIDeclFunction function;
        CIDeclFunctionGen function_gen;
        CIDeclStruct struct_;
        CIDeclStructGen struct_gen;
        CIDeclVariable variable;
        CIDeclUnion union_;
        CIDeclUnionGen union_gen;
    };
} CIDecl;

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_ENUM).
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    enum,
                    int storage_class_flag,
                    bool is_prototype,
                    String *typedef_name,
                    CIDeclEnum enum_);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_FUNCTION).
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    function,
                    int storage_class_flag,
                    bool is_prototype,
                    CIDeclFunction function);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_FUNCTION_GEN).
 * @param called_generic_params CIGenericParams*? (&)
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    function_gen,
                    CIDecl *function,
                    CIGenericParams *called_generic_params,
                    String *name);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_STRUCT).
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    struct,
                    int storage_class_flag,
                    bool is_prototype,
                    String *typedef_name,
                    CIDeclStruct struct_);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_STRUCT_GEN).
 * @param called_generic_params CIGenericParams* (&)
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    struct_gen,
                    CIDecl *struct_,
                    CIGenericParams *called_generic_params,
                    String *name);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_UNION).
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    union,
                    int storage_class_flag,
                    bool is_prototype,
                    String *typedef_name,
                    CIDeclUnion union_);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_UNION_GEN).
 * @param called_generic_params CIGenericParams* (&)
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    union_gen,
                    CIDecl *union_,
                    CIGenericParams *called_generic_params,
                    String *name);

/**
 *
 * @brief Construct CIDecl type (CI_DECL_KIND_VARIABLE).
 */
VARIANT_CONSTRUCTOR(CIDecl *,
                    CIDecl,
                    variable,
                    int storage_class_flag,
                    bool is_prototype,
                    CIDeclVariable variable);

/**
 *
 * @brief Check if the passed `generic_params` contains generic data type.
 */
bool
is_generic_params_contains_generic__CIDecl(
  const CIGenericParams *generic_params);

/**
 *
 * @brief Increment `ref_count` field.
 */
inline CIDecl *
ref__CIDecl(CIDecl *self)
{
    ++self->ref_count;
    return self;
}

/**
 *
 * @brief Serialize typedef name.
 * @return String*?
 */
String *
serialize_typedef_name__CIDecl(const CIDecl *self,
                               const CIGenericParams *called_generic_params);

/**
 *
 * @brief Get typedef name from declaration.
 * @return String*? (&)
 */
inline String *
get_typedef_name__CIDecl(const CIDecl *self)
{
    return self->typedef_name;
}

/**
 *
 * @brief Get name from declaration.
 * @return String* (&)
 */
String *
get_name__CIDecl(const CIDecl *self);

/**
 *
 * @brief Check if the declaration has generic (e.g. generic_params)
 */
bool
has_generic__CIDecl(const CIDecl *self);

/**
 *
 * @brief Get expected data type from declaration.
 * @return CIDataType*? (&)
 */
CIDataType *
get_expected_data_type__CIDecl(const CIDecl *self);

/**
 *
 * @brief Convert CIDecl in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDecl, const CIDecl *self);
#endif

/**
 *
 * @brief Free CIDecl type.
 */
DESTRUCTOR(CIDecl, CIDecl *self);

enum CIExprBinaryKind
{
    CI_EXPR_BINARY_KIND_ASSIGN,            // =
    CI_EXPR_BINARY_KIND_ASSIGN_ADD,        // +=
    CI_EXPR_BINARY_KIND_ASSIGN_SUB,        // -=
    CI_EXPR_BINARY_KIND_ASSIGN_MUL,        // *=
    CI_EXPR_BINARY_KIND_ASSIGN_DIV,        // /=
    CI_EXPR_BINARY_KIND_ASSIGN_MOD,        // %=
    CI_EXPR_BINARY_KIND_ASSIGN_BIT_AND,    // &=
    CI_EXPR_BINARY_KIND_ASSIGN_BIT_OR,     // |=
    CI_EXPR_BINARY_KIND_ASSIGN_XOR,        // ^=
    CI_EXPR_BINARY_KIND_ASSIGN_BIT_LSHIFT, // <<=
    CI_EXPR_BINARY_KIND_ASSIGN_BIT_RSHIFT, // >>=
    CI_EXPR_BINARY_KIND_ADD,               // +
    CI_EXPR_BINARY_KIND_SUB,               // -
    CI_EXPR_BINARY_KIND_MUL,               // *
    CI_EXPR_BINARY_KIND_DIV,               // /
    CI_EXPR_BINARY_KIND_MOD,               // %
    CI_EXPR_BINARY_KIND_BIT_AND,           // &
    CI_EXPR_BINARY_KIND_BIT_OR,            // |
    CI_EXPR_BINARY_KIND_BIT_XOR,           // ^
    CI_EXPR_BINARY_KIND_BIT_LSHIFT,        // <<
    CI_EXPR_BINARY_KIND_BIT_RSHIFT,        // >>
    CI_EXPR_BINARY_KIND_AND,               // &&
    CI_EXPR_BINARY_KIND_OR,                // ||
    CI_EXPR_BINARY_KIND_EQ,                // ==
    CI_EXPR_BINARY_KIND_NE,                // !=
    CI_EXPR_BINARY_KIND_LESS,              // <
    CI_EXPR_BINARY_KIND_GREATER,           // >
    CI_EXPR_BINARY_KIND_LESS_EQ,           // <=
    CI_EXPR_BINARY_KIND_GREATER_EQ,        // >=
    CI_EXPR_BINARY_KIND_DOT,               // .
    CI_EXPR_BINARY_KIND_ARROW,             // ->
};

/**
 *
 * @brief Convert CIExprBinaryKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIExprBinaryKind, enum CIExprBinaryKind self);
#endif

/**
 *
 * @brief Get precedence from binary.
 */
Uint8
to_precedence__CIExprBinaryKind(enum CIExprBinaryKind kind);

/**
 *
 * @brief Convert token to binary.
 */
enum CIExprBinaryKind
from_token__CIExprBinaryKind(const CIToken *token);

typedef struct CIExprBinary
{
    enum CIExprBinaryKind kind;
    CIExpr *left;
    CIExpr *right;
} CIExprBinary;

/**
 *
 * @brief Construct CIExprBinary type.
 */
inline CONSTRUCTOR(CIExprBinary,
                   CIExprBinary,
                   enum CIExprBinaryKind kind,
                   CIExpr *left,
                   CIExpr *right)
{
    return (CIExprBinary){ .kind = kind, .left = left, .right = right };
}

/**
 *
 * @brief Convert CIExprBinary in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprBinary, const CIExprBinary *self);
#endif

/**
 *
 * @brief Free CIExprBinary type.
 */
DESTRUCTOR(CIExprBinary, const CIExprBinary *self);

enum CIExprLiteralKind
{
    CI_EXPR_LITERAL_KIND_BOOL, // NOTE: since C23
    CI_EXPR_LITERAL_KIND_CHAR,
    CI_EXPR_LITERAL_KIND_FLOAT,
    CI_EXPR_LITERAL_KIND_SIGNED_INT,
    CI_EXPR_LITERAL_KIND_STRING,
    CI_EXPR_LITERAL_KIND_UNSIGNED_INT,
};

/**
 *
 * @brief Convert CIExprLiteral in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIExprLiteralKind, enum CIExprLiteralKind self);
#endif

typedef struct CIExprLiteral
{
    enum CIExprLiteralKind kind;
    union
    {
        bool bool_;
        char char_;
        double float_;
        Isize signed_int;
        String *string; // String* (&)
        Usize unsigned_int;
    };
} CIExprLiteral;

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_BOOL).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral, CIExprLiteral, bool, bool bool_)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_BOOL, .bool_ = bool_ };
}

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_CHAR).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral, CIExprLiteral, char, char char_)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_CHAR, .char_ = char_ };
}

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_FLOAT).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral, CIExprLiteral, float, double float_)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_FLOAT,
                            .float_ = float_ };
}

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_SIGNED_INT).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral,
                           CIExprLiteral,
                           signed_int,
                           Isize signed_int)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_SIGNED_INT,
                            .signed_int = signed_int };
}

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_STRING).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral, CIExprLiteral, string, String *string)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_STRING,
                            .string = string };
}

/**
 *
 * @brief Construct CIExprLiteral type (CI_EXPR_LITERAL_KIND_UNSIGNED_INT).
 */
inline VARIANT_CONSTRUCTOR(CIExprLiteral,
                           CIExprLiteral,
                           unsigned_int,
                           Usize unsigned_int)
{
    return (CIExprLiteral){ .kind = CI_EXPR_LITERAL_KIND_UNSIGNED_INT,
                            .unsigned_int = unsigned_int };
}

/**
 *
 * @brief Convert CIExprLiteral in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprLiteral, const CIExprLiteral *self);
#endif

enum CIExprUnaryKind
{
    CI_EXPR_UNARY_KIND_PRE_INCREMENT,  // ++<expr>
    CI_EXPR_UNARY_KIND_PRE_DECREMENT,  // --<expr>
    CI_EXPR_UNARY_KIND_POST_INCREMENT, // <expr>++
    CI_EXPR_UNARY_KIND_POST_DECREMENT, // <expr>--
    CI_EXPR_UNARY_KIND_POSITIVE,       // +
    CI_EXPR_UNARY_KIND_NEGATIVE,       // -
    CI_EXPR_UNARY_KIND_BIT_NOT,        // ~
    CI_EXPR_UNARY_KIND_NOT,            // !
    CI_EXPR_UNARY_KIND_DEREFERENCE,    // *
    CI_EXPR_UNARY_KIND_REF,            // &
};

/**
 *
 * @brief Convert CIExprUnaryKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIExprUnaryKind, enum CIExprUnaryKind self);
#endif

/**
 *
 * @brief Get precedence from unary.
 */
Uint8
to_precedence__CIExprUnaryKind(enum CIExprUnaryKind kind);

typedef struct CIExprUnary
{
    enum CIExprUnaryKind kind;
    CIExpr *expr;
} CIExprUnary;

/**
 *
 * @brief Construct CIExprUnary type.
 */
inline CONSTRUCTOR(CIExprUnary,
                   CIExprUnary,
                   enum CIExprUnaryKind kind,
                   CIExpr *expr)
{
    return (CIExprUnary){ .kind = kind, .expr = expr };
}

/**
 *
 * @brief Convert CIExprUnary in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprUnary, const CIExprUnary *self);
#endif

/**
 *
 * @brief Free CIExprUnary type.
 */
DESTRUCTOR(CIExprUnary, const CIExprUnary *self);

typedef struct CIExprTernary
{
    CIExpr *cond;
    CIExpr *if_;
    CIExpr *else_;
} CIExprTernary;

/**
 *
 * @brief Construct CIExprTernary type.
 */
inline CONSTRUCTOR(CIExprTernary,
                   CIExprTernary,
                   CIExpr *cond,
                   CIExpr *if_,
                   CIExpr *else_)
{
    return (CIExprTernary){ .cond = cond, .if_ = if_, .else_ = else_ };
}

/**
 *
 * @brief Convert CIExprTernary in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprTernary, const CIExprTernary *self);
#endif

/**
 *
 * @brief Free CIExprTernary type.
 */
DESTRUCTOR(CIExprTernary, const CIExprTernary *self);

typedef struct CIExprCast
{
    CIDataType *data_type;
    CIExpr *expr;
} CIExprCast;

/**
 *
 * @brief Construct CIExprCast type.
 */
inline CONSTRUCTOR(CIExprCast, CIExprCast, CIDataType *data_type, CIExpr *expr)
{
    return (CIExprCast){ .data_type = data_type, .expr = expr };
}

/**
 *
 * @brief Convert CIExprCast in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprCast, const CIExprCast *self);
#endif

/**
 *
 * @brief Free CIExprCast type.
 */
DESTRUCTOR(CIExprCast, const CIExprCast *self);

typedef struct CIExprFunctionCall
{
    String *identifier;              // String* (&)
    Vec *params;                     // Vec<CIExpr*>*
    CIGenericParams *generic_params; // CIGenericParams*?
} CIExprFunctionCall;

/**
 *
 * @brief Construct CIExprFunctionCall type.
 */
inline CONSTRUCTOR(CIExprFunctionCall,
                   CIExprFunctionCall,
                   String *identifier,
                   Vec *params,
                   CIGenericParams *generic_params)
{
    return (CIExprFunctionCall){ .identifier = identifier,
                                 .params = params,
                                 .generic_params = generic_params };
}

/**
 *
 * @brief Convert CIExprFunctionCall in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprFunctionCall, const CIExprFunctionCall *self);
#endif

/**
 *
 * @brief Free CIExprFunctionCall type.
 */
DESTRUCTOR(CIExprFunctionCall, const CIExprFunctionCall *self);

typedef struct CIExprStructFieldCall
{
    Vec *path; // Vec<String* (&)>*
    CIExpr *value;
} CIExprStructFieldCall;

/**
 *
 * @brief Construct CIExprStructFieldCall type.
 */
CONSTRUCTOR(CIExprStructFieldCall *,
            CIExprStructFieldCall,
            Vec *path,
            CIExpr *value);

/**
 *
 * @brief Convert CIExprStructFieldCall in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               CIExprStructFieldCall,
               const CIExprStructFieldCall *self);
#endif

/**
 *
 * @brief Free CIExprStructFieldCall type.
 */
DESTRUCTOR(CIExprStructFieldCall, CIExprStructFieldCall *self);

typedef struct CIExprStructCall
{
    Vec *fields; // Vec<CIExprStructFieldCall*>*
} CIExprStructCall;

/**
 *
 * @brief Construct CIExprStructCall type.
 */
inline CONSTRUCTOR(CIExprStructCall, CIExprStructCall, Vec *fields)
{
    return (CIExprStructCall){ .fields = fields };
}

/**
 *
 * @brief Convert CIExprStructCall in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExprStructCall, const CIExprStructCall *self);
#endif

/**
 *
 * @brief Free CIExprStructCall type.
 */
DESTRUCTOR(CIExprStructCall, const CIExprStructCall *self);

enum CIExprKind
{
    CI_EXPR_KIND_ALIGNOF,
    CI_EXPR_KIND_BINARY,
    CI_EXPR_KIND_CAST,
    CI_EXPR_KIND_DATA_TYPE,
    CI_EXPR_KIND_FUNCTION_CALL,
    CI_EXPR_KIND_GROUPING,
    CI_EXPR_KIND_IDENTIFIER,
    CI_EXPR_KIND_LITERAL,
    CI_EXPR_KIND_SIZEOF,
    CI_EXPR_KIND_STRUCT_CALL,
    CI_EXPR_KIND_TERNARY,
    CI_EXPR_KIND_UNARY,
};

/**
 *
 * @brief Convert CIExprKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIExprKind, enum CIExprKind self);
#endif

struct CIExpr
{
    enum CIExprKind kind;
    union
    {
        CIExpr *alignof_;
        CIExprBinary binary;
        CIExprCast cast;
        CIDataType *data_type;
        CIExprFunctionCall function_call;
        CIExpr *grouping;
        String *identifier; // String* (&)
        CIExprLiteral literal;
        CIExpr *sizeof_;
        CIExprStructCall struct_call;
        CIExprTernary ternary;
        CIExprUnary unary;
    };
};

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_ALIGNOF).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, alignof, CIExpr *alignof_);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_BINARY).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, binary, CIExprBinary binary);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_CAST).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, cast, CIExprCast cast);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_DATA_TYPE).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, data_type, CIDataType *data_type);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_FUNCTION_CALL).
 */
VARIANT_CONSTRUCTOR(CIExpr *,
                    CIExpr,
                    function_call,
                    CIExprFunctionCall function_call);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_GROUPING).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, grouping, CIExpr *grouping);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_IDENTIFIER).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, identifier, String *identifier);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_LITERAL).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, literal, CIExprLiteral literal);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_SIZEOF).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, sizeof, CIExpr *sizeof_);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_STRUCT_CALL).
 */
VARIANT_CONSTRUCTOR(CIExpr *,
                    CIExpr,
                    struct_call,
                    CIExprStructCall struct_call);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_TERNARY).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, ternary, CIExprTernary ternary);

/**
 *
 * @brief Construct CIExpr type (CI_EXPR_KIND_UNARY).
 */
VARIANT_CONSTRUCTOR(CIExpr *, CIExpr, unary, CIExprUnary unary);

/**
 *
 * @brief Get data type from expression.
 * @note If it is not possible to determine the type of the expression, the
 * function returns NULL.
 * @return CIDataType*?
 */
CIDataType *
get_data_type__CIExpr(const CIExpr *self);

/**
 *
 * @brief Get precedence from expression.
 */
Uint8
to_precedence__CIExpr(const CIExpr *self);

/**
 *
 * @brief Convert CIExpr in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIExpr, const CIExpr *self);
#endif

/**
 *
 * @brief Free CIExpr type.
 */
DESTRUCTOR(CIExpr, CIExpr *self);

enum CIStmtKind
{
    CI_STMT_KIND_BLOCK,
    CI_STMT_KIND_BREAK,
    CI_STMT_KIND_CASE,
    CI_STMT_KIND_CONTINUE,
    CI_STMT_KIND_DEFAULT,
    CI_STMT_KIND_DO_WHILE,
    CI_STMT_KIND_FOR,
    CI_STMT_KIND_GOTO,
    CI_STMT_KIND_IF,
    CI_STMT_KIND_LABEL,
    CI_STMT_KIND_RETURN,
    CI_STMT_KIND_SWITCH,
    CI_STMT_KIND_WHILE,
};

/**
 *
 * @brief Convert CIStmtKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string, CIStmtKind, enum CIStmtKind self);
#endif

typedef struct CIStmtBlock
{
    Vec *body; // Vec<CIDeclFunctionItem*>*
} CIStmtBlock;

/**
 *
 * @brief Construct CIStmtBlock type.
 */
inline CONSTRUCTOR(CIStmtBlock, CIStmtBlock, Vec *body)
{
    return (CIStmtBlock){ .body = body };
}

/**
 *
 * @brief Convert CIStmtBlock in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtBlock, const CIStmtBlock *self);
#endif

/**
 *
 * @brief Free CIStmtBlock type.
 */
DESTRUCTOR(CIStmtBlock, const CIStmtBlock *self);

typedef struct CIStmtDoWhile
{
    Vec *body; // Vec<CIDeclFunctionItem*>*
    CIExpr *cond;
} CIStmtDoWhile;

/**
 *
 * @brief Construct CIStmtDoWhile type.
 */
inline CONSTRUCTOR(CIStmtDoWhile, CIStmtDoWhile, Vec *body, CIExpr *cond)
{
    return (CIStmtDoWhile){ .body = body, .cond = cond };
}

/**
 *
 * @brief Convert CIStmtDoWhile in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtDoWhile, const CIStmtDoWhile *self);
#endif

/**
 *
 * @brief Free CIStmtDoWhile type.
 */
DESTRUCTOR(CIStmtDoWhile, const CIStmtDoWhile *self);

typedef struct CIStmtFor
{
    Vec *body;                       // Vec<CIDeclFunctionItem*>*
    CIDeclFunctionItem *init_clause; // CIDeclFunctionItem*?
    CIExpr *expr1;                   // CIExpr*?
    Vec *exprs2;                     // Vec<CIExpr*>*?
} CIStmtFor;

/**
 *
 * @brief Construct CIStmtFor type.
 */
inline CONSTRUCTOR(CIStmtFor,
                   CIStmtFor,
                   Vec *body,
                   CIDeclFunctionItem *init_clause,
                   CIExpr *expr1,
                   Vec *exprs2)
{
    return (CIStmtFor){ .body = body,
                        .init_clause = init_clause,
                        .expr1 = expr1,
                        .exprs2 = exprs2 };
}

/**
 *
 * @brief Convert CIStmtFor in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtFor, const CIStmtFor *self);
#endif

/**
 *
 * @brief Free CIStmtFor type.
 */
DESTRUCTOR(CIStmtFor, const CIStmtFor *self);

typedef struct CIStmtIfBranch
{
    CIExpr *cond;
    Vec *body; // Vec<CIDeclFunctionItem*>*
} CIStmtIfBranch;

/**
 *
 * @brief Construct CIStmtIfBranch type.
 */
CONSTRUCTOR(CIStmtIfBranch *, CIStmtIfBranch, CIExpr *cond, Vec *body);

/**
 *
 * @brief Convert CIStmtIfBranch in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtIfBranch, const CIStmtIfBranch *self);
#endif

/**
 *
 * @brief Free CIStmtIfBranch type.
 */
DESTRUCTOR(CIStmtIfBranch, CIStmtIfBranch *self);

typedef struct CIStmtIf
{
    CIStmtIfBranch *if_;
    Vec *else_ifs; // Vec<CIStmtIfBranch*>*?
    Vec *else_;    // Vec<CIDeclFunctionItem*>*?
} CIStmtIf;

/**
 *
 * @brief Construct CIStmtIf type.
 */
inline CONSTRUCTOR(CIStmtIf,
                   CIStmtIf,
                   CIStmtIfBranch *if_,
                   Vec *else_ifs,
                   Vec *else_)
{
    return (CIStmtIf){ .if_ = if_, .else_ifs = else_ifs, .else_ = else_ };
}

/**
 *
 * @brief Convert CIStmtIf in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtIf, const CIStmtIf *self);
#endif

/**
 *
 * @brief Free CIStmtIf type.
 */
DESTRUCTOR(CIStmtIf, const CIStmtIf *self);

typedef struct CIStmtSwitchCase
{
    CIExpr *value;
} CIStmtSwitchCase;

/**
 *
 * @brief Construct CIStmtSwitchCase type.
 */
inline CONSTRUCTOR(CIStmtSwitchCase, CIStmtSwitchCase, CIExpr *value)
{
    return (CIStmtSwitchCase){ .value = value };
}

/**
 *
 * @brief Convert CIStmtSwitchCase in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtSwitchCase, const CIStmtSwitchCase *self);
#endif

/**
 *
 * @brief Free CIStmtSwitchCase type.
 */
DESTRUCTOR(CIStmtSwitchCase, const CIStmtSwitchCase *self);

typedef struct CIStmtSwitch
{
    CIExpr *expr;
    Vec *body; // Vec<CIDeclFunctionItem*>*
} CIStmtSwitch;

/**
 *
 * @brief Construct CIStmtSwitch type.
 */
inline CONSTRUCTOR(CIStmtSwitch, CIStmtSwitch, CIExpr *expr, Vec *body)
{
    return (CIStmtSwitch){ .expr = expr, .body = body };
}

/**
 *
 * @brief Convert CIStmtSwitch in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtSwitch, const CIStmtSwitch *self);
#endif

/**
 *
 * @brief Free CIStmtSwitch type.
 */
DESTRUCTOR(CIStmtSwitch, const CIStmtSwitch *self);

typedef struct CIStmtWhile
{
    CIExpr *cond;
    Vec *body; // Vec<CIDeclFunctionItem*>*
} CIStmtWhile;

/**
 *
 * @brief Construct CIStmtWhile type.
 */
inline CONSTRUCTOR(CIStmtWhile, CIStmtWhile, CIExpr *cond, Vec *body)
{
    return (CIStmtWhile){ .cond = cond, .body = body };
}

/**
 *
 * @brief Convert CIStmtWhile in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmtWhile, const CIStmtWhile *self);
#endif

/**
 *
 * @brief Free CIStmtWhile type.
 */
DESTRUCTOR(CIStmtWhile, const CIStmtWhile *self);

typedef struct CIStmt
{
    enum CIStmtKind kind;
    union
    {
        CIStmtBlock block;
        CIStmtSwitchCase case_;
        CIStmtDoWhile do_while;
        CIStmtFor for_;
        String *goto_; // String* (&)
        CIStmtIf if_;
        String *label; // String* (&)
        CIExpr *return_;
        CIStmtSwitch switch_;
        CIStmtWhile while_;
    };
} CIStmt;

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_BLOCK).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, block, CIStmtBlock block)
{
    return (CIStmt){ .kind = CI_STMT_KIND_BLOCK, .block = block };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_BREAK).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, break)
{
    return (CIStmt){ .kind = CI_STMT_KIND_BREAK };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_CASE).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, case, CIStmtSwitchCase case_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_CASE, .case_ = case_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_DEFAULT).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, default)
{
    return (CIStmt){ .kind = CI_STMT_KIND_DEFAULT };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_CONTINUE).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, continue)
{
    return (CIStmt){ .kind = CI_STMT_KIND_CONTINUE };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_DO_WHILE).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, do_while, CIStmtDoWhile do_while)
{
    return (CIStmt){ .kind = CI_STMT_KIND_DO_WHILE, .do_while = do_while };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_FOR).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, for, CIStmtFor for_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_FOR, .for_ = for_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_GOTO).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, goto, String *goto_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_GOTO, .goto_ = goto_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_IF).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, if, CIStmtIf if_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_IF, .if_ = if_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_LABEL).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, label, String *label)
{
    return (CIStmt){ .kind = CI_STMT_KIND_LABEL, .label = label };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_RETURN).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, return, CIExpr *return_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_RETURN, .return_ = return_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_SWITCH).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, switch, CIStmtSwitch switch_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_SWITCH, .switch_ = switch_ };
}

/**
 *
 * @brief Construct CIStmt type (CI_STMT_KIND_WHILE).
 */
inline VARIANT_CONSTRUCTOR(CIStmt, CIStmt, while, CIStmtWhile while_)
{
    return (CIStmt){ .kind = CI_STMT_KIND_WHILE, .while_ = while_ };
}

/**
 *
 * @brief Convert CIStmt in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIStmt, const CIStmt *self);
#endif

/**
 *
 * @brief Free CIStmt type.
 */
DESTRUCTOR(CIStmt, const CIStmt *self);

enum CIDeclFunctionItemKind
{
    CI_DECL_FUNCTION_ITEM_KIND_DECL,
    CI_DECL_FUNCTION_ITEM_KIND_EXPR,
    CI_DECL_FUNCTION_ITEM_KIND_STMT,
};

/**
 *
 * @brief Convert CIDeclFunctionItemKind in string.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
char *
IMPL_FOR_DEBUG(to_string,
               CIDeclFunctionItemKind,
               enum CIDeclFunctionItemKind self);
#endif

struct CIDeclFunctionItem
{
    enum CIDeclFunctionItemKind kind;
    union
    {
        CIDecl *decl;
        CIExpr *expr;
        CIStmt stmt;
    };
};

/**
 *
 * @brief Construct CIDeclFunctionItem type (CI_DECL_FUNCTION_ITEM_KIND_DECL).
 */
VARIANT_CONSTRUCTOR(CIDeclFunctionItem *,
                    CIDeclFunctionItem,
                    decl,
                    CIDecl *decl);

/**
 *
 * @brief Construct CIDeclFunctionItem type (CI_DECL_FUNCTION_ITEM_KIND_EXPR).
 */
VARIANT_CONSTRUCTOR(CIDeclFunctionItem *,
                    CIDeclFunctionItem,
                    expr,
                    CIExpr *expr);

/**
 *
 * @brief Construct CIDeclFunctionItem type (CI_DECL_FUNCTION_ITEM_KIND_STMT).
 */
VARIANT_CONSTRUCTOR(CIDeclFunctionItem *,
                    CIDeclFunctionItem,
                    stmt,
                    CIStmt stmt);

/**
 *
 * @brief Convert CIDeclFunctionItem in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, CIDeclFunctionItem, const CIDeclFunctionItem *self);
#endif

/**
 *
 * @brief Free CIDeclFunctionItem type.
 */
DESTRUCTOR(CIDeclFunctionItem, CIDeclFunctionItem *self);

#endif // LILY_CORE_CC_CI_AST_H
