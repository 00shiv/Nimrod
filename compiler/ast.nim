#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# abstract syntax tree + symbol table

import 
  msgs, hashes, nversion, options, strutils, crc, ropes, idents, lists, 
  intsets, idgen

type
  TCallingConvention* = enum 
    ccDefault,                # proc has no explicit calling convention
    ccStdCall,                # procedure is stdcall
    ccCDecl,                  # cdecl
    ccSafeCall,               # safecall
    ccSysCall,                # system call
    ccInline,                 # proc should be inlined
    ccNoInline,               # proc should not be inlined
    ccFastCall,               # fastcall (pass parameters in registers)
    ccClosure,                # proc has a closure
    ccNoConvention            # needed for generating proper C procs sometimes

const 
  CallingConvToStr*: array[TCallingConvention, string] = ["", "stdcall", 
    "cdecl", "safecall", "syscall", "inline", "noinline", "fastcall",
    "closure", "noconv"]

type 
  TNodeKind* = enum # order is extremely important, because ranges are used
                    # to check whether a node belongs to a certain class
    nkNone,               # unknown node kind: indicates an error
                          # Expressions:
                          # Atoms:
    nkEmpty,              # the node is empty
    nkIdent,              # node is an identifier
    nkSym,                # node is a symbol
    nkType,               # node is used for its typ field

    nkCharLit,            # a character literal ''
    nkIntLit,             # an integer literal
    nkInt8Lit,
    nkInt16Lit,
    nkInt32Lit,
    nkInt64Lit,
    nkUIntLit,            # an unsigned integer literal
    nkUInt8Lit,
    nkUInt16Lit,
    nkUInt32Lit,
    nkUInt64Lit,
    nkFloatLit,           # a floating point literal
    nkFloat32Lit,
    nkFloat64Lit,
    nkFloat128Lit,
    nkStrLit,             # a string literal ""
    nkRStrLit,            # a raw string literal r""
    nkTripleStrLit,       # a triple string literal """
    nkNilLit,             # the nil literal
                          # end of atoms
    nkMetaNode_Obsolete,  # difficult to explain; represents itself
                          # (used for macros)
    nkDotCall,            # used to temporarily flag a nkCall node; 
                          # this is used
                          # for transforming ``s.len`` to ``len(s)``

    nkCommand,            # a call like ``p 2, 4`` without parenthesis
    nkCall,               # a call like p(x, y) or an operation like +(a, b)
    nkCallStrLit,         # a call with a string literal 
                          # x"abc" has two sons: nkIdent, nkRStrLit
                          # x"""abc""" has two sons: nkIdent, nkTripleStrLit
    nkInfix,              # a call like (a + b)
    nkPrefix,             # a call like !a
    nkPostfix,            # something like a! (also used for visibility)
    nkHiddenCallConv,     # an implicit type conversion via a type converter

    nkExprEqExpr,         # a named parameter with equals: ''expr = expr''
    nkExprColonExpr,      # a named parameter with colon: ''expr: expr''
    nkIdentDefs,          # a definition like `a, b: typeDesc = expr`
                          # either typeDesc or expr may be nil; used in
                          # formal parameters, var statements, etc.
    nkVarTuple,           # a ``var (a, b) = expr`` construct
    nkPar,                # syntactic (); may be a tuple constructor
    nkObjConstr,          # object constructor: T(a: 1, b: 2)
    nkCurly,              # syntactic {}
    nkCurlyExpr,          # an expression like a{i}
    nkBracket,            # syntactic []
    nkBracketExpr,        # an expression like a[i..j, k]
    nkPragmaExpr,         # an expression like a{.pragmas.}
    nkRange,              # an expression like i..j
    nkDotExpr,            # a.b
    nkCheckedFieldExpr,   # a.b, but b is a field that needs to be checked
    nkDerefExpr,          # a^
    nkIfExpr,             # if as an expression
    nkElifExpr,
    nkElseExpr,
    nkLambda,             # lambda expression
    nkDo,                 # lambda block appering as trailing proc param
    nkAccQuoted,          # `a` as a node

    nkTableConstr,        # a table constructor {expr: expr}
    nkBind,               # ``bind expr`` node
    nkClosedSymChoice,    # symbol choice node; a list of nkSyms (closed)
    nkOpenSymChoice,      # symbol choice node; a list of nkSyms (open)
    nkHiddenStdConv,      # an implicit standard type conversion
    nkHiddenSubConv,      # an implicit type conversion from a subtype
                          # to a supertype
    nkConv,               # a type conversion
    nkCast,               # a type cast
    nkStaticExpr,         # a static expr
    nkAddr,               # a addr expression
    nkHiddenAddr,         # implicit address operator
    nkHiddenDeref,        # implicit ^ operator
    nkObjDownConv,        # down conversion between object types
    nkObjUpConv,          # up conversion between object types
    nkChckRangeF,         # range check for floats
    nkChckRange64,        # range check for 64 bit ints
    nkChckRange,          # range check for ints
    nkStringToCString,    # string to cstring
    nkCStringToString,    # cstring to string
                          # end of expressions

    nkAsgn,               # a = b
    nkFastAsgn,           # internal node for a fast ``a = b``
                          # (no string copy) 
    nkGenericParams,      # generic parameters
    nkFormalParams,       # formal parameters
    nkOfInherit,          # inherited from symbol

    nkImportAs,           # a 'as' b in an import statement
    nkProcDef,            # a proc
    nkMethodDef,          # a method
    nkConverterDef,       # a converter
    nkMacroDef,           # a macro
    nkTemplateDef,        # a template
    nkIteratorDef,        # an iterator

    nkOfBranch,           # used inside case statements
                          # for (cond, action)-pairs
    nkElifBranch,         # used in if statements
    nkExceptBranch,       # an except section
    nkElse,               # an else part
    nkAsmStmt,            # an assembler block
    nkPragma,             # a pragma statement
    nkPragmaBlock,        # a pragma with a block
    nkIfStmt,             # an if statement
    nkWhenStmt,           # a when expression or statement
    nkForStmt,            # a for statement
    nkParForStmt,         # a parallel for statement
    nkWhileStmt,          # a while statement
    nkCaseStmt,           # a case statement
    nkTypeSection,        # a type section (consists of type definitions)
    nkVarSection,         # a var section
    nkLetSection,         # a let section
    nkConstSection,       # a const section
    nkConstDef,           # a const definition
    nkTypeDef,            # a type definition
    nkYieldStmt,          # the yield statement as a tree
    nkTryStmt,            # a try statement
    nkFinally,            # a finally section
    nkRaiseStmt,          # a raise statement
    nkReturnStmt,         # a return statement
    nkBreakStmt,          # a break statement
    nkContinueStmt,       # a continue statement
    nkBlockStmt,          # a block statement
    nkStaticStmt,         # a static statement
    nkDiscardStmt,        # a discard statement
    nkStmtList,           # a list of statements
    nkImportStmt,         # an import statement
    nkImportExceptStmt,   # an import x except a statement
    nkExportStmt,         # an export statement
    nkExportExceptStmt,   # an 'export except' statement
    nkFromStmt,           # a from * import statement
    nkIncludeStmt,        # an include statement
    nkBindStmt,           # a bind statement
    nkMixinStmt,          # a mixin statement
    nkUsingStmt,          # an using statement
    nkCommentStmt,        # a comment statement
    nkStmtListExpr,       # a statement list followed by an expr; this is used
                          # to allow powerful multi-line templates
    nkBlockExpr,          # a statement block ending in an expr; this is used
                          # to allowe powerful multi-line templates that open a
                          # temporary scope
    nkStmtListType,       # a statement list ending in a type; for macros
    nkBlockType,          # a statement block ending in a type; for macros
                          # types as syntactic trees:

    nkWith,               # distinct with `foo`
    nkWithout,            # distinct without `foo`
    
    nkTypeOfExpr,         # type(1+2)
    nkObjectTy,           # object body
    nkTupleTy,            # tuple body
    nkTypeClassTy,        # user-defined type class
    nkStaticTy,           # ``static[T]``
    nkRecList,            # list of object parts
    nkRecCase,            # case section of object
    nkRecWhen,            # when section of object
    nkRefTy,              # ``ref T``
    nkPtrTy,              # ``ptr T``
    nkVarTy,              # ``var T``
    nkConstTy,            # ``const T``
    nkMutableTy,          # ``mutable T``
    nkDistinctTy,         # distinct type
    nkProcTy,             # proc type
    nkIteratorTy,         # iterator type
    nkSharedTy,           # 'shared T'
                          # we use 'nkPostFix' for the 'not nil' addition
    nkEnumTy,             # enum body
    nkEnumFieldDef,       # `ident = expr` in an enumeration
    nkArgList,            # argument list
    nkPattern,            # a special pattern; used for matching
    nkReturnToken,        # token used for interpretation
    nkClosure,            # (prc, env)-pair (internally used for code gen)
    nkGotoState,          # used for the state machine (for iterators)
    nkState,              # give a label to a code section (for iterators)
    nkBreakState,         # special break statement for easier code generation
  TNodeKinds* = set[TNodeKind]

type
  TSymFlag* = enum    # already 32 flags!
    sfUsed,           # read access of sym (for warnings) or simply used
    sfExported,       # symbol is exported from module
    sfFromGeneric,    # symbol is instantiation of a generic; this is needed 
                      # for symbol file generation; such symbols should always
                      # be written into the ROD file
    sfGlobal,         # symbol is at global scope

    sfForward,        # symbol is forward declared
    sfImportc,        # symbol is external; imported
    sfExportc,        # symbol is exported (under a specified name)
    sfVolatile,       # variable is volatile
    sfRegister,       # variable should be placed in a register
    sfPure,           # object is "pure" that means it has no type-information
                      # enum is "pure", its values need qualified access
                      # variable is "pure"; it's an explicit "global"
    sfNoSideEffect,   # proc has no side effects
    sfSideEffect,     # proc may have side effects; cannot prove it has none
    sfMainModule,     # module is the main module
    sfSystemModule,   # module is the system module
    sfNoReturn,       # proc never returns (an exit proc)
    sfAddrTaken,      # the variable's address is taken (ex- or implicitly);
                      # *OR*: a proc is indirectly called (used as first class)
    sfCompilerProc,   # proc is a compiler proc, that is a C proc that is
                      # needed for the code generator
    sfProcvar,        # proc can be passed to a proc var
    sfDiscriminant,   # field is a discriminant in a record/object
    sfDeprecated,     # symbol is deprecated
    sfError,          # usage of symbol should trigger a compile-time error
    sfShadowed,       # a symbol that was shadowed in some inner scope
    sfThread,         # proc will run as a thread
                      # variable is a thread variable
    sfCompileTime,    # proc can be evaluated at compile time
    sfMerge,          # proc can be merged with itself
    sfDeadCodeElim,   # dead code elimination for the module is turned on
    sfBorrow,         # proc is borrowed
    sfInfixCall,      # symbol needs infix call syntax in target language;
                      # for interfacing with C++, JS
    sfNamedParamCall, # symbol needs named parameter call syntax in target
                      # language; for interfacing with Objective C
    sfDiscardable,    # returned value may be discarded implicitly
    sfOverriden,      # proc is overriden
    sfGenSym          # symbol is 'gensym'ed; do not add to symbol table

  TSymFlags* = set[TSymFlag]

const
  sfFakeConst* = sfDeadCodeElim  # const cannot be put into a data section
  sfDispatcher* = sfDeadCodeElim # copied method symbol is the dispatcher
  sfNoInit* = sfMainModule       # don't generate code to init the variable

  sfImmediate* = sfDeadCodeElim
    # macro or template is immediately expanded
    # without considering any possible overloads

  sfDirty* = sfPure
    # template is not hygienic (old styled template)
    # module, compiled from a dirty-buffer

  sfAnon* = sfDiscardable
    # symbol name that was generated by the compiler
    # the compiler will avoid printing such names 
    # in user messages.
      
  sfNoForward* = sfRegister
    # forward declarations are not required (per module)

  sfNoRoot* = sfBorrow # a local variable is provably no root so it doesn't
                       # require RC ops

const
  # getting ready for the future expr/stmt merge
  nkWhen* = nkWhenStmt
  nkWhenExpr* = nkWhenStmt
  nkEffectList* = nkArgList 
  # hacks ahead: an nkEffectList is a node with 4 children:
  exceptionEffects* = 0 # exceptions at position 0
  usesEffects* = 1      # read effects at position 1
  writeEffects* = 2     # write effects at position 2
  tagEffects* = 3       # user defined tag ('gc', 'time' etc.)
  effectListLen* = 4    # list of effects list

type
  TTypeKind* = enum  # order is important!
                     # Don't forget to change hti.nim if you make a change here
                     # XXX put this into an include file to avoid this issue!
    tyNone, tyBool, tyChar,
    tyEmpty, tyArrayConstr, tyNil, tyExpr, tyStmt, tyTypeDesc,
    tyGenericInvokation, # ``T[a, b]`` for types to invoke
    tyGenericBody,       # ``T[a, b, body]`` last parameter is the body
    tyGenericInst,       # ``T[a, b, realInstance]`` instantiated generic type
                         # realInstance will be a concrete type like tyObject
                         # unless this is an instance of a generic alias type.
                         # then realInstance will be the tyGenericInst of the
                         # completely (recursively) resolved alias.
                         
    tyGenericParam,      # ``a`` in the above patterns
    tyDistinct,
    tyEnum,
    tyOrdinal,           # integer types (including enums and boolean)
    tyArray,
    tyObject,
    tyTuple,
    tySet,
    tyRange,
    tyPtr, tyRef,
    tyVar,
    tySequence,
    tyProc,
    tyPointer, tyOpenArray,
    tyString, tyCString, tyForward,
    tyInt, tyInt8, tyInt16, tyInt32, tyInt64, # signed integers
    tyFloat, tyFloat32, tyFloat64, tyFloat128,
    tyUInt, tyUInt8, tyUInt16, tyUInt32, tyUInt64,
    tyBigNum, 
    tyConst, tyMutable, tyVarargs, 
    tyIter, # unused
    tyProxy # used as errornous type (for idetools)
    
    tyBuiltInTypeClass #\
      # Type such as the catch-all object, tuple, seq, etc
    
    tyUserTypeClass #\
      # the body of a user-defined type class

    tyUserTypeClassInst #\
      # Instance of a parametric user-defined type class.
      # Structured similarly to tyGenericInst.
      # tyGenericInst represents concrete types, while
      # this is still a "generic param" that will bind types
      # and resolves them during sigmatch and instantiation.
    
    tyCompositeTypeClass #\
      # Type such as seq[Number]
      # The notes for tyUserTypeClassInst apply here as well 
      # sons[0]: the original expression used by the user.
      # sons[1]: fully expanded and instantiated meta type
      # (potentially following aliases)
    
    tyAnd, tyOr, tyNot #\
      # boolean type classes such as `string|int`,`not seq`,
      # `Sortable and Enumable`, etc
    
    tyAnything #\
      # a type class matching any type
    
    tyStatic #\
      # a value known at compile type (the underlying type is .base)
    
    tyFromExpr #\
      # This is a type representing an expression that depends
      # on generic parameters (the exprsesion is stored in t.n)
      # It will be converted to a real type only during generic
      # instantiation and prior to this it has the potential to
      # be any type.

    tyFieldAccessor #\
      # Expressions such as Type.field (valid in contexts such
      # as the `is` operator and magics like `high` and `low`).
      # Could be lifted to a single argument proc returning the
      # field value.
      # sons[0]: type of containing object or tuple
      # sons[1]: field type
      # .n: nkDotExpr storing the field name
    
static:
  # remind us when TTypeKind stops to fit in a single 64-bit word
  assert TTypeKind.high.ord <= 63

const
  tyPureObject* = tyTuple
  GcTypeKinds* = {tyRef, tySequence, tyString}
  tyError* = tyProxy # as an errornous node should match everything

  tyUnknownTypes* = {tyError, tyFromExpr}

  tyTypeClasses* = {tyBuiltInTypeClass, tyCompositeTypeClass,
                    tyUserTypeClass, tyUserTypeClassInst,
                    tyAnd, tyOr, tyNot, tyAnything}

  tyMetaTypes* = {tyGenericParam, tyTypeDesc, tyExpr} + tyTypeClasses
 
type
  TTypeKinds* = set[TTypeKind]

  TNodeFlag* = enum
    nfNone,
    nfBase2,    # nfBase10 is default, so not needed
    nfBase8,
    nfBase16,
    nfAllConst, # used to mark complex expressions constant; easy to get rid of
                # but unfortunately it has measurable impact for compilation
                # efficiency
    nfTransf,   # node has been transformed
    nfSem       # node has been checked for semantics
    nfLL        # node has gone through lambda lifting
    nfDotField  # the call can use a dot operator
    nfDotSetter # the call can use a setter dot operarator
    nfExplicitCall # x.y() was used instead of x.y
    nfExprCall  # this is an attempt to call a regular expression
    nfIsRef     # this node is a 'ref' node; used for the VM
 
  TNodeFlags* = set[TNodeFlag]
  TTypeFlag* = enum   # keep below 32 for efficiency reasons (now: 28)
    tfVarargs,        # procedure has C styled varargs
    tfNoSideEffect,   # procedure type does not allow side effects
    tfFinal,          # is the object final?
    tfInheritable,    # is the object inheritable?
    tfAcyclic,        # type is acyclic (for GC optimization)
    tfEnumHasHoles,   # enum cannot be mapped into a range
    tfShallow,        # type can be shallow copied on assignment
    tfThread,         # proc type is marked as ``thread``; alias for ``gcsafe``
    tfFromGeneric,    # type is an instantiation of a generic; this is needed
                      # because for instantiations of objects, structural
                      # type equality has to be used
    tfUnresolved,     # marks unresolved typedesc/static params: e.g.
                      # proc foo(T: typedesc, list: seq[T]): var T
                      # proc foo(L: static[int]): array[L, int]
                      # can be attached to ranges to indicate that the range
                      # depends on unresolved static params.
    tfRetType,        # marks return types in proc (used to detect type classes 
                      # used as return types for return type inference)
    tfCapturesEnv,    # whether proc really captures some environment
    tfByCopy,         # pass object/tuple by copy (C backend)
    tfByRef,          # pass object/tuple by reference (C backend)
    tfIterator,       # type is really an iterator, not a tyProc
    tfShared,         # type is 'shared'
    tfNotNil,         # type cannot be 'nil'
    
    tfNeedsInit,      # type constains a "not nil" constraint somewhere or some
                      # other type so that it requires inititalization
    tfHasShared,      # type constains a "shared" constraint modifier somewhere
    tfHasMeta,        # type contains "wildcard" sub-types such as generic params
                      # or other type classes
    tfHasGCedMem,     # type contains GC'ed memory
    tfPacked
    tfHasStatic
    tfGenericTypeParam
    tfImplicitTypeParam
    tfWildcard        # consider a proc like foo[T, I](x: Type[T, I])
                      # T and I here can bind to both typedesc and static types
                      # before this is determined, we'll consider them to be a
                      # wildcard type.
    tfGuarded         # guarded pointer
    tfBorrowDot       # distinct type borrows '.'

  TTypeFlags* = set[TTypeFlag]

  TSymKind* = enum        # the different symbols (start with the prefix sk);
                          # order is important for the documentation generator!
    skUnknown,            # unknown symbol: used for parsing assembler blocks
                          # and first phase symbol lookup in generics
    skConditional,        # symbol for the preprocessor (may become obsolete)
    skDynLib,             # symbol represents a dynamic library; this is used
                          # internally; it does not exist in Nimrod code
    skParam,              # a parameter
    skGenericParam,       # a generic parameter; eq in ``proc x[eq=`==`]()``
    skTemp,               # a temporary variable (introduced by compiler)
    skModule,             # module identifier
    skType,               # a type
    skVar,                # a variable
    skLet,                # a 'let' symbol
    skConst,              # a constant
    skResult,             # special 'result' variable
    skProc,               # a proc
    skMethod,             # a method
    skIterator,           # an inline iterator
    skClosureIterator,    # a resumable closure iterator
    skConverter,          # a type converter
    skMacro,              # a macro
    skTemplate,           # a template; currently also misused for user-defined
                          # pragmas
    skField,              # a field in a record or object
    skEnumField,          # an identifier in an enum
    skForVar,             # a for loop variable
    skLabel,              # a label (for block statement)
    skStub,               # symbol is a stub and not yet loaded from the ROD
                          # file (it is loaded on demand, which may
                          # mean: never)
    skPackage             # symbol is a package (used for canonicalization)
  TSymKinds* = set[TSymKind]

const
  routineKinds* = {skProc, skMethod, skIterator, skClosureIterator,
                   skConverter, skMacro, skTemplate}
  tfIncompleteStruct* = tfVarargs
  tfUncheckedArray* = tfVarargs
  tfUnion* = tfNoSideEffect
  tfGcSafe* = tfThread
  tfObjHasKids* = tfEnumHasHoles
  skError* = skUnknown
  
  # type flags that are essential for type equality:
  eqTypeFlags* = {tfIterator, tfShared, tfNotNil}

type
  TMagic* = enum # symbols that require compiler magic:
    mNone,
    mDefined, mDefinedInScope, mCompiles,
    mLow, mHigh, mSizeOf, mTypeTrait, mIs, mOf,
    mEcho, mShallowCopy, mSlurp, mStaticExec,
    mParseExprToAst, mParseStmtToAst, mExpandToAst, mQuoteAst,
    mUnaryLt, mSucc, 
    mPred, mInc, mDec, mOrd, mNew, mNewFinalize, mNewSeq, mLengthOpenArray, 
    mLengthStr, mLengthArray, mLengthSeq, mIncl, mExcl, mCard, mChr, mGCref, 
    mGCunref, mAddI, mSubI, mMulI, mDivI, mModI, mAddI64, mSubI64, mMulI64, 
    mDivI64, mModI64,
    mAddF64, mSubF64, mMulF64, mDivF64,
    mShrI, mShlI, mBitandI, mBitorI, mBitxorI, mMinI, mMaxI, 
    mShrI64, mShlI64, mBitandI64, mBitorI64, mBitxorI64, mMinI64, mMaxI64,
    mMinF64, mMaxF64, mAddU, mSubU, mMulU, 
    mDivU, mModU, mEqI, mLeI,
    mLtI, 
    mEqI64, mLeI64, mLtI64, mEqF64, mLeF64, mLtF64, 
    mLeU, mLtU, mLeU64, mLtU64, 
    mEqEnum, mLeEnum, mLtEnum, mEqCh, mLeCh, mLtCh, mEqB, mLeB, mLtB, mEqRef, 
    mEqUntracedRef, mLePtr, mLtPtr, mEqCString, mXor, mEqProc, mUnaryMinusI,
    mUnaryMinusI64, mAbsI, mAbsI64, mNot, 
    mUnaryPlusI, mBitnotI, mUnaryPlusI64, 
    mBitnotI64, mUnaryPlusF64, mUnaryMinusF64, mAbsF64, mZe8ToI, mZe8ToI64, 
    mZe16ToI, mZe16ToI64, mZe32ToI64, mZeIToI64, mToU8, mToU16, mToU32, 
    mToFloat, mToBiggestFloat, mToInt, mToBiggestInt, mCharToStr, mBoolToStr, 
    mIntToStr, mInt64ToStr, mFloatToStr, mCStrToStr, mStrToStr, mEnumToStr, 
    mAnd, mOr, mEqStr, mLeStr, mLtStr, mEqSet, mLeSet, mLtSet, mMulSet, 
    mPlusSet, mMinusSet, mSymDiffSet, mConStrStr, mConArrArr, mConArrT, 
    mConTArr, mConTT, mSlice,
    mFields, mFieldPairs, mOmpParFor,
    mAppendStrCh, mAppendStrStr, mAppendSeqElem,
    mInRange, mInSet, mRepr, mExit, mSetLengthStr, mSetLengthSeq,
    mIsPartOf, mAstToStr, mParallel,
    mSwap, mIsNil, mArrToSeq, mCopyStr, mCopyStrLast,
    mNewString, mNewStringOfCap, mParseBiggestFloat,
    mReset,
    mArray, mOpenArray, mRange, mSet, mSeq, mVarargs,
    mOrdinal,
    mInt, mInt8, mInt16, mInt32, mInt64,
    mUInt, mUInt8, mUInt16, mUInt32, mUInt64,
    mFloat, mFloat32, mFloat64, mFloat128,
    mBool, mChar, mString, mCstring,
    mPointer, mEmptySet, mIntSetBaseType, mNil, mExpr, mStmt, mTypeDesc,
    mVoidType, mPNimrodNode, mShared, mGuarded, mLock, mSpawn, mDeepCopy,
    mIsMainModule, mCompileDate, mCompileTime, mNimrodVersion, mNimrodMajor,
    mNimrodMinor, mNimrodPatch, mCpuEndian, mHostOS, mHostCPU, mAppType,
    mNaN, mInf, mNegInf,
    mCompileOption, mCompileOptionArg,
    mNLen, mNChild, mNSetChild, mNAdd, mNAddMultiple, mNDel, mNKind,
    mNIntVal, mNFloatVal, mNSymbol, mNIdent, mNGetType, mNStrVal, mNSetIntVal,
    mNSetFloatVal, mNSetSymbol, mNSetIdent, mNSetType, mNSetStrVal, mNLineInfo,
    mNNewNimNode, mNCopyNimNode, mNCopyNimTree, mStrToIdent, mIdentToStr,
    mNBindSym, mLocals, mNCallSite,
    mEqIdent, mEqNimrodNode, mNHint, mNWarning, mNError,
    mInstantiationInfo, mGetTypeInfo, mNGenSym

# things that we can evaluate safely at compile time, even if not asked for it:
const
  ctfeWhitelist* = {mNone, mUnaryLt, mSucc, 
    mPred, mInc, mDec, mOrd, mLengthOpenArray, 
    mLengthStr, mLengthArray, mLengthSeq, mIncl, mExcl, mCard, mChr, 
    mAddI, mSubI, mMulI, mDivI, mModI, mAddI64, mSubI64, mMulI64, 
    mDivI64, mModI64, mAddF64, mSubF64, mMulF64, mDivF64,
    mShrI, mShlI, mBitandI, mBitorI, mBitxorI, mMinI, mMaxI, 
    mShrI64, mShlI64, mBitandI64, mBitorI64, mBitxorI64, mMinI64, mMaxI64,
    mMinF64, mMaxF64, mAddU, mSubU, mMulU, 
    mDivU, mModU, mEqI, mLeI,
    mLtI, 
    mEqI64, mLeI64, mLtI64, mEqF64, mLeF64, mLtF64, 
    mLeU, mLtU, mLeU64, mLtU64, 
    mEqEnum, mLeEnum, mLtEnum, mEqCh, mLeCh, mLtCh, mEqB, mLeB, mLtB, mEqRef, 
    mEqProc, mEqUntracedRef, mLePtr, mLtPtr, mEqCString, mXor, mUnaryMinusI, 
    mUnaryMinusI64, mAbsI, mAbsI64, mNot, 
    mUnaryPlusI, mBitnotI, mUnaryPlusI64, 
    mBitnotI64, mUnaryPlusF64, mUnaryMinusF64, mAbsF64, mZe8ToI, mZe8ToI64, 
    mZe16ToI, mZe16ToI64, mZe32ToI64, mZeIToI64, mToU8, mToU16, mToU32, 
    mToFloat, mToBiggestFloat, mToInt, mToBiggestInt, mCharToStr, mBoolToStr, 
    mIntToStr, mInt64ToStr, mFloatToStr, mCStrToStr, mStrToStr, mEnumToStr, 
    mAnd, mOr, mEqStr, mLeStr, mLtStr, mEqSet, mLeSet, mLtSet, mMulSet, 
    mPlusSet, mMinusSet, mSymDiffSet, mConStrStr, mConArrArr, mConArrT, 
    mConTArr, mConTT,
    mAppendStrCh, mAppendStrStr, mAppendSeqElem, 
    mInRange, mInSet, mRepr,
    mCopyStr, mCopyStrLast}
  # magics that require special semantic checking and
  # thus cannot be overloaded (also documented in the spec!):
  SpecialSemMagics* = {
    mDefined, mDefinedInScope, mCompiles, mLow, mHigh, mSizeOf, mIs, mOf, 
    mEcho, mShallowCopy, mExpandToAst, mParallel, mSpawn}

type
  PNode* = ref TNode
  TNodeSeq* = seq[PNode]
  PType* = ref TType
  PSym* = ref TSym
  TNode*{.final, acyclic.} = object # on a 32bit machine, this takes 32 bytes
    when defined(useNodeIds):
      id*: int
    typ*: PType
    info*: TLineInfo
    flags*: TNodeFlags
    case kind*: TNodeKind
    of nkCharLit..nkUInt64Lit:
      intVal*: BiggestInt
    of nkFloatLit..nkFloat128Lit:
      floatVal*: BiggestFloat
    of nkStrLit..nkTripleStrLit:
      strVal*: string
    of nkSym: 
      sym*: PSym
    of nkIdent: 
      ident*: PIdent
    else:
      sons*: TNodeSeq
    comment*: string
  
  TSymSeq* = seq[PSym]
  TStrTable* = object         # a table[PIdent] of PSym
    counter*: int
    data*: TSymSeq
  
  # -------------- backend information -------------------------------
  TLocKind* = enum 
    locNone,                  # no location
    locTemp,                  # temporary location
    locLocalVar,              # location is a local variable
    locGlobalVar,             # location is a global variable
    locParam,                 # location is a parameter
    locField,                 # location is a record field
    locArrayElem,             # location is an array element
    locExpr,                  # "location" is really an expression
    locProc,                  # location is a proc (an address of a procedure)
    locData,                  # location is a constant
    locCall,                  # location is a call expression
    locOther                  # location is something other
  TLocFlag* = enum 
    lfIndirect,               # backend introduced a pointer
    lfParamCopy,              # backend introduced a parameter copy (LLVM)
    lfNoDeepCopy,             # no need for a deep copy
    lfNoDecl,                 # do not declare it in C
    lfDynamicLib,             # link symbol to dynamic library
    lfExportLib,              # export symbol for dynamic library generation
    lfHeader,                 # include header file for symbol
    lfImportCompilerProc      # ``importc`` of a compilerproc
  TStorageLoc* = enum 
    OnUnknown,                # location is unknown (stack, heap or static)
    OnStack,                  # location is on hardware stack
    OnHeap                    # location is on heap or global
                              # (reference counting needed)
  TLocFlags* = set[TLocFlag]
  TLoc*{.final.} = object     
    k*: TLocKind              # kind of location
    s*: TStorageLoc
    flags*: TLocFlags         # location's flags
    t*: PType                 # type of location
    r*: PRope                 # rope value of location (code generators)
    heapRoot*: PRope          # keeps track of the enclosing heap object that
                              # owns this location (required by GC algorithms
                              # employing heap snapshots or sliding views)
    a*: int                   # location's "address", i.e. slot for temporaries

  # ---------------- end of backend information ------------------------------

  TLibKind* = enum 
    libHeader, libDynamic
  TLib* = object of lists.TListEntry # also misused for headers!
    kind*: TLibKind
    generated*: bool          # needed for the backends:
    isOverriden*: bool
    name*: PRope
    path*: PNode              # can be a string literal!
  
  TInstantiation* = object
    sym*: PSym
    concreteTypes*: seq[PType]
    usedBy*: seq[int32]       # list of modules using the generic
                              # needed in caas mode for purging the cache
                              # XXX: it's possible to switch to a 
                              # simple ref count here
  
  PInstantiation* = ref TInstantiation
 
  TScope* = object
    depthLevel*: int
    symbols*: TStrTable
    usingSyms*: seq[PNode]
    parent*: PScope

  PScope* = ref TScope

  PLib* = ref TLib
  TSym* {.acyclic.} = object of TIdObj
    # proc and type instantiations are cached in the generic symbol
    case kind*: TSymKind
    of skType, skGenericParam:
      typeInstCache*: seq[PType]
      typScope*: PScope
    of routineKinds:
      procInstCache*: seq[PInstantiation]
      scope*: PScope          # the scope where the proc was defined
    of skModule:
      # modules keep track of the generic symbols they use from other modules.
      # this is because in incremental compilation, when a module is about to
      # be replaced with a newer version, we must decrement the usage count
      # of all previously used generics.
      # For 'import as' we copy the module symbol but shallowCopy the 'tab'
      # and set the 'usedGenerics' to ... XXX gah! Better set module.name
      # instead? But this doesn't work either. --> We need an skModuleAlias?
      # No need, just leave it as skModule but set the owner accordingly and
      # check for the owner when touching 'usedGenerics'.
      usedGenerics*: seq[PInstantiation]
      tab*: TStrTable         # interface table for modules
    else: nil

    magic*: TMagic
    typ*: PType
    name*: PIdent
    info*: TLineInfo
    owner*: PSym
    flags*: TSymFlags
    ast*: PNode               # syntax tree of proc, iterator, etc.:
                              # the whole proc including header; this is used
                              # for easy generation of proper error messages
                              # for variant record fields the discriminant
                              # expression
                              # for modules, it's a placeholder for compiler
                              # generated code that will be appended to the
                              # module after the sem pass (see appendToModule)
    options*: TOptions
    position*: int            # used for many different things:
                              # for enum fields its position;
                              # for fields its offset
                              # for parameters its position
                              # for a conditional:
                              # 1 iff the symbol is defined, else 0
                              # (or not in symbol table)
                              # for modules, an unique index corresponding
                              # to the module's fileIdx
                              # for variables a slot index for the evaluator
                              # for routines a superop-ID
    offset*: int              # offset of record field
    loc*: TLoc
    annex*: PLib              # additional fields (seldom used, so we use a
                              # reference to another object to safe space)
    constraint*: PNode        # additional constraints like 'lit|result'; also
                              # misused for the codegenDecl pragma in the hope
                              # it won't cause problems
  
  TTypeSeq* = seq[PType]
  TType* {.acyclic.} = object of TIdObj # \
                              # types are identical iff they have the
                              # same id; there may be multiple copies of a type
                              # in memory!
    kind*: TTypeKind          # kind of type
    callConv*: TCallingConvention # for procs
    flags*: TTypeFlags        # flags of the type
    sons*: TTypeSeq           # base types, etc.
    n*: PNode                 # node for types:
                              # for range types a nkRange node
                              # for record types a nkRecord node
                              # for enum types a list of symbols
                              # for tyInt it can be the int literal
                              # for procs and tyGenericBody, it's the
                              # the body of the user-defined type class
                              # formal param list
                              # else: unused
    owner*: PSym              # the 'owner' of the type
    sym*: PSym                # types have the sym associated with them
                              # it is used for converting types to strings
    destructor*: PSym         # destructor. warning: nil here may not necessary
                              # mean that there is no destructor.
                              # see instantiateDestructor in semdestruct.nim
    deepCopy*: PSym           # overriden 'deepCopy' operation
    size*: BiggestInt         # the size of the type in bytes
                              # -1 means that the size is unkwown
    align*: int               # the type's alignment requirements
    loc*: TLoc

  TPair*{.final.} = object 
    key*, val*: PObject

  TPairSeq* = seq[TPair]
  TTable*{.final.} = object   # the same as table[PObject] of PObject
    counter*: int
    data*: TPairSeq

  TIdPair*{.final.} = object 
    key*: PIdObj
    val*: PObject

  TIdPairSeq* = seq[TIdPair]
  TIdTable*{.final.} = object # the same as table[PIdent] of PObject
    counter*: int
    data*: TIdPairSeq

  TIdNodePair*{.final.} = object 
    key*: PIdObj
    val*: PNode

  TIdNodePairSeq* = seq[TIdNodePair]
  TIdNodeTable*{.final.} = object # the same as table[PIdObj] of PNode
    counter*: int
    data*: TIdNodePairSeq

  TNodePair*{.final.} = object 
    h*: THash                 # because it is expensive to compute!
    key*: PNode
    val*: int

  TNodePairSeq* = seq[TNodePair]
  TNodeTable*{.final.} = object # the same as table[PNode] of int;
                                # nodes are compared by structure!
    counter*: int
    data*: TNodePairSeq

  TObjectSeq* = seq[PObject]
  TObjectSet*{.final.} = object 
    counter*: int
    data*: TObjectSeq

  TImplication* = enum
    impUnknown, impNo, impYes

# BUGFIX: a module is overloadable so that a proc can have the
# same name as an imported module. This is necessary because of
# the poor naming choices in the standard library.

const 
  OverloadableSyms* = {skProc, skMethod, skIterator, skClosureIterator,
    skConverter, skModule, skTemplate, skMacro}

  GenericTypes*: TTypeKinds = {tyGenericInvokation, tyGenericBody, 
    tyGenericParam}
  
  StructuralEquivTypes*: TTypeKinds = {tyArrayConstr, tyNil, tyTuple, tyArray, 
    tySet, tyRange, tyPtr, tyRef, tyVar, tySequence, tyProc, tyOpenArray,
    tyVarargs}
  
  ConcreteTypes*: TTypeKinds = { # types of the expr that may occur in::
                                 # var x = expr
    tyBool, tyChar, tyEnum, tyArray, tyObject, 
    tySet, tyTuple, tyRange, tyPtr, tyRef, tyVar, tySequence, tyProc,
    tyPointer, 
    tyOpenArray, tyString, tyCString, tyInt..tyInt64, tyFloat..tyFloat128,
    tyUInt..tyUInt64}
  IntegralTypes* = {tyBool, tyChar, tyEnum, tyInt..tyInt64,
    tyFloat..tyFloat128, tyUInt..tyUInt64}
  ConstantDataTypes*: TTypeKinds = {tyArrayConstr, tyArray, tySet, 
                                    tyTuple, tySequence}
  NilableTypes*: TTypeKinds = {tyPointer, tyCString, tyRef, tyPtr, tySequence,
    tyProc, tyString, tyError}
  ExportableSymKinds* = {skVar, skConst, skProc, skMethod, skType,
    skIterator, skClosureIterator,
    skMacro, skTemplate, skConverter, skEnumField, skLet, skStub}
  PersistentNodeFlags*: TNodeFlags = {nfBase2, nfBase8, nfBase16,
                                      nfDotSetter, nfDotField,
                                      nfIsRef}
  namePos* = 0
  patternPos* = 1    # empty except for term rewriting macros
  genericParamsPos* = 2
  paramsPos* = 3
  pragmasPos* = 4
  optimizedCodePos* = 5  # will be used for exception tracking
  bodyPos* = 6       # position of body; use rodread.getBody() instead!
  resultPos* = 7
  dispatcherPos* = 8 # caution: if method has no 'result' it can be position 7!

  nkCallKinds* = {nkCall, nkInfix, nkPrefix, nkPostfix,
                  nkCommand, nkCallStrLit, nkHiddenCallConv}
  nkIdentKinds* = {nkIdent, nkSym, nkAccQuoted, nkOpenSymChoice,
                   nkClosedSymChoice}

  nkLiterals* = {nkCharLit..nkTripleStrLit}
  nkLambdaKinds* = {nkLambda, nkDo}
  declarativeDefs* = {nkProcDef, nkMethodDef, nkIteratorDef, nkConverterDef}
  procDefs* = nkLambdaKinds + declarativeDefs

  nkSymChoices* = {nkClosedSymChoice, nkOpenSymChoice}
  nkStrKinds* = {nkStrLit..nkTripleStrLit}

  skLocalVars* = {skVar, skLet, skForVar, skParam, skResult}
  skProcKinds* = {skProc, skTemplate, skMacro, skIterator, skClosureIterator,
                  skMethod, skConverter}

  skIterators* = {skIterator, skClosureIterator}

  lfFullExternalName* = lfParamCopy # \
    # only used when 'gCmd == cmdPretty': Indicates that the symbol has been
    # imported via 'importc: "fullname"' and no format string.

# creator procs:
proc newSym*(symKind: TSymKind, name: PIdent, owner: PSym,
             info: TLineInfo): PSym
proc newType*(kind: TTypeKind, owner: PSym): PType
proc newNode*(kind: TNodeKind): PNode
proc newIntNode*(kind: TNodeKind, intVal: BiggestInt): PNode
proc newIntTypeNode*(kind: TNodeKind, intVal: BiggestInt, typ: PType): PNode
proc newFloatNode*(kind: TNodeKind, floatVal: BiggestFloat): PNode
proc newStrNode*(kind: TNodeKind, strVal: string): PNode
proc newIdentNode*(ident: PIdent, info: TLineInfo): PNode
proc newSymNode*(sym: PSym): PNode
proc newNodeI*(kind: TNodeKind, info: TLineInfo): PNode
proc newNodeIT*(kind: TNodeKind, info: TLineInfo, typ: PType): PNode
proc initStrTable*(x: var TStrTable)
proc initTable*(x: var TTable)
proc initIdTable*(x: var TIdTable)
proc initObjectSet*(x: var TObjectSet)
proc initIdNodeTable*(x: var TIdNodeTable)
proc initNodeTable*(x: var TNodeTable)
  
# copy procs:
proc copyType*(t: PType, owner: PSym, keepId: bool): PType
proc copySym*(s: PSym, keepId: bool = false): PSym
proc assignType*(dest, src: PType)
proc copyStrTable*(dest: var TStrTable, src: TStrTable)
proc copyTable*(dest: var TTable, src: TTable)
proc copyObjectSet*(dest: var TObjectSet, src: TObjectSet)
proc copyIdTable*(dest: var TIdTable, src: TIdTable)
proc sonsLen*(n: PNode): int {.inline.}
proc sonsLen*(n: PType): int {.inline.}
proc lastSon*(n: PNode): PNode {.inline.}
proc lastSon*(n: PType): PType {.inline.}
proc newSons*(father: PNode, length: int)
proc newSons*(father: PType, length: int)
proc addSon*(father, son: PNode)
proc delSon*(father: PNode, idx: int)
proc hasSonWith*(n: PNode, kind: TNodeKind): bool
proc hasSubnodeWith*(n: PNode, kind: TNodeKind): bool
proc replaceSons*(n: PNode, oldKind, newKind: TNodeKind)
proc copyNode*(src: PNode): PNode
  # does not copy its sons!
proc copyTree*(src: PNode): PNode
  # does copy its sons!

proc isCallExpr*(n: PNode): bool =
  result = n.kind in nkCallKinds

proc discardSons*(father: PNode)

proc len*(n: PNode): int {.inline.} =
  if isNil(n.sons): result = 0
  else: result = len(n.sons)
  
proc safeLen*(n: PNode): int {.inline.} =
  ## works even for leaves.
  if n.kind in {nkNone..nkNilLit} or isNil(n.sons): result = 0
  else: result = len(n.sons)
  
proc add*(father, son: PNode) =
  assert son != nil
  if isNil(father.sons): father.sons = @[]
  add(father.sons, son)
  
proc `[]`*(n: PNode, i: int): PNode {.inline.} =
  result = n.sons[i]

# son access operators with support for negative indices
template `{}`*(n: PNode, i: int): expr = n[i -| n]
template `{}=`*(n: PNode, i: int, s: PNode): stmt =
  n.sons[i -| n] = s
  
var emptyNode* = newNode(nkEmpty)
# There is a single empty node that is shared! Do not overwrite it!

var anyGlobal* = newSym(skVar, getIdent("*"), nil, unknownLineInfo())

proc isMetaType*(t: PType): bool =
  return t.kind in tyMetaTypes or
         (t.kind == tyStatic and t.n == nil) or
         tfHasMeta in t.flags

proc linkTo*(t: PType, s: PSym): PType {.discardable.} =
  t.sym = s
  s.typ = t
  result = t

proc linkTo*(s: PSym, t: PType): PSym {.discardable.} =
  t.sym = s
  s.typ = t
  result = s

template fileIdx*(c: PSym): int32 =
  # XXX: this should be used only on module symbols
  c.position.int32

template filename*(c: PSym): string =
  # XXX: this should be used only on module symbols
  c.position.int32.toFilename

proc appendToModule*(m: PSym, n: PNode) =
  ## The compiler will use this internally to add nodes that will be
  ## appended to the module after the sem pass
  if m.ast == nil:
    m.ast = newNode(nkStmtList)
    m.ast.sons = @[n]
  else:
    assert m.ast.kind == nkStmtList
    m.ast.sons.add(n)
  
const                         # for all kind of hash tables:
  GrowthFactor* = 2           # must be power of 2, > 0
  StartSize* = 8              # must be power of 2, > 0

proc copyStrTable(dest: var TStrTable, src: TStrTable) = 
  dest.counter = src.counter
  if isNil(src.data): return 
  setLen(dest.data, len(src.data))
  for i in countup(0, high(src.data)): dest.data[i] = src.data[i]
  
proc copyIdTable(dest: var TIdTable, src: TIdTable) = 
  dest.counter = src.counter
  if isNil(src.data): return 
  newSeq(dest.data, len(src.data))
  for i in countup(0, high(src.data)): dest.data[i] = src.data[i]
  
proc copyTable(dest: var TTable, src: TTable) = 
  dest.counter = src.counter
  if isNil(src.data): return 
  setLen(dest.data, len(src.data))
  for i in countup(0, high(src.data)): dest.data[i] = src.data[i]
  
proc copyObjectSet(dest: var TObjectSet, src: TObjectSet) = 
  dest.counter = src.counter
  if isNil(src.data): return 
  setLen(dest.data, len(src.data))
  for i in countup(0, high(src.data)): dest.data[i] = src.data[i]
  
proc discardSons(father: PNode) = 
  father.sons = nil

when defined(useNodeIds):
  const nodeIdToDebug* = 310841 # 612794
  #612840 # 612905 # 614635 # 614637 # 614641
  # 423408
  #429107 # 430443 # 441048 # 441090 # 441153
  var gNodeId: int

proc newNode(kind: TNodeKind): PNode = 
  new(result)
  result.kind = kind
  #result.info = UnknownLineInfo() inlined:
  result.info.fileIndex = int32(- 1)
  result.info.col = int16(- 1)
  result.info.line = int16(- 1)
  when defined(useNodeIds):
    result.id = gNodeId
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()
    inc gNodeId

proc newIntNode(kind: TNodeKind, intVal: BiggestInt): PNode = 
  result = newNode(kind)
  result.intVal = intVal

proc newIntTypeNode(kind: TNodeKind, intVal: BiggestInt, typ: PType): PNode = 
  result = newIntNode(kind, intVal)
  result.typ = typ

proc newFloatNode(kind: TNodeKind, floatVal: BiggestFloat): PNode = 
  result = newNode(kind)
  result.floatVal = floatVal

proc newStrNode(kind: TNodeKind, strVal: string): PNode = 
  result = newNode(kind)
  result.strVal = strVal

proc withInfo*(n: PNode, info: TLineInfo): PNode =
  n.info = info
  return n

proc newIdentNode(ident: PIdent, info: TLineInfo): PNode = 
  result = newNode(nkIdent)
  result.ident = ident
  result.info = info

proc newSymNode(sym: PSym): PNode = 
  result = newNode(nkSym)
  result.sym = sym
  result.typ = sym.typ
  result.info = sym.info

proc newSymNode*(sym: PSym, info: TLineInfo): PNode = 
  result = newNode(nkSym)
  result.sym = sym
  result.typ = sym.typ
  result.info = info

proc newNodeI(kind: TNodeKind, info: TLineInfo): PNode =
  new(result)
  result.kind = kind
  result.info = info
  when defined(useNodeIds):
    result.id = gNodeId
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()
    inc gNodeId

proc newNodeI*(kind: TNodeKind, info: TLineInfo, children: int): PNode =
  new(result)
  result.kind = kind
  result.info = info
  if children > 0:
    newSeq(result.sons, children)
  when defined(useNodeIds):
    result.id = gNodeId
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()
    inc gNodeId

proc newNode*(kind: TNodeKind, info: TLineInfo, sons: TNodeSeq = @[],
             typ: PType = nil): PNode =
  new(result)
  result.kind = kind
  result.info = info
  result.typ = typ
  # XXX use shallowCopy here for ownership transfer:
  result.sons = sons
  when defined(useNodeIds):
    result.id = gNodeId
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()
    inc gNodeId

proc newNodeIT(kind: TNodeKind, info: TLineInfo, typ: PType): PNode = 
  result = newNode(kind)
  result.info = info
  result.typ = typ

var emptyParams = newNode(nkFormalParams)
emptyParams.addSon(emptyNode)

proc newProcNode*(kind: TNodeKind, info: TLineInfo, body: PNode,
                 params = emptyParams,
                 name, pattern, genericParams,
                 pragmas, exceptions = ast.emptyNode): PNode =
  result = newNodeI(kind, info)
  result.sons = @[name, pattern, genericParams, params,
                  pragmas, exceptions, body]


proc newType(kind: TTypeKind, owner: PSym): PType = 
  new(result)
  result.kind = kind
  result.owner = owner
  result.size = - 1
  result.align = 2            # default alignment
  result.id = getID()
  when debugIds:
    registerId(result)
  #if result.id < 2000 then
  #  MessageOut(typeKindToStr[kind] & ' has id: ' & toString(result.id))
  
proc mergeLoc(a: var TLoc, b: TLoc) =
  if a.k == low(a.k): a.k = b.k
  if a.s == low(a.s): a.s = b.s
  a.flags = a.flags + b.flags
  if a.t == nil: a.t = b.t
  if a.r == nil: a.r = b.r
  if a.a == 0: a.a = b.a
  
proc assignType(dest, src: PType) = 
  dest.kind = src.kind
  dest.flags = src.flags
  dest.callConv = src.callConv
  dest.n = src.n
  dest.size = src.size
  dest.align = src.align
  dest.destructor = src.destructor
  dest.deepCopy = src.deepCopy
  # this fixes 'type TLock = TSysLock':
  if src.sym != nil:
    if dest.sym != nil:
      dest.sym.flags = dest.sym.flags + src.sym.flags
      if dest.sym.annex == nil: dest.sym.annex = src.sym.annex
      mergeLoc(dest.sym.loc, src.sym.loc)
    else:
      dest.sym = src.sym
  newSons(dest, sonsLen(src))
  for i in countup(0, sonsLen(src) - 1): dest.sons[i] = src.sons[i]
  
proc copyType(t: PType, owner: PSym, keepId: bool): PType = 
  result = newType(t.kind, owner)
  assignType(result, t)
  if keepId: 
    result.id = t.id
  else: 
    when debugIds: registerId(result)
  result.sym = t.sym          # backend-info should not be copied
  
proc copySym(s: PSym, keepId: bool = false): PSym = 
  result = newSym(s.kind, s.name, s.owner, s.info)
  #result.ast = nil            # BUGFIX; was: s.ast which made problems
  result.typ = s.typ
  if keepId:
    result.id = s.id
  else: 
    result.id = getID()
    when debugIds: registerId(result)
  result.flags = s.flags
  result.magic = s.magic
  if s.kind == skModule:
    copyStrTable(result.tab, s.tab)
  result.options = s.options
  result.position = s.position
  result.loc = s.loc
  result.annex = s.annex      # BUGFIX

proc createModuleAlias*(s: PSym, newIdent: PIdent, info: TLineInfo): PSym =
  result = newSym(s.kind, newIdent, s.owner, info)
  # keep ID!
  result.ast = s.ast
  result.id = s.id
  result.flags = s.flags
  system.shallowCopy(result.tab, s.tab)
  result.options = s.options
  result.position = s.position
  result.loc = s.loc
  result.annex = s.annex
  # XXX once usedGenerics is used, ensure module aliases keep working!
  assert s.usedGenerics == nil
  
proc newSym(symKind: TSymKind, name: PIdent, owner: PSym,
            info: TLineInfo): PSym = 
  # generates a symbol and initializes the hash field too
  new(result)
  result.name = name
  result.kind = symKind
  result.flags = {}
  result.info = info
  result.options = gOptions
  result.owner = owner
  result.offset = - 1
  result.id = getID()
  when debugIds: 
    registerId(result)
  #if result.id < 2000:
  #  MessageOut(name.s & " has id: " & toString(result.id))
  
proc initStrTable(x: var TStrTable) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc newStrTable*: TStrTable =
  initStrTable(result)

proc initTable(x: var TTable) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc initIdTable(x: var TIdTable) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc initObjectSet(x: var TObjectSet) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc initIdNodeTable(x: var TIdNodeTable) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc initNodeTable(x: var TNodeTable) = 
  x.counter = 0
  newSeq(x.data, StartSize)

proc sonsLen(n: PType): int = 
  if isNil(n.sons): result = 0
  else: result = len(n.sons)

proc len*(n: PType): int = 
  if isNil(n.sons): result = 0
  else: result = len(n.sons)
  
proc newSons(father: PType, length: int) = 
  if isNil(father.sons): 
    newSeq(father.sons, length)
  else:
    setLen(father.sons, length)

proc sonsLen(n: PNode): int = 
  if isNil(n.sons): result = 0
  else: result = len(n.sons)
  
proc newSons(father: PNode, length: int) = 
  if isNil(father.sons): 
    newSeq(father.sons, length)
  else:
    setLen(father.sons, length)

proc skipTypes*(t: PType, kinds: TTypeKinds): PType =
  ## Used throughout the compiler code to test whether a type tree contains or
  ## doesn't contain a specific type/types - it is often the case that only the
  ## last child nodes of a type tree need to be searched. This is a really hot
  ## path within the compiler!
  result = t
  while result.kind in kinds: result = lastSon(result)

proc isGCedMem*(t: PType): bool {.inline.} =
  result = t.kind in {tyString, tyRef, tySequence} or
           t.kind == tyProc and t.callConv == ccClosure

proc propagateToOwner*(owner, elem: PType) =
  const HaveTheirOwnEmpty = {tySequence, tySet}
  owner.flags = owner.flags + (elem.flags * {tfHasShared, tfHasMeta})
  if tfNotNil in elem.flags:
    if owner.kind in {tyGenericInst, tyGenericBody, tyGenericInvokation}:
      owner.flags.incl tfNotNil
    elif owner.kind notin HaveTheirOwnEmpty:
      owner.flags.incl tfNeedsInit
  
  if tfNeedsInit in elem.flags:
    if owner.kind in HaveTheirOwnEmpty: discard
    else: owner.flags.incl tfNeedsInit
    
  if tfShared in elem.flags:
    owner.flags.incl tfHasShared

  if elem.isMetaType:
    owner.flags.incl tfHasMeta

  if owner.kind != tyProc:
    if elem.isGCedMem or tfHasGCedMem in elem.flags:
      owner.flags.incl tfHasGCedMem

proc rawAddSon*(father, son: PType) =
  if isNil(father.sons): father.sons = @[]
  add(father.sons, son)
  if not son.isNil: propagateToOwner(father, son)

proc addSon(father, son: PNode) = 
  assert son != nil
  if isNil(father.sons): father.sons = @[]
  add(father.sons, son)

proc addSonNilAllowed*(father, son: PNode) =
  if isNil(father.sons): father.sons = @[]
  add(father.sons, son)

proc delSon(father: PNode, idx: int) = 
  if isNil(father.sons): return 
  var length = sonsLen(father)
  for i in countup(idx, length - 2): father.sons[i] = father.sons[i + 1]
  setLen(father.sons, length - 1)

proc copyNode(src: PNode): PNode = 
  # does not copy its sons!
  if src == nil: 
    return nil
  result = newNode(src.kind)
  result.info = src.info
  result.typ = src.typ
  result.flags = src.flags * PersistentNodeFlags
  when defined(useNodeIds):
    if result.id == nodeIdToDebug:
      echo "COMES FROM ", src.id
  case src.kind
  of nkCharLit..nkUInt64Lit: result.intVal = src.intVal
  of nkFloatLit..nkFloat128Lit: result.floatVal = src.floatVal
  of nkSym: result.sym = src.sym
  of nkIdent: result.ident = src.ident
  of nkStrLit..nkTripleStrLit: result.strVal = src.strVal
  else: discard

proc shallowCopy*(src: PNode): PNode = 
  # does not copy its sons, but provides space for them:
  if src == nil: return nil
  result = newNode(src.kind)
  result.info = src.info
  result.typ = src.typ
  result.flags = src.flags * PersistentNodeFlags
  when defined(useNodeIds):
    if result.id == nodeIdToDebug:
      echo "COMES FROM ", src.id
  case src.kind
  of nkCharLit..nkUInt64Lit: result.intVal = src.intVal
  of nkFloatLit..nkFloat128Lit: result.floatVal = src.floatVal
  of nkSym: result.sym = src.sym
  of nkIdent: result.ident = src.ident
  of nkStrLit..nkTripleStrLit: result.strVal = src.strVal
  else: newSeq(result.sons, sonsLen(src))

proc copyTree(src: PNode): PNode = 
  # copy a whole syntax tree; performs deep copying
  if src == nil: 
    return nil
  result = newNode(src.kind)
  result.info = src.info
  result.typ = src.typ
  result.flags = src.flags * PersistentNodeFlags
  when defined(useNodeIds):
    if result.id == nodeIdToDebug:
      echo "COMES FROM ", src.id
  case src.kind
  of nkCharLit..nkUInt64Lit: result.intVal = src.intVal
  of nkFloatLit..nkFloat128Lit: result.floatVal = src.floatVal
  of nkSym: result.sym = src.sym
  of nkIdent: result.ident = src.ident
  of nkStrLit..nkTripleStrLit: result.strVal = src.strVal
  else: 
    newSeq(result.sons, sonsLen(src))
    for i in countup(0, sonsLen(src) - 1): 
      result.sons[i] = copyTree(src.sons[i])
  
proc lastSon(n: PNode): PNode = 
  result = n.sons[sonsLen(n) - 1]

proc lastSon(n: PType): PType = 
  result = n.sons[sonsLen(n) - 1]

proc hasSonWith(n: PNode, kind: TNodeKind): bool = 
  for i in countup(0, sonsLen(n) - 1): 
    if n.sons[i].kind == kind: 
      return true
  result = false

proc hasNilSon*(n: PNode): bool = 
  for i in countup(0, safeLen(n) - 1): 
    if n.sons[i] == nil: 
      return true
    elif hasNilSon(n.sons[i]):
      return true
  result = false

proc containsNode*(n: PNode, kinds: TNodeKinds): bool =
  if n == nil: return
  case n.kind
  of nkEmpty..nkNilLit: result = n.kind in kinds
  else:
    for i in countup(0, sonsLen(n) - 1):
      if n.kind in kinds or containsNode(n.sons[i], kinds): return true

proc hasSubnodeWith(n: PNode, kind: TNodeKind): bool = 
  case n.kind
  of nkEmpty..nkNilLit: result = n.kind == kind
  else: 
    for i in countup(0, sonsLen(n) - 1): 
      if (n.sons[i].kind == kind) or hasSubnodeWith(n.sons[i], kind): 
        return true
    result = false

proc replaceSons(n: PNode, oldKind, newKind: TNodeKind) = 
  for i in countup(0, sonsLen(n) - 1): 
    if n.sons[i].kind == oldKind: n.sons[i].kind = newKind
  
proc sonsNotNil(n: PNode): bool = 
  for i in countup(0, sonsLen(n) - 1): 
    if n.sons[i] == nil: 
      return false
  result = true

proc getInt*(a: PNode): BiggestInt = 
  case a.kind
  of nkIntLit..nkUInt64Lit: result = a.intVal
  else: 
    internalError(a.info, "getInt")
    result = 0

proc getFloat*(a: PNode): BiggestFloat = 
  case a.kind
  of nkFloatLit..nkFloat128Lit: result = a.floatVal
  else: 
    internalError(a.info, "getFloat")
    result = 0.0

proc getStr*(a: PNode): string = 
  case a.kind
  of nkStrLit..nkTripleStrLit: result = a.strVal
  else: 
    internalError(a.info, "getStr")
    result = ""

proc getStrOrChar*(a: PNode): string = 
  case a.kind
  of nkStrLit..nkTripleStrLit: result = a.strVal
  of nkCharLit: result = $chr(int(a.intVal))
  else: 
    internalError(a.info, "getStrOrChar")
    result = ""

proc isGenericRoutine*(s: PSym): bool = 
  case s.kind
  of skProcKinds:
    result = sfFromGeneric in s.flags or
             (s.ast != nil and s.ast[genericParamsPos].kind != nkEmpty)
  else: discard

proc skipGenericOwner*(s: PSym): PSym =
  internalAssert s.kind in skProcKinds
  ## Generic instantiations are owned by their originating generic
  ## symbol. This proc skips such owners and goes straight to the owner
  ## of the generic itself (the module or the enclosing proc).
  result = if sfFromGeneric in s.flags: s.owner.owner
           else: s.owner

proc originatingModule*(s: PSym): PSym =
  result = s.owner
  while result.kind != skModule: result = result.owner

proc isRoutine*(s: PSym): bool {.inline.} =
  result = s.kind in skProcKinds

proc hasPattern*(s: PSym): bool {.inline.} =
  result = isRoutine(s) and s.ast.sons[patternPos].kind != nkEmpty

iterator items*(n: PNode): PNode =
  for i in 0.. <n.len: yield n.sons[i]

iterator pairs*(n: PNode): tuple[i: int, n: PNode] =
  for i in 0.. <n.len: yield (i, n.sons[i])

proc isAtom*(n: PNode): bool {.inline.} =
  result = n.kind >= nkNone and n.kind <= nkNilLit

proc isEmptyType*(t: PType): bool {.inline.} =
  ## 'void' and 'stmt' types are often equivalent to 'nil' these days:
  result = t == nil or t.kind in {tyEmpty, tyStmt}

