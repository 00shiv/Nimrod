#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The compiler depends on the System module to work properly and the System
## module depends on the compiler. Most of the routines listed here use
## special compiler magic.
## Each module implicitly imports the System module; it must not be listed
## explicitly. Because of this there cannot be a user-defined module named
## ``system``.
##
## Exception hierarchy
## ===================
##
## For visual convenience here is the exception inheritance hierarchy
## represented as a tree:
##
## .. include:: ../doc/exception_hierarchy_fragment.txt
##
## Module system
## =============
##

# That lonesome header above is to prevent :idx: entries from being mentioned
# in the global index as part of the previous header (Exception hierarchy).

type
  int* {.magic: Int.} ## default integer type; bitwidth depends on
                      ## architecture, but is always the same as a pointer
  int8* {.magic: Int8.} ## signed 8 bit integer type
  int16* {.magic: Int16.} ## signed 16 bit integer type
  int32* {.magic: Int32.} ## signed 32 bit integer type
  int64* {.magic: Int64.} ## signed 64 bit integer type
  uint* {.magic: UInt.} ## unsigned default integer type
  uint8* {.magic: UInt8.} ## unsigned 8 bit integer type
  uint16* {.magic: UInt16.} ## unsigned 16 bit integer type
  uint32* {.magic: UInt32.} ## unsigned 32 bit integer type
  uint64* {.magic: UInt64.} ## unsigned 64 bit integer type
  float* {.magic: Float.} ## default floating point type
  float32* {.magic: Float32.} ## 32 bit floating point type
  float64* {.magic: Float.} ## 64 bit floating point type

# 'float64' is now an alias to 'float'; this solves many problems

type # we need to start a new type section here, so that ``0`` can have a type
  bool* {.magic: Bool.} = enum ## built-in boolean type
    false = 0, true = 1

type
  char* {.magic: Char.} ## built-in 8 bit character type (unsigned)
  string* {.magic: String.} ## built-in string type
  cstring* {.magic: Cstring.} ## built-in cstring (*compatible string*) type
  pointer* {.magic: Pointer.} ## built-in pointer type, use the ``addr``
                              ## operator to get a pointer to a variable
const
  on* = true    ## alias for ``true``
  off* = false  ## alias for ``false``

{.push warning[GcMem]: off.}
{.push hints: off.}

type
  Ordinal* {.magic: Ordinal.}[T]
  `ptr`* {.magic: Pointer.}[T] ## built-in generic untraced pointer type
  `ref`* {.magic: Pointer.}[T] ## built-in generic traced pointer type

  `nil` {.magic: "Nil".}
  expr* {.magic: Expr.} ## meta type to denote an expression (for templates)
  stmt* {.magic: Stmt.} ## meta type to denote a statement (for templates)
  typedesc* {.magic: TypeDesc.} ## meta type to denote a type description
  void* {.magic: "VoidType".}   ## meta type to denote the absense of any type
  auto* = expr
  any* = distinct auto

  TSignedInt* = int|int8|int16|int32|int64
    ## type class matching all signed integer types

  TUnsignedInt* = uint|uint8|uint16|uint32|uint64
    ## type class matching all unsigned integer types

  TInteger* = TSignedInt|TUnsignedInt
    ## type class matching all integer types

  TOrdinal* = int|int8|int16|int32|int64|bool|enum|uint8|uint16|uint32
    ## type class matching all ordinal types; however this includes enums with
    ## holes.
  
  TReal* = float|float32|float64
    ## type class matching all floating point number types

  TNumber* = TInteger|TReal
    ## type class matching all number types

proc defined*(x: expr): bool {.magic: "Defined", noSideEffect.}
  ## Special compile-time procedure that checks whether `x` is
  ## defined.
  ## `x` is an external symbol introduced through the compiler's
  ## `-d:x switch <nimrodc.html#compile-time-symbols>`_ to enable build time
  ## conditionals:
  ##
  ## .. code-block:: Nimrod
  ##   when not defined(release):
  ##     # Do here programmer friendly expensive sanity checks.
  ##   # Put here the normal code

proc declared*(x: expr): bool {.magic: "Defined", noSideEffect.}
  ## Special compile-time procedure that checks whether `x` is
  ## declared. `x` has to be an identifier or a qualified identifier.
  ## This can be used to check whether a library provides a certain
  ## feature or not:
  ##
  ## .. code-block:: Nimrod
  ##   when not defined(strutils.toUpper):
  ##     # provide our own toUpper proc here, because strutils is
  ##     # missing it.

when defined(useNimRtl):
  {.deadCodeElim: on.}

proc definedInScope*(x: expr): bool {.
  magic: "DefinedInScope", noSideEffect, deprecated.}
  ## **Deprecated since version 0.9.6**: Use ``declaredInScope`` instead.

proc declaredInScope*(x: expr): bool {.
  magic: "DefinedInScope", noSideEffect.}
  ## Special compile-time procedure that checks whether `x` is
  ## declared in the current scope. `x` has to be an identifier.

proc `not` *(x: bool): bool {.magic: "Not", noSideEffect.}
  ## Boolean not; returns true iff ``x == false``.

proc `and`*(x, y: bool): bool {.magic: "And", noSideEffect.}
  ## Boolean ``and``; returns true iff ``x == y == true``.
  ## Evaluation is lazy: if ``x`` is false,
  ## ``y`` will not even be evaluated.
proc `or`*(x, y: bool): bool {.magic: "Or", noSideEffect.}
  ## Boolean ``or``; returns true iff ``not (not x and not y)``.
  ## Evaluation is lazy: if ``x`` is true,
  ## ``y`` will not even be evaluated.
proc `xor`*(x, y: bool): bool {.magic: "Xor", noSideEffect.}
  ## Boolean `exclusive or`; returns true iff ``x != y``.

proc new*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.

proc new*(T: typedesc): ref T =
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it as result value
  new(result)

proc unsafeNew*[T](a: var ref T, size: int) {.magic: "New", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``. This is **unsafe** as it allocates an object
  ## of the passed ``size``. This should only be used for optimization
  ## purposes when you know what you're doing!

proc internalNew*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## leaked implementation detail. Do not use.

proc new*[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``. When the garbage collector frees the object,
  ## `finalizer` is called. The `finalizer` may not keep a reference to the
  ## object pointed to by `x`. The `finalizer` cannot prevent the GC from
  ## freeing the object. Note: The `finalizer` refers to the type `T`, not to
  ## the object! This means that for each object of type `T` the finalizer
  ## will be called!
  
proc reset*[T](obj: var T) {.magic: "Reset", noSideEffect.}
  ## resets an object `obj` to its initial (binary zero) value. This needs to
  ## be called before any possible `object branch transition`:idx:.

# for low and high the return type T may not be correct, but
# we handle that with compiler magic in semLowHigh()
proc high*[T](x: T): T {.magic: "High", noSideEffect.}
  ## returns the highest possible index of an array, a sequence, a string or
  ## the highest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.

proc low*[T](x: T): T {.magic: "Low", noSideEffect.}
  ## returns the lowest possible index of an array, a sequence, a string or
  ## the lowest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.

type
  range*{.magic: "Range".}[T] ## Generic type to construct range types.
  array*{.magic: "Array".}[I, T]  ## Generic type to construct
                                  ## fixed-length arrays.
  openArray*{.magic: "OpenArray".}[T]  ## Generic type to construct open arrays.
                                       ## Open arrays are implemented as a
                                       ## pointer to the array data and a
                                       ## length field.
  varargs*{.magic: "Varargs".}[T] ## Generic type to construct a varargs type.
  seq*{.magic: "Seq".}[T]  ## Generic type to construct sequences.
  set*{.magic: "Set".}[T]  ## Generic type to construct bit sets.

type
  TSlice* {.final, pure.}[T] = object ## builtin slice type
    a*, b*: T                         ## the bounds

proc `..`*[T](a, b: T): TSlice[T] {.noSideEffect, inline.} =
  ## `slice`:idx: operator that constructs an interval ``[a, b]``, both `a`
  ## and `b` are inclusive. Slices can also be used in the set constructor
  ## and in ordinal case statements, but then they are special-cased by the
  ## compiler.
  result.a = a
  result.b = b

proc `..`*[T](b: T): TSlice[T] {.noSideEffect, inline.} =
  ## `slice`:idx: operator that constructs an interval ``[default(T), b]``
  result.b = b

when not defined(niminheritable):
  {.pragma: inheritable.}
when not defined(nimunion):
  {.pragma: unchecked.}

when defined(nimNewShared):
  type
    `shared`* {.magic: "Shared".}
    guarded* {.magic: "Guarded".}

# comparison operators:
proc `==` *[TEnum: enum](x, y: TEnum): bool {.magic: "EqEnum", noSideEffect.}
proc `==` *(x, y: pointer): bool {.magic: "EqRef", noSideEffect.}
proc `==` *(x, y: string): bool {.magic: "EqStr", noSideEffect.}
proc `==` *(x, y: cstring): bool {.magic: "EqCString", noSideEffect.}
proc `==` *(x, y: char): bool {.magic: "EqCh", noSideEffect.}
proc `==` *(x, y: bool): bool {.magic: "EqB", noSideEffect.}
proc `==` *[T](x, y: set[T]): bool {.magic: "EqSet", noSideEffect.}
proc `==` *[T](x, y: ref T): bool {.magic: "EqRef", noSideEffect.}
proc `==` *[T](x, y: ptr T): bool {.magic: "EqRef", noSideEffect.}
proc `==` *[T: proc](x, y: T): bool {.magic: "EqProc", noSideEffect.}

proc `<=` *[TEnum: enum](x, y: TEnum): bool {.magic: "LeEnum", noSideEffect.}
proc `<=` *(x, y: string): bool {.magic: "LeStr", noSideEffect.}
proc `<=` *(x, y: char): bool {.magic: "LeCh", noSideEffect.}
proc `<=` *[T](x, y: set[T]): bool {.magic: "LeSet", noSideEffect.}
proc `<=` *(x, y: bool): bool {.magic: "LeB", noSideEffect.}
proc `<=` *[T](x, y: ref T): bool {.magic: "LePtr", noSideEffect.}
proc `<=` *(x, y: pointer): bool {.magic: "LePtr", noSideEffect.}

proc `<` *[TEnum: enum](x, y: TEnum): bool {.magic: "LtEnum", noSideEffect.}
proc `<` *(x, y: string): bool {.magic: "LtStr", noSideEffect.}
proc `<` *(x, y: char): bool {.magic: "LtCh", noSideEffect.}
proc `<` *[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
proc `<` *(x, y: bool): bool {.magic: "LtB", noSideEffect.}
proc `<` *[T](x, y: ref T): bool {.magic: "LtPtr", noSideEffect.}
proc `<` *[T](x, y: ptr T): bool {.magic: "LtPtr", noSideEffect.}
proc `<` *(x, y: pointer): bool {.magic: "LtPtr", noSideEffect.}

template `!=` * (x, y: expr): expr {.immediate.} =
  ## unequals operator. This is a shorthand for ``not (x == y)``.
  not (x == y)

template `>=` * (x, y: expr): expr {.immediate.} =
  ## "is greater or equals" operator. This is the same as ``y <= x``.
  y <= x

template `>` * (x, y: expr): expr {.immediate.} =
  ## "is greater" operator. This is the same as ``y < x``.
  y < x

const
  appType* {.magic: "AppType"}: string = ""
    ## a string that describes the application type. Possible values:
    ## "console", "gui", "lib".

include "system/inclrtl"

const NoFakeVars* = defined(NimrodVM) ## true if the backend doesn't support \
  ## "fake variables" like 'var EBADF {.importc.}: cint'.

when not defined(JS):
  type
    TGenericSeq {.compilerproc, pure, inheritable.} = object
      len, reserved: int
    PGenericSeq {.exportc.} = ptr TGenericSeq
    UncheckedCharArray {.unchecked.} = array[0..100_000_000, char]
    # len and space without counting the terminating zero:
    NimStringDesc {.compilerproc, final.} = object of TGenericSeq
      data: UncheckedCharArray
    NimString = ptr NimStringDesc

when not defined(JS) and not defined(NimrodVM):
  template space(s: PGenericSeq): int {.dirty.} =
    s.reserved and not seqShallowFlag

  include "system/hti"

type
  Byte* = uint8 ## this is an alias for ``uint8``, that is an unsigned
                ## int 8 bits wide.

  Natural* = range[0..high(int)]
    ## is an int type ranging from zero to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  Positive* = range[1..high(int)]
    ## is an int type ranging from one to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  TObject* {.exportc: "TNimObject", inheritable.} =
    object ## the root of Nimrod's object hierarchy. Objects should
           ## inherit from TObject or one of its descendants. However,
           ## objects that have no ancestor are allowed.
  PObject* = ref TObject ## reference to TObject

  TEffect* {.compilerproc.} = object of TObject ## \
    ## base effect class; each effect should
    ## inherit from `TEffect` unless you know what
    ## you doing.
  FTime* = object of TEffect   ## Time effect.
  FIO* = object of TEffect     ## IO effect.
  FReadIO* = object of FIO     ## Effect describing a read IO operation.
  FWriteIO* = object of FIO    ## Effect describing a write IO operation.
  FExecIO* = object of FIO     ## Effect describing an executing IO operation.

  E_Base* {.compilerproc.} = object of TObject ## \
    ## Base exception class.
    ##
    ## Each exception has to inherit from `E_Base`. See the full `exception
    ## hierarchy`_.
    parent: ref E_Base        ## parent exception (can be used as a stack)
    name: cstring             ## The exception's name is its Nimrod identifier.
                              ## This field is filled automatically in the
                              ## ``raise`` statement.
    msg* {.exportc: "message".}: string ## the exception's message. Not
                                        ## providing an exception message 
                                        ## is bad style.
    trace: string

  EAsynch* = object of E_Base ## \
    ## Abstract exception class for *asynchronous exceptions* (interrupts).
    ##
    ## This is rarely needed: most exception types inherit from `ESynch
    ## <#ESynch>`_. See the full `exception hierarchy`_.
  EControlC* = object of EAsynch ## \
    ## Raised for Ctrl+C key presses in console applications.
    ##
    ## See the full `exception hierarchy`_.
  ESynch* = object of E_Base ## \
    ## Abstract exception class for *synchronous exceptions*.
    ##
    ## Most exceptions should be inherited (directly or indirectly) from
    ## `ESynch` instead of from `EAsynch <#EAsynch>`_. See the full `exception
    ## hierarchy`_.
  ESystem* = object of ESynch ## \
    ## Abstract class for exceptions that the runtime system raises.
    ##
    ## See the full `exception hierarchy`_.
  EIO* = object of ESystem ## \
    ## Raised if an IO error occured.
    ##
    ## See the full `exception hierarchy`_.
  EOS* = object of ESystem ## \
    ## Raised if an operating system service failed.
    ##
    ## See the full `exception hierarchy`_.
    errorCode*: int32 ## OS-defined error code describing this error.
  EInvalidLibrary* = object of EOS ## \
    ## Raised if a dynamic library could not be loaded.
    ##
    ## See the full `exception hierarchy`_.
  EResourceExhausted* = object of ESystem ## \
    ## Raised if a resource request could not be fullfilled.
    ##
    ## See the full `exception hierarchy`_.
  EArithmetic* = object of ESynch ## \
    ## Raised if any kind of arithmetic error occured.
    ##
    ## See the full `exception hierarchy`_.
  EDivByZero* {.compilerproc.} = object of EArithmetic ## \
    ## Raised for runtime integer divide-by-zero errors.
    ##
    ## See the full `exception hierarchy`_.
  EOverflow* {.compilerproc.} = object of EArithmetic ## \
    ## Raised for runtime integer overflows.
    ##
    ## This happens for calculations whose results are too large to fit in the
    ## provided bits.  See the full `exception hierarchy`_.
  EAccessViolation* {.compilerproc.} = object of ESynch ## \
    ## Raised for invalid memory access errors
    ##
    ## See the full `exception hierarchy`_.
  EAssertionFailed* {.compilerproc.} = object of ESynch ## \
    ## Raised when assertion is proved wrong.
    ##
    ## Usually the result of using the `assert() template <#assert>`_.  See the
    ## full `exception hierarchy`_.
  EInvalidValue* = object of ESynch ## \
    ## Raised for string and object conversion errors.
  EInvalidKey* = object of EInvalidValue ## \
    ## Raised if a key cannot be found in a table.
    ##
    ## Mostly used by the `tables <tables.html>`_ module, it can also be raised
    ## by other collection modules like `sets <sets.html>`_ or `strtabs
    ## <strtabs.html>`_. See the full `exception hierarchy`_.
  EOutOfMemory* = object of ESystem ## \
    ## Raised for unsuccessful attempts to allocate memory.
    ##
    ## See the full `exception hierarchy`_.
  EInvalidIndex* = object of ESynch ## \
    ## Raised if an array index is out of bounds.
    ##
    ## See the full `exception hierarchy`_.
  EInvalidField* = object of ESynch ## \
    ## Raised if a record field is not accessible because its dicriminant's
    ## value does not fit.
    ##
    ## See the full `exception hierarchy`_.
  EOutOfRange* = object of ESynch ## \
    ## Raised if a range check error occurred.
    ##
    ## See the full `exception hierarchy`_.
  EStackOverflow* = object of ESystem ## \
    ## Raised if the hardware stack used for subroutine calls overflowed.
    ##
    ## See the full `exception hierarchy`_.
  ENoExceptionToReraise* = object of ESynch ## \
    ## Raised if there is no exception to reraise.
    ##
    ## See the full `exception hierarchy`_.
  EInvalidObjectAssignment* = object of ESynch ## \
    ## Raised if an object gets assigned to its parent's object.
    ##
    ## See the full `exception hierarchy`_.
  EInvalidObjectConversion* = object of ESynch ## \
    ## Raised if an object is converted to an incompatible object type.
    ##
    ## See the full `exception hierarchy`_.
  EFloatingPoint* = object of ESynch ## \
    ## Base class for floating point exceptions.
    ##
    ## See the full `exception hierarchy`_.
  EFloatInvalidOp* {.compilerproc.} = object of EFloatingPoint ## \
    ## Raised by invalid operations according to IEEE.
    ##
    ## Raised by ``0.0/0.0``, for example.  See the full `exception
    ## hierarchy`_.
  EFloatDivByZero* {.compilerproc.} = object of EFloatingPoint ## \
    ## Raised by division by zero.
    ##
    ## Divisor is zero and dividend is a finite nonzero number.  See the full
    ## `exception hierarchy`_.
  EFloatOverflow* {.compilerproc.} = object of EFloatingPoint ## \
    ## Raised for overflows.
    ##
    ## The operation produced a result that exceeds the range of the exponent.
    ## See the full `exception hierarchy`_.
  EFloatUnderflow* {.compilerproc.} = object of EFloatingPoint ## \
    ## Raised for underflows.
    ##
    ## The operation produced a result that is too small to be represented as a
    ## normal number. See the full `exception hierarchy`_.
  EFloatInexact* {.compilerproc.} = object of EFloatingPoint ## \
    ## Raised for inexact results.
    ##
    ## The operation produced a result that cannot be represented with infinite
    ## precision -- for example: ``2.0 / 3.0, log(1.1)``
    ##
    ## **NOTE**: Nimrod currently does not detect these!  See the full
    ## `exception hierarchy`_.
  EDeadThread* = object of ESynch ## \
    ## Raised if it is attempted to send a message to a dead thread.
    ##
    ## See the full `exception hierarchy`_.

  TResult* = enum Failure, Success

proc sizeof*[T](x: T): Natural {.magic: "SizeOf", noSideEffect.}
  ## returns the size of ``x`` in bytes. Since this is a low-level proc,
  ## its usage is discouraged - using ``new`` for the most cases suffices
  ## that one never needs to know ``x``'s size. As a special semantic rule,
  ## ``x`` may also be a type identifier (``sizeof(int)`` is valid).

proc `<`*[T](x: Ordinal[T]): T {.magic: "UnaryLt", noSideEffect.}
  ## unary ``<`` that can be used for nice looking excluding ranges:
  ## 
  ## .. code-block:: nimrod
  ##   for i in 0 .. <10: echo i
  ##
  ## Semantically this is the same as ``pred``. 

proc succ*[T](x: Ordinal[T], y = 1): T {.magic: "Succ", noSideEffect.}
  ## returns the ``y``-th successor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc pred*[T](x: Ordinal[T], y = 1): T {.magic: "Pred", noSideEffect.}
  ## returns the ``y``-th predecessor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc inc*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Inc", noSideEffect.}
  ## increments the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = succ(x, y)``.

proc dec*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Dec", noSideEffect.}
  ## decrements the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = pred(x, y)``.
  
proc newSeq*[T](s: var seq[T], len: int) {.magic: "NewSeq", noSideEffect.}
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ## This is equivalent to ``s = @[]; setlen(s, len)``, but more
  ## efficient since no reallocation is needed.
  ##
  ## Note that the sequence will be filled with zeroed entries, which can be a
  ## problem for sequences containing strings since their value will be
  ## ``nil``. After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them. Example:
  ##
  ## .. code-block:: nimrod
  ##   var inputStrings : seq[string]
  ##   newSeq(inputStrings, 3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"

proc newSeq*[T](len = 0): seq[T] =
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## Note that the sequence will be filled with zeroed entries, which can be a
  ## problem for sequences containing strings since their value will be
  ## ``nil``. After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them. Example:
  ##
  ## .. code-block:: nimrod
  ##   var inputStrings = newSeq[string](3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"
  newSeq(result, len)

proc len*[TOpenArray: openArray|varargs](x: TOpenArray): int {.
  magic: "LengthOpenArray", noSideEffect.}
proc len*(x: string): int {.magic: "LengthStr", noSideEffect.}
proc len*(x: cstring): int {.magic: "LengthStr", noSideEffect.}
proc len*[I, T](x: array[I, T]): int {.magic: "LengthArray", noSideEffect.}
proc len*[T](x: seq[T]): int {.magic: "LengthSeq", noSideEffect.}
  ## returns the length of an array, an openarray, a sequence or a string.
  ## This is rougly the same as ``high(T)-low(T)+1``, but its resulting type is
  ## always an int.

# set routines:
proc incl*[T](x: var set[T], y: T) {.magic: "Incl", noSideEffect.}
  ## includes element ``y`` to the set ``x``. This is the same as
  ## ``x = x + {y}``, but it might be more efficient.

template incl*[T](s: var set[T], flags: set[T]) =
  ## includes the set of flags to the set ``x``.
  s = s + flags

proc excl*[T](x: var set[T], y: T) {.magic: "Excl", noSideEffect.}
  ## excludes element ``y`` to the set ``x``. This is the same as
  ## ``x = x - {y}``, but it might be more efficient.

template excl*[T](s: var set[T], flags: set[T]) =
  ## excludes the set of flags to ``x``.
  s = s - flags

proc card*[T](x: set[T]): int {.magic: "Card", noSideEffect.}
  ## returns the cardinality of the set ``x``, i.e. the number of elements
  ## in the set.

proc ord*[T](x: T): int {.magic: "Ord", noSideEffect.}
  ## returns the internal int value of an ordinal value ``x``.

proc chr*(u: range[0..255]): char {.magic: "Chr", noSideEffect.}
  ## converts an int in the range 0..255 to a character.

# --------------------------------------------------------------------------
# built-in operators

when not defined(JS):
  proc ze*(x: int8): int {.magic: "Ze8ToI", noSideEffect.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.
  proc ze*(x: int16): int {.magic: "Ze16ToI", noSideEffect.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.

  proc ze64*(x: int8): int64 {.magic: "Ze8ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
  proc ze64*(x: int16): int64 {.magic: "Ze16ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.

  proc ze64*(x: int32): int64 {.magic: "Ze32ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
  proc ze64*(x: int): int64 {.magic: "ZeIToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned. Does nothing if the size of an ``int`` is the same as ``int64``.
    ## (This is the case on 64 bit processors.)

  proc toU8*(x: int): int8 {.magic: "ToU8", noSideEffect.}
    ## treats `x` as unsigned and converts it to a byte by taking the last 8 bits
    ## from `x`.    
  proc toU16*(x: int): int16 {.magic: "ToU16", noSideEffect.}
    ## treats `x` as unsigned and converts it to an ``int16`` by taking the last
    ## 16 bits from `x`.
  proc toU32*(x: int64): int32 {.magic: "ToU32", noSideEffect.}
    ## treats `x` as unsigned and converts it to an ``int32`` by taking the
    ## last 32 bits from `x`.

# integer calculations:
proc `+` *(x: int): int {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int8): int8 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int16): int16 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int32): int32 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int64): int64 {.magic: "UnaryPlusI64", noSideEffect.}
  ## Unary `+` operator for an integer. Has no effect.

proc `-` *(x: int): int {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int8): int8 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int16): int16 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int32): int32 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int64): int64 {.magic: "UnaryMinusI64", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

proc `not` *(x: int): int {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int8): int8 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int16): int16 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int32): int32 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int64): int64 {.magic: "BitnotI64", noSideEffect.}
  ## computes the `bitwise complement` of the integer `x`.

proc `+` *(x, y: int): int {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int8): int8 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int16): int16 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int32): int32 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int64): int64 {.magic: "AddI64", noSideEffect.}
  ## Binary `+` operator for an integer.

proc `-` *(x, y: int): int {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int8): int8 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int16): int16 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int32): int32 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int64): int64 {.magic: "SubI64", noSideEffect.}
  ## Binary `-` operator for an integer.

proc `*` *(x, y: int): int {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int8): int8 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int16): int16 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int32): int32 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int64): int64 {.magic: "MulI64", noSideEffect.}
  ## Binary `*` operator for an integer.

proc `div` *(x, y: int): int {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int8): int8 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int16): int16 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int32): int32 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int64): int64 {.magic: "DivI64", noSideEffect.}
  ## computes the integer division. This is roughly the same as
  ## ``floor(x/y)``.
  ## .. code-block:: Nimrod
  ##   1 div 2 == 0
  ##   2 div 2 == 1
  ##   3 div 2 == 1

proc `mod` *(x, y: int): int {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int8): int8 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int16): int16 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int32): int32 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int64): int64 {.magic: "ModI64", noSideEffect.}
  ## computes the integer modulo operation. This is the same as
  ## ``x - (x div y) * y``.

proc `shr` *(x, y: int): int {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int8): int8 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int16): int16 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int32): int32 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int64): int64 {.magic: "ShrI64", noSideEffect.}
  ## computes the `shift right` operation of `x` and `y`.
  ## .. code-block:: Nimrod
  ##   0b0001_0000'i8 shr 2 == 0b0100_0000'i8
  ##   0b1000_0000'i8 shr 2 == 0b0000_0000'i8
  ##   0b0000_0001'i8 shr 9 == 0b0000_0000'i8

proc `shl` *(x, y: int): int {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int8): int8 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int16): int16 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int32): int32 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int64): int64 {.magic: "ShlI64", noSideEffect.}
  ## computes the `shift left` operation of `x` and `y`.

proc `and` *(x, y: int): int {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int8): int8 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int16): int16 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int32): int32 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int64): int64 {.magic: "BitandI64", noSideEffect.}
  ## computes the `bitwise and` of numbers `x` and `y`.

proc `or` *(x, y: int): int {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int8): int8 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int16): int16 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int32): int32 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int64): int64 {.magic: "BitorI64", noSideEffect.}
  ## computes the `bitwise or` of numbers `x` and `y`.

proc `xor` *(x, y: int): int {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int8): int8 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int16): int16 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int32): int32 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int64): int64 {.magic: "BitxorI64", noSideEffect.}
  ## computes the `bitwise xor` of numbers `x` and `y`.

proc `==` *(x, y: int): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int8): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int16): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int32): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int64): bool {.magic: "EqI64", noSideEffect.}
  ## Compares two integers for equality.

proc `<=` *(x, y: int): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int8): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int16): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int32): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int64): bool {.magic: "LeI64", noSideEffect.}
  ## Returns true iff `x` is less than or equal to `y`.

proc `<` *(x, y: int): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int8): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int16): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int32): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int64): bool {.magic: "LtI64", noSideEffect.}
  ## Returns true iff `x` is less than `y`.

type
  IntMax32 = int|int8|int16|int32

proc `+%` *(x, y: IntMax32): IntMax32 {.magic: "AddU", noSideEffect.}
proc `+%` *(x, y: int64): int64 {.magic: "AddU", noSideEffect.}
  ## treats `x` and `y` as unsigned and adds them. The result is truncated to
  ## fit into the result. This implements modulo arithmetic. No overflow
  ## errors are possible.

proc `-%` *(x, y: IntMax32): IntMax32 {.magic: "SubU", noSideEffect.}
proc `-%` *(x, y: int64): int64 {.magic: "SubU", noSideEffect.}
  ## treats `x` and `y` as unsigned and subtracts them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `*%` *(x, y: IntMax32): IntMax32 {.magic: "MulU", noSideEffect.}
proc `*%` *(x, y: int64): int64 {.magic: "MulU", noSideEffect.}
  ## treats `x` and `y` as unsigned and multiplies them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `/%` *(x, y: IntMax32): IntMax32 {.magic: "DivU", noSideEffect.}
proc `/%` *(x, y: int64): int64 {.magic: "DivU", noSideEffect.}
  ## treats `x` and `y` as unsigned and divides them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `%%` *(x, y: IntMax32): IntMax32 {.magic: "ModU", noSideEffect.}
proc `%%` *(x, y: int64): int64 {.magic: "ModU", noSideEffect.}
  ## treats `x` and `y` as unsigned and compute the modulo of `x` and `y`.
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic.
  ## No overflow errors are possible.
  
proc `<=%` *(x, y: IntMax32): bool {.magic: "LeU", noSideEffect.}
proc `<=%` *(x, y: int64): bool {.magic: "LeU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) <= unsigned(y)``.

proc `<%` *(x, y: IntMax32): bool {.magic: "LtU", noSideEffect.}
proc `<%` *(x, y: int64): bool {.magic: "LtU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) < unsigned(y)``.


# floating point operations:

proc `+` *(x: float32): float32 {.magic: "UnaryPlusF64", noSideEffect.}
proc `-` *(x: float32): float32 {.magic: "UnaryMinusF64", noSideEffect.}
proc `+` *(x, y: float32): float32 {.magic: "AddF64", noSideEffect.}
proc `-` *(x, y: float32): float32 {.magic: "SubF64", noSideEffect.}
proc `*` *(x, y: float32): float32 {.magic: "MulF64", noSideEffect.}
proc `/` *(x, y: float32): float32 {.magic: "DivF64", noSideEffect.}

proc `+` *(x: float): float {.magic: "UnaryPlusF64", noSideEffect.}
proc `-` *(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
proc `+` *(x, y: float): float {.magic: "AddF64", noSideEffect.}
proc `-` *(x, y: float): float {.magic: "SubF64", noSideEffect.}
proc `*` *(x, y: float): float {.magic: "MulF64", noSideEffect.}
proc `/` *(x, y: float): float {.magic: "DivF64", noSideEffect.}
  ## computes the floating point division

proc `==` *(x, y: float32): bool {.magic: "EqF64", noSideEffect.}
proc `<=` *(x, y: float32): bool {.magic: "LeF64", noSideEffect.}
proc `<`  *(x, y: float32): bool {.magic: "LtF64", noSideEffect.}

proc `==` *(x, y: float): bool {.magic: "EqF64", noSideEffect.}
proc `<=` *(x, y: float): bool {.magic: "LeF64", noSideEffect.}
proc `<`  *(x, y: float): bool {.magic: "LtF64", noSideEffect.}

# set operators
proc `*` *[T](x, y: set[T]): set[T] {.magic: "MulSet", noSideEffect.}
  ## This operator computes the intersection of two sets.
proc `+` *[T](x, y: set[T]): set[T] {.magic: "PlusSet", noSideEffect.}
  ## This operator computes the union of two sets.
proc `-` *[T](x, y: set[T]): set[T] {.magic: "MinusSet", noSideEffect.}
  ## This operator computes the difference of two sets.
proc `-+-` *[T](x, y: set[T]): set[T] {.magic: "SymDiffSet", noSideEffect.}
  ## computes the symmetric set difference. This is the same as
  ## ``(A - B) + (B - A)``, but more efficient.

proc contains*[T](x: set[T], y: T): bool {.magic: "InSet", noSideEffect.}
  ## One should overload this proc if one wants to overload the ``in`` operator.
  ## The parameters are in reverse order! ``a in b`` is a template for
  ## ``contains(b, a)``.
  ## This is because the unification algorithm that Nimrod uses for overload
  ## resolution works from left to right.
  ## But for the ``in`` operator that would be the wrong direction for this
  ## piece of code:
  ##
  ## .. code-block:: Nimrod
  ##   var s: set[range['a'..'z']] = {'a'..'c'}
  ##   writeln(stdout, 'b' in s)
  ##
  ## If ``in`` had been declared as ``[T](elem: T, s: set[T])`` then ``T`` would
  ## have been bound to ``char``. But ``s`` is not compatible to type
  ## ``set[char]``! The solution is to bind ``T`` to ``range['a'..'z']``. This
  ## is achieved by reversing the parameters for ``contains``; ``in`` then
  ## passes its arguments in reverse order.

proc contains*[T](s: TSlice[T], value: T): bool {.noSideEffect, inline.} =
  ## Checks if `value` is withing the range of `s`; returns true iff
  ## `value >= s.a and value <= s.b`
  ##
  ## .. code-block:: Nimrod
  ##   assert((1..3).contains(1) == true)
  ##   assert((1..3).contains(2) == true)
  ##   assert((1..3).contains(4) == false)
  result = s.a <= value and value <= s.b

template `in` * (x, y: expr): expr {.immediate.} = contains(y, x)
  ## Sugar for contains
  ##
  ## .. code-block:: Nimrod
  ##   assert(1 in (1..3) == true)
  ##   assert(5 in (1..3) == false)
template `notin` * (x, y: expr): expr {.immediate.} = not contains(y, x)
  ## Sugar for not containing
  ##
  ## .. code-block:: Nimrod
  ##   assert(1 notin (1..3) == false)
  ##   assert(5 notin (1..3) == true)

proc `is` *[T, S](x: T, y: S): bool {.magic: "Is", noSideEffect.}
  ## Checks if T is of the same type as S
  ## 
  ## .. code-block:: Nimrod
  ##   proc test[T](a: T): int =
  ##     when (T is int):
  ##       return a
  ##     else:
  ##       return 0
  ##
  ##   assert(test[int](3) == 3)
  ##   assert(test[string]("xyz") == 0)
template `isnot` *(x, y: expr): expr {.immediate.} = not (x is y)
  ## Negated version of `is`. Equivalent to ``not(x is y)``.

proc `of` *[T, S](x: T, y: S): bool {.magic: "Of", noSideEffect.}
  ## Checks if `x` has a type of `y`
  ##
  ## .. code-block:: Nimrod
  ##   assert(EFloatingPoint of EBase)
  ##   assert(EIO of ESystem)
  ##   assert(EDivByZero of EBase)

proc cmp*[T](x, y: T): int {.procvar.} =
  ## Generic compare proc. Returns a value < 0 iff x < y, a value > 0 iff x > y
  ## and 0 iff x == y. This is useful for writing generic algorithms without
  ## performance loss. This generic implementation uses the `==` and `<`
  ## operators.
  if x == y: return 0
  if x < y: return -1
  return 1

proc cmp*(x, y: string): int {.noSideEffect, procvar.}
  ## Compare proc for strings. More efficient than the generic version.

proc `@` * [IDX, T](a: array[IDX, T]): seq[T] {.
  magic: "ArrToSeq", nosideeffect.}
  ## turns an array into a sequence. This most often useful for constructing
  ## sequences with the array constructor: ``@[1, 2, 3]`` has the type 
  ## ``seq[int]``, while ``[1, 2, 3]`` has the type ``array[0..2, int]``.

proc setLen*[T](s: var seq[T], newlen: int) {.
  magic: "SetLengthSeq", noSideEffect.}
  ## sets the length of `s` to `newlen`.
  ## ``T`` may be any sequence type.
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated. `s` cannot be nil! To initialize a sequence with
  ## a size, use ``newSeq`` instead. 

proc setLen*(s: var string, newlen: int) {.
  magic: "SetLengthStr", noSideEffect.}
  ## sets the length of `s` to `newlen`.
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated. `s` cannot be nil! To initialize a string with
  ## a size, use ``newString`` instead. 

proc newString*(len: int): string {.
  magic: "NewString", importc: "mnewString", noSideEffect.}
  ## returns a new string of length ``len`` but with uninitialized
  ## content. One needs to fill the string character after character
  ## with the index operator ``s[i]``. This procedure exists only for
  ## optimization purposes; the same effect can be achieved with the
  ## ``&`` operator or with ``add``.

proc newStringOfCap*(cap: int): string {.
  magic: "NewStringOfCap", importc: "rawNewString", noSideEffect.}
  ## returns a new string of length ``0`` but with capacity `cap`.This
  ## procedure exists only for optimization purposes; the same effect can 
  ## be achieved with the ``&`` operator or with ``add``.

proc `&` * (x: string, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` with `y`
  ##
  ## .. code-block:: Nimrod
  ##   assert("ab" & 'c' == "abc")
proc `&` * (x: char, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` and `y` into a string
  ##
  ## .. code-block:: Nimrod
  ##   assert('a' & 'b' == "ab")
proc `&` * (x, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` and `y`
  ##
  ## .. code-block:: Nimrod
  ##   assert("ab" & "cd" == "abcd")
proc `&` * (x: char, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` with `y`
  ##
  ## .. code-block:: Nimrod
  ##   assert('a' & "bc" == "abc")

# implementation note: These must all have the same magic value "ConStrStr" so
# that the merge optimization works properly. 

proc add*(x: var string, y: char) {.magic: "AppendStrCh", noSideEffect.}
  ## Appends `y` to `x` in place
  ##
  ## .. code-block:: Nimrod
  ##   var tmp = ""
  ##   tmp.add('a')
  ##   tmp.add('b')
  ##   assert(tmp == "ab")
proc add*(x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}
  ## Concatenates `x` and `y` in place
  ##
  ## .. code-block:: Nimrod
  ##   var tmp = ""
  ##   tmp.add("ab")
  ##   tmp.add("cd")
  ##   assert(tmp == "abcd")

type
  TEndian* = enum ## is a type describing the endianness of a processor.
    littleEndian, bigEndian

const
  isMainModule* {.magic: "IsMainModule".}: bool = false
    ## is true only when accessed in the main module. This works thanks to
    ## compiler magic. It is useful to embed testing code in a module.

  CompileDate* {.magic: "CompileDate"}: string = "0000-00-00"
    ## is the date of compilation as a string of the form
    ## ``YYYY-MM-DD``. This works thanks to compiler magic.

  CompileTime* {.magic: "CompileTime"}: string = "00:00:00"
    ## is the time of compilation as a string of the form
    ## ``HH:MM:SS``. This works thanks to compiler magic.

  NimrodVersion* {.magic: "NimrodVersion"}: string = "0.0.0"
    ## is the version of Nimrod as a string.
    ## This works thanks to compiler magic.

  NimrodMajor* {.magic: "NimrodMajor"}: int = 0
    ## is the major number of Nimrod's version.
    ## This works thanks to compiler magic.

  NimrodMinor* {.magic: "NimrodMinor"}: int = 0
    ## is the minor number of Nimrod's version.
    ## This works thanks to compiler magic.

  NimrodPatch* {.magic: "NimrodPatch"}: int = 0
    ## is the patch number of Nimrod's version.
    ## This works thanks to compiler magic.

  cpuEndian* {.magic: "CpuEndian"}: TEndian = littleEndian
    ## is the endianness of the target CPU. This is a valuable piece of
    ## information for low-level code only. This works thanks to compiler
    ## magic.
    
  hostOS* {.magic: "HostOS"}: string = ""
    ## a string that describes the host operating system. Possible values:
    ## "windows", "macosx", "linux", "netbsd", "freebsd", "openbsd", "solaris",
    ## "aix", "standalone".
        
  hostCPU* {.magic: "HostCPU"}: string = ""
    ## a string that describes the host CPU. Possible values:
    ## "i386", "alpha", "powerpc", "sparc", "amd64", "mips", "arm".
  
  seqShallowFlag = low(int)
  
proc compileOption*(option: string): bool {.
  magic: "CompileOption", noSideEffect.}
  ## can be used to determine an on|off compile-time option. Example:
  ##
  ## .. code-block:: nimrod
  ##   when compileOption("floatchecks"): 
  ##     echo "compiled with floating point NaN and Inf checks"
  
proc compileOption*(option, arg: string): bool {.
  magic: "CompileOptionArg", noSideEffect.}
  ## can be used to determine an enum compile-time option. Example:
  ##
  ## .. code-block:: nimrod
  ##   when compileOption("opt", "size") and compileOption("gc", "boehm"): 
  ##     echo "compiled with optimization for size and uses Boehm's GC"

const
  hasThreadSupport = compileOption("threads")
  hasSharedHeap = defined(boehmgc) # don't share heaps; every thread has its own
  taintMode = compileOption("taintmode")

when taintMode:
  type TaintedString* = distinct string ## a distinct string type that 
                                        ## is `tainted`:idx:. It is an alias for
                                        ## ``string`` if the taint mode is not
                                        ## turned on. Use the ``-d:taintMode``
                                        ## command line switch to turn the taint
                                        ## mode on.
  
  proc len*(s: TaintedString): int {.borrow.}
else:
  type TaintedString* = string          ## a distinct string type that 
                                        ## is `tainted`:idx:. It is an alias for
                                        ## ``string`` if the taint mode is not
                                        ## turned on. Use the ``-d:taintMode``
                                        ## command line switch to turn the taint
                                        ## mode on.

when defined(profiler):
  proc nimProfile() {.compilerProc, noinline.}
when hasThreadSupport:
  {.pragma: rtlThreadVar, threadvar.}
else:
  {.pragma: rtlThreadVar.}

const
  QuitSuccess* = 0
    ## is the value that should be passed to ``quit`` to indicate
    ## success.

  QuitFailure* = 1
    ## is the value that should be passed to ``quit`` to indicate
    ## failure.

var programResult* {.exportc: "nim_program_result".}: int
  ## modify this variable to specify the exit code of the program
  ## under normal circumstances. When the program is terminated
  ## prematurely using ``quit``, this value is ignored.

proc quit*(errorcode: int = QuitSuccess) {.
  magic: "Exit", importc: "exit", header: "<stdlib.h>", noReturn.}
  ## Stops the program immediately with an exit code.
  ##
  ## Before stopping the program the "quit procedures" are called in the
  ## opposite order they were added with ``addQuitProc``. ``quit`` never
  ## returns and ignores any exception that may have been raised by the quit
  ## procedures.  It does *not* call the garbage collector to free all the
  ## memory, unless a quit procedure calls ``GC_collect``.
  ##
  ## The proc ``quit(QuitSuccess)`` is called implicitly when your nimrod
  ## program finishes without incident. A raised unhandled exception is
  ## equivalent to calling ``quit(QuitFailure)``.
  ##
  ## Note that this is a *runtime* call and using ``quit`` inside a macro won't
  ## have any compile time effect. If you need to stop the compiler inside a
  ## macro, use the ``error`` or ``fatal`` pragmas.

template sysAssert(cond: bool, msg: string) =
  when defined(useSysAssert):
    if not cond:
      echo "[SYSASSERT] ", msg
      quit 1

when not defined(JS) and not defined(nimrodVm) and hostOS != "standalone":
  include "system/cgprocs"

proc add *[T](x: var seq[T], y: T) {.magic: "AppendSeqElem", noSideEffect.}
proc add *[T](x: var seq[T], y: openArray[T]) {.noSideEffect.} =
  ## Generic proc for adding a data item `y` to a container `x`.
  ## For containers that have an order, `add` means *append*. New generic
  ## containers should also call their adding proc `add` for consistency.
  ## Generic code becomes much easier to write if the Nimrod naming scheme is
  ## respected.
  let xl = x.len
  setLen(x, xl + y.len)
  for i in 0..high(y): x[xl+i] = y[i]

proc shallowCopy*[T](x: var T, y: T) {.noSideEffect, magic: "ShallowCopy".}
  ## use this instead of `=` for a `shallow copy`:idx:. The shallow copy
  ## only changes the semantics for sequences and strings (and types which
  ## contain those). Be careful with the changed semantics though! There 
  ## is a reason why the default assignment does a deep copy of sequences
  ## and strings.

proc del*[T](x: var seq[T], i: int) {.noSideEffect.} = 
  ## deletes the item at index `i` by putting ``x[high(x)]`` into position `i`.
  ## This is an O(1) operation.
  let xl = x.len
  shallowCopy(x[i], x[xl-1])
  setLen(x, xl-1)
  
proc delete*[T](x: var seq[T], i: int) {.noSideEffect.} = 
  ## deletes the item at index `i` by moving ``x[i+1..]`` by one position.
  ## This is an O(n) operation.
  let xl = x.len
  for j in i..xl-2: shallowCopy(x[j], x[j+1]) 
  setLen(x, xl-1)
  
proc insert*[T](x: var seq[T], item: T, i = 0) {.noSideEffect.} = 
  ## inserts `item` into `x` at position `i`.
  let xl = x.len
  setLen(x, xl+1)
  var j = xl-1
  while j >= i:
    shallowCopy(x[j+1], x[j])
    dec(j)
  x[i] = item

proc repr*[T](x: T): string {.magic: "Repr", noSideEffect.}
  ## takes any Nimrod variable and returns its string representation. It
  ## works even for complex data graphs with cycles. This is a great
  ## debugging tool.

type
  TAddress* = int
    ## is the signed integer type that should be used for converting
    ## pointers to integer addresses for readability.

  BiggestInt* = int64
    ## is an alias for the biggest signed integer type the Nimrod compiler
    ## supports. Currently this is ``int64``, but it is platform-dependant
    ## in general.

  BiggestFloat* = float64
    ## is an alias for the biggest floating point type the Nimrod
    ## compiler supports. Currently this is ``float64``, but it is
    ## platform-dependant in general.

when defined(windows):
  type
    clong* {.importc: "long", nodecl.} = int32
      ## This is the same as the type ``long`` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint32
      ## This is the same as the type ``unsigned long`` in *C*.
else:
  type
    clong* {.importc: "long", nodecl.} = int
      ## This is the same as the type ``long`` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint
      ## This is the same as the type ``unsigned long`` in *C*.

type # these work for most platforms:
  cchar* {.importc: "char", nodecl.} = char
    ## This is the same as the type ``char`` in *C*.
  cschar* {.importc: "signed char", nodecl.} = int8
    ## This is the same as the type ``signed char`` in *C*.
  cshort* {.importc: "short", nodecl.} = int16
    ## This is the same as the type ``short`` in *C*.
  cint* {.importc: "int", nodecl.} = int32
    ## This is the same as the type ``int`` in *C*.
  csize* {.importc: "size_t", nodecl.} = int
    ## This is the same as the type ``size_t`` in *C*.
  clonglong* {.importc: "long long", nodecl.} = int64
    ## This is the same as the type ``long long`` in *C*.
  cfloat* {.importc: "float", nodecl.} = float32
    ## This is the same as the type ``float`` in *C*.
  cdouble* {.importc: "double", nodecl.} = float64
    ## This is the same as the type ``double`` in *C*.
  clongdouble* {.importc: "long double", nodecl.} = BiggestFloat
    ## This is the same as the type ``long double`` in *C*.
    ## This C type is not supported by Nimrod's code generator

  cuchar* {.importc: "unsigned char", nodecl.} = char
    ## This is the same as the type ``unsigned char`` in *C*.
  cushort* {.importc: "unsigned short", nodecl.} = uint16
    ## This is the same as the type ``unsigned short`` in *C*.
  cuint* {.importc: "int", nodecl.} = uint32
    ## This is the same as the type ``unsigned int`` in *C*.
  culonglong* {.importc: "unsigned long long", nodecl.} = uint64
    ## This is the same as the type ``unsigned long long`` in *C*.

  cstringArray* {.importc: "char**", nodecl.} = ptr array [0..50_000, cstring]
    ## This is binary compatible to the type ``char**`` in *C*. The array's
    ## high value is large enough to disable bounds checking in practice.
    ## Use `cstringArrayToSeq` to convert it into a ``seq[string]``.
  
  PFloat32* = ptr float32 ## an alias for ``ptr float32``
  PFloat64* = ptr float64 ## an alias for ``ptr float64``
  PInt64* = ptr int64 ## an alias for ``ptr int64``
  PInt32* = ptr int32 ## an alias for ``ptr int32``

proc toFloat*(i: int): float {.
  magic: "ToFloat", noSideEffect, importc: "toFloat".}
  ## converts an integer `i` into a ``float``. If the conversion
  ## fails, `EInvalidValue` is raised. However, on most platforms the
  ## conversion cannot fail.

proc toBiggestFloat*(i: BiggestInt): BiggestFloat {.
  magic: "ToBiggestFloat", noSideEffect, importc: "toBiggestFloat".}
  ## converts an biggestint `i` into a ``biggestfloat``. If the conversion
  ## fails, `EInvalidValue` is raised. However, on most platforms the
  ## conversion cannot fail.

proc toInt*(f: float): int {.
  magic: "ToInt", noSideEffect, importc: "toInt".}
  ## converts a floating point number `f` into an ``int``. Conversion
  ## rounds `f` if it does not contain an integer value. If the conversion
  ## fails (because `f` is infinite for example), `EInvalidValue` is raised.

proc toBiggestInt*(f: BiggestFloat): BiggestInt {.
  magic: "ToBiggestInt", noSideEffect, importc: "toBiggestInt".}
  ## converts a biggestfloat `f` into a ``biggestint``. Conversion
  ## rounds `f` if it does not contain an integer value. If the conversion
  ## fails (because `f` is infinite for example), `EInvalidValue` is raised.

proc addQuitProc*(QuitProc: proc() {.noconv.}) {.
  importc: "atexit", header: "<stdlib.h>".}
  ## adds/registers a quit procedure. Each call to ``addQuitProc``
  ## registers another quit procedure. Up to 30 procedures can be
  ## registered. They are executed on a last-in, first-out basis
  ## (that is, the last function registered is the first to be executed).
  ## ``addQuitProc`` raises an EOutOfIndex if ``quitProc`` cannot be
  ## registered.

# Support for addQuitProc() is done by Ansi C's facilities here.
# In case of an unhandled exeption the exit handlers should
# not be called explicitly! The user may decide to do this manually though.

proc copy*(s: string, first = 0): string {.
  magic: "CopyStr", importc: "copyStr", noSideEffect, deprecated.}
proc copy*(s: string, first, last: int): string {.
  magic: "CopyStrLast", importc: "copyStrLast", noSideEffect, 
  deprecated.}
  ## copies a slice of `s` into a new string and returns this new
  ## string. The bounds `first` and `last` denote the indices of
  ## the first and last characters that shall be copied. If ``last``
  ## is omitted, it is treated as ``high(s)``.
  ## **Deprecated since version 0.8.12**: Use ``substr`` instead.

proc substr*(s: string, first = 0): string {.
  magic: "CopyStr", importc: "copyStr", noSideEffect.}
proc substr*(s: string, first, last: int): string {.
  magic: "CopyStrLast", importc: "copyStrLast", noSideEffect.}
  ## copies a slice of `s` into a new string and returns this new
  ## string. The bounds `first` and `last` denote the indices of
  ## the first and last characters that shall be copied. If ``last``
  ## is omitted, it is treated as ``high(s)``. If ``last >= s.len``, ``s.len``
  ## is used instead: This means ``substr`` can also be used to `cut`:idx:
  ## or `limit`:idx: a string's length.

when not defined(nimrodVM):
  proc zeroMem*(p: pointer, size: int) {.importc, noDecl, gcsafe.}
    ## overwrites the contents of the memory at ``p`` with the value 0.
    ## Exactly ``size`` bytes will be overwritten. Like any procedure
    ## dealing with raw memory this is *unsafe*.

  proc copyMem*(dest, source: pointer, size: int) {.
    importc: "memcpy", header: "<string.h>", gcsafe.}
    ## copies the contents from the memory at ``source`` to the memory
    ## at ``dest``. Exactly ``size`` bytes will be copied. The memory
    ## regions may not overlap. Like any procedure dealing with raw
    ## memory this is *unsafe*.

  proc moveMem*(dest, source: pointer, size: int) {.
    importc: "memmove", header: "<string.h>", gcsafe.}
    ## copies the contents from the memory at ``source`` to the memory
    ## at ``dest``. Exactly ``size`` bytes will be copied. The memory
    ## regions may overlap, ``moveMem`` handles this case appropriately
    ## and is thus somewhat more safe than ``copyMem``. Like any procedure
    ## dealing with raw memory this is still *unsafe*, though.

  proc equalMem*(a, b: pointer, size: int): bool {.
    importc: "equalMem", noDecl, noSideEffect.}
    ## compares the memory blocks ``a`` and ``b``. ``size`` bytes will
    ## be compared. If the blocks are equal, true is returned, false
    ## otherwise. Like any procedure dealing with raw memory this is
    ## *unsafe*.

  when hostOS != "standalone":
    proc alloc*(size: int): pointer {.noconv, rtl, tags: [], gcsafe.}
      ## allocates a new memory block with at least ``size`` bytes. The
      ## block has to be freed with ``realloc(block, 0)`` or
      ## ``dealloc(block)``. The block is not initialized, so reading
      ## from it before writing to it is undefined behaviour!
      ## The allocated memory belongs to its allocating thread!
      ## Use `allocShared` to allocate from a shared heap.
    proc createU*(T: typedesc, size = 1.Positive): ptr T {.inline, gcsafe.} =
      ## allocates a new memory block with at least ``T.sizeof * size``
      ## bytes. The block has to be freed with ``resize(block, 0)`` or
      ## ``free(block)``. The block is not initialized, so reading
      ## from it before writing to it is undefined behaviour!
      ## The allocated memory belongs to its allocating thread!
      ## Use `createSharedU` to allocate from a shared heap.
      cast[ptr T](alloc(T.sizeof * size))
    proc alloc0*(size: int): pointer {.noconv, rtl, tags: [], gcsafe.}
      ## allocates a new memory block with at least ``size`` bytes. The
      ## block has to be freed with ``realloc(block, 0)`` or
      ## ``dealloc(block)``. The block is initialized with all bytes
      ## containing zero, so it is somewhat safer than ``alloc``.
      ## The allocated memory belongs to its allocating thread!
      ## Use `allocShared0` to allocate from a shared heap.
    proc create*(T: typedesc, size = 1.Positive): ptr T {.inline, gcsafe.} =
      ## allocates a new memory block with at least ``T.sizeof * size``
      ## bytes. The block has to be freed with ``resize(block, 0)`` or
      ## ``free(block)``. The block is initialized with all bytes
      ## containing zero, so it is somewhat safer than ``createU``.
      ## The allocated memory belongs to its allocating thread!
      ## Use `createShared` to allocate from a shared heap.
      cast[ptr T](alloc0(T.sizeof * size))
    proc realloc*(p: pointer, newSize: int): pointer {.noconv, rtl, tags: [], 
                                                       gcsafe.}
      ## grows or shrinks a given memory block. If p is **nil** then a new
      ## memory block is returned. In either way the block has at least
      ## ``newSize`` bytes. If ``newSize == 0`` and p is not **nil**
      ## ``realloc`` calls ``dealloc(p)``. In other cases the block has to
      ## be freed with ``dealloc``.
      ## The allocated memory belongs to its allocating thread!
      ## Use `reallocShared` to reallocate from a shared heap.
    proc resize*[T](p: ptr T, newSize: Natural): ptr T {.inline, gcsafe.} =
      ## grows or shrinks a given memory block. If p is **nil** then a new
      ## memory block is returned. In either way the block has at least
      ## ``T.sizeof * newSize`` bytes. If ``newSize == 0`` and p is not
      ## **nil** ``resize`` calls ``free(p)``. In other cases the block
      ## has to be freed with ``free``. The allocated memory belongs to
      ## its allocating thread!
      ## Use `resizeShared` to reallocate from a shared heap.
      cast[ptr T](realloc(p, T.sizeof * newSize))
    proc dealloc*(p: pointer) {.noconv, rtl, tags: [], gcsafe.}
      ## frees the memory allocated with ``alloc``, ``alloc0`` or
      ## ``realloc``. This procedure is dangerous! If one forgets to
      ## free the memory a leak occurs; if one tries to access freed
      ## memory (or just freeing it twice!) a core dump may happen
      ## or other memory may be corrupted. 
      ## The freed memory must belong to its allocating thread!
      ## Use `deallocShared` to deallocate from a shared heap.
    proc free*[T](p: ptr T) {.inline, gcsafe.} =
      dealloc(p)
    proc allocShared*(size: int): pointer {.noconv, rtl, gcsafe.}
      ## allocates a new memory block on the shared heap with at
      ## least ``size`` bytes. The block has to be freed with
      ## ``reallocShared(block, 0)`` or ``deallocShared(block)``. The block
      ## is not initialized, so reading from it before writing to it is 
      ## undefined behaviour!
    proc createSharedU*(T: typedesc, size = 1.Positive): ptr T {.inline, 
                                                                 gcsafe.} =
      ## allocates a new memory block on the shared heap with at
      ## least ``T.sizeof * size`` bytes. The block has to be freed with
      ## ``resizeShared(block, 0)`` or ``freeShared(block)``. The block
      ## is not initialized, so reading from it before writing to it is 
      ## undefined behaviour!
      cast[ptr T](allocShared(T.sizeof * size))
    proc allocShared0*(size: int): pointer {.noconv, rtl, gcsafe.}
      ## allocates a new memory block on the shared heap with at 
      ## least ``size`` bytes. The block has to be freed with
      ## ``reallocShared(block, 0)`` or ``deallocShared(block)``.
      ## The block is initialized with all bytes
      ## containing zero, so it is somewhat safer than ``allocShared``.
    proc createShared*(T: typedesc, size = 1.Positive): ptr T {.inline.} =
      ## allocates a new memory block on the shared heap with at 
      ## least ``T.sizeof * size`` bytes. The block has to be freed with
      ## ``resizeShared(block, 0)`` or ``freeShared(block)``.
      ## The block is initialized with all bytes
      ## containing zero, so it is somewhat safer than ``createSharedU``.
      cast[ptr T](allocShared0(T.sizeof * size))
    proc reallocShared*(p: pointer, newSize: int): pointer {.noconv, rtl, 
                                                             gcsafe.}
      ## grows or shrinks a given memory block on the heap. If p is **nil**
      ## then a new memory block is returned. In either way the block has at
      ## least ``newSize`` bytes. If ``newSize == 0`` and p is not **nil**
      ## ``reallocShared`` calls ``deallocShared(p)``. In other cases the
      ## block has to be freed with ``deallocShared``.
    proc resizeShared*[T](p: ptr T, newSize: Natural): ptr T {.inline.} =
      ## grows or shrinks a given memory block on the heap. If p is **nil**
      ## then a new memory block is returned. In either way the block has at
      ## least ``T.sizeof * newSize`` bytes. If ``newSize == 0`` and p is
      ## not **nil** ``resizeShared`` calls ``freeShared(p)``. In other
      ## cases the block has to be freed with ``freeShared``.
      cast[ptr T](reallocShared(p, T.sizeof * newSize))
    proc deallocShared*(p: pointer) {.noconv, rtl, gcsafe.}
      ## frees the memory allocated with ``allocShared``, ``allocShared0`` or
      ## ``reallocShared``. This procedure is dangerous! If one forgets to
      ## free the memory a leak occurs; if one tries to access freed
      ## memory (or just freeing it twice!) a core dump may happen
      ## or other memory may be corrupted.
    proc freeShared*[T](p: ptr T) {.inline, gcsafe.} =
      ## frees the memory allocated with ``createShared``, ``createSharedU`` or
      ## ``resizeShared``. This procedure is dangerous! If one forgets to
      ## free the memory a leak occurs; if one tries to access freed
      ## memory (or just freeing it twice!) a core dump may happen
      ## or other memory may be corrupted.
      deallocShared(p)

proc swap*[T](a, b: var T) {.magic: "Swap", noSideEffect.}
  ## swaps the values `a` and `b`. This is often more efficient than
  ## ``tmp = a; a = b; b = tmp``. Particularly useful for sorting algorithms.

template `>=%` *(x, y: expr): expr {.immediate.} = y <=% x
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) >= unsigned(y)``.

template `>%` *(x, y: expr): expr {.immediate.} = y <% x
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) > unsigned(y)``.

proc `$` *(x: int): string {.magic: "IntToStr", noSideEffect.}
  ## The stringify operator for an integer argument. Returns `x`
  ## converted to a decimal string.

proc `$` *(x: int64): string {.magic: "Int64ToStr", noSideEffect.}
  ## The stringify operator for an integer argument. Returns `x`
  ## converted to a decimal string.

when not defined(NimrodVM):
  when not defined(JS) and hostOS != "standalone":
    proc `$` *(x: uint64): string {.noSideEffect.}
      ## The stringify operator for an unsigned integer argument. Returns `x`
      ## converted to a decimal string.

proc `$` *(x: float): string {.magic: "FloatToStr", noSideEffect.}
  ## The stringify operator for a float argument. Returns `x`
  ## converted to a decimal string.

proc `$` *(x: bool): string {.magic: "BoolToStr", noSideEffect.}
  ## The stringify operator for a boolean argument. Returns `x`
  ## converted to the string "false" or "true".

proc `$` *(x: char): string {.magic: "CharToStr", noSideEffect.}
  ## The stringify operator for a character argument. Returns `x`
  ## converted to a string.

proc `$` *(x: cstring): string {.magic: "CStrToStr", noSideEffect.}
  ## The stringify operator for a CString argument. Returns `x`
  ## converted to a string.

proc `$` *(x: string): string {.magic: "StrToStr", noSideEffect.}
  ## The stringify operator for a string argument. Returns `x`
  ## as it is. This operator is useful for generic code, so
  ## that ``$expr`` also works if ``expr`` is already a string.

proc `$` *[TEnum: enum](x: TEnum): string {.magic: "EnumToStr", noSideEffect.}
  ## The stringify operator for an enumeration argument. This works for
  ## any enumeration type thanks to compiler magic. If
  ## a ``$`` operator for a concrete enumeration is provided, this is
  ## used instead. (In other words: *Overwriting* is possible.)

# undocumented:
proc getRefcount*[T](x: ref T): int {.importc: "getRefcount", noSideEffect.}
proc getRefcount*(x: string): int {.importc: "getRefcount", noSideEffect.}
proc getRefcount*[T](x: seq[T]): int {.importc: "getRefcount", noSideEffect.}
  ## retrieves the reference count of an heap-allocated object. The
  ## value is implementation-dependent.


const
  Inf* {.magic: "Inf".} = 1.0 / 0.0
    ## contains the IEEE floating point value of positive infinity.
  NegInf* {.magic: "NegInf".} = -Inf
    ## contains the IEEE floating point value of negative infinity.
  NaN* {.magic: "NaN".} = 0.0 / 0.0
    ## contains an IEEE floating point value of *Not A Number*. Note
    ## that you cannot compare a floating point value to this value
    ## and expect a reasonable result - use the `classify` procedure
    ## in the module ``math`` for checking for NaN.

# GC interface:

when not defined(nimrodVM) and hostOS != "standalone":
  proc getOccupiedMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process and hold data.

  proc getFreeMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process, but do not
    ## hold any meaningful data.

  proc getTotalMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process.

  when hasThreadSupport:
    proc getOccupiedSharedMem*(): int {.rtl.}
      ## returns the number of bytes that are owned by the process
      ## on the shared heap and hold data. This is only available when
      ## threads are enabled.

    proc getFreeSharedMem*(): int {.rtl.}
      ## returns the number of bytes that are owned by the
      ## process on the shared heap, but do not hold any meaningful data.
      ## This is only available when threads are enabled.

    proc getTotalSharedMem*(): int {.rtl.}
      ## returns the number of bytes on the shared heap that are owned by the
      ## process. This is only available when threads are enabled.

iterator countdown*[T](a, b: T, step = 1): T {.inline.} =
  ## Counts from ordinal value `a` down to `b` with the given
  ## step count. `T` may be any ordinal type, `step` may only
  ## be positive.
  var res = a
  while res >= b:
    yield res
    dec(res, step)

iterator countup*[S, T](a: S, b: T, step = 1): T {.inline.} =
  ## Counts from ordinal value `a` up to `b` with the given
  ## step count. `S`, `T` may be any ordinal type, `step` may only
  ## be positive.
  var res: T = T(a)
  while res <= b:
    yield res
    inc(res, step)

iterator `..`*[S, T](a: S, b: T): T {.inline.} =
  ## An alias for `countup`.
  var res: T = T(a)
  while res <= b:
    yield res
    inc res

iterator `||`*[S, T](a: S, b: T, annotation=""): T {.
  inline, magic: "OmpParFor", sideEffect.} =
  ## parallel loop iterator. Same as `..` but the loop may run in parallel.
  ## `annotation` is an additional annotation for the code generator to use.
  ## Note that the compiler maps that to
  ## the ``#pragma omp parallel for`` construct of `OpenMP`:idx: and as
  ## such isn't aware of the parallelism in your code! Be careful! Later
  ## versions of ``||`` will get proper support by Nimrod's code generator
  ## and GC.
  discard

{.push stackTrace:off.}
proc min*(x, y: int): int {.magic: "MinI", noSideEffect.} =
  if x <= y: x else: y
proc min*(x, y: int8): int8 {.magic: "MinI", noSideEffect.} =
  if x <= y: x else: y
proc min*(x, y: int16): int16 {.magic: "MinI", noSideEffect.} =
  if x <= y: x else: y
proc min*(x, y: int32): int32 {.magic: "MinI", noSideEffect.} =
  if x <= y: x else: y
proc min*(x, y: int64): int64 {.magic: "MinI64", noSideEffect.} =
  ## The minimum value of two integers.
  if x <= y: x else: y

proc min*[T](x: varargs[T]): T =
  ## The minimum value of `x`. ``T`` needs to have a ``<`` operator.
  result = x[0]
  for i in 1..high(x):
    if x[i] < result: result = x[i]

proc max*(x, y: int): int {.magic: "MaxI", noSideEffect.} =
  if y <= x: x else: y
proc max*(x, y: int8): int8 {.magic: "MaxI", noSideEffect.} =
  if y <= x: x else: y
proc max*(x, y: int16): int16 {.magic: "MaxI", noSideEffect.} =
  if y <= x: x else: y
proc max*(x, y: int32): int32 {.magic: "MaxI", noSideEffect.} =
  if y <= x: x else: y
proc max*(x, y: int64): int64 {.magic: "MaxI64", noSideEffect.} =
  ## The maximum value of two integers.
  if y <= x: x else: y

proc max*[T](x: varargs[T]): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  result = x[0]
  for i in 1..high(x):
    if result < x[i]: result = x[i]

proc abs*(x: float): float {.magic: "AbsF64", noSideEffect.} =
  if x < 0.0: -x else: x
proc min*(x, y: float): float {.magic: "MinF64", noSideEffect.} =
  if x <= y: x else: y
proc max*(x, y: float): float {.magic: "MaxF64", noSideEffect.} =
  if y <= x: x else: y
{.pop.}

proc clamp*[T](x, a, b: T): T =
  ## limits the value ``x`` within the interval [a, b]
  ##
  ## .. code-block:: Nimrod
  ##   assert((1.4).clamp(0.0, 1.0) == 1.0)
  ##   assert((0.5).clamp(0.0, 1.0) == 0.5)
  if x < a: return a
  if x > b: return b
  return x

iterator items*[T](a: openArray[T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while i < len(a):
    yield a[i]
    inc(i)

iterator items*[IX, T](a: array[IX, T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = low(IX)
  if i <= high(IX):
    while true:
      yield a[i]
      if i >= high(IX): break
      inc(i)

iterator items*[T](a: set[T]): T {.inline.} =
  ## iterates over each element of `a`. `items` iterates only over the
  ## elements that are really in the set (and not over the ones the set is
  ## able to hold).
  var i = low(T)
  if i <= high(T):
    while true:
      if i in a: yield i
      if i >= high(T): break
      inc(i)

iterator items*(a: cstring): char {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while a[i] != '\0':
    yield a[i]
    inc(i)

iterator items*(E: typedesc[enum]): E =
  ## iterates over the values of the enum ``E``.
  for v in low(E)..high(E):
    yield v

iterator pairs*[T](a: openArray[T]): tuple[key: int, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)

iterator pairs*[IX, T](a: array[IX, T]): tuple[key: IX, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = low(IX)
  if i <= high(IX):
    while true:
      yield (i, a[i])
      if i >= high(IX): break
      inc(i)

iterator pairs*[T](a: seq[T]): tuple[key: int, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)

iterator pairs*(a: string): tuple[key: int, val: char] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)


proc isNil*[T](x: seq[T]): bool {.noSideEffect, magic: "IsNil".}
proc isNil*[T](x: ref T): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: string): bool {.noSideEffect, magic: "IsNil".}
proc isNil*[T](x: ptr T): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: pointer): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: cstring): bool {.noSideEffect, magic: "IsNil".}
proc isNil*[T: proc](x: T): bool {.noSideEffect, magic: "IsNil".}
  ## Fast check whether `x` is nil. This is sometimes more efficient than
  ## ``== nil``.

proc `==` *[I, T](x, y: array[I, T]): bool =
  for f in low(x)..high(x):
    if x[f] != y[f]:
      return
  result = true

proc `@`*[T](a: openArray[T]): seq[T] = 
  ## turns an openarray into a sequence. This is not as efficient as turning
  ## a fixed length array into a sequence as it always copies every element
  ## of `a`.
  newSeq(result, a.len)
  for i in 0..a.len-1: result[i] = a[i]

proc `&` *[T](x, y: seq[T]): seq[T] {.noSideEffect.} =
  ## Concatenates two sequences.
  ## Requires copying of the sequences.
  ##
  ## .. code-block:: Nimrod
  ##   assert(@[1, 2, 3, 4] & @[5, 6] == @[1, 2, 3, 4, 5, 6])
  newSeq(result, x.len + y.len)
  for i in 0..x.len-1:
    result[i] = x[i]
  for i in 0..y.len-1:
    result[i+x.len] = y[i]

proc `&` *[T](x: seq[T], y: T): seq[T] {.noSideEffect.} =
  ## Appends element y to the end of the sequence.
  ## Requires copying of the sequence
  ##
  ## .. code-block:: Nimrod
  ##   assert(@[1, 2, 3] & 4 == @[1, 2, 3, 4])
  newSeq(result, x.len + 1)
  for i in 0..x.len-1:
    result[i] = x[i]
  result[x.len] = y

proc `&` *[T](x: T, y: seq[T]): seq[T] {.noSideEffect.} =
  ## Prepends the element x to the beginning of the sequence.
  ## Requires copying of the sequence
  ##
  ## .. code-block:: Nimrod
  ##   assert(1 & @[2, 3, 4] == @[1, 2, 3, 4])
  newSeq(result, y.len + 1)
  result[0] = x
  for i in 0..y.len-1:
    result[i+1] = y[i]

when not defined(NimrodVM):
  when not defined(JS):
    proc seqToPtr[T](x: seq[T]): pointer {.inline, nosideeffect.} =
      result = cast[pointer](x)
  else:
    proc seqToPtr[T](x: seq[T]): pointer {.asmNoStackFrame, nosideeffect.} =
      asm """return `x`"""
  
  proc `==` *[T](x, y: seq[T]): bool {.noSideEffect.} =
    ## Generic equals operator for sequences: relies on a equals operator for
    ## the element type `T`.
    if seqToPtr(x) == seqToPtr(y):
      result = true
    elif seqToPtr(x) == nil or seqToPtr(y) == nil:
      result = false
    elif x.len == y.len:
      for i in 0..x.len-1:
        if x[i] != y[i]: return false
      result = true

proc find*[T, S](a: T, item: S): int {.inline.}=
  ## Returns the first index of `item` in `a` or -1 if not found. This requires
  ## appropriate `items` and `==` operations to work.
  for i in items(a):
    if i == item: return
    inc(result)
  result = -1

proc contains*[T](a: openArray[T], item: T): bool {.inline.}=
  ## Returns true if `item` is in `a` or false if not found. This is a shortcut
  ## for ``find(a, item) >= 0``.
  return find(a, item) >= 0

proc pop*[T](s: var seq[T]): T {.inline, noSideEffect.} = 
  ## returns the last item of `s` and decreases ``s.len`` by one. This treats
  ## `s` as a stack and implements the common *pop* operation.
  var L = s.len-1
  result = s[L]
  setLen(s, L)

proc each*[T, S](data: openArray[T], op: proc (x: T): S {.closure.}): seq[S] {.
  deprecated.} =
  ## The well-known ``map`` operation from functional programming. Applies
  ## `op` to every item in `data` and returns the result as a sequence.
  ##
  ## **Deprecated since version 0.9:** Use the ``map`` proc instead.
  newSeq(result, data.len)
  for i in 0..data.len-1: result[i] = op(data[i])

proc each*[T](data: var openArray[T], op: proc (x: var T) {.closure.}) {.
  deprecated.} =
  ## The well-known ``map`` operation from functional programming. Applies
  ## `op` to every item in `data` modifying it directly.
  ##
  ## **Deprecated since version 0.9:** Use the ``map`` proc instead.
  for i in 0..data.len-1: op(data[i])

proc map*[T, S](data: openArray[T], op: proc (x: T): S {.closure.}): seq[S] =
  ## Returns a new sequence with the results of `op` applied to every item in
  ## `data`.
  ##
  ## Since the input is not modified you can use this version of ``map`` to
  ## transform the type of the elements in the input sequence. Example:
  ##
  ## .. code-block:: nimrod
  ##   let
  ##     a = @[1, 2, 3, 4]
  ##     b = map(a, proc(x: int): string = $x)
  ##   assert b == @["1", "2", "3", "4"]
  newSeq(result, data.len)
  for i in 0..data.len-1: result[i] = op(data[i])

proc map*[T](data: var openArray[T], op: proc (x: var T) {.closure.}) =
  ## Applies `op` to every item in `data` modifying it directly.
  ##
  ## Note that this version of ``map`` requires your input and output types to
  ## be the same, since they are modified in-place. Example:
  ##
  ## .. code-block:: nimrod
  ##   var a = @["1", "2", "3", "4"]
  ##   echo repr(a)
  ##   # --> ["1", "2", "3", "4"]
  ##   map(a, proc(x: var string) = x &= "42")
  ##   echo repr(a)
  ##   # --> ["142", "242", "342", "442"]
  for i in 0..data.len-1: op(data[i])

iterator fields*[T: tuple|object](x: T): TObject {.
  magic: "Fields", noSideEffect.}
  ## iterates over every field of `x`. Warning: This really transforms
  ## the 'for' and unrolls the loop. The current implementation also has a bug
  ## that affects symbol binding in the loop body.
iterator fields*[S:tuple|object, T:tuple|object](x: S, y: T): tuple[a,b: expr] {.
  magic: "Fields", noSideEffect.}
  ## iterates over every field of `x` and `y`.
  ## Warning: This is really transforms the 'for' and unrolls the loop. 
  ## The current implementation also has a bug that affects symbol binding
  ## in the loop body.
iterator fieldPairs*[T: tuple|object](x: T): TObject {.
  magic: "FieldPairs", noSideEffect.}
  ## Iterates over every field of `x` returning their name and value.
  ##
  ## When you iterate over objects with different field types you have to use
  ## the compile time ``when`` instead of a runtime ``if`` to select the code
  ## you want to run for each type. To perform the comparison use the `is
  ## operator <manual.html#is-operator>`_. Example:
  ##
  ## .. code-block:: Nimrod
  ##
  ##   type
  ##     Custom = object
  ##       foo: string
  ##       bar: bool
  ##
  ##   proc `$`(x: Custom): string =
  ##     result = "Custom:"
  ##     for name, value in x.fieldPairs:
  ##       when value is bool:
  ##         result.add("\n\t" & name & " is " & $value)
  ##       else:
  ##         if value.isNil:
  ##           result.add("\n\t" & name & " (nil)")
  ##         else:
  ##           result.add("\n\t" & name & " '" & value & "'")
  ##
  ## Another way to do the same without ``when`` is to leave the task of
  ## picking the appropriate code to a secondary proc which you overload for
  ## each field type and pass the `value` to.
  ##
  ## Warning: This really transforms the 'for' and unrolls the loop. The
  ## current implementation also has a bug that affects symbol binding in the
  ## loop body.
iterator fieldPairs*[S: tuple|object, T: tuple|object](x: S, y: T): tuple[
  a, b: expr] {.
  magic: "FieldPairs", noSideEffect.}
  ## iterates over every field of `x` and `y`.
  ## Warning: This really transforms the 'for' and unrolls the loop. 
  ## The current implementation also has a bug that affects symbol binding
  ## in the loop body.

proc `==`*[T: tuple|object](x, y: T): bool = 
  ## generic ``==`` operator for tuples that is lifted from the components
  ## of `x` and `y`.
  for a, b in fields(x, y):
    if a != b: return false
  return true

proc `<=`*[T: tuple](x, y: T): bool = 
  ## generic ``<=`` operator for tuples that is lifted from the components
  ## of `x` and `y`. This implementation uses `cmp`.
  for a, b in fields(x, y):
    var c = cmp(a, b)
    if c < 0: return true
    if c > 0: return false
  return true

proc `<`*[T: tuple](x, y: T): bool = 
  ## generic ``<`` operator for tuples that is lifted from the components
  ## of `x` and `y`. This implementation uses `cmp`.
  for a, b in fields(x, y):
    var c = cmp(a, b)
    if c < 0: return true
    if c > 0: return false
  return false

proc `$`*[T: tuple|object](x: T): string = 
  ## generic ``$`` operator for tuples that is lifted from the components
  ## of `x`. Example:
  ##
  ## .. code-block:: nimrod
  ##   $(23, 45) == "(23, 45)"
  ##   $() == "()"
  result = "("
  var firstElement = true
  for name, value in fieldPairs(x):
    if not(firstElement): result.add(", ")
    result.add(name)
    result.add(": ")
    result.add($value)
    firstElement = false
  result.add(")")
  
proc collectionToString[T](x: T, b, e: string): string =
  result = b
  var firstElement = true
  for value in items(x):
    if not firstElement: result.add(", ")
    result.add($value)
    firstElement = false
  result.add(e)

proc `$`*[T](x: set[T]): string = 
  ## generic ``$`` operator for sets that is lifted from the components
  ## of `x`. Example:
  ##
  ## .. code-block:: nimrod
  ##   ${23, 45} == "{23, 45}"
  collectionToString(x, "{", "}")

proc `$`*[T](x: seq[T]): string = 
  ## generic ``$`` operator for seqs that is lifted from the components
  ## of `x`. Example:
  ##
  ## .. code-block:: nimrod
  ##   $(@[23, 45]) == "@[23, 45]"
  collectionToString(x, "@[", "]")

when false:
  # causes bootstrapping to fail as we use array of chars and cstring should
  # match better ...
  proc `$`*[T, IDX](x: array[IDX, T]): string = 
    collectionToString(x, "[", "]")

# ----------------- GC interface ---------------------------------------------

when not defined(nimrodVM) and hostOS != "standalone":
  proc GC_disable*() {.rtl, inl.}
    ## disables the GC. If called n-times, n calls to `GC_enable` are needed to
    ## reactivate the GC. Note that in most circumstances one should only disable
    ## the mark and sweep phase with `GC_disableMarkAndSweep`.

  proc GC_enable*() {.rtl, inl.}
    ## enables the GC again.

  proc GC_fullCollect*() {.rtl.}
    ## forces a full garbage collection pass.
    ## Ordinary code does not need to call this (and should not).

  type
    TGC_Strategy* = enum ## the strategy the GC should use for the application
      gcThroughput,      ## optimize for throughput
      gcResponsiveness,  ## optimize for responsiveness (default)
      gcOptimizeTime,    ## optimize for speed
      gcOptimizeSpace    ## optimize for memory footprint

  proc GC_setStrategy*(strategy: TGC_Strategy) {.rtl, deprecated.}
    ## tells the GC the desired strategy for the application.
    ## **Deprecated** since version 0.8.14. This has always been a nop.

  proc GC_enableMarkAndSweep*() {.rtl.}
  proc GC_disableMarkAndSweep*() {.rtl.}
    ## the current implementation uses a reference counting garbage collector
    ## with a seldomly run mark and sweep phase to free cycles. The mark and
    ## sweep phase may take a long time and is not needed if the application
    ## does not create cycles. Thus the mark and sweep phase can be deactivated
    ## and activated separately from the rest of the GC.

  proc GC_getStatistics*(): string {.rtl.}
    ## returns an informative string about the GC's activity. This may be useful
    ## for tweaking.
    
  proc GC_ref*[T](x: ref T) {.magic: "GCref", gcsafe.}
  proc GC_ref*[T](x: seq[T]) {.magic: "GCref", gcsafe.}
  proc GC_ref*(x: string) {.magic: "GCref", gcsafe.}
    ## marks the object `x` as referenced, so that it will not be freed until
    ## it is unmarked via `GC_unref`. If called n-times for the same object `x`,
    ## n calls to `GC_unref` are needed to unmark `x`. 
    
  proc GC_unref*[T](x: ref T) {.magic: "GCunref", gcsafe.}
  proc GC_unref*[T](x: seq[T]) {.magic: "GCunref", gcsafe.}
  proc GC_unref*(x: string) {.magic: "GCunref", gcsafe.}
    ## see the documentation of `GC_ref`.

template accumulateResult*(iter: expr) =
  ## helps to convert an iterator to a proc.
  result = @[]
  for x in iter: add(result, x)

# we have to compute this here before turning it off in except.nim anyway ...
const nimrodStackTrace = compileOption("stacktrace")

{.push checks: off.}
# obviously we cannot generate checking operations here :-)
# because it would yield into an endless recursion
# however, stack-traces are available for most parts
# of the code

var
  globalRaiseHook*: proc (e: ref E_Base): bool {.nimcall, gcsafe.}
    ## with this hook you can influence exception handling on a global level.
    ## If not nil, every 'raise' statement ends up calling this hook. Ordinary
    ## application code should never set this hook! You better know what you
    ## do when setting this. If ``globalRaiseHook`` returns false, the
    ## exception is caught and does not propagate further through the call
    ## stack.

  localRaiseHook* {.threadvar.}: proc (e: ref E_Base): bool {.nimcall, gcsafe.}
    ## with this hook you can influence exception handling on a
    ## thread local level.
    ## If not nil, every 'raise' statement ends up calling this hook. Ordinary
    ## application code should never set this hook! You better know what you
    ## do when setting this. If ``localRaiseHook`` returns false, the exception
    ## is caught and does not propagate further through the call stack.
    
  outOfMemHook*: proc () {.nimcall, tags: [], gcsafe.}
    ## set this variable to provide a procedure that should be called 
    ## in case of an `out of memory`:idx: event. The standard handler
    ## writes an error message and terminates the program. `outOfMemHook` can
    ## be used to raise an exception in case of OOM like so:
    ## 
    ## .. code-block:: nimrod
    ##
    ##   var gOutOfMem: ref EOutOfMemory
    ##   new(gOutOfMem) # need to be allocated *before* OOM really happened!
    ##   gOutOfMem.msg = "out of memory"
    ## 
    ##   proc handleOOM() =
    ##     raise gOutOfMem
    ##
    ##   system.outOfMemHook = handleOOM
    ##
    ## If the handler does not raise an exception, ordinary control flow
    ## continues and the program is terminated.

type
  PFrame* = ptr TFrame  ## represents a runtime frame of the call stack;
                        ## part of the debugger API.
  TFrame* {.importc, nodecl, final.} = object ## the frame itself
    prev*: PFrame       ## previous frame; used for chaining the call stack
    procname*: cstring  ## name of the proc that is currently executing
    line*: int          ## line number of the proc that is currently executing
    filename*: cstring  ## filename of the proc that is currently executing
    len*: int16         ## length of the inspectable slots
    calldepth*: int16   ## used for max call depth checking

when defined(JS):
  proc add*(x: var string, y: cstring) {.asmNoStackFrame.} =
    asm """
      var len = `x`[0].length-1;
      for (var i = 0; i < `y`.length; ++i) {
        `x`[0][len] = `y`.charCodeAt(i);
        ++len;
      }
      `x`[0][len] = 0
    """
  proc add*(x: var cstring, y: cstring) {.magic: "AppendStrStr".}

elif hostOS != "standalone":
  {.push stack_trace:off, profiler:off.}
  proc add*(x: var string, y: cstring) =
    var i = 0
    while y[i] != '\0':
      add(x, y[i])
      inc(i)
  {.pop.}

proc echo*[T](x: varargs[T, `$`]) {.magic: "Echo", tags: [FWriteIO], gcsafe.}
  ## Writes and flushes the parameters to the standard output.
  ##
  ## Special built-in that takes a variable number of arguments. Each argument
  ## is converted to a string via ``$``, so it works for user-defined
  ## types that have an overloaded ``$`` operator.
  ## It is roughly equivalent to ``writeln(stdout, x); flushFile(stdout)``, but
  ## available for the JavaScript target too.
  ##
  ## Unlike other IO operations this is guaranteed to be thread-safe as
  ## ``echo`` is very often used for debugging convenience. If you want to use
  ## ``echo`` inside a `proc without side effects
  ## <manual.html#nosideeffect-pragma>`_ you can use `debugEcho <#debugEcho>`_
  ## instead.

proc debugEcho*[T](x: varargs[T, `$`]) {.magic: "Echo", noSideEffect, 
                                         tags: [], raises: [].}
  ## Same as `echo <#echo>`_, but as a special semantic rule, ``debugEcho``
  ## pretends to be free of side effects, so that it can be used for debugging
  ## routines marked as `noSideEffect <manual.html#nosideeffect-pragma>`_.

template newException*(exceptn: typedesc, message: string): expr =
  ## creates an exception object of type ``exceptn`` and sets its ``msg`` field
  ## to `message`. Returns the new exception object.
  var
    e: ref exceptn
  new(e)
  e.msg = message
  e

when hostOS == "standalone":
  include panicoverride

when not declared(sysFatal):
  template sysFatal(exceptn: typedesc, message: string) =
    when hostOS == "standalone":
      panic(message)
    else:
      var e: ref exceptn
      new(e)
      e.msg = message
      raise e

  template sysFatal(exceptn: typedesc, message, arg: string) =
    when hostOS == "standalone":
      rawoutput(message)
      panic(arg)
    else:
      var e: ref exceptn
      new(e)
      e.msg = message & arg
      raise e

proc getTypeInfo*[T](x: T): pointer {.magic: "GetTypeInfo", gcsafe.}
  ## get type information for `x`. Ordinary code should not use this, but
  ## the `typeinfo` module instead.

{.push stackTrace: off.}
proc abs*(x: int): int {.magic: "AbsI", noSideEffect.} =
  if x < 0: -x else: x
proc abs*(x: int8): int8 {.magic: "AbsI", noSideEffect.} =
  if x < 0: -x else: x
proc abs*(x: int16): int16 {.magic: "AbsI", noSideEffect.} =
  if x < 0: -x else: x
proc abs*(x: int32): int32 {.magic: "AbsI", noSideEffect.} =
  if x < 0: -x else: x
proc abs*(x: int64): int64 {.magic: "AbsI64", noSideEffect.} =
  ## returns the absolute value of `x`. If `x` is ``low(x)`` (that 
  ## is -MININT for its type), an overflow exception is thrown (if overflow
  ## checking is turned on).
  if x < 0: -x else: x
{.pop.}

when not defined(JS): #and not defined(NimrodVM):
  {.push stack_trace: off, profiler:off.}

  when not defined(NimrodVM) and hostOS != "standalone":
    proc initGC()
    when not defined(boehmgc) and not defined(useMalloc):
      proc initAllocator() {.inline.}

    proc initStackBottom() {.inline, compilerproc.} =
      # WARNING: This is very fragile! An array size of 8 does not work on my
      # Linux 64bit system. -- That's because the stack direction is the other
      # way round.
      when declared(setStackBottom):
        var locals {.volatile.}: pointer
        locals = addr(locals)
        setStackBottom(locals)

    proc initStackBottomWith(locals: pointer) {.inline, compilerproc.} =
      # We need to keep initStackBottom around for now to avoid
      # bootstrapping problems.
      when declared(setStackBottom):
        setStackBottom(locals)

    var
      strDesc: TNimType

    strDesc.size = sizeof(string)
    strDesc.kind = tyString
    strDesc.flags = {ntfAcyclic}

  include "system/ansi_c"

  proc cmp(x, y: string): int =
    result = int(c_strcmp(x, y))

  const pccHack = if defined(pcc): "_" else: "" # Hack for PCC
  when not defined(NimrodVM):
    when defined(windows):
      # work-around C's sucking abstraction:
      # BUGFIX: stdin and stdout should be binary files!
      proc setmode(handle, mode: int) {.importc: pccHack & "setmode",
                                        header: "<io.h>".}
      proc fileno(f: C_TextFileStar): int {.importc: pccHack & "fileno",
                                            header: "<fcntl.h>".}
      var
        O_BINARY {.importc: pccHack & "O_BINARY", nodecl.}: int

      # we use binary mode in Windows:
      setmode(fileno(c_stdin), O_BINARY)
      setmode(fileno(c_stdout), O_BINARY)
    
    when defined(endb):
      proc endbStep()

  # ----------------- IO Part ------------------------------------------------
  when hostOS != "standalone":
    type
      CFile {.importc: "FILE", header: "<stdio.h>", 
              final, incompletestruct.} = object
      TFile* = ptr CFile ## The type representing a file handle.

      TFileMode* = enum           ## The file mode when opening a file.
        fmRead,                   ## Open the file for read access only.
        fmWrite,                  ## Open the file for write access only.
        fmReadWrite,              ## Open the file for read and write access.
                                  ## If the file does not exist, it will be
                                  ## created.
        fmReadWriteExisting,      ## Open the file for read and write access.
                                  ## If the file does not exist, it will not be
                                  ## created.
        fmAppend                  ## Open the file for writing only; append data
                                  ## at the end.

      TFileHandle* = cint ## type that represents an OS file handle; this is
                          ## useful for low-level file access

    # text file handling:
    var
      stdin* {.importc: "stdin", header: "<stdio.h>".}: TFile
        ## The standard input stream.
      stdout* {.importc: "stdout", header: "<stdio.h>".}: TFile
        ## The standard output stream.
      stderr* {.importc: "stderr", header: "<stdio.h>".}: TFile
        ## The standard error stream.

    when defined(useStdoutAsStdmsg):
      template stdmsg*: TFile = stdout
    else:
      template stdmsg*: TFile = stderr
        ## Template which expands to either stdout or stderr depending on
        ## `useStdoutAsStdmsg` compile-time switch.

    proc open*(f: var TFile, filename: string,
               mode: TFileMode = fmRead, bufSize: int = -1): bool {.tags: [],
               gcsafe.}
      ## Opens a file named `filename` with given `mode`.
      ##
      ## Default mode is readonly. Returns true iff the file could be opened.
      ## This throws no exception if the file could not be opened.

    proc open*(f: var TFile, filehandle: TFileHandle,
               mode: TFileMode = fmRead): bool {.tags: [], gcsafe.}
      ## Creates a ``TFile`` from a `filehandle` with given `mode`.
      ##
      ## Default mode is readonly. Returns true iff the file could be opened.
      
    proc open*(filename: string,
               mode: TFileMode = fmRead, bufSize: int = -1): TFile = 
      ## Opens a file named `filename` with given `mode`.
      ##
      ## Default mode is readonly. Raises an ``IO`` exception if the file
      ## could not be opened.
      if not open(result, filename, mode, bufSize):
        sysFatal(EIO, "cannot open: ", filename)

    proc reopen*(f: TFile, filename: string, mode: TFileMode = fmRead): bool {.
      tags: [], gcsafe.}
      ## reopens the file `f` with given `filename` and `mode`. This 
      ## is often used to redirect the `stdin`, `stdout` or `stderr`
      ## file variables.
      ##
      ## Default mode is readonly. Returns true iff the file could be reopened.

    proc close*(f: TFile) {.importc: "fclose", header: "<stdio.h>", tags: [].}
      ## Closes the file.

    proc endOfFile*(f: TFile): bool {.tags: [], gcsafe.}
      ## Returns true iff `f` is at the end.
      
    proc readChar*(f: TFile): char {.
      importc: "fgetc", header: "<stdio.h>", tags: [FReadIO].}
      ## Reads a single character from the stream `f`.
    proc flushFile*(f: TFile) {.
      importc: "fflush", header: "<stdio.h>", tags: [FWriteIO].}
      ## Flushes `f`'s buffer.

    proc readAll*(file: TFile): TaintedString {.tags: [FReadIO], gcsafe.}
      ## Reads all data from the stream `file`.
      ##
      ## Raises an IO exception in case of an error. It is an error if the
      ## current file position is not at the beginning of the file.
    
    proc readFile*(filename: string): TaintedString {.tags: [FReadIO], gcsafe.}
      ## Opens a file named `filename` for reading. Then calls `readAll`
      ## and closes the file afterwards. Returns the string. 
      ## Raises an IO exception in case of an error.

    proc writeFile*(filename, content: string) {.tags: [FWriteIO], gcsafe.}
      ## Opens a file named `filename` for writing. Then writes the
      ## `content` completely to the file and closes the file afterwards.
      ## Raises an IO exception in case of an error.

    proc write*(f: TFile, r: float32) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, i: int) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, i: BiggestInt) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, r: BiggestFloat) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, s: string) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, b: bool) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, c: char) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, c: cstring) {.tags: [FWriteIO], gcsafe.}
    proc write*(f: TFile, a: varargs[string, `$`]) {.tags: [FWriteIO], gcsafe.}
      ## Writes a value to the file `f`. May throw an IO exception.

    proc readLine*(f: TFile): TaintedString  {.tags: [FReadIO], gcsafe.}
      ## reads a line of text from the file `f`. May throw an IO exception.
      ## A line of text may be delimited by ``CR``, ``LF`` or
      ## ``CRLF``. The newline character(s) are not part of the returned string.
    
    proc readLine*(f: TFile, line: var TaintedString): bool {.tags: [FReadIO], 
                  gcsafe.}
      ## reads a line of text from the file `f` into `line`. `line` must not be
      ## ``nil``! May throw an IO exception.
      ## A line of text may be delimited by ``CR``, ``LF`` or
      ## ``CRLF``. The newline character(s) are not part of the returned string.
      ## Returns ``false`` if the end of the file has been reached, ``true``
      ## otherwise. If ``false`` is returned `line` contains no new data.

    when not defined(booting):
      proc writeln*[Ty](f: TFile, x: varargs[Ty, `$`]) {.inline, 
                               tags: [FWriteIO], gcsafe.}
        ## writes the values `x` to `f` and then writes "\n".
        ## May throw an IO exception.
    else:
      proc writeln*[Ty](f: TFile, x: varargs[Ty, `$`]) {.inline, 
                               tags: [FWriteIO].}

    proc getFileSize*(f: TFile): int64 {.tags: [FReadIO], gcsafe.}
      ## retrieves the file size (in bytes) of `f`.

    proc readBytes*(f: TFile, a: var openArray[int8], start, len: int): int {.
      tags: [FReadIO], gcsafe.}
      ## reads `len` bytes into the buffer `a` starting at ``a[start]``. Returns
      ## the actual number of bytes that have been read which may be less than
      ## `len` (if not as many bytes are remaining), but not greater.

    proc readChars*(f: TFile, a: var openArray[char], start, len: int): int {.
      tags: [FReadIO], gcsafe.}
      ## reads `len` bytes into the buffer `a` starting at ``a[start]``. Returns
      ## the actual number of bytes that have been read which may be less than
      ## `len` (if not as many bytes are remaining), but not greater.

    proc readBuffer*(f: TFile, buffer: pointer, len: int): int {.
      tags: [FReadIO], gcsafe.}
      ## reads `len` bytes into the buffer pointed to by `buffer`. Returns
      ## the actual number of bytes that have been read which may be less than
      ## `len` (if not as many bytes are remaining), but not greater.

    proc writeBytes*(f: TFile, a: openArray[int8], start, len: int): int {.
      tags: [FWriteIO], gcsafe.}
      ## writes the bytes of ``a[start..start+len-1]`` to the file `f`. Returns
      ## the number of actual written bytes, which may be less than `len` in case
      ## of an error.

    proc writeChars*(f: TFile, a: openArray[char], start, len: int): int {.
      tags: [FWriteIO], gcsafe.}
      ## writes the bytes of ``a[start..start+len-1]`` to the file `f`. Returns
      ## the number of actual written bytes, which may be less than `len` in case
      ## of an error.

    proc writeBuffer*(f: TFile, buffer: pointer, len: int): int {.
      tags: [FWriteIO], gcsafe.}
      ## writes the bytes of buffer pointed to by the parameter `buffer` to the
      ## file `f`. Returns the number of actual written bytes, which may be less
      ## than `len` in case of an error.

    proc setFilePos*(f: TFile, pos: int64) {.gcsafe.}
      ## sets the position of the file pointer that is used for read/write
      ## operations. The file's first byte has the index zero.

    proc getFilePos*(f: TFile): int64 {.gcsafe.}
      ## retrieves the current position of the file pointer that is used to
      ## read from the file `f`. The file's first byte has the index zero.

    proc fileHandle*(f: TFile): TFileHandle {.importc: "fileno",
                                              header: "<stdio.h>"}
      ## returns the OS file handle of the file ``f``. This is only useful for
      ## platform specific programming.

    proc cstringArrayToSeq*(a: cstringArray, len: int): seq[string] =
      ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
      ## of length ``len``.
      newSeq(result, len)
      for i in 0..len-1: result[i] = $a[i]

    proc cstringArrayToSeq*(a: cstringArray): seq[string] =
      ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
      ## terminated by ``nil``.
      var L = 0
      while a[L] != nil: inc(L)
      result = cstringArrayToSeq(a, L)

  # -------------------------------------------------------------------------

  when not defined(NimrodVM) and hostOS != "standalone":
    proc allocCStringArray*(a: openArray[string]): cstringArray =
      ## creates a NULL terminated cstringArray from `a`. The result has to
      ## be freed with `deallocCStringArray` after it's not needed anymore.
      result = cast[cstringArray](alloc0((a.len+1) * sizeof(cstring)))
      let x = cast[ptr array[0..20_000, string]](a)
      for i in 0 .. a.high:
        result[i] = cast[cstring](alloc0(x[i].len+1))
        copyMem(result[i], addr(x[i][0]), x[i].len)

    proc deallocCStringArray*(a: cstringArray) =
      ## frees a NULL terminated cstringArray.
      var i = 0
      while a[i] != nil:
        dealloc(a[i])
        inc(i)
      dealloc(a)

  when not defined(NimrodVM):
    proc atomicInc*(memLoc: var int, x: int = 1): int {.inline, 
      discardable, gcsafe.}
      ## atomic increment of `memLoc`. Returns the value after the operation.
    
    proc atomicDec*(memLoc: var int, x: int = 1): int {.inline, 
      discardable, gcsafe.}
      ## atomic decrement of `memLoc`. Returns the value after the operation.

    include "system/atomics"

  type
    PSafePoint = ptr TSafePoint
    TSafePoint {.compilerproc, final.} = object
      prev: PSafePoint # points to next safe point ON THE STACK
      status: int
      context: C_JmpBuf
      hasRaiseAction: bool
      raiseAction: proc (e: ref E_Base): bool {.closure.}
  
  when declared(initAllocator):
    initAllocator()
  when hasThreadSupport:
    include "system/syslocks"
    include "system/threads"
  elif not defined(nogc) and not defined(NimrodVM) and hostOS != "standalone":
    when not defined(useNimRtl) and not defined(createNimRtl): initStackBottom()
    initGC()

  when not defined(NimrodVM):
    proc setControlCHook*(hook: proc () {.noconv.})
      ## allows you to override the behaviour of your application when CTRL+C
      ## is pressed. Only one such hook is supported.
      
    proc writeStackTrace*() {.tags: [FWriteIO].}
      ## writes the current stack trace to ``stderr``. This is only works
      ## for debug builds.
    when hostOS != "standalone":
      proc getStackTrace*(): string
        ## gets the current stack trace. This only works for debug builds.

      proc getStackTrace*(e: ref E_Base): string
        ## gets the stack trace associated with `e`, which is the stack that
        ## lead to the ``raise`` statement. This only works for debug builds.
        
    {.push stack_trace: off, profiler:off.}
    when hostOS == "standalone":
      include "system/embedded"
    else:
      include "system/excpt"
    include "system/chcks"
      
    # we cannot compile this with stack tracing on
    # as it would recurse endlessly!
    include "system/arithm"
    {.pop.} # stack trace
  {.pop.} # stack trace
      
  when hostOS != "standalone" and not defined(NimrodVM):
    include "system/dyncalls"
  when not defined(NimrodVM):
    include "system/sets"

    const
      GenericSeqSize = (2 * sizeof(int))
      
    proc getDiscriminant(aa: pointer, n: ptr TNimNode): int =
      sysAssert(n.kind == nkCase, "getDiscriminant: node != nkCase")
      var d: int
      var a = cast[TAddress](aa)
      case n.typ.size
      of 1: d = ze(cast[ptr int8](a +% n.offset)[])
      of 2: d = ze(cast[ptr int16](a +% n.offset)[])
      of 4: d = int(cast[ptr int32](a +% n.offset)[])
      else: sysAssert(false, "getDiscriminant: invalid n.typ.size")
      return d

    proc selectBranch(aa: pointer, n: ptr TNimNode): ptr TNimNode =
      var discr = getDiscriminant(aa, n)
      if discr <% n.len:
        result = n.sons[discr]
        if result == nil: result = n.sons[n.len]
        # n.sons[n.len] contains the ``else`` part (but may be nil)
      else:
        result = n.sons[n.len]

    when hostOS != "standalone": include "system/mmdisp"
    {.push stack_trace: off, profiler:off.}
    when hostOS != "standalone": include "system/sysstr"
    {.pop.}

    when hostOS != "standalone": include "system/sysio"
    when hasThreadSupport:
      when hostOS != "standalone": include "system/channels"
  else:
    include "system/sysio"

  when hostOS != "standalone":
    iterator lines*(filename: string): TaintedString {.tags: [FReadIO].} =
      ## Iterates over any line in the file named `filename`.
      ##
      ## If the file does not exist `EIO` is raised. The trailing newline
      ## character(s) are removed from the iterated lines. Example:
      ##
      ## .. code-block:: nimrod
      ##   import strutils
      ##
      ##   proc transformLetters(filename: string) =
      ##     var buffer = ""
      ##     for line in filename.lines:
      ##       buffer.add(line.replace("a", "0") & '\x0A')
      ##     writeFile(filename, buffer)
      var f = open(filename, bufSize=8000)
      var res = TaintedString(newStringOfCap(80))
      while f.readLine(res): yield res
      close(f)

    iterator lines*(f: TFile): TaintedString {.tags: [FReadIO].} =
      ## Iterate over any line in the file `f`.
      ##
      ## The trailing newline character(s) are removed from the iterated lines.
      ## Example:
      ##
      ## .. code-block:: nimrod
      ##   proc countZeros(filename: TFile): tuple[lines, zeros: int] =
      ##     for line in filename.lines:
      ##       for letter in line:
      ##         if letter == '0':
      ##           result.zeros += 1
      ##       result.lines += 1
      var res = TaintedString(newStringOfCap(80))
      while f.readLine(res): yield res

  when hostOS != "standalone" and not defined(NimrodVM):
    include "system/assign"
    include "system/repr"

    proc getCurrentException*(): ref E_Base {.compilerRtl, inl, gcsafe.} =
      ## retrieves the current exception; if there is none, nil is returned.
      result = currException

    proc getCurrentExceptionMsg*(): string {.inline, gcsafe.} =
      ## retrieves the error message that was attached to the current
      ## exception; if there is none, "" is returned.
      var e = getCurrentException()
      return if e == nil: "" else: e.msg

    proc onRaise*(action: proc(e: ref E_Base): bool{.closure.}) =
      ## can be used in a ``try`` statement to setup a Lisp-like
      ## `condition system`:idx:\: This prevents the 'raise' statement to
      ## raise an exception but instead calls ``action``.
      ## If ``action`` returns false, the exception has been handled and
      ## does not propagate further through the call stack.
      if not isNil(excHandler):
        excHandler.hasRaiseAction = true
        excHandler.raiseAction = action

  {.push stack_trace: off, profiler:off.}
  when defined(endb) and not defined(NimrodVM):
    include "system/debugger"

  when defined(profiler) or defined(memProfiler):
    include "system/profiler"
  {.pop.} # stacktrace

  when not defined(NimrodVM):
    proc likely*(val: bool): bool {.importc: "likely", nodecl, nosideeffect.}
      ## Hints the optimizer that `val` is likely going to be true.
      ##
      ## You can use this proc to decorate a branch condition. On certain
      ## platforms this can help the processor predict better which branch is
      ## going to be run. Example:
      ##
      ## .. code-block:: nimrod
      ##   for value in inputValues:
      ##     if likely(value <= 100):
      ##       process(value)
      ##     else:
      ##       echo "Value too big!"
    
    proc unlikely*(val: bool): bool {.importc: "unlikely", nodecl, nosideeffect.}
      ## Hints the optimizer that `val` is likely going to be false.
      ##
      ## You can use this proc to decorate a branch condition. On certain
      ## platforms this can help the processor predict better which branch is
      ## going to be run. Example:
      ##
      ## .. code-block:: nimrod
      ##   for value in inputValues:
      ##     if unlikely(value > 100):
      ##       echo "Value too big!"
      ##     else:
      ##       process(value)
      
    proc rawProc*[T: proc](x: T): pointer {.noSideEffect, inline.} =
      ## retrieves the raw proc pointer of the closure `x`. This is
      ## useful for interfacing closures with C.
      {.emit: """
      `result` = `x`.ClPrc;
      """.}

    proc rawEnv*[T: proc](x: T): pointer {.noSideEffect, inline.} =
      ## retrieves the raw environment pointer of the closure `x`. This is
      ## useful for interfacing closures with C.
      {.emit: """
      `result` = `x`.ClEnv;
      """.}

    proc finished*[T: proc](x: T): bool {.noSideEffect, inline.} =
      ## can be used to determine if a first class iterator has finished.
      {.emit: """
      `result` = *((NI*) `x`.ClEnv) < 0;
      """.}

elif defined(JS):
  # Stubs:
  proc nimGCvisit(d: pointer, op: int) {.compilerRtl.} = discard

  proc GC_disable() = discard
  proc GC_enable() = discard
  proc GC_fullCollect() = discard
  proc GC_setStrategy(strategy: TGC_Strategy) = discard
  proc GC_enableMarkAndSweep() = discard
  proc GC_disableMarkAndSweep() = discard
  proc GC_getStatistics(): string = return ""
  
  proc getOccupiedMem(): int = return -1
  proc getFreeMem(): int = return -1
  proc getTotalMem(): int = return -1

  proc dealloc(p: pointer) = discard
  proc alloc(size: int): pointer = discard
  proc alloc0(size: int): pointer = discard
  proc realloc(p: Pointer, newsize: int): pointer = discard

  proc allocShared(size: int): pointer = discard
  proc allocShared0(size: int): pointer = discard
  proc deallocShared(p: pointer) = discard
  proc reallocShared(p: pointer, newsize: int): pointer = discard

  when defined(JS):
    include "system/jssys"
    include "system/reprjs"
  elif defined(NimrodVM):
    proc cmp(x, y: string): int =
      if x == y: return 0
      if x < y: return -1
      return 1
  
  when defined(nimffi):
    include "system/sysio"


proc quit*(errormsg: string, errorcode = QuitFailure) {.noReturn.} =
  ## a shorthand for ``echo(errormsg); quit(errorcode)``.
  echo(errormsg)
  quit(errorcode)

{.pop.} # checks
{.pop.} # hints

proc `/`*(x, y: int): float {.inline, noSideEffect.} =
  ## integer division that results in a float.
  result = toFloat(x) / toFloat(y)

template `-|`*(b, s: expr): expr =
  (if b >= 0: b else: s.len + b)

template spliceImpl(s, a, L, b: expr): stmt {.immediate.} =
  # make room for additional elements or cut:
  var slen = s.len
  var shift = b.len - L
  var newLen = slen + shift
  if shift > 0:
    # enlarge:
    setLen(s, newLen)
    for i in countdown(newLen-1, a+shift+1): shallowCopy(s[i], s[i-shift])
  else:
    for i in countup(a+b.len, s.len-1+shift): shallowCopy(s[i], s[i-shift])
    # cut down:
    setLen(s, newLen)
  # fill the hole:
  for i in 0 .. <b.len: s[i+a] = b[i]  

when hostOS != "standalone":
  proc `[]`*(s: string, x: TSlice[int]): string {.inline.} =
    ## slice operation for strings. Negative indexes are supported.
    result = s.substr(x.a-|s, x.b-|s)

  proc `[]=`*(s: var string, x: TSlice[int], b: string) = 
    ## slice assignment for strings. Negative indexes are supported. If
    ## ``b.len`` is not exactly the number of elements that are referred to
    ## by `x`, a `splice`:idx: is performed:
    ##
    ## .. code-block:: nimrod
    ##   var s = "abcdef"
    ##   s[1 .. -2] = "xyz"
    ##   assert s == "axyzf"
    var a = x.a-|s
    var L = x.b-|s - a + 1
    if L == b.len:
      for i in 0 .. <L: s[i+a] = b[i]
    else:
      spliceImpl(s, a, L, b)

proc `[]`*[Idx, T](a: array[Idx, T], x: TSlice[int]): seq[T] =
  ## slice operation for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  when low(a) < 0:
    {.error: "Slicing for arrays with negative indices is unsupported.".}
  var L = x.b - x.a + 1
  newSeq(result, L)
  for i in 0.. <L: result[i] = a[i + x.a]

proc `[]=`*[Idx, T](a: var array[Idx, T], x: TSlice[int], b: openArray[T]) =
  ## slice assignment for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  when low(a) < 0:
    {.error: "Slicing for arrays with negative indices is unsupported.".}
  var L = x.b - x.a + 1
  if L == b.len:
    for i in 0 .. <L: a[i+x.a] = b[i]
  else:
    sysFatal(EOutOfRange, "different lengths for slice assignment")

proc `[]`*[Idx, T](a: array[Idx, T], x: TSlice[Idx]): seq[T] =
  ## slice operation for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = ord(x.b) - ord(x.a) + 1
  newSeq(result, L)
  var j = x.a
  for i in 0.. <L: 
    result[i] = a[j]
    inc(j)

proc `[]=`*[Idx, T](a: var array[Idx, T], x: TSlice[Idx], b: openArray[T]) =
  ## slice assignment for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = ord(x.b) - ord(x.a) + 1
  if L == b.len:
    var j = x.a
    for i in 0 .. <L: 
      a[j] = b[i]
      inc(j)
  else:
    sysFatal(EOutOfRange, "different lengths for slice assignment")

proc `[]`*[T](s: seq[T], x: TSlice[int]): seq[T] = 
  ## slice operation for sequences. Negative indexes are supported.
  var a = x.a-|s
  var L = x.b-|s - a + 1
  newSeq(result, L)
  for i in 0.. <L: result[i] = s[i + a]

proc `[]=`*[T](s: var seq[T], x: TSlice[int], b: openArray[T]) = 
  ## slice assignment for sequences. Negative indexes are supported. If
  ## ``b.len`` is not exactly the number of elements that are referred to
  ## by `x`, a `splice`:idx: is performed. 
  var a = x.a-|s
  var L = x.b-|s - a + 1
  if L == b.len:
    for i in 0 .. <L: s[i+a] = b[i]
  else:
    spliceImpl(s, a, L, b)

proc slurp*(filename: string): string {.magic: "Slurp".}
  ## This is an alias for ``staticRead``.

proc staticRead*(filename: string): string {.magic: "Slurp".}
  ## Compile-time ``readFile`` proc for easy `resource`:idx: embedding:
  ##
  ## .. code-block:: nimrod
  ##     const myResource = staticRead"mydatafile.bin"
  ##
  ## ``slurp`` is an alias for ``staticRead``.

proc gorge*(command: string, input = ""): string {.
  magic: "StaticExec".} = discard
  ## This is an alias for ``staticExec``.

proc staticExec*(command: string, input = ""): string {.
  magic: "StaticExec".} = discard
  ## Executes an external process at compile-time.
  ## if `input` is not an empty string, it will be passed as a standard input
  ## to the executed program.
  ##
  ## .. code-block:: nimrod
  ##     const buildInfo = "Revision " & staticExec("git rev-parse HEAD") & 
  ##                       "\nCompiled on " & staticExec("uname -v")
  ##
  ## ``gorge`` is an alias for ``staticExec``. Note that you can use this proc
  ## inside a pragma like `passC <nimrodc.html#passc-pragma>`_ or `passL
  ## <nimrodc.html#passl-pragma>`_.

proc `+=`*[T: TOrdinal|uint|uint64](x: var T, y: T) {.magic: "Inc", noSideEffect.}
  ## Increments an ordinal

proc `-=`*[T: TOrdinal|uint|uint64](x: var T, y: T) {.magic: "Dec", noSideEffect.}
  ## Decrements an ordinal

proc `*=`*[T: TOrdinal|uint|uint64](x: var T, y: T) {.inline, noSideEffect.} =
  ## Binary `*=` operator for ordinals
  x = x * y

proc `+=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Increments in placee a floating point number
  x = x + y

proc `-=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Decrements in place a floating point number
  x = x - y

proc `*=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Multiplies in place a floating point number
  x = x * y

proc `/=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Divides in place a floating point number
  x = x / y

proc `&=`* (x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}

proc astToStr*[T](x: T): string {.magic: "AstToStr", noSideEffect.}
  ## converts the AST of `x` into a string representation. This is very useful
  ## for debugging.
  
proc instantiationInfo*(index = -1, fullPaths = false): tuple[
  filename: string, line: int] {. magic: "InstantiationInfo", noSideEffect.}
  ## provides access to the compiler's instantiation stack line information.
  ##
  ## This proc is mostly useful for meta programming (eg. ``assert`` template)
  ## to retrieve information about the current filename and line number.
  ## Example:
  ##
  ## .. code-block:: nimrod
  ##   import strutils
  ##
  ##   template testException(exception, code: expr): stmt =
  ##     try:
  ##       let pos = instantiationInfo()
  ##       discard(code)
  ##       echo "Test failure at $1:$2 with '$3'" % [pos.filename,
  ##         $pos.line, astToStr(code)]
  ##       assert false, "A test expecting failure succeeded?"
  ##     except exception:
  ##       discard
  ##
  ##   proc tester(pos: int): int =
  ##     let
  ##       a = @[1, 2, 3]
  ##     result = a[pos]
  ##
  ##   when isMainModule:
  ##     testException(EInvalidIndex, tester(30))
  ##     testException(EInvalidIndex, tester(1))
  ##     # --> Test failure at example.nim:20 with 'tester(1)'

template currentSourcePath*: string = instantiationInfo(-1, true).filename
  ## returns the full file-system path of the current source

proc raiseAssert*(msg: string) {.noinline.} =
  sysFatal(EAssertionFailed, msg)

when true:
  proc failedAssertImpl*(msg: string) {.raises: [], tags: [].} =
    # trick the compiler to not list ``EAssertionFailed`` when called
    # by ``assert``.
    type THide = proc (msg: string) {.noinline, raises: [], noSideEffect,
                                      tags: [].}
    THide(raiseAssert)(msg)

template assert*(cond: bool, msg = "") =
  ## Raises ``EAssertionFailure`` with `msg` if `cond` is false.
  ##
  ## Provides a means to implement `programming by contracts`:idx: in Nimrod.
  ## ``assert`` evaluates expression ``cond`` and if ``cond`` is false, it
  ## raises an ``EAssertionFailure`` exception. However, the compiler may not
  ## generate any code at all for ``assert`` if it is advised to do so through
  ## the ``-d:release`` or ``--assertions:off`` `command line switches
  ## <nimrodc.html#command-line-switches>`_.
  ##
  ## Use ``assert`` for debugging purposes only.
  bind instantiationInfo
  mixin failedAssertImpl
  when compileOption("assertions"):
    {.line.}:
      if not cond: failedAssertImpl(astToStr(cond) & ' ' & msg)

template doAssert*(cond: bool, msg = "") =
  ## same as `assert` but is always turned on and not affected by the
  ## ``--assertions`` command line switch.
  bind instantiationInfo
  {.line: instantiationInfo().}:
    if not cond:
      raiseAssert(astToStr(cond) & ' ' & msg)

iterator items*[T](a: seq[T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  let L = len(a)
  while i < L:
    yield a[i]
    inc(i)
    assert(len(a) == L, "seq modified while iterating over it")

iterator items*(a: string): char {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  let L = len(a)
  while i < L:
    yield a[i]
    inc(i)
    assert(len(a) == L, "string modified while iterating over it")

when not defined(nimhygiene):
  {.pragma: inject.}

template onFailedAssert*(msg: expr, code: stmt): stmt {.dirty, immediate.} =
  ## Sets an assertion failure handler that will intercept any assert
  ## statements following `onFailedAssert` in the current lexical scope.
  ## Can be defined multiple times in a single function.
  ##  
  ## .. code-block:: nimrod
  ##
  ##   proc example(x: int): TErrorCode =
  ##     onFailedAssert(msg):
  ##       log msg
  ##       return E_FAIL
  ## 
  ##     assert(...)
  ##     
  ##     onFailedAssert(msg):
  ##       raise newException(EMyException, msg)
  ##
  ##     assert(...)
  ##
  template failedAssertImpl(msgIMPL: string): stmt {.dirty, immediate.} =
    let msg = msgIMPL
    code

proc shallow*[T](s: var seq[T]) {.noSideEffect, inline.} =
  ## marks a sequence `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`. This is only useful for optimization 
  ## purposes.
  when not defined(JS) and not defined(NimrodVM):
    var s = cast[PGenericSeq](s)
    s.reserved = s.reserved or seqShallowFlag

proc shallow*(s: var string) {.noSideEffect, inline.} =
  ## marks a string `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`. This is only useful for optimization 
  ## purposes.
  when not defined(JS) and not defined(NimrodVM):
    var s = cast[PGenericSeq](s)
    s.reserved = s.reserved or seqShallowFlag

type
  TNimrodNode {.final.} = object
  PNimrodNode* {.magic: "PNimrodNode".} = ref TNimrodNode
    ## represents a Nimrod AST node. Macros operate on this type.

when false:
  template eval*(blk: stmt): stmt =
    ## executes a block of code at compile time just as if it was a macro
    ## optionally, the block can return an AST tree that will replace the 
    ## eval expression
    macro payload: stmt {.gensym.} = blk
    payload()

when hostOS != "standalone":
  proc insert*(x: var string, item: string, i = 0) {.noSideEffect.} = 
    ## inserts `item` into `x` at position `i`.
    var xl = x.len
    setLen(x, xl+item.len)
    var j = xl-1
    while j >= i:
      shallowCopy(x[j+item.len], x[j])
      dec(j)
    j = 0
    while j < item.len:
      x[j+i] = item[j]
      inc(j)

proc compiles*(x): bool {.magic: "Compiles", noSideEffect.} =
  ## Special compile-time procedure that checks whether `x` can be compiled
  ## without any semantic error.
  ## This can be used to check whether a type supports some operation:
  ##
  ## .. code-block:: Nimrod
  ##   when not compiles(3 + 4):
  ##     echo "'+' for integers is available"
  discard

when declared(initDebugger):
  initDebugger()

when hostOS != "standalone":
  # XXX: make these the default (or implement the NilObject optimization)
  proc safeAdd*[T](x: var seq[T], y: T) {.noSideEffect.} =
    if x == nil: x = @[y]
    else: x.add(y)

  proc safeAdd*(x: var string, y: char) =
    if x == nil: x = ""
    x.add(y)

  proc safeAdd*(x: var string, y: string) =
    if x == nil: x = y
    else: x.add(y)

proc locals*(): TObject {.magic: "Locals", noSideEffect.} =
  ## generates a tuple constructor expression listing all the local variables
  ## in the current scope. This is quite fast as it does not rely
  ## on any debug or runtime information. Note that in constrast to what
  ## the official signature says, the return type is not ``TObject`` but a
  ## tuple of a structure that depends on the current scope. Example:
  ##
  ## .. code-block:: nimrod
  ##   proc testLocals() =
  ##     var
  ##       a = "something"
  ##       b = 4
  ##       c = locals()
  ##       d = "super!"
  ##
  ##     b = 1
  ##     for name, value in fieldPairs(c):
  ##       echo "name ", name, " with value ", value
  ##     echo "B is ", b
  ##   # -> name a with value something
  ##   # -> name b with value 4
  ##   # -> B is 1
  discard

when hostOS != "standalone" and not defined(NimrodVM) and not defined(JS):
  proc deepCopy*[T](x: var T, y: T) {.noSideEffect, magic: "DeepCopy".} =
    ## performs a deep copy of `x`. This is also used by the code generator
    ## for the implementation of ``spawn``.
    discard

  include "system/deepcopy"

{.pop.} #{.push warning[GcMem]: off.}
