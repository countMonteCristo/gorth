# Gorth
**Gorth is like Forth, but in Go**

*Inspired by [Porth](https://gitlab.com/tsoding/porth) project of Tsoding.*

**IMPORTANT: Language is not finished. Use it on your own risks**

**Gorth** is a Concatenative Stack-Oriented Programming Language.

## Examples

### HelloWorld:
```gorth
include "std.gorth"

func main : int do
  "Hello, world!\n" puts
  0
end
```

### Print all input arguments:
```gorth
include "std.gorth"

func main : int do
  argv while dup @32 dup 0 != do
    dup strlen println
    sizeof(ptr) +
  end drop drop
  0
end
```
Output:
```console
> go run gorth.go print-args.gorth arg1 arg2 3 argument4
print-args.gorth
arg1
arg2
3
argument4
```

## Quick Start
### Compilation
```console
> go build -o bin/gorth gorth.go
> go build -o bin/gorth_test tester/test.go
```
### Run tests
Run all tests:
```console
> ./bin/gorth_test test
```
Run tests from directory:
```console
> ./bin/gorth_test test Gorth/examples
```
Run single test:
```console
> ./bin/gorth_test test Gorth/examples/helloworld.gorth
```
Record test outputs:
```console
> ./bin/gorth_test record [dir_or_file]
```

## Usage
```
> ./bin/gorth [-mode debug|profile|interprete] [-env] [-disable-typecheck] [-I include_dir] [-O level] [-log error|warn|debug|info] Gorth/examples/helloworld.gorth [args...]
```

Supported flags:
* `mode` - interpreter mode:
  * `interpreter` - default value, simply execute gorth script
  * `debug` - run as debugger
  * `profile` - run profiler
* `env` - adds environment variables to VM memory (turned off by default)
* `check` - perform type checking before running the script
* `I` - provide additional include directories (i.e. "-I dir1 -I dir2 ... -I dirN")
* `O` - optimization level (`0` - no optimizations, `1` - eliminate dead code)
* `log` - logging level (`info` by default)

## Debugger
Run script with debugger.
```console
> ./bin/gorth -mode debug Gorth/examples/helloworld.gorth
```
Availavle commands:
 * `n` [`count`]       - process at most `count` instructions (by default `count`=1)
 * `c`                 - continue (process all instructions to the break point or to the end)
 * `u`                 - go out of current function
 * `bs` `a1 a2 .. ak`  - set break points for functions or addresses
 * `bl`                - list all break points
 * `br` `a1 a2 .. ak`  - remove break points from functions or addresses
 * `t`                 - print current token
 * `o` [`ctx`]         - print current operation (+-`ctx` operations, by default `ctx`=0)
 * `ol`                - print operations list
 * `s`                 - print current stack state
 * `m`                 - print current memory state
 * `mo` `addr` `size`  - print memory chunk of size `size` at address `addr`
 * `p` `n1 n2 .. nk`   - print consts, allocs or functions
 * `e` [`type`]        - print current environment (consts, allocs), `type` could be [`all`, `local`, `global`], by default `type`=`all`
 * `h`                 - print help
 * `q`                 - exit debugger

## Profiler
Run script in profiler mode:
```console
> ./bin/gorth -mode profile Gorth/examples/helloworld.gorth
```
Profiler will save all stats into `Gorth/examples/helloworld.gorth.prof` file.

# Language Reference
## Literals
### Integers
Integer literal is a sequence of decimal digits (optionally starts with + or -). When an integer is encountered it is pushed onto the stack. All integers from range [-2^63; 2^63 - 1] are supported.

Example:
```gorth
func main : int do
  1 2 + puti
  0
end
```
This code pushes 1 and 2 onto the stack, pops two numbers and pushes their sum back. So, afterall, there will be 3 on top of the stack.

### Strings
String literal is a sequence of characters enclosed by double quotes (`"`). Multilined strings are not supported, but it is possible to ues escaped characters like `'\n'` inside the string. String literals are stored in a specific memory region as null-terminated strings. When string literal is encountered the interpreter pushes pointer to it and size of the string onto the stack.

Example:
```gorth
include "std.gorth"

func main : int do
  "Hello, world!" println
  0
end
```
This code pushes pointer to a null-terminated string "Hello, world" and its size onto the stack. After that `puts` pops these to arguments and prints this string to stdout.

### Character
Character is a single byte enclosed by single quotes ('). When character literal encountered the interpreter pushes its ASCII code onto the stack

Example:
```gorth
include "std.gorth"

func main : int do
  '\n' putc
  0
end
```

## Types
Only four types are supported for now: `int`, `bool`, `ptr` and `fptr`:
- `int`  - integer number
- `bool` - boolean (`true` or `false`). `0` is treated as `false`, other numbers are converted to `true`
- `ptr`  - pointer in memory
- `fptr` - pointer to function

## Intrincisc
### Stack manipulation
| Name    | Signature        | Description                                 |
| ---     | ---              | ---                                         |
| `dup`   | `a : a a`       | duplicate an element on top of the stack.   |
| `swap`  | `a b : b a`     | swap 2 elements on the top of the stack.    |
| `drop`  | `a b : a`       | drops the top element of the stack.         |
| `over`  | `a b : a b a`   | copy the element below the top of the stack |
| `rot`   | `a b c : c a b` | rotate the top three stack elements.        |

### Comparison
| Name | Signature                              | Description                                                  |
| ---  | ---                                    | ---                                                          |
| `=`  | `[a: int] [b: int] : [a == b : bool]` | checks if two elements on top of the stack are equal.        |
| `!=` | `[a: int] [b: int] : [a != b : bool]` | checks if two elements on top of the stack are not equal.    |
| `>`  | `[a: int] [b: int] : [a > b  : bool]` | applies the greater comparison on top two elements.          |
| `<`  | `[a: int] [b: int] : [a < b  : bool]` | applies the less comparison on top two elements.             |
| `>=` | `[a: int] [b: int] : [a >= b : bool]` | applies the greater or equal comparison on top two elements  |
| `<=` | `[a: int] [b: int] : [a <= b : bool]` | applies the greater or equal comparison on top two elements. |

### Arithmetic
| Name | Signature                           | Description                                              |
| ---  | ---                                 | ---                                                      |
| `+`  | `[a: int] [b: int] : [a + b: int]` | sums up two elements on the top of the stack.            |
| `-`  | `[a: int] [b: int] : [a - b: int]` | subtracts two elements on the top of the stack           |
| `*`  | `[a: int] [b: int] : [a * b: int]` | multiples two elements on top of the stack               |
| `/`  | `[a: int] [b: int] : [a / b: int]` | integer division of two elements on the top of the stack |
| `%`  | `[a: int] [b: int] : [a / b: int]` | gets the reminder after integer diviion `a` by `b`       |

### Bitwise
| Name | Signature                            | Description     |
| ---  | ---                                  | ---             |
| `>>` | `[a: int] [b: int] : [a >> b: int]` | right bit shift |
| `<<` | `[a: int] [b: int] : [a << b: int]` | left bit shift  |
| `\|` | `[a: int] [b: int] : [a \| b: int]` | bit `or`        |
| `&`  | `[a: int] [b: int] : [a & b: int]`  | bit `and`       |
| `^`  | `[a: int] [b: int] : [a ^ b: int]`  | bit `xor`       |
| `~`  | `[a: int]          : [~b: int]`     | bit `not`       |

### Logical
| Name   | Signature                                 | Description |
| ---    | ---                                       | ---         |
| `&&`   | `[a: bool] [b: bool] : [a && b: bool]`   | logical AND |
| `\|\|` | `[a: bool] [b: bool] : [a \|\| b: bool]` | logical OR  |
| `!`    | `[a: bool] : [!a: bool]`                 | logical NOT |

### Memory
| Name         | Signature                      |Description                                        |
| ---          | ---                            | ---                                               |
| `!8`         | `[byte: int] [place: ptr] : ` | store a given byte at the address on the stack    |
| `@8`         | `[place: ptr] : [byte: int]`  | load a byte from the address on the stack         |
| `!16`        | `[byte: int] [place: ptr] :`  | store an 2-byte word at the address on the stack  |
| `@16`        | `[place: ptr] : [byte: int]`  | load an 2-byte word from the address on the stack |
| `!32`        | `[byte: int] [place: ptr] :`  | store an 4-byte word at the address on the stack  |
| `@32`        | `[place: ptr] : [byte: int]`  | load an 4-byte word from the address on the stack |
| `!64`        | `[byte: int] [place: ptr] :`  | store an 8-byte word at the address on the stack  |
| `@64`        | `[place: ptr] : [byte: int]`  | load an 8-byte word from the address on the stack |

### Misc
- `???` - prints stack state
- `!!!` - prints type stack size (only with `-check` option)
- `argc` - pushes count of input arguments onto the stack
- `argv` - pushes the pointer to the null-terminated list of pointers to the input arguments
- `env`  - pushes the pointer to the null-terminated list of pointers to the environment variables
- `assert` - gets condition, pointer to the string literal and its size from the top of the stack and perform an exit if condition is false

### Cast types
- `cast(int)` - converts top element of the stack to `int`
- `cast(bool)` - converts top element of the stack to `bool`
- `cast(ptr)` - converts top element of the stack to `ptr`
- `cast(fptr)` - converts top element of the stack to `fptr`
Cast intrinsics are needed only for type checking phase. They will be skipped at runtime.

### System calls:
- `syscall{N}` - perform system call (description: ```man 2 syscalls```)

Supported syscall operations:
- `syscall0`
- `syscall1`
- `syscall2`
- `syscall3`
- `syscall4`
- `syscall5`
- `syscall6`

For now only a few system calls are supported. See [cat.gorth](https://github.com/countMonteCristo/gorth/tree/main/Gorth/examples/cat.gorth) and [basic-web-server.gorth](https://github.com/countMonteCristo/gorth/tree/main/Gorth/examples/basic-web-server.gorth) as an example.

## Control Flow
### If-do-[elif-do-[else-]]end
```gorth
include "std.gorth"

func main : int do
  2
  if dup 1 = do
    10
  elif dup 2 = do
    20
  elif dup 3 = do
    30
  else
    50
  end puti '\n' putc drop
  0
end
```
### While loop
```gorth
include "std.gorth"

// print all numbers
func main : int do
  0 while dup 10 <= do
    if dup 2 = do         // but skip 2
      1 +
      continue
    end

    if dup 2 % 0 = do     // print only even ones
      dup puti '\n' putc
    end

    1 +
    if dup 6 > do         // break if number is greater than 6
      break
    end
  end drop 0
end
```
### Include
```gorth
include "foo.gorth"
```
Includes all tokens from `foo.gorth` to the current file. Circular imports are permitted. Import system remembers
all inluded files, so it won't include file if it has been already inlcuded somewhere.

### Functions
```gorth
include "std.gorth"

// prints asterisk and given string
func print_item do
  " * " puts println
end

func main : int do
  argv while dup @32 dup 0 != do
    dup strlen print_item
    sizeof(ptr) +
  end drop drop
  0
end
```
Defines function with given name. You can use define and use your functions anywhere you want.

Function definition looks like this:
```gorth
[inline] func <function_name>
  <input_type_1> <input_type_2> ... <input_type_M>
    :
  <output_type_1> <output_type_2> ... <output_type_N> do
  // your code goes here
end
```
where all of `input_type_K` and `output_type_L` are from set {`int`, `bool`, `ptr`, `any`}.

`inline` functions work like macros - calling such function will be fully substituted with its body. Therefore `return`s and local `alloc`s are not allowed inside `inline`d functions.

#### Return from function
```gorth
func foo do
  0 while dup 10 < do
    if dup 5 > do drop return end
    dup puti
    1 +
  end drop
end

func main : int do
  foo
  0
end
```

#### Recursion
You can call ordinary function from itself. Recursion for `inline`d functions is not allowed. Here is the typical example of how the factorial function can be implemented using recursion:

```gorth
func factorial int : int do
  if dup 2 < do
    drop
    1 return
  end
  dup 1 - factorial
  *
end

func main : int do
  5 factorial puti // -> "120"
  0
end
```

#### Function pointers
There is an ability to get the pointer to the functions. Here is an example of how it works:
```gorth
inline func f int : int do 2 * end

func g int : int do dup * end

func main : int do
  7 fptr-of f call-like g puti  // -> 14
  0
end
```
Here `fptr-of f` pushes pointer to function `f` onto the stack, and `call-like g` pops this pointer from the stack and calls it as function with the same signature as function `g`.

### Constants
```gorth
const N 10 end
const M 10 end
const CHAR_WIDTH M 4 * 3 + end
```
Defines constant with given name and given value. Simple arithmetic and logical operations can be used to calculate const value.
Constants may be delcared inside functions. Local constants CAN NOT HIDE global ones with the same name.

For example:
```gorth
include "std.gorth"

const N 10 end

func main : int do
  N puti ' ' putc
  const N 20 end
  N puti '\n' putc
end
```
this code will not compile.

Constants should be one of the existing types: `int`, `bool` or `ptr`. Constants of type `any` are not allowed.

### Memory Allocations
```gorth
const N 100 end
alloc array N 1 + end
```
Allocates the region of memory of the given size. Simple arithmetic and logical operations can be used to calculate allocated region size. Allocations can be declared inside functions.

Local alloctaions CAN NOT HIDE global ones.

For example:
```gorth
include "std.gorth"

alloc x 1 end

func main : int do
  13 x !8
  x @8 puti ' ' putc
  alloc x 1 end
  15 x !8
  x @8 puti ' ' putc
end
```
this code will not compile.

Allocations should be one of type `int`. Other types (`bool`, `ptr` or `any`) are not allowed.

# Captures
There is an ability to capture current values from stack:
```gorth
func main : int do
  1 2
  capture x int y int do
    3 4
    capture z int t int do
      z puti t puti x puti y puti
    end drop drop
    x puti y puti
  end drop drop
  0
end
```
In this example `x` and `y` capture `1` and `2` from stack. After that `3` and `4` are being pushed onto the stack. Next, `z` and `t` capture `3` and `4`. Note that capturing does not pop items from stack.

It is not allowed for the captured value name and constant, allocation of function to have the same name.

Capture blocks can be nested and can be placed inside while-loops and if(-else)-end blocks. `break`s and `continue`s inside a capture block are not allowed (though you can do `return` from capture block).

# Type checking
Type checking is enabled by default. You can run your script with `-disanble-typecheck` option to disable type checking.

The process of type checking is quite similar to the process of interpretation the script. Type checker process all the script operations using types instead of actual numbers. Here are some type checking rules:
* only one iteration of while-loop is processed, and after it the type stack should remain the same as before
* both branches of if-else block are processed individually, and the results should be the same
* every function gets the type stack filled with the types corresponding to its input type list and should push to the type stack arguments corresponding to its output type list (`any` can match any type)
* in case of no `else` in if-block the "true" branch should not change the type stack
* in case of having `break` or `continue` inside the while-loop the state of the type stack will be saved and checked at the end of processing the loop
* in case of having `return` inside if-block or while-loop the state of the type stack will be saved and checked at the end of processing the function
