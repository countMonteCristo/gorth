# Gorth
**Gorth is like Forth, but in Go**

*Inspired by [Porth](https://gitlab.com/tsoding/porth) project of Tsoding.*

**IMPORTANT: Language is not finished. Use it on your own risks**

**Gorth** is a Concatenative Stack-Oriented Programming Language.

## Examples

### HelloWorld:
```gorth
func main do
  "Hello, World\n" puts
end
```

### Print all input arguments:
```gorth
func strlen do
  dup
  while dup @8 0 != do
    1 +
  end
  over -
end

func main do
  argv 0
  while dup argc < do
    over strlen over over puts '\n' putc
    + rot drop 1 + swap
    1 +
  end
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
```console
> ./bin/gorth Gorth/examples/helloworld.gorth
```

# Language Reference
## Literals
### Integers
Integer literal is a sequence of decimal digits (optionally starts with + or -). When an integer is encountered it is pushed onto the stack. All integers from range [-2^63; 2^63 - 1] are supported.

Example:
```gorth
1 2 +
```
This code pushes 1 and 2 onto the stack, pops two numbers and pushes their sum back. So, afterall, there will be 3 on top of the stack.

### Strings
String literal is a sequence of characters enclosed by double quotes ("). Multilined strings are not supported, but it is possible to ues escaped characters like '\n' inside the string. String literals are stored in a specific memory region as null-terminated strings. When string literal is encountered the interpreter pushes pointer to it and size of the string onto the stack.

Example:
```gorth
"Hello, world!\n" puts
```
This code pushes pointer to a null-terminated string "Hello, world\n" and its size onto the stack. After that `puts` pops these to arguments and prints this string to stdout.

### Character
Character is a single byte enclosed by single quotes ('). When character literal encountered the interpreter pushes its ASCII code onto the stack

Example:
```gorth
'\n' putc
```

## Types
Only two types are supported for now: `int` and `bool`.

## Intrincisc
### Stack manipulation
| Name    | Signature        | Description                                 |
| ---     | ---              | ---                                         |
| `dup`   | `a -- a a`       | duplicate an element on top of the stack.   |
| `swap`  | `a b -- b a`     | swap 2 elements on the top of the stack.    |
| `drop`  | `a b -- a`       | drops the top element of the stack.         |
| `over`  | `a b -- a b a`   | copy the element below the top of the stack |
| `rot`   | `a b c -- c a b` | rotate the top three stack elements.        |

### Comparison
| Name | Signature                              | Description                                                  |
| ---  | ---                                    | ---                                                          |
| `=`  | `[a: int] [b: int] -- [a == b : bool]` | checks if two elements on top of the stack are equal.        |
| `!=` | `[a: int] [b: int] -- [a != b : bool]` | checks if two elements on top of the stack are not equal.    |
| `>`  | `[a: int] [b: int] -- [a > b  : bool]` | applies the greater comparison on top two elements.          |
| `<`  | `[a: int] [b: int] -- [a < b  : bool]` | applies the less comparison on top two elements.             |
| `>=` | `[a: int] [b: int] -- [a >= b : bool]` | applies the greater or equal comparison on top two elements  |
| `<=` | `[a: int] [b: int] -- [a <= b : bool]` | applies the greater or equal comparison on top two elements. |

### Arithmetic
| Name     | Signature                                        | Description                                              |
| ---      | ---                                              | ---                                                      |
| `+`      | `[a: int] [b: int] -- [a + b: int]`              | sums up two elements on the top of the stack.            |
| `-`      | `[a: int] [b: int] -- [a - b: int]`              | subtracts two elements on the top of the stack           |
| `*`      | `[a: int] [b: int] -- [a * b: int]`              | multiples two elements on top of the stack               |
| `/`      | `[a: int] [b: int] -- [a / b: int]`              | integer division of two elements on the top of the stack |
| `%`      | `[a: int] [b: int] -- [a / b: int]`              | gets the reminder after integer diviion `a` by `b`       |

### Bitwise
| Name  | Signature                            | Description      |
| ---   | ---                                  | ---              |
| `>>`  | `[a: int] [b: int] -- [a >> b: int]` | right bit shift. |
| `<<`  | `[a: int] [b: int] -- [a << b: int]` | left bit shift.  |
| `\|`  | `[a: int] [b: int] -- [a \| b: int]` | bit `or`.        |
| `&`   | `[a: int] [b: int] -- [a & b: int]`  | bit `and`.       |
| `^`   | `[a: int] -- [^a: int]`              | bit `xor`.       |

### Logical
| Name   | Signature                                 | Description |
| ---    | ---                                       | ---         |
| `&&`   | `[a: bool] [b: bool] -- [a && b: bool]`   | logical AND |
| `\|\|` | `[a: bool] [b: bool] -- [a \|\| b: bool]` | logical OR  |
| `!`    | `[a: bool] -- [!a: bool]`                 | logical NOT |

### Memory
| Name         | Signature                      |Description                                         |
| ---          | ---                            | ---                                                |
| `!8`         | `[byte: int] [place: ptr] -- ` | store a given byte at the address on the stack.    |
| `@8`         | `[place: ptr] -- [byte: int]`  | load a byte from the address on the stack.         |
| `!16`        | `[byte: int] [place: ptr] --`  | store an 2-byte word at the address on the stack.  |
| `@16`        | `[place: ptr] -- [byte: int]`  | load an 2-byte word from the address on the stack. |
| `!32`        | `[byte: int] [place: ptr] --`  | store an 4-byte word at the address on the stack.  |
| `@32`        | `[place: ptr] -- [byte: int]`  | load an 4-byte word from the address on the stack. |
| `!64`        | `[byte: int] [place: ptr] --`  | store an 8-byte word at the address on the stack.  |
| `@64`        | `[place: ptr] -- [byte: int]`  | load an 8-byte word from the address on the stack. |

### Misc
- `???` - prints stack state
- `argc` - pushes count of input arguments onto the stack
- `argv` - pushes the pointer to the first input argument

### System calls:
- `syscall` - perform system call (description: ```man 2 syscalls```)

For now only 4 system calls are supported: `open`, `read`, `write` and `close`. See [cat.gorth](https://github.com/countMonteCristo/gorth/tree/main/Gorth/examples/cat.gorth) as an example.

## Control Flow
### If-else-end
```gorth
10 20 < if
  30 puti '\n' putc
else
  40 puti '\n' putc
end
```
### While loop
```gorth
0 while dup 10 <= do
  dup 2 = if
    1 +
    continue
  end

  dup 2 % 0 = if
    dup puti '\n' putc
  end

  1 +
  dup 4 > if
    break
  end
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
func strlen do
  dup
  while dup @8 0 != do
    1 +
  end
  over -
end

func main do
  argv 0
  while dup argc < do
    over strlen over over puts '\n' putc
    + rot drop 1 + swap
    1 +
  end
end
```
Defines function with given name. You can use define and use your functions anywhere you want.

### Constants
```gorth
const N 10 end
const M 10 end
const CHAR_WIDTH M 4 * 3 + end
```
Defines constant with given name and given value. Simple arithmetic operations can be used to calculate const value.
Constants may be delcared inside functions. Local constants hide global ones with the same name.

For example:
```gorth
const N 10 end

func main do
  N puti ' ' putc
  const N 20 end
  N puti '\n' putc
end
```
this code prints `10 20`, because local constant `N` (=20) in function `main` hides global constant `N` (=10).

### Memory Allocations
```gorth
const N 100 end
alloc array N 1 + end
```
Allocates the region of memory of the given size. Simple arithmetic operations can be used to calculate allocated region size. Allocations can be declared inside functions.

If local alloctaion has the same name as global, it hides global one.

For example:
```gorth
alloc x 1 end

func main do
  13 x !8
  x @8 puti ' ' putc
  alloc x 1 end
  15 x !8
  x @8 puti ' ' putc
end
```
this code prints `13 15`, because local allocation `x` in function `main` hides global allocation `x`.
