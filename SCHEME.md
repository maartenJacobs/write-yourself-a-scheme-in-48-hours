# Scheme

Expanded form of the lexical structure of Scheme as described in [R5RS, chapter 7.1.1](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.1.1). Some parts were quite complex (specifically numbers, complex pun unintended); this document is an attempt at expanding the concise syntax from the document, so as to easily verify the implementation.

This document is purely for my own benefit, as I have not written anything substantial in Scheme. Otherwise it is quite definitely to specify my expectations in tests if I don't fully understand the specification.

Additional regex syntax is used:

- `?`: 0 or 1 element, or optional pattern.

## Numbers

Interesting features:
- Multiple bases are supported.
- `#` is used as a zero. This is quite confusing to me, as `#` is also a common prefix, e.g. `#b` for binary numbers or `#i` for inexact numbers.
- Polar form of complex numbers, represented as real@real, e.g. -1@1.57079. This took me a while to figure out because I had no idea this syntax was supported and the @-sign was basically unsearchable.

`number -> num 2 | num 8 | num 10 | num 16`

### Non-decimal bases

Base 10 has more options than the other bases. The non-decimal bases will therefore be covered first.

Note the lack of float numbers and exponent markers.

```
ND -> 2 | 8 | 16

num ND -> (prefix ND) (complex ND)

prefix ND -> (radix ND) (exactness)? | (exactness)? (radix ND)
radix ND -> #b | #o | #h
exactness -> #i | #e

complex ND ->   (real ND)
                | (real ND) @ (real ND)
                | i
                | (real ND)? + (unsignedReal ND)? i
                | (real ND)? - (unsignedReal ND)? i

real ND -> (sign)? (unsignedReal ND)
unsignedReal ND -> (unsignedInteger ND) / (unsignedInteger ND)
                | (unsignedInteger ND)

unsignedInteger ND -> (digit ND)+ #*

sign -> + | -
```

### Decimal bases

```
num -> (prefix) (complex)

prefix -> (radix)? (exactness)? | (exactness)? (radix)?
radix -> #d
exactness -> #i | #e

complex ->  (real)
            | (real) @ (real)
            | i
            | (real ND)? + (unsignedReal ND)? i
            | (real ND)? - (unsignedReal ND)? i

real -> (sign)? (unsignedReal)
unsignedReal -> (unsignedInteger) / (unsignedInteger)
                | (unsignedInteger)
                | (decimal)

decimal ->  (digit)+ #* (suffix)?
            | . (digit)+ #* (suffix)?
            | (digit)+ . (digit)* #* (suffix)?
            | (digit)+ #+ . #* (suffix)?

unsignedInteger -> (digit)+ #*

suffix -> (exponentMarker) (sign)? (digit)+

exponentMarker -> e | s | f | d | l

sign -> + | -
```
