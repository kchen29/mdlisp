What is MDLISP:
MDLISP implements MDL in the form of lisp, written as a simple extension of lisp.
The improvements are that the programmer is able to use all the expressive power of a lisp to define images and animations.
Importantly, the programmer should be able to define macros and if need be, functions.
Additionally, since MDLISP is really just lisp, the programmer is able to use the SLIME + EMACS combination to code.

Differences from MDL:
Each command becomes wrapped in parentheses. Commands are used for their side effects.
push becomes push-stack
pop becomes pop-stack
The "optional" tokens for commands becomes keyword arguments.

Frames, basename, and vary are deprecated.
To make animations, a macro MAKE-ANIMATION is provided.
It takes the frames, basename, vary definitions, and finally the code.
