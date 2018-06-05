What is MDLISP:
MDLISP implements MDL in the form of lisp, written as a simple extension of Common Lisp.
The improvements are that the programmer is able to use all the expressive power of Common Lisp to define images and animations.
Importantly, the programmer should be able to define macros and if need be, functions.
Additionally, since MDLISP is really just lisp, the programmer is able to use the SLIME + EMACS combination to code.

Differences from MDL:
Each command becomes wrapped in parentheses. Commands are used for their side effects.

push becomes push-stack
pop becomes pop-stack

The "optional" tokens for commands become keyword arguments.

Frames, basename, and vary are deprecated.

To make animations, a macro ANIMATE is provided. It provides the setup for creating animations.
It takes the basename, frames, vary definitions, and animation code.
Each vary list is a symbol followed by the value for each frame. The macro VARY is provided
for conveniently defining piece-wise linear varys.

Additionally, the programmer is able to use vary not only as a knob, but as any function argument.

EXAMPLES:
IMAGES:
robot.lisp is a direct translation of robot.mdl into mdlisp
robot2.lisp is a better way of writing robot.lisp using a macro to abstract away things that are parts of other things.

ANIMATIONS:
simple_anim.lisp is a direct translation of simple_anim.mdl
cat.lisp is a better way of writing cat.mdl

varys:
rolling.lisp is an example of using nonlinear varys.
projectile.lisp is an example of using vary not only as a knob.

physics simulations:
pendulum.lisp
n-bodies.lisp
