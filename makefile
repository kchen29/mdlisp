sbcl := sbcl --noinform --non-interactive --load "load.lisp" --load
script := cat.lisp

all:
	$(sbcl) "$(script)"

clean:
	rm -f *~ *.fasl *.ppm *.png anim/*
