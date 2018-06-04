sbcl := sbcl --noinform --non-interactive --load "load.lisp" --load
script := robot.lisp

all:
	$(sbcl) "$(script)"

clean:
	rm -f *~ *.fasl *.ppm *.png anim/*
