(load-file "../src/compile.el")
(find-file "test.teco")
(compile-teco-to-emacs-lisp)
(pop-to-buffer "*TECO*")
(write-file "output.el")
