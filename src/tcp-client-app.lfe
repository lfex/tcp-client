(defmodule tcp-client-app
  (behaviour application)
  (export
   (start 2)
   (stop 1)))

(defun SUP () 'tcp-client-sup)


;;;===================================================================
;;; Application callbacks
;;;===================================================================

(defun start (_type _args)
  (apply (SUP) 'start_link '()))

(defun stop (_state)
  'ok)
