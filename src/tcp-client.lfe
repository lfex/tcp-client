(defmodule tcp-client
  (export
   (call-msg 1)
   (cast-msg 1)))

(defun MGR () #(global tcp-client-mgr))

;;;===================================================================
;;; API
;;;===================================================================

(defun call-msg (data)
  (gen_statem:call (MGR) `#(request ,data)))

(defun cast-msg (data)
  (gen_statem:cast (MGR) `#(request ,data)))
