(defmodule tcp-client-sup
  (behaviour supervisor)
  (export
   (start_link 0)
   (init 1)))

(defun SERVER () (MODULE))
(defun MGR () 'tcp-client-mgr)
(defun supervisor-opts () '())

;;;===================================================================
;;; API functions
;;;===================================================================

(defun start_link ()
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

;;;===================================================================
;;; Supervisor callbacks
;;;===================================================================

(defun init(_)
  (let* ((`#(ok ,server) (application:get_env 'tcp-client 'server))
         (`#(host ,host) (lists:keyfind 'host 1 server))
         (`#(port ,port) (lists:keyfind 'port 1 server))
         (`#(options ,opts) (lists:keyfind 'options 1 server)))
    `#(ok #(,(sup-flags) (,(child host port opts))))))

;;;===================================================================
;;; Private functions
;;;===================================================================

(defun sup-flags ()
  #m(strategy one_for_one
     intensity 5
     period 10))

(defun child (host port options)
  `#m(id ,(MGR)
      start #(,(MGR) start_link (,host ,port ,options))
      restart permanent
      shutdown 5000
      type worker
      modules (,(MGR))))
