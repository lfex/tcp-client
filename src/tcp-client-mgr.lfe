(defmodule tcp-client-mgr
  (behaviour gen_statem)
  ;; API
  (export
   (request 2)
   (start_link 3))
  ;; gen_statem Callbacks
  (export
   (callback_mode 0)
   (connected 3)
   (disconnected 3)
   (init 1))
  ;; Parser Callbacks
  (export
   (parse_response 2)
   (report 1)))

(include-lib "lfe/include/clj.lfe")

(defun SERVER () (MODULE))
(defun STATEM_OPTS () '())
(defun TIMEOUT () 500) ; milliseconds

;;; Public API.

(defun start_link (host port opts)
  (gen_statem:start_link `#(global ,(SERVER)) (MODULE) `#(,host ,port ,opts) (STATEM_OPTS)))

(defun request (pid req)
  (gen_statem:call pid `#(request ,req)))

;;; gen_statem callbacks

(defun callback_mode ()
  '(state_functions state_enter))

(defun init
  ((`#(,host ,port ,opts))
   (let* ((`#(tcp ,tcp-opts) (lists:keyfind 'tcp 1 opts))
          (`#(parser ,parser) (lists:keyfind 'parser 1 opts))
          (`#(reporter ,reporter) (lists:keyfind 'reporter 1 opts))
          (data `#m(host ,host
                    port ,port
                    parser ,parser
                    reporter ,reporter
                    tcp_opts ,tcp-opts
                    requests #m()
                    from undefined))
          (actions '(#(next_event internal connect))))
     `#(ok disconnected ,data ,actions))))

(defun disconnected
  (('enter 'disconnected _data)
   'keep_state_and_data)
  (('enter 'connected (= `#m(requests ,reqs) data))
   (io:format "Connection closed~n")
   (lists:foreach (match-lambda ((`#(,_ ,from))
                                 (gen_statem:reply from '#(error disconnected))))
                  reqs)
   (let ((data (clj:->> data
                        (maps:put 'socket 'undefined)
                        (maps:put 'requests #m())))
         (actions `(#(#(timeout reconnect) ,(TIMEOUT) undefined))))
     `#(keep_state ,data ,actions)))
  (('internal 'connect (= `#m(host ,host port ,port tcp_opts ,opts) data))
   (case (gen_tcp:connect host port opts)
     (`#(ok ,sock) `#(next_state connected ,(maps:put 'socket sock data)))
     (`#(error ,err) (progn
                       (io:format "Connection failed: ~ts~n" `(,(inet:format_error err)))
                       'keep_state_and_data))))
  ((`#(timeout reconnect) _ data)
   `#(keep_state ,data (#(next_event internal connect))))
  ((`#(call ,from) `#(request ,_) _data)
   `#(keep_state_and_data `(#(reply ,from #(error disconnected)))))
  ((event-type event-content data)
   (io:format "Got unexpected event: ~p~n" `(#m(event-type ,event-type
                                                event-content ,event-content
                                                data ,data)))
   'keep_state_and_data))

(defun connected
  (('enter _old-state _data)
   'keep_state_and_data)
  (('info `#(tcp_closed ,sock) (= `#m(socket ,sock) data))
   `#(next_state disconnected ,data))
  ((`#(call ,from) `#(request ,req) (= `#m(socket ,sock) data))
   ;;(io:format "Call from ~p~n" `(,from))
   ;; XXX Setting 'from' in the map / data like this is probably horribly
   ;;     unsafe; let's fix it ...
   (send sock req (maps:put 'from from data)))
  (('cast `#(request ,req) (= `#m(socket ,sock) data))
   (send sock req (maps:put 'from 'undefined data)))
  (('info `#(tcp ,sock ,pkt) (= `#m(socket ,sock
                                    from ,from
                                    parser #(,p-mod ,p-fun)
                                    reporter ,rep) data))
   ;; Call the configured parser function
   (let ((resp (case (apply p-mod p-fun `(,pkt ,rep))
                 (`(,r) r)
                 (r r))))
     ;; XXX Nasty hack!
     (case (=/= from 'undefined)
       ('true (gen_statem:reply from resp))
       (_ 'ok))
     `#(keep_state ,data))))

;;; Private functions

;; Intended to be overriden by cosuming library.
(defun parse_response
  ((pkt `#(,reporter-mod ,reporter-fun))
    pkt))

;; Intended to be overriden by cosuming library.
(defun report (_data)
  'ok)

(defun send (sock req data)
  (case (gen_tcp:send sock req)
    ('ok `#(keep_state ,data))
    (`#(error ,_) (let (('ok (gen_tcp:close sock)))
                    `#(next_state disconnected ,data)))))
