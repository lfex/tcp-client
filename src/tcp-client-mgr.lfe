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
   (parse-response 2)
   (report 1)))

(include-lib "lfe/include/clj.lfe")
(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
(defun STATEM_OPTS () '())

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
   (let* ((opts (maps:from_list opts))
          (data (maps:merge
                 opts
                 `#m(host ,host
                     port ,port
                     requests #m()
                     from undefined
                     backoff ,(backoff:init (mref opts 'init-backoff)
                                            (mref opts 'max-backoff))
                     timer undefined)))
          (actions '(#(next_event internal connect))))
     `#(ok disconnected ,data ,actions))))

(defun disconnected
  (('enter 'disconnected _data)
   'keep_state_and_data)
  (('enter 'connected (= `#m(requests ,reqs backoff ,b) data))
   (log-debug "Got requests: ~p" `(,reqs))
   (log-info "Connection closed")
   (case reqs
     (`#m() (log-debug "No requests to process."))
     (_ (lists:foreach (match-lambda (((= `#(,_ ,from) req))
                                      (log-debug "Got request: ~p" `(,req))
                                      (gen_statem:reply from '#(error disconnected)))
                                     ((req)
                                      (log-warn "Got unexpected request: ~p" `(,req))))
                       reqs)))
   (let* ((data (clj:->> data
                        (maps:put 'socket 'undefined)
                        (maps:put 'requests #m())))
         (actions `(#(#(timeout reconnect) ,(backoff:get b) undefined))))
     `#(keep_state ,data ,actions)))
  (('internal 'connect (= `#m(host ,host port ,port tcp-opts ,opts backoff ,b) data))
   (case (gen_tcp:connect host port opts)
     (`#(ok ,sock) `#(next_state connected ,(maps:put 'socket sock data)))
     (`#(error ,err) (let ((actions `(#(#(timeout reconnect) ,(backoff:get b) undefined))))
                       (log-warn "Connection failed: ~ts" `(,(inet:format_error err)))
                       `#(keep_state ,data ,actions)))))
  (('#(timeout reconnect) _ (= `#m(backoff ,b) data))
   (log-info "Attempting to reconnect ...")
   (let ((`#(,_ ,b) (backoff:fail b)))
     `#(keep_state ,(mset data 'backoff b) (#(next_event internal connect)))))
  ((`#(call ,from) `#(request ,_) _data)
   `#(keep_state_and_data `(#(reply ,from #(error disconnected)))))
  ((event-type event-content data)
   (log-warn "Got unexpected disconnected event: ~p"
             `(#m(event-type ,event-type
                  event-content ,event-content
                  data ,data)))
   'keep_state_and_data))

(defun connected
  (('enter _old-state (= `#m(backoff ,b) data))
   (let ((`#(,_ ,b) (backoff:succeed b)))
     (log-notice "Connected.")
     `#(keep_state ,(mset data 'backoff b))))
  (('info `#(tcp_closed ,sock) (= `#m(socket ,sock) data))
   `#(next_state disconnected ,data))
  ((`#(call ,from) `#(request ,req) (= `#m(socket ,sock) data))
   (log-debug "Call from ~p" `(,from))
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
     `#(keep_state ,data)))
  ((event-type event-content data)
   (log-warn "Got unexpected connected event: ~p"
             `(#m(event-type ,event-type
                  event-content ,event-content
                  data ,data)))
   'keep_state_and_data))

;;; Private functions

;; Intended to be overriden by cosuming library.
(defun parse-response
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
