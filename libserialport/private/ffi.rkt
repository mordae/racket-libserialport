#lang racket/base
;
; libserialport bindings
;

(require
  (rename-in ffi/unsafe (-> -->)))

(require racket/match
         racket/generator
         ffi/unsafe/define
         ffi/unsafe/alloc)

(require misc1/throw)

(provide
  (all-defined-out))


(define-ffi-definer define-libc #f)
(define-ffi-definer define-sp (ffi-lib "libserialport" '("0" "")))


(define-syntax-rule (define-ffi-wrapper ((name arg ...) proc) body ...)
  (define ((name arg ...) proc)
    (let ((wrapper (begin body ...))
          (proc-name (object-name proc)))
      (if proc-name (procedure-rename wrapper proc-name) wrapper))))

(define-ffi-wrapper ((check-result) proc)
  (let ((name (or (object-name proc) 'libserialport)))
    (λ args
      (let ((result (apply proc args)))
        (match result
          ('argument    (throw exn:fail:contract name "invalid argument"))
          ('failure     (throw exn:fail name "operation failed"))
          ('memory      (throw exn:fail name "insufficient memory"))
          ('unsupported (throw exn:fail:unsupported name "operation not supported"))
          (else result))))))

(define-ffi-wrapper ((convert convertor) proc)
  (λ args
    (let ((result (apply proc args)))
      (convertor result))))

(define (result-or-value result (value (void)))
  (if (eq? result 'ok) value result))


;; Opaque pointer representing the serial port object.
(define-cpointer-type _sp-port-pointer)


;; Windows needs a pointer-sized value, nixes just an integer.
(define _handle
  (match (system-type 'os)
    ('windows _intptr)
    (else _int)))


;; Error codes returned by virtually all functions.
(define _sp-return
  (_enum '(ok          =  0
           argument    = -1
           failure     = -2
           memory      = -3
           unsupported = -4)
         _int))

;; Port access modes.
(define _sp-mode
  (_enum '(read  = 1
           write = 2
           read-write = 3)
         _int))

;; Parity settings.
(define _sp-parity
  (_enum '(invalid = -1
           none    =  0
           odd     =  1
           even    =  2
           mark    =  3
           space   =  4)
         _int))

;; RTS or DTR pin behaviour.
(define _sp-rts-dtr
  (_enum '(invalid      = -1
           off          =  0
           on           =  1
           flow-control =  2)
         _int))

;; CTS or DSR pin behaviour.
(define _sp-cts-dsr
  (_enum '(invalid      = -1
           ignore       =  0
           flow-control =  1)
         _int))

;; XON/XOFF flow control behaviour.
(define _sp-xonxoff
  (_enum '(invalid  = -1
           disabled =  0
           in       =  1
           out      =  2
           in+out   =  3)
         _int))

;; Standard flow control combinations.
(define _sp-flow-control
  (_enum '(none    = 0
           xonxoff = 1
           rtscts  = 2
           dtrdsr  = 3)
         _int))


(define-sp sp_free_port
           (_fun _sp-port-pointer --> _void)
           #:wrap (releaser))

(define-sp sp_free_port_list
           (_fun (_cpointer _sp-port-pointer/null) --> _void)
           #:wrap (releaser))

(define-sp sp_get_port_by_name
           (_fun (name : _string/utf-8)
                 (port : (_ptr o _sp-port-pointer/null))
                 --> (result : _sp-return)
                 --> (result-or-value result port))
           #:wrap (compose (allocator sp_free_port)
                           (check-result)))

(define-sp sp_list_ports
           (_fun (ports : (_ptr o (_cpointer/null _sp-port-pointer/null)))
                 --> (result : _sp-return)
                 --> (result-or-value result ports))
           #:wrap (compose (allocator sp_free_port_list)
                           (check-result)))

(define-sp sp_get_port_name
           (_fun _sp-port-pointer --> _string/utf-8))

(define-sp sp_copy_port
           (_fun _sp-port-pointer
                 (port : (_ptr o _sp-port-pointer/null))
                 --> (result : _sp-return)
                 --> (result-or-value result port))
           #:wrap (compose (allocator sp_free_port)
                           (check-result)))

(define-sp sp_open
           (_fun _sp-port-pointer
                 _sp-mode
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_close
           (_fun _sp-port-pointer
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_get_port_handle
           (_fun _sp-port-pointer
                 (fd : (_ptr o _handle))
                 --> (result : _sp-return)
                 --> (result-or-value result fd))
           #:wrap (check-result))

(define-sp sp_set_baudrate
           (_fun _sp-port-pointer
                 _int
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_bits
           (_fun _sp-port-pointer
                 _int
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_parity
           (_fun _sp-port-pointer
                 _sp-parity
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_stopbits
           (_fun _sp-port-pointer
                 _int
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_rts
           (_fun _sp-port-pointer
                 _sp-rts-dtr
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_cts
           (_fun _sp-port-pointer
                 _sp-cts-dsr
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_dtr
           (_fun _sp-port-pointer
                 _sp-rts-dtr
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_dsr
           (_fun _sp-port-pointer
                 _sp-cts-dsr
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_xon_xoff
           (_fun _sp-port-pointer
                 _sp-xonxoff
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_set_flowcontrol
           (_fun _sp-port-pointer
                 _sp-flow-control
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_start_break
           (_fun _sp-port-pointer
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_end_break
           (_fun _sp-port-pointer
                 --> (result : _sp-return)
                 --> (result-or-value result))
           #:wrap (check-result))

(define-sp sp_free_error_message
           (_fun _bytes --> _void)
           #:wrap (releaser))

(define-sp sp_last_error_message
           (_fun --> _bytes)
           #:wrap (compose (convert bytes->string/utf-8)
                           (allocator sp_free_error_message)))

(define-libc scheme_make_fd_output_port
             (_fun (fd : _handle)
                   (name : _racket)
                   (regular? : _bool = #f)
                   (text-mode? : _bool = #f)
                   (read-too? : _bool = #t)
                   --> _racket))


(define (in-serial-ports)
  (in-generator
    (let ((ports (sp_list_ports)))
      (let next ((index 0))
        (let ((port (ptr-ref ports _sp-port-pointer/null index)))
          (cond
            (port
              (yield (sp_get_port_name port))
              (next (add1 index)))
            (else
              (sp_free_port_list ports))))))))


; vim:set ts=2 sw=2 et:
