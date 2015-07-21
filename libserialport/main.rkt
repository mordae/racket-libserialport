#lang racket/base
;
; Serial Port Access
;

(require racket/contract
         racket/sequence
         racket/path
         ffi/file)

(require libserialport/private/ffi)

(provide
  (contract-out
    (in-serial-ports (-> sequence?))
    (serial-ports (-> (listof path-string?)))

    (open-serial-port
      (->* (path-string?)
           (#:baudrate exact-positive-integer?
            #:bits exact-positive-integer?
            #:parity (or/c 'none 'odd 'even 'mark 'space)
            #:stopbits exact-positive-integer?
            #:flowcontrol (or/c 'none 'xonxoff 'rtscts 'dtrdsr))
           (values input-port? output-port?)))))



(define (serial-ports)
  (sequence->list
    (in-serial-ports)))


(define (open-serial-port path
                          #:baudrate (baudrate 9600)
                          #:bits (bits 8)
                          #:parity (parity 'none)
                          #:stopbits (stopbits 1)
                          #:flowcontrol (flowcontrol 'none))
  (security-guard-check-file 'open-serial-port path '(read write))

  (let ((port (sp_get_port_by_name path)))
    (sp_open port 'write)
    (sp_set_baudrate port baudrate)
    (sp_set_bits port bits)
    (sp_set_parity port parity)
    (sp_set_stopbits port stopbits)
    (sp_set_flowcontrol port flowcontrol)

    (let ((fd (sp_get_port_handle port))
          (name (path->port-name path)))
      (scheme_make_fd_output_port fd name))))


(define (path->port-name path)
  (string->symbol
    (path->string
      (file-name-from-path path))))


; vim:set ts=2 sw=2 et:
