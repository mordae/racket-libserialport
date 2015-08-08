#lang typed/racket/base
;
; Serial Port Access
;

(require racket/sequence
         racket/path)

(require/typed ffi/file
  (security-guard-check-file
    (-> Symbol Path-String (Listof (U 'read 'write)) Void)))

(require/typed libserialport/private/ffi
  (#:opaque Serial-Port sp-port-pointer?)

  (in-serial-ports
    (-> (Sequenceof Path-String)))

  (sp_get_port_by_name
    (-> Path-String Serial-Port))

  (sp_open
    (-> Serial-Port (U 'read 'write) Void))

  (sp_set_baudrate
    (-> Serial-Port Positive-Integer Void))

  (sp_set_bits
    (-> Serial-Port Positive-Integer Void))

  (sp_set_parity
    (-> Serial-Port Parity Void))

  (sp_set_stopbits
    (-> Serial-Port Natural Void))

  (sp_set_flowcontrol
    (-> Serial-Port Flow-Control Void))

  (sp_get_port_handle
    (-> Serial-Port Integer))

  (scheme_make_fd_output_port
    (-> Integer Symbol (values Input-Port Output-Port))))


(provide serial-ports
         in-serial-ports
         open-serial-port
         Parity
         Flow-Control)


(define-type Parity
  (U 'invalid 'none 'odd 'even 'mark 'space))

(define-type Flow-Control
  (U 'none 'xonxoff 'rtscts 'dtrdsr))


(: serial-ports (-> (Listof Path-String)))
(define (serial-ports)
  (sequence->list
    (in-serial-ports)))


(: open-serial-port
  (->* (Path-String)
       (#:baudrate Positive-Integer
        #:bits Positive-Integer
        #:parity Parity
        #:stopbits Natural
        #:flowcontrol Flow-Control)
       (values Input-Port Output-Port)))
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


(: path->port-name (-> Path-String Symbol))
(define (path->port-name path)
  (let ((path (file-name-from-path path)))
    (string->symbol
      (if path (path->string path) "unknown"))))


; vim:set ts=2 sw=2 et:
