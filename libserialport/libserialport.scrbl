#lang scribble/manual

@require[(for-label libserialport)
         (for-label typed/racket/base)]

@require[scribble/eval unstable/sandbox]

@(define libserialport-eval (make-log-based-eval "libserialport-log" 'replay))
@interaction-eval[#:eval libserialport-eval (require libserialport)]

@title{libserial: Portable Serial Port Access}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

Serial port handling for Racket utilizing @filepath{libserialport}.

The libserialport itself is a minimal, cross-platform shared library
written in C that is intended to take care of the OS-specific details
when writing software that uses serial ports.

It must be installed externally to this Racket module, preferrably via
the system package manager.

@defmodule[libserialport]

A high-level interface to port enumeration, access and configuration.

@defidform[Parity]{
  Type for parity options. Equivalent to
  @racket[(U 'invalid 'none 'odd 'even 'mark 'space)].
}

@defidform[Flow-Control]{
  Type for flow control options. Equivalent to
  @racket[(U 'none 'xonxoff 'rtscts 'dtrdsr)].
}

@defproc[(serial-ports) (Listof Path-String)]{
  Produce list of system serial port paths or names.

  @examples[#:eval libserialport-eval
    (serial-ports)
  ]
}

@defproc[(in-serial-ports) (Sequenceof Path-String)]{
  Iterate over known serial port paths or names.
  Produces @racket[Path-String] values.

  @examples[#:eval libserialport-eval
    (for ((serial-port (in-serial-ports)))
      (printf "found ~a\n" serial-port))
  ]
}

@defproc[(open-serial-port
           (path Path-String)
           (#:baudrate baudrate Positive-Integer 9600)
           (#:bits bits Positive-Integer 8)
           (#:parity parity Parity 'none)
           (#:stopbits stopbits Natural 1)
           (#:flowcontrol flowcontrol Flow-Control 'none))
           (values Input-Port Output-Port)]{
  Opens (and configures) selected serial port for both reading and writing.
  The default options correspond to the most frequently used null-modem case
  typical for switches and other similar embedded devices.

  @examples[#:eval libserialport-eval
    (define-values (in out)
      (open-serial-port "/dev/ttyUSB0" #:baudrate 115200))
    (write-bytes #"x1AVx3\n" out)
    (let loop ()
      (define read-result (read-bytes-avail!* read-buffer port))
      (cond [(or (eof-object? read-result)
                 (and (number? read-result) (not (= read-result 0))))
             (display (bytes->string/utf-8 (read-bytes read-result port)))
             (display "")] ; flash output
            [else (sleep 0.1)])
      (loop))
  ]
}

@; vim:set ft=scribble sw=2 ts=2 et:
