#lang scribble/manual

@require[(for-label libserialport)
         (for-label racket)]

@require[scribble/eval unstable/sandbox]

@(define libserialport-eval (make-log-based-eval "libserialport-log" 'record))
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

The high-level interface provides port enumeration and a routine that
combines opening and configuration of a serial port in a single step.

@defproc[(serial-ports) (listof path-string?)]{
  Produce list of system serial port paths or names.

  @examples[#:eval libserialport-eval
    (serial-ports)
  ]
}

@defproc[(in-serial-ports) sequence?]{
  Iterate over known serial port paths or names.
  Produces @racket[path-string?] values.

  @examples[#:eval libserialport-eval
    (for ((serial-port (in-serial-ports)))
      (printf "found ~a\n" serial-port))
  ]
}

@defproc[(open-serial-port
           (path path-string?)
           (#:baudrate baudrate exact-positive-integer? 9600)
           (#:bits bits exact-positive-integer? 8)
           (#:parity parity (or/c 'none 'odd 'even 'mark 'space) 'none)
           (#:stopbits stopbits exact-positive-integer? 1)
           (#:flowcontrol flowcontrol (or/c 'none 'xonxoff 'rtscts 'dtrdsr) 'none))
           (values input-port? output-port?)]{
  Opens (and configures) selected serial port for both reading and writing.
  The default options correspond to the most frequently used null-modem case
  typical for switches and other similar embedded devices.

  @examples[#:eval libserialport-eval
    (define-values (in out)
      (open-serial-port "/dev/ttyUSB0" #:baudrate 115200))
    (write-bytes #"x1AVx3\n" out)
  ]
}

@; vim:set ft=scribble sw=2 ts=2 et:
