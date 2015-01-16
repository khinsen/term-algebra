#lang racket

(provide (struct-out rule))

(struct rule (pattern condition replacement)
        #:transparent)

