;;;; grammar.lisp --- Swim markup parser

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; Grammar version 0.0.1
;;; See https://github.com/ingydotnet/swim-pgx

;;;; Code:

(defpackage #:swim-grammar
  (:use #:cl #:esrap))

(in-package #:swim-grammar)

(defrule document (* block-top)
  (:lambda (r)
    `(:document ,r)))

(defrule block-top (or block-blank
                       block-comment
                       line-comment
                       block-rule
                       block-meta
                       block-head
                       block-code
                       block-pref
                       block-list
                       block-title
                       block-verse
                       block-para))

(defrule block-blank line-blank)

(defrule block-comment-sep (and HASH HASH HASH EOL))

(defrule block-comment (and block-comment-sep
                            (* (and (! block-comment-sep)
                                    (* (and (! EOL) ANY)) EOL))
                            block-comment-sep
                            (? line-blank))
  (:destructure (sep1 comment sep2 &rest rest)
    (declare (ignore sep1 sep2 rest))
    `(:comment ,(text comment))))

(defrule line-comment (and HASH (? SPACE) (* (and (! EOL) ANY)) EOL
                           (? line-blank)))

(defrule block-rule (and "----" EOL
                         (? line-blank)))

(defrule block-meta-beg (and "---" EOL))
(defrule block-meta-end (and "..." EOL))

(defrule block-meta (and block-meta-beg
                         (* (and (! block-meta-end)
                                 (* (and (! EOL) ALL)) EOL))
                         block-meta-end))

(defrule block-head (and (or "====" "===" "==" "=") (+ SPACE)
                         (or block-head-plain
                             block-head-multi
                             block-head-marker)
                         (? line-blank)))

(defrule block-head-plain (and (+ (and (! SPACE) ANY))
                               (+ SPACE) (+ EQUAL) EOL))

(defrule block-head-multi (and (+ ANY) EOL
                               (* (and (not WS) (* ANY) EOL))
                               (not WS) (* (and (! block-head-multi-end) ANY))
                               block-head-multi-end))

(defrule block-head-multi-end (and (+ SPACE) (+ EQUAL) EOL))

(defrule block-head-marker (and (+ ANY) EOL
                                (* (and (not WS) (* ANY) EOL))
                                (& (or marker-block-start EOL EOS))))

(defrule block-code "///")

(defrule block-pref (and (+ (and (* line-blank)
                                 SPACE SPACE (* ANY) EOL))
                         (? line-blank)))

(defrule block-list (or block-list-bullet
                        block-list-number
                        block-list-data))

(defrule block-list-bullet (and line-list-item-bullet
                                (* (or line-list-item-bullet
                                       line-blank
                                       line-indented))
                                (? line-blank)))

(defrule block-list-number (and line-list-item-number
                                (* (or line-list-item-number
                                       line-blank
                                       line-indented))
                                (? line-blank)))

(defrule block-list-data (and line-list-item-data
                              (* (or line-list-item-data
                                     line-blank
                                     line-indented))))

(defrule line-list-item-bullet (and STAR SPACE (* ANY) EOL))

(defrule line-list-item-number (and PLUS SPACE (* ANY) EOL))

(defrule line-list-item-data (and DASH SPACE (* ANY) EOL))

(defrule block-list-item (* (or block-blank
                                block-comment
                                line-comment
                                block-head
                                block-pref
                                block-list
                                block-title
                                block-verse
                                block-para)))

(defrule line-indented (and SPACE SPACE (* ANY) EOL))

(defrule block-title (and text-line
                          EQUAL EQUAL EQUAL (* EQUAL) EOL
                          (? (and line-blank
                                  text-line
                                  (& (or line-blank EOS))))
                          (? line-blank)))

(defrule block-verse (and DOT EOL
                          (+ text-line)
                          (? line-blank)))

(defrule block-para (and (+ text-line)
                         (? line-blank)))

(defrule text-markup (+ phrase-markup))

(defrule phrase-markup (or phrase-text
                           marker-escape
                           phrase-meta
                           phrase-func
                           phrase-code
                           phrase-bold
                           phrase-emph
                           phrase-del
                           phrase-under
                           phrase-hyper
                           phrase-link
                           marker-next))

(defrule marker-escape (and BACK ANY))

(defrule phrase-text (+ (and (! (or marker-phrase-start (and "http" (? #\s) #\:)))
                             ALL)))

(defrule phrase-code (and marker-code
                          (* (not marker-code))
                          marker-code))

(defrule phrase-meta (and marker-func-start #\$
                          (+ (not marker-func-end))
                          marker-func-end))

(defrule phrase-func (and marker-func-start
                          (+ (not marker-func-end))
                          marker-func-end))

(defrule phrase-bold (and marker-bold (& (and NS (not marker-bold)))
                          (+ (and (! marker-bold) phrase-markup))
                          marker-bold))

(defrule phrase-emph (and marker-emph (& (and NS (not marker-emph)))
                          (+ (and (! marker-emph) phrase-markup))
                          marker-emph))

(defrule phrase-del (and marker-del (& (and NS (not marker-del)))
                        (+ (and (! marker-del) phrase-markup))
                        marker-del))

(defrule phrase-under (and marker-under (& (and NS (not marker-under)))
                           (+ (and (! marker-under) phrase-markup))
                           marker-under))

(defrule phrase-hyper (or phrase-hyper-named
                          phrase-hyper-explicit
                          phrase-hyper-implicit))

(defrule phrase-hyper-named (and DOUBLE (+ (not DOUBLE)) DOUBLE
                                 LSQUARE http-scheme (* (and (! RSQUARE) NS))
                                 RSQUARE))

(defrule phrase-hyper-explicit (and LSQUARE http-scheme (* (and (! RSQUARE) NS))
                                    RSQUARE))

(defrule phrase-hyper-implicit (and http-scheme (+ NS)))

(defrule phrase-link (or phrase-link-named
                         phrase-link-plain))

(defrule phrase-link-named (and DOUBLE (+ (not DOUBLE)) DOUBLE
                                LSQUARE (* (and (! RSQUARE)))
                                RSQUARE))

(defrule phrase-link-plain (and LSQUARE (* (and (! RSQUARE)))
                                RSQUARE))

(defrule marker-next ALL)

(defrule text-line (and (! (and (or marker-block-start NL) SPACE))
                        (* ANY) NS (* ANY) (or EOL EOS))
  (:text t))

(defrule line-blank (and (* SPACE) EOL))

(defrule marker-block-start (or marker-pref
                                marker-list
                                marker-head
                                marker-comment))

(defrule marker-phrase-start (or marker-func-start
                                 marker-code
                                 marker-bold
                                 marker-emph
                                 marker-del
                                 marker-under
                                 marker-link
                                 marker-esc))

(defrule marker-pref SPACE)
(defrule marker-list STAR)
(defrule marker-head EQUAL)
(defrule marker-comment HASH)
(defrule marker-func-start #\<)
(defrule marker-func-end #\>)
(defrule marker-code #\`)
(defrule marker-bold STAR)
(defrule marker-emph #\/)
(defrule marker-del (and DASH DASH))
(defrule marker-under #\_)
(defrule marker-link (and DOUBLE LSQUARE))
(defrule marker-esc BACK)

(defrule http-scheme (and "http" (? #\s) #\:))

(defrule ALL character)
(defrule ANY (not #\Newline))
(defrule BACK #\\)
(defrule DASH #\-)
(defrule DOT #\.)
(defrule DOUBLE #\")
(defrule EOL #\Newline)
(defrule EOS (! (string 1)) (:constant nil))
(defrule EQUAL #\=)
(defrule HASH #\#)
(defrule LSQUARE #\[)
(defrule NL #\Linefeed)
(defrule NS (not #\Space))
(defrule PLUS #\+)
(defrule RSQUARE #\])
(defrule SPACE #\Space)
(defrule STAR #\*)
(defrule WS (or #\Space #\Tab))

;;; grammar.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
