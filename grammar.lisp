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

(defrule document (* block-top))

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

(defrule block-comment (and "###" EOL
                            (* (and (! (and "###" EOL))
                                    (* (not EOL)) EOL))
                            "###" EOL
                            (? line-blank)))

(defrule line-comment (and #\# (? #\Space) (* (not EOL)) EOL
                           (? line-blank)))

(defrule block-rule (and "----" EOL
                         (? line-blank)))

(defrule block-meta (and "---" EOL
                         (* (and (! (and "..." EOL))
                                 (* (not EOL)) EOL))
                         "..." EOL))

(defrule block-head (and (or "====" "===" "==" "=") (+ #\Space)
                         (or block-head-plain
                             block-head-multi
                             block-head-marker)
                         (? line-blank)))

(defrule block-head-plain (and (+ (and (! #\Space) ANY))
                               (+ #\Space) (+ #\=) EOL))

(defrule block-head-multi (and (+ ANY) EOL
                               (* (and (! block-head-multi-end)
                                       (not WS) (* ANY) EOL))
                               block-head-multi-end))

(defrule block-head-multi-end (and (not WS) (* (and (! #\Space) ANY))
                                   (+ #\Space) (+ #\=) EOL))

(defrule block-head-marker (and (+ ANY) EOL
                                (* (and (! block-head-marker-end)
                                        (not WS) (* ANY) EOL))
                                (& block-head-marker-end)))

(defrule block-head-marker-end (or marker-block-start EOL))

(defrule block-code "///")

(defrule block-pref (and (+ (and (* line-blank)
                                 #\Space #\Space (* ANY) EOL))
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

(defrule line-list-item-bullet (and #\* #\Space (* ANY) EOL))

(defrule line-list-item-number (and #\+ #\Space (* ANY) EOL))

(defrule line-list-item-data (and #\- #\Space (* ANY) EOL))

(defrule block-list-item (* (or block-blank
                                block-comment
                                line-comment
                                block-head
                                block-pref
                                block-list
                                block-title
                                block-verse
                                block-para)))

(defrule line-indented (and #\Space #\Space (* ANY) EOL))

(defrule block-title (and text-line
                          (and "===" (? (* #\=))) EOL
                          (? (and line-blank
                                  text-line
                                  (& (or line-blank))))
                          (? line-blank)))

(defrule block-verse (and #\. EOL
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

(defrule marker-escape (and #\\ ANY))

(defrule phrase-text (+ (and (! (or marker-phrase-start (and "http" (? #\s) #\:)))
                             ALL)))

(defrule phrase-code (and marker-code
                          (? (* (not marker-code)))
                          marker-code))

(defrule phrase-meta (and marker-func-start #\$
                          (+ (not marker-func-end))
                          marker-func-end))

(defrule phrase-func (and marker-func-start
                          (+ (not marker-func-end))
                          marker-func-end))

(defrule phrase-bold (and marker-bold (& (and (not #\Space) (not marker-bold)))
                          (+ (and (! marker-bold) phrase-markup))
                          marker-bold))

(defrule phrase-emph (and marker-emph (& (and (not #\Space) (not marker-emph)))
                          (+ (and (! marker-emph) phrase-markup))
                          marker-emph))

(defrule phrase-del (and marker-del (& (and (not #\Space) (not marker-del)))
                        (+ (and (! marker-del) phrase-markup))
                        marker-del))

(defrule phrase-under (and marker-under (& (and (not #\Space) (not marker-under)))
                           (+ (and (! marker-under) phrase-markup))
                           marker-under))

(defrule phrase-hyper (or phrase-hyper-named
                          phrase-hyper-explicit
                          phrase-hyper-implicit))

(defrule phrase-hyper-named (and #\" (+ (not #\")) #\"
                                 #\[ (and "http" (? #\s) #\: (* (not (or #\Space #\])))) #\]))

(defrule phrase-hyper-explicit (and #\[ (and "http" (? #\s) #\: (* (not (or #\Space #\])))) #\]))

(defrule phrase-hyper-implicit (and (and "http" (? #\s) #\: (+ (not #\Space)))))

(defrule phrase-link (or phrase-link-named
                         phrase-link-plain))

(defrule phrase-link-named (and #\" (+ (not #\")) #\"
                                #\[ (? (* (not #\Space))) #\]))

(defrule phrase-link-plain (and #\[ (? (* (not #\Space))) #\]))

(defrule marker-next ALL)

(defrule text-line (and (! (and (or marker-block-start #\Linefeed) #\Space))
                        (* ANY) (not #\Space) (* ANY) (or #\Newline)))

(defrule line-blank (and (* #\Space) EOL))

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

(defrule marker-pref #\Space)
(defrule marker-list #\*)
(defrule marker-head #\=)
(defrule marker-comment #\#)
(defrule marker-func-start #\<)
(defrule marker-func-end #\>)
(defrule marker-code #\`)
(defrule marker-bold #\*)
(defrule marker-emph #\/)
(defrule marker-del (and #\- #\-))
(defrule marker-under #\_)
(defrule marker-link (and #\" #\[))
(defrule marker-esc #\\)

(defrule EOL #\Newline)
(defrule ALL character)
(defrule ANY (not #\Newline))
(defrule WS (or #\Space #\Tab))

;;; grammar.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
