;; NS-CORE
;;
;; Copyright (c) 2012, Chris Barrett
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;; DESCRIPTION:
;; Core namespace functions.
;;

(provide 'ns-core)
(require 'cl)


(defstruct ns-meta
  public?
  mutable?)

(defconst ns/symbols-table (make-hash-table :test 'equal)
  "hash * name -> ns-meta")


;;; --------------------------- Symbol Processing ---------------------------------

(defun ns/hash (sym)
  "Hash a symbol for insertion into lookup tables."
  (let ((hash (secure-hash 'sha256 (symbol-name sym))))
    (intern (concat "__ns/namespaced_sym__" hash))))

(defun ns/split-sym (sym)
  "Splits SYM into a tuple of (namespace * symbol)."
  (let* ((xs (split-string (symbol-name sym) "/"))
         (s1 (intern (car xs)))
         (s2 (when (nth 1 xs) (intern (nth 1 xs)))))
    (if (< 1 (length xs))
        `(,s1 . ,s2)
      `(,nil . ,s1))))

(defun ns/qualify (ns sym)
  "Ensure SYM is qualified with namespace NS."
  (let* ((tpl  (ns/split-sym sym))
         (ns   (symbol-name ns))
         (name (symbol-name (cdr tpl))))
    (intern (concat ns "/" name))))


(defun ns/make-key (ns sym)
  "Make a key for the symbols table."
  (let* ((sym  (ns/qualify ns sym))
         (hash (ns/hash sym)))
    `(,hash . ,sym)))

;;; --------------------------- Table Accessors -----------------------------------

(defun ns/intern (ns sym)
  "Intern the given ns/sym into the namespace table. Returns the tuple of (hash * sym) used as the key."
  (let ((key (ns/make-key ns sym)))
    ;; Make sure existing metadata isn't lost.
    (unless (gethash key ns/symbols-table)
      (puthash key (make-ns-meta) ns/symbols-table))
    key))

(defun hash-keys (table)
  (loop for k being the hash-keys of table
        collect k))

(defun ns/get-symbol-name (hash)
  "Returns the interned symbol name corresponding to HASH, or nil"
  (let ((filtered (remove-if-not (lambda (tpl) (equal hash (car tpl)))
                                 (hash-keys ns/symbols-table))))
    (cdr-safe (car-safe filtered))))

(defun ns/get-symbol-hash (ns sym)
  "Returns the interned symbol name corresponding to HASH, or nil"
  (let* ((sym (ns/qualify ns sym))
         (filtered (remove-if-not (lambda (tpl) (equal sym (cdr tpl)))
                                  (hash-keys ns/symbols-table))))
    (car-safe (car-safe filtered))))

(defun ns/get-symbol-meta (ns sym)
  "Gets the metadata for the given symbol, or nil no such symbol exists."
  (gethash (ns/make-key ns sym) ns/symbols-table))


;;; --------------------------- Imports/Exports -----------------------------------

(defconst ns/imports-table (make-hash-table)
  "ns -> (hash * name) -> (hash * name)")

(defconst ns/exports-table (make-hash-table)
  "ns -> (hash * name) -> (hash * name)")

(defun ns/puthash-in (outer-key inner-key inner-value table)
  "Insert a value into a nested hash-table, creating the outer table if needed."
  (let ((tbl (gethash outer-key table)))
    (unless tbl
      (setq tbl (make-hash-table :test 'equal))
      (puthash outer-key tbl table))
    (puthash inner-key inner-value tbl)))

(defun ns/export (ns sym)
  "Make SYM publicly accessible."
  ;; Ensure metadata exists for SYM
  (ns/intern ns sym)
  (let ((tpl  (ns/make-key ns sym) )
        (meta (ns/get-symbol-meta ns sym)))
    (ns/puthash-in ns tpl tpl ns/exports-table)
    (setf (ns-meta-public? meta) t)
    tpl))

(defun ns/import (ns-from ns-to sym)
  "Import a symbol defined by one namespace into another."
  ;; Ensure SYM is publicly accessible.
  (let* ((meta    (ns/get-symbol-meta ns-from sym))
         (public? (when meta (ns-meta-public? meta))))
    (assert public? nil
            "Symbol `%s` is undefined or inaccessible from namespace `%s`"
            (ns/qualify ns-from sym) ns-to))
  ;; Add symbol to imports for NS-TO.
  (let ((tpl (ns/make-key ns-from sym)))
    (ns/puthash-in ns-to tpl tpl ns/imports-table)
    tpl))

(defun ns/import-all (from-ns into-ns)
  "Import all public symbols from one namespace into another."
  (mapcar (lambda (tpl)
            (ns/import from-ns into-ns (cdr tpl)))
          (hash-keys (gethash from-ns ns/exports-table))))



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
