(use srfi-69)

(module hash-table ()
  (import scheme chicken)
  (import (only srfi-69
    make-hash-table hash-table? hash-table-ref/default hash-table-set!
    hash-table-delete! hash-table-walk hash-table-copy))
  (import (rename (only srfi-69 hash-table-size)
    (hash-table-size hash-table-length)))
  (export make-hash-table hash-table)
  (export hash-table? hash-table-contains? hash-table=?)
  (export hash-table-ref hash-table-ref/default)
  (export hash-table-set! hash-table-set-all! hash-table-set-entries!
    hash-table-set-alist! hash-table-delete! hash-table-delete-keys!
    hash-table-extend! hash-table-extend!/default
    hash-table-replace! hash-table-replace!/default
    hash-table-update! hash-table-update!/default)
  (export hash-table-set hash-table-set-all hash-table-set-entries
    hash-table-set-alist hash-table-delete hash-table-delete-keys
    hash-table-extend hash-table-extend/default hash-table-replace
    hash-table-replace/default hash-table-update hash-table-update/default)
  (export hash-table-clear! hash-table-length hash-table-keys
    hash-table-values hash-table-entries hash-table-find hash-table-count
    hash-table-remove!)
  (export hash-table-map hash-table-map! hash-table-for-each
    hash-table-map->list hash-table-fold hash-table-unfold)
  (export hash-table-copy hash-table->alist alist->hash-table)
  (export hash-table-accessor hash-table-accessor/default
    hash-table-mutator hash-table-deleter hash-table-extender
    hash-table-extender/default hash-table-replacer
    hash-table-replacer/default hash-table-updater
    hash-table-updater/default)
  (export hash-table-union hash-table-union! hash-table-intersection
    hash-table-intersection! hash-table-difference hash-table-difference!)
  (export hash-table-error? hash-table-not-found?)
  (include "hash-table-impl.scm")
)
