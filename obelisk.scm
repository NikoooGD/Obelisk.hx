;; obelisk.hx - fixed, no getenv

(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

;; simple vault path - no getenv to avoid FreeIdentifier error
(define vault-path "/home/alya/Documents/Obsidian Vault")

(define (file-exists? path)
  (let ((exists #f))
    (with-handler (lambda (e) (set! exists #f))
      (let ((p (open-input-file path)))
        (set! exists #t)
        (close-input-port p)))
    exists))

;; if that vault doesn't exist, try ~/Obsidian
(when (not (file-exists? vault-path))
  (set! vault-path "/home/alya/Obsidian"))

;;@doc Open vault
(define (obelisk-open)
  (helix.open vault-path))

;;@doc Open daily note
(define (obelisk-daily)
  (helix.open (string-append vault-path "/Daily.md")))

;;@doc Follow [[wikilink]]
(define (obelisk-follow)
  (helix.open vault-path))

;;@doc List #tags
(define (obelisk-tags)
  (helix.vsplit)
  (helix.open vault-path))

;;@doc Set vault path manually
(define (obelisk-set-vault path)
  (set! vault-path path))

(provide obelisk-open obelisk-daily obelisk-follow obelisk-tags obelisk-set-vault)

;; compat aliases
(define obsidian-open obelisk-open)
(define obsidian-daily obelisk-daily)
(define obsidian-follow obelisk-follow)
(define obsidian-tags obelisk-tags)
(provide obsidian-open obsidian-daily obsidian-follow obsidian-tags)
