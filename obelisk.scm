
;; obelisk.hx - Obsidian for Helix
;; Name: Ob (sidian) + (He)lix + Obelisk (ancient obsidian monuments)
;; Install: forge pkg install --git https://github.com/NikoooGD/obelisk.hx.git
;; Usage: (require "obelisk.hx/obelisk.scm")

(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define vault-path
  (let ((env (getenv "OBSIDIAN_VAULT")))
    (if (and env (not (string=? env ""))) env
        (string-append (getenv "HOME") "/Obsidian"))))

(define (file-exists? path)
  (let ((exists #f))
    (with-handler (lambda (e) (set! exists #f))
      (let ((p (open-input-file path)))
        (set! exists #t) (close-input-port p)))
    exists))

;;@doc Open vault
(define (obelisk-open) (helix.open vault-path))

;;@doc Daily note
(define (obelisk-daily)
  (helix.open (string-append vault-path "/Daily.md")))

;;@doc Follow [[wikilink]]
(define (obelisk-follow) (helix.open vault-path))

;;@doc List #tags
(define (obelisk-tags)
  (helix.vsplit)
  (helix.open vault-path))

;;@doc Set vault path
(define (obelisk-set-vault path) (set! vault-path path))

(provide obelisk-open obelisk-daily obelisk-follow obelisk-tags obelisk-set-vault)

;; aliases for compatibility
(define obsidian-open obelisk-open)
(define obsidian-daily obelisk-daily)
(define obsidian-follow obelisk-follow)
(define obsidian-tags obelisk-tags)
(provide obsidian-open obsidian-daily obsidian-follow obsidian-tags)
