;; obelisk.hx v1.1 - Obsidian for Helix
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define vault-path "/home/alya/Documents/Obsidian Vault")

(define (file-exists? path)
  (let ((exists #f))
    (with-handler (lambda (e) (set! exists #f))
      (let ((p (open-input-file path)))
        (set! exists #t)
        (close-input-port p)))
    exists))

(when (not (file-exists? vault-path))
  (when (file-exists? "/home/alya/Obsidian")
    (set! vault-path "/home/alya/Obsidian")))

(define (my-trim s)
  (let ((len (string-length s)))
    (let loop ((st 0) (en (- len 1)))
      (cond
        ((> st en) "")
        ((or (char=? (string-ref s st) #\space) (char=? (string-ref s st) #\tab))
         (loop (+ st 1) en))
        ((or (char=? (string-ref s en) #\space) (char=? (string-ref s en) #\tab))
         (loop st (- en 1)))
        (else (substring s st (+ en 1)))))))

(define (my-split-lines s)
  (let ((len (string-length s)))
    (let loop ((i 0) (start 0) (out '()))
      (if (>= i len)
          (reverse (cons (substring s start len) out))
          (if (char=? (string-ref s i) #\newline)
              (loop (+ i 1) (+ i 1) (cons (substring s start i) out))
              (loop (+ i 1) start out))))))

(define (my-join-lines lst sep)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc (loop (cdr rest) (string-append acc sep (car rest)))))))

(define (read-file-simple path)
  (let ((port (open-input-file path)))
    (let loop ((acc '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin (close-input-port port) (my-join-lines (reverse acc) "\n"))
            (loop (cons line acc)))))))

(define (write-file-simple path content)
  (let ((out (open-output-file path)))
    (display content out)
    (close-output-port out)))

(define (string-contains? str sub)
  (let ((ls (string-length str)) (lsub (string-length sub)))
    (let loop ((i 0))
      (if (> (+ i lsub) ls) #f
          (if (string=? (substring str i (+ i lsub)) sub) #t (loop (+ i 1)))))))

(define (extract-wikilink line)
  (let ((start (let loop ((i 0))
                 (if (>= (+ i 1) (string-length line)) #f
                     (if (and (char=? (string-ref line i) #\[) (char=? (string-ref line (+ i 1)) #\[)) i (loop (+ i 1)))))))
    (if (not start) #f
        (let ((end (let loop ((i (+ start 2)))
                     (if (>= (+ i 1) (string-length line)) #f
                         (if (and (char=? (string-ref line i) #\]) (char=? (string-ref line (+ i 1)) #\])) i (loop (+ i 1)))))))
          (if (not end) #f
              (let ((raw (substring line (+ start 2) end)))
                (let ((pipe-pos (let loop ((i 0)) (if (>= i (string-length raw)) #f (if (char=? (string-ref raw i) #\|) i (loop (+ i 1))))))
                      (hash-pos (let loop ((i 0)) (if (>= i (string-length raw)) #f (if (char=? (string-ref raw i) #\#) i (loop (+ i 1)))))))
                  (cond (pipe-pos (substring raw 0 pipe-pos))
                        (hash-pos (substring raw 0 hash-pos))
                        (else raw)))))))))

(define (get-current-file-path)
  (let* ((focus (editor-focus)) (doc-id (editor->doc-id focus)) (path (editor-document->path doc-id))) path))

(define (get-current-file-name)
  (let ((p (get-current-file-path)))
    (if (not p) ""
        (let loop ((i 0) (last 0))
          (if (>= i (string-length p)) (substring p last (string-length p))
              (if (char=? (string-ref p i) #\/) (loop (+ i 1) (+ i 1)) (loop (+ i 1) last)))))))

;; --- vault scan ---
;; Steel has no stable read-dir, so we use vault/index.txt
;; Generate it with: cd vault && rg --files > index.txt
(define (vault-files)
  (let ((idx (string-append vault-path "/index.txt")))
    (if (file-exists? idx)
        (my-split-lines (read-file-simple idx))
        (list (string-append vault-path "/Daily.md")))))

(define (find-in-file path needle)
  (if (not (file-exists? path)) '()
      (let ((lines (my-split-lines (read-file-simple path))))
        (let loop ((ls lines) (n 1) (out '()))
          (if (null? ls) (reverse out)
              (if (string-contains? (car ls) needle)
                  (loop (cdr ls) (+ n 1) (cons (string-append path ":" (number->string n) ": " (car ls)) out))
                  (loop (cdr ls) (+ n 1) out)))))))

(define (find-in-vault needle)
  (let ((files (vault-files)))
    (let loop ((fs files) (out '()))
      (if (null? fs) out
          (loop (cdr fs) (append out (find-in-file (car fs) needle)))))))

(define (write-results-file name lines)
  (let ((p (string-append "/tmp/obelisk-" name ".md")))
    (write-file-simple p (string-append "# Obelisk " name "\n\n" (my-join-lines lines "\n") "\n"))
    p))

;; --- commands ---

;;@doc Open vault
(define (obelisk-open) (helix.open vault-path))

;;@doc Daily
(define (obelisk-daily)
  (let ((daily (string-append vault-path "/Daily.md")))
    (when (not (file-exists? daily)) (write-file-simple daily "# Daily Notes\n\n"))
    (helix.open daily)))

;;@doc Follow [[wikilink]]
(define (obelisk-follow)
  (let ((path (get-current-file-path)))
    (if (not path) (helix.open vault-path)
        (let ((lines (my-split-lines (read-file-simple path))))
          (let loop ((ls lines))
            (if (null? ls) (helix.open vault-path)
                (let ((link (extract-wikilink (car ls))))
                  (if (not link) (loop (cdr ls))
                      (let* ((trimmed (my-trim link))
                             (with-md (if (string-contains? trimmed ".md") trimmed (string-append trimmed ".md")))
                             (full-path (string-append vault-path "/" with-md)))
                        (if (file-exists? full-path) (helix.open full-path)
                            (begin (write-file-simple full-path (string-append "# " trimmed "\n\n")) (helix.open full-path)))))))))))))

;;@doc New note: (obelisk-new "My Note")
(define (obelisk-new name)
  (let* ((trimmed (my-trim name))
         (with-md (if (string-contains? trimmed ".md") trimmed (string-append trimmed ".md")))
         (full (string-append vault-path "/" with-md)))
    (when (not (file-exists? full)) (write-file-simple full (string-append "# " trimmed "\n\n")))
    (helix.open full)))

;;@doc Search vault: (obelisk-search "query")
(define (obelisk-search query)
  (let ((results (find-in-vault query)))
    (if (null? results) (helix.open vault-path)
        (let ((p (write-results-file "search" (cons (string-append "Search: " query) results))))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Real tags
(define (obelisk-tags)
  (let ((results (find-in-vault "#")))
    (if (null? results) (helix.open vault-path)
        (let ((p (write-results-file "tags" results)))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Real backlinks
(define (obelisk-backlinks)
  (let* ((fname (get-current-file-name))
         (base (if (string-contains? fname ".md") (substring fname 0 (- (string-length fname) 3)) fname))
         (needle (string-append "[[" base))
         (results (find-in-vault needle)))
    (if (null? results)
        (let ((p (write-results-file "backlinks" (list (string-append "No backlinks for [[" base "]]") "" "Tip: cd vault && rg --files > index.txt"))))
          (helix.vsplit)
          (helix.open p))
        (let ((p (write-results-file "backlinks" (cons (string-append "Backlinks for [[" base "]]") results))))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Set vault
(define (obelisk-set-vault path) (set! vault-path path))

;;@doc Info
(define (obelisk-info) (helix.open vault-path))

(provide obelisk-open obelisk-daily obelisk-follow obelisk-tags obelisk-backlinks obelisk-search obelisk-new obelisk-set-vault obelisk-info)

(define obsidian-open obelisk-open)
(define obsidian-daily obelisk-daily)
(define obsidian-follow obelisk-follow)
(define obsidian-tags obelisk-tags)
(define obsidian-search obelisk-search)
(define obsidian-new obelisk-new)
(provide obsidian-open obsidian-daily obsidian-follow obsidian-tags obsidian-search obsidian-new)
