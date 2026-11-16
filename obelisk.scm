;; obelisk.hx v1.3.3 - Obsidian for Helix, no Obsidian app required
;; Fixes: v1.3.1 ensure-vault + v1.3.2 variadic commands (ArityMismatch) + v1.3.3 wikilink pipe syntax
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/static.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define (file-exists? path)
  (let ((exists #f))
    (with-handler (lambda (e) (set! exists #f))
      (let ((p (open-input-file path)))
        (set! exists #t)
        (close-input-port p)))
    exists))

;; --- ENV - from trail.scm ---
(define (obelisk-env-var name)
  (with-handler
    (lambda (_) "")
    (let* ([p (open-input-file "/proc/self/environ")]
           [raw (read-port-to-string p)])
      (close-input-port p)
      (let* ([key (string-append name "=")]
             [klen (string-length key)]
             [rlen (string-length raw)])
        (let outer ([i 0])
          (cond
            [(> (+ i klen) rlen) ""]
            [(let inner ([j 0])
               (cond [(= j klen) #true]
                     [(char=? (string-ref raw (+ i j)) (string-ref key j)) (inner (+ j 1))]
                     [else #false]))
             (let* ([start (+ i klen)]
                    [end (let scan ([k start])
                           (if (or (>= k rlen) (char=? (string-ref raw k) (integer->char 0)))
                               k (scan (+ k 1))))])
               (substring raw start end))]
            [else (outer (+ i 1))]))))))

(define *obelisk-home* (obelisk-env-var "HOME"))
(when (equal? *obelisk-home* "") (set! *obelisk-home* "/home/alya"))

(define vault-path (string-append *obelisk-home* "/Documents/Obsidian Vault"))

;; --- helpers ---
(define (my-trim s)
  (let ((len (string-length s)))
    (let loop ((st 0) (en (- len 1)))
      (cond
        ((> st en) "")
        ((char=? (string-ref s st) #\space) (loop (+ st 1) en))
        ((char=? (string-ref s en) #\space) (loop st (- en 1)))
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
  (with-handler (lambda (_) #false) (delete-file! path))
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
                ;; Handle [[link|display]] pipe syntax - extract only the link part
                (let ((pipe-pos (let loop ((i 0))
                                  (if (>= i (string-length raw)) #f
                                      (if (char=? (string-ref raw i) #\|) i (loop (+ i 1)))))))
                  (if pipe-pos
                      (substring raw 0 pipe-pos)
                      raw))))))))

(define (get-current-file-path)
  (let* ((focus (editor-focus)) (doc-id (editor->doc-id focus)) (path (editor-document->path doc-id))) path))

(define (get-current-file-name)
  (let ((p (get-current-file-path)))
    (if (not p) ""
        (let loop ((i 0) (last 0))
          (if (>= i (string-length p)) (substring p last (string-length p))
              (if (char=? (string-ref p i) #\/) (loop (+ i 1) (+ i 1)) (loop (+ i 1) last)))))))

(define (get-base-name p)
  (let loop ((i 0) (last 0))
    (if (>= i (string-length p))
        (substring p last (- (string-length p) 3))
        (if (char=? (string-ref p i) #\/)
            (loop (+ i 1) (+ i 1))
            (loop (+ i 1) last)))))

;; --- v1.3.1: ensure vault exists even if Obsidian never installed ---
(define (obelisk-mkdir-p! path)
  (let ([proc (~> (command "mkdir" (list "-p" path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (when (Ok? proc)
      (read-port-to-string (child-stdout (Ok->value proc))))))

(define (obelisk-ensure-vault!)
  (obelisk-mkdir-p! (string-append *obelisk-home* "/Documents"))
  (obelisk-mkdir-p! vault-path)
  (when (not (file-exists? (string-append vault-path "/Daily.md")))
    (write-file-simple (string-append vault-path "/Daily.md")
      "# Daily Notes\n\nWelcome to Obelisk.hx - Obsidian for Helix, no Obsidian app needed.\n\n## Try\n- Create a note with [[My First Note]] then :obelisk-follow\n- Tag something #hello then :obelisk-tags\n"))
  (when (not (file-exists? (string-append vault-path "/Welcome.md")))
    (write-file-simple (string-append vault-path "/Welcome.md")
      "# Welcome to Obelisk\n\nThis vault was created by Obelisk.hx, not by Obsidian.\n\n- [[Daily]] - daily notes\n- Use Space o f to follow links\n")))

(obelisk-ensure-vault!)

;; --- SCAN - from trail-scan-files ---
(define *obelisk-ignore-set*
  (hashset ".git" "node_modules" "target" ".direnv" "__pycache__" ".hg" ".venv" "dist" "build" ".jj" ".obsidian"))

(define (obelisk-scan-files root)
  (define acc '())
  (define (walk dir)
    (for-each
     (lambda (p)
       (define name (file-name p))
       (unless (hashset-contains? *obelisk-ignore-set* name)
         (if (is-dir? p)
             (walk p)
             (when (string-contains? p ".md")
               (set! acc (cons p acc))))))
     (with-handler (lambda (_) '()) (read-dir dir))))
  (walk root)
  acc)

(define (vault-files)
  (obelisk-ensure-vault!)
  (obelisk-scan-files vault-path))

(define (find-in-file path needle)
  (if (not (file-exists? path)) '()
      (let ((lines (my-split-lines (read-file-simple path))))
        (let loop ((ls lines) (n 1) (out '()))
          (if (null? ls) (reverse out)
              (if (string-contains? (car ls) needle)
                  (loop (cdr ls) (+ n 1)
                        (cons (string-append "[[" (get-base-name path) "]] :" (number->string n) ": " (car ls)) out))
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

(define (extract-first-link path)
  (let ((lines (my-split-lines (read-file-simple path))))
    (let loop ((ls lines))
      (if (null? ls) #f
          (let ((link (extract-wikilink (car ls))))
            (if link link (loop (cdr ls))))))))

(define (open-link link)
  (let* ((trimmed (my-trim link))
         (with-md (if (string-contains? trimmed ".md") trimmed (string-append trimmed ".md")))
         (full-path (string-append vault-path "/" with-md)))
    (if (file-exists? full-path)
        (helix.open full-path)
        (begin
          (write-file-simple full-path (string-append "# " trimmed "\n\n"))
          (helix.open full-path)))))

;; --- commands: 0-arg commands use () , 1-arg commands use (. args) ---

;;@doc Open vault
(define (obelisk-open)
  (obelisk-ensure-vault!)
  (helix.open vault-path))

;;@doc Daily
(define (obelisk-daily)
  (obelisk-ensure-vault!)
  (let ((daily (string-append vault-path "/Daily.md")))
    (when (not (file-exists? daily)) (write-file-simple daily "# Daily Notes\n\n"))
    (helix.open daily)))

;;@doc Follow [[wikilink]]
(define (obelisk-follow)
  (obelisk-ensure-vault!)
  (let ((path (get-current-file-path)))
    (if (not path)
        (helix.open vault-path)
        (let ((link (extract-first-link path)))
          (if (not link)
              (helix.open vault-path)
              (open-link link))))))

;;@doc New note - FIXED v1.3.2: variadic, :obelisk-new works with 0 args
(define (obelisk-new . args)
  (obelisk-ensure-vault!)
  (let* ((name (if (null? args) "Untitled" (car args)))
         (trimmed (my-trim name))
         (with-md (if (string-contains? trimmed ".md") trimmed (string-append trimmed ".md")))
         (full (string-append vault-path "/" with-md)))
    (when (not (file-exists? full)) (write-file-simple full (string-append "# " trimmed "\n\n")))
    (helix.open full)))

;;@doc Search vault - FIXED v1.3.2: variadic
(define (obelisk-search . args)
  (let* ((query (if (null? args) "" (car args)))
         (results (if (equal? query "")
                      (map (lambda (p) (string-append "[[" (get-base-name p) "]]")) (vault-files))
                      (find-in-vault query))))
    (if (null? results)
        (helix.open vault-path)
        (let ((p (write-results-file "search" (cons (string-append "Search: " query) results))))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Tags
(define (obelisk-tags)
  (let ((results (find-in-vault "#")))
    (if (null? results)
        (helix.open vault-path)
        (let ((p (write-results-file "tags" results)))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Backlinks
(define (obelisk-backlinks)
  (let* ((fname (get-current-file-name))
         (base (if (string-contains? fname ".md") (substring fname 0 (- (string-length fname) 3)) fname))
         (needle (string-append "[[" base))
         (results (find-in-vault needle)))
    (if (null? results)
        (let ((p (write-results-file "backlinks" (list (string-append "No backlinks for [[" base "]]")))))
          (helix.vsplit)
          (helix.open p))
        (let ((p (write-results-file "backlinks" (cons (string-append "Backlinks for [[" base "]]") results))))
          (helix.vsplit)
          (helix.open p)))))

;;@doc Set vault - FIXED v1.3.2: variadic
(define (obelisk-set-vault . args)
  (when (not (null? args))
    (set! vault-path (car args)))
  (obelisk-ensure-vault!)
  (helix.open vault-path))

;;@doc Info
(define (obelisk-info)
  (obelisk-ensure-vault!)
  (helix.open vault-path))

(provide obelisk-open obelisk-daily obelisk-follow obelisk-tags obelisk-backlinks obelisk-search obelisk-new obelisk-set-vault obelisk-info)

(define obsidian-open obelisk-open)
(define obsidian-daily obelisk-daily)
(define obsidian-follow obelisk-follow)
(define obsidian-tags obelisk-tags)
(define obsidian-search obelisk-search)
(define obsidian-new obelisk-new)
(provide obsidian-open obsidian-daily obsidian-follow obsidian-tags obsidian-search obsidian-new)
