
;; obelisk.hx v1.0 - Obsidian for Helix
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
  (if (null? lst)
      ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc (loop (cdr rest) (string-append acc sep (car rest)))))))

(define (read-file-simple path)
  (let ((port (open-input-file path)))
    (let loop ((acc '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin (close-input-port port) (my-join-lines (reverse acc) "\n"))
            (loop (cons line acc)))))))

(define (string-contains? str sub)
  (let ((ls (string-length str)) (lsub (string-length sub)))
    (let loop ((i 0))
      (if (> (+ i lsub) ls)
          #f
          (if (string=? (substring str i (+ i lsub)) sub)
              #t
              (loop (+ i 1)))))))

(define (extract-wikilink line)
  (let ((start (let loop ((i 0))
                 (if (>= (+ i 1) (string-length line))
                     #f
                     (if (and (char=? (string-ref line i) #\[) (char=? (string-ref line (+ i 1)) #\[))
                         i
                         (loop (+ i 1)))))))
    (if (not start)
        #f
        (let ((end (let loop ((i (+ start 2)))
                     (if (>= (+ i 1) (string-length line))
                         #f
                         (if (and (char=? (string-ref line i) #\]) (char=? (string-ref line (+ i 1)) #\]))
                             i
                             (loop (+ i 1)))))))
          (if (not end)
              #f
              (let ((raw (substring line (+ start 2) end)))
                (let ((pipe-pos (let loop ((i 0))
                                  (if (>= i (string-length raw))
                                      #f
                                      (if (char=? (string-ref raw i) #\|)
                                          i
                                          (loop (+ i 1))))))
                      (hash-pos (let loop ((i 0))
                                  (if (>= i (string-length raw))
                                      #f
                                      (if (char=? (string-ref raw i) #\#)
                                          i
                                          (loop (+ i 1)))))))
                  (cond
                    (pipe-pos (substring raw 0 pipe-pos))
                    (hash-pos (substring raw 0 hash-pos))
                    (else raw)))))))))

(define (get-current-file-path)
  (let* ((focus (editor-focus)) (doc-id (editor->doc-id focus)) (path (editor-document->path doc-id)))
    path))

(define (get-current-line-content)
  (let ((path (get-current-file-path)))
    (if (not path)
        ""
        (let ((content (read-file-simple path)))
          (let ((lines (my-split-lines content)))
            (let loop ((ls lines))
              (if (null? ls)
                  ""
                  (if (string-contains? (car ls) "[[")
                      (car ls)
                      (loop (cdr ls))))))))))

;;@doc Open vault
(define (obelisk-open)
  (helix.open vault-path))

;;@doc Open daily note
(define (obelisk-daily)
  (let ((daily (string-append vault-path "/Daily.md")))
    (when (not (file-exists? daily))
      (let ((out (open-output-file daily)))
        (display "# Daily Notes\n\n" out)
        (close-output-port out)))
    (helix.open daily)))

;;@doc Follow [[wikilink]]
(define (obelisk-follow)
  (let* ((line (get-current-line-content))
         (link (extract-wikilink line)))
    (if (not link)
        (helix.open vault-path)
        (let* ((trimmed (my-trim link))
               (with-md (if (string-contains? trimmed ".md") trimmed (string-append trimmed ".md")))
               (full-path (string-append vault-path "/" with-md)))
          (if (file-exists? full-path)
              (helix.open full-path)
              (begin
                (let ((out (open-output-file full-path)))
                  (display (string-append "# " trimmed "\n\n") out)
                  (close-output-port out))
                (helix.open full-path)))))))

;;@doc List tags
(define (obelisk-tags)
  (helix.vsplit)
  (helix.open vault-path))

;;@doc Backlinks
(define (obelisk-backlinks)
  (helix.vsplit)
  (helix.open vault-path))

;;@doc Set vault path
(define (obelisk-set-vault path)
  (set! vault-path path))

;;@doc Info
(define (obelisk-info)
  (helix.open vault-path))

(provide obelisk-open obelisk-daily obelisk-follow obelisk-tags obelisk-backlinks obelisk-set-vault obelisk-info)

(define obsidian-open obelisk-open)
(define obsidian-daily obelisk-daily)
(define obsidian-follow obelisk-follow)
(define obsidian-tags obelisk-tags)
(provide obsidian-open obsidian-daily obsidian-follow obsidian-tags)
