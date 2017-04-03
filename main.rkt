#lang racket

(require web-server/http)

(define racks-path
  (local [(define (drop-collects-subpath path)
            (define-values (root-path collects dir?)
              (split-path path))
            root-path)

          (define (compose-racks-path path)
            (build-path path "bin" "racks"))

          (define (find-existing-racks-path paths)
            (cond [(findf file-exists? paths) => identity]
                  [else
                   (error "Failed to find 'racks'; install Racketscript with 'raco pkg'")]))]

    ((compose find-existing-racks-path
              (curry map (compose compose-racks-path
                                  drop-collects-subpath)))
     (current-library-collection-paths))))

(define kill-channel (make-channel))
(define compiler-done-channel (make-channel))

(define (compile-racketscript! source targets-dir)
  (match-define (list stdout stdin pid stderr control)
    (process* (path->string racks-path)
              "-d" targets-dir
              "--target" "traceur-browser"
              (path->string source)))

  (let loop ()
    (sync (handle-evt (eof-evt stdout)
                      (λ _
                        (display "Done compiling: ")
                        (displayln (path->string source))))

          (handle-evt (read-line-evt stdout 'any)
                      (λ (str)
                        (display "Racketscript: ")
                        (displayln str)
                        (loop)))

          (handle-evt (read-line-evt stderr 'any)
                      (λ (str)
                        (display "Racketscript Error: ")
                        (displayln str)
                        (loop))))))

(define (clean-targets! targets-dir)
  (when (directory-exists? targets-dir)
    (delete-directory targets-dir)))

(define (init-gunracks sources-dir targets-dir
                       [#:on-file-change on-file-change (λ _ (void))]
                       [#:on-directory-change on-directory-change (λ _ (void))])
  (displayln "Loading gunracks...")
  (define (sources [dir sources-dir])
    (find-files (curry regexp-match? #px"^[^#]*\\.rkt$")
                dir
                #:follow-links? #f))

  (define (source-dirs)
    (find-files directory-exists?
                sources-dir
                #:follow-links? #f))

  (define (compile-all! srcs)
    (for [(source (in-list srcs))]
      (compile-racketscript! source targets-dir)))

  (compile-all! (sources))

  (define watcher
    (thread
     (λ _
       (let loop ([files (sources)]
                  [dirs  (source-dirs)])

         (displayln "Resetting vigil")

         (define (handle-directory-change dir)
           (display "Directory change detected! ")
           (displayln dir)
           (cond [(directory-exists? dir)
                  (define new-files (sources dir))
                  (compile-all! new-files)]
                 [else
                  (clean-targets! targets-dir)
                  (compile-all! (sources))])
           (on-directory-change dir)
           (loop (sources) (source-dirs)))

         (define (handle-file-change path)
           (display "File change detected! ")
           (displayln path)
           (when (file-exists? path)
             (compile-racketscript! path targets-dir))
           (on-file-change path)
           (loop (sources) dirs))

         (define file-events
           (for/list ([f (in-list files)])
             (handle-evt (filesystem-change-evt f)
                         (λ _ (handle-file-change f)))))

         (define directory-events
           (for/list ([d (in-list dirs)])
             (handle-evt (filesystem-change-evt d)
                         (λ _ (handle-directory-change d)))))

         (displayln "Listening...")
         (sync (apply choice-evt file-events)
               (apply choice-evt directory-events)
               (handle-evt kill-channel
                           (λ _
                             (displayln "Unloading gunracks...")
                             (channel-put kill-channel 'ok))))))))

  (values watcher
          (λ ()
            (channel-put kill-channel 'die)
            (sync kill-channel)
            (displayln "Gunracks finished: out-of-ammo."))))
