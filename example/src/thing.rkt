#lang racketscript/base

(require racketscript/htdp/universe
         racketscript/htdp/image
         racketscript/interop
         racketscript/browser)

(struct world (frame))

(define (tick world-state)
  (world (+ 1 (world-frame world-state))))

(#js.console.log "making ufo")
(define ufo
  (begin
    (overlay/align "middle" "middle"
                   (rectangle 40 10 "solid" "green")
                   (circle 10 "solid" "green"))))

(#js.console.log "making background")
(define background
  (rectangle 100 100 "solid" "white"))

(define (draw world-state)
  (place-image ufo
               50
               (remainder (world-frame world-state) 100)
               (empty-scene 100 100)))

(big-bang (world 1)
          (on-tick tick)
          (to-draw draw))

