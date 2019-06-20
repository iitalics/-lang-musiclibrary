#lang musiclibrary

(displayln "Hello.")

(define test-audio
  (build-path "test-audio.ogg"))

(define t
  (track #:audio test-audio
         #:output "test-audio"
         (title: "Test")
         (track-num: 1)))

(album t)

(begin
  (define useless 3)

  (track #:audio test-audio
         #:output "test-audio-2"
         (title: "Test 2")
         (track-num: 2)))
