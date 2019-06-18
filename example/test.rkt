#lang musiclibrary

(define a
  (album
   (track #:audio-src (build-path "test-audio.ogg")
          #:output-path (build-path "test-audio.mp3"))))

(queue-album! a)
