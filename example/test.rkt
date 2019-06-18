#lang racket
(require
 musiclibrary)

(define a
  (album
   (track #:audio-src (build-path "test-audio.ogg")
          #:output-path (build-path "test-audio.mp3"))))

(musiclibrary
 (queue-album! a))
