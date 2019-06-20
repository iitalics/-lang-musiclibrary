#lang musiclibrary

(define a
  (album
   (track (build-path "test-audio.ogg")
          #:output "test-audio"
          (title: "Test")
          (track-num: 1))))

(queue-album! a)
