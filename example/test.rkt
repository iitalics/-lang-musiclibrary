#lang musiclibrary

(define test-audio
  (fs (build-path "test-audio.ogg")))

(define test-cover
  (fs (build-path "lain.png")))

(define gecs-cover
  (net "https://pbs.twimg.com/media/D73gOzRV4AA5a5_.jpg"))

(define t
  (track #:audio test-audio
         #:output "test-audio"
         (title: "Test")
         (cover-art: test-cover)))

(album "The Test" t)

(begin
  (define useless 3)

  (track #:audio test-audio
         #:output "test-audio-2"
         (title: "Test 2")
         (track-num: 2))

  (track #:audio test-audio
         #:output "test-audio-3"
         (title: "Test 2 + cover")
         (cover-art: gecs-cover)
         (track-num: 2)))
