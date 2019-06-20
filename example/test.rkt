#lang musiclibrary

(album
 (track #:audio (build-path "test-audio.ogg")
        #:output "test-audio"
        (title: "Test")
        (track-num: 1)))
