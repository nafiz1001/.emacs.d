;; -*- lexical-binding: t; -*-

(setopt initial-scratch-message nil)

;; The default threshold (800 KB) made redisplay collect garbage about
;; every 20 line moves in tree-sitter buffers. 64 MB cut scroll time
;; in markdown-ts-view-mode by about 25%.
(setopt gc-cons-threshold (* 64 1024 1024))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
