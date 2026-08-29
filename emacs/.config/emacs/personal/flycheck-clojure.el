;; -*- lexical-binding: t; -*-

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
    (require 'flycheck-clj-kondo))
