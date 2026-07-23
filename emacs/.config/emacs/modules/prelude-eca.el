;;; prelude-eca.el --- Emacs Prelude: Editor Code Assistant (ECA) configuration.
;;
;; Copyright © 2026 James B. Greer
;;
;; Author: James B. Greer <jbgreer@gmail.com>
;; URL: 

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for using LLM-backed AI assistants in Emacs via
;; eca.  eca is "an AI-powered pair-programming client for Emacs."
;; See https://eca.dev/ for more details
;;
;; This module installs eca-emacs including downloading the eca binary
;; on first use.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; eca: an LLM-backed AI assistant pair-programmers for Emacs. 
(use-package eca
  :ensure t
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs"
       :rev :newest))

(provide 'prelude-eca)
;;; prelude-ai.el ends here
