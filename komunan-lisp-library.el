;;; komunan-lisp-library.el --- 自分用に便利な機能を定義

;; Copyright (C) 2021 by Yuta Fujita

;; Author: Yuta Fujita <ofnhwx@komunan.net>
;; URL: https://github.com/ofnhwx/komunan-lisp-library
;; Version: 0.5.1
;; Package-Requires: ((s "1.12.0") (dash "2.17.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'dash)
  (require 's))

;;;###autoload
(defun kllib:project-root (&optional path)
  "指定した PATH のプロジェクトルートを取得."
  (when-let (project (project-current nil (or path default-directory)))
    (car (project-roots project))))

;;;###autoload
(defun kllib:project-name (path)
  (and path
       (file-name-nondirectory (directory-file-name path))))

;;;###autoload
(defun kllib:convert (string &rest options)
  (cl-dolist (option options string)
    (case option
      (:pluralize   (setq string (inflection-pluralize-string   string)))
      (:singularize (setq string (inflection-singularize-string string)))
      (:camelize    (setq string (string-inflection-upper-camelcase-function string)))
      (:underscore  (setq string (string-inflection-underscore-function      string)))
      (:dasherize   (setq string (s-replace "_" "-" (kllib:convert string :underscore))))
      (:tableize    (setq string (kllib:convert string :pluralize :underscore)))
      (:classify    (setq string (kllib:convert string :singularize :camelize))))))

;;;###autoload
(defun kllib:shell-command-to-string (command)
  "コマンドの結果を文字列で取得."
  (s-trim (shell-command-to-string command)))

;;;###autoload
(defun kllib:shell-command-to-list (command)
  "コマンドの結果をリストで取得."
  (s-split "\n" (kllib:shell-command-to-string command)))

;;;###autoload
(defun kllib:propertize-message (format &rest args)
  "プロパティを設定したメッセージを出力する."
  (let ((inhibit-read-only t))
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (apply #'format format args))
      (unless (bolp)
        (insert "\n")))))

;;;###autoload
(defun kllib:unpropertize (text)
  "TEXT からテキストプロパティを除いた文字列を取得する."
  (let ((s (s-concat text)))
    (set-text-properties 0 (length s) nil s)
    s))

(provide 'komunan-lisp-library)

;;; komunan-lisp-library.el ends here
