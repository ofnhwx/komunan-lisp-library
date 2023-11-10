;;; komunan-lisp-library.el --- 自分用に便利な機能を定義

;; Copyright (C) 2021 by Yuta Fujita

;; Author: Yuta Fujita <ofnhwx@komunan.net>
;; URL: https://github.com/ofnhwx/komunan-lisp-library
;; Version: 0.6.0
;; Package-Requires: ((emacs "28") (s "1.13.1") (dash "2.19.1") (inflections "1.1") (string-inflection "1.0.16"))

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
  (require 's)
  (require 'inflections)
  (require 'string-inflection))

(defun kllib:file-path ()
  "バッファのファイルパスを取得."
  (if-let ((file-path (or buffer-file-name
                          (bound-and-true-p magit-buffer-file-name)
                          (bound-and-true-p gist-filename))))
      (f-expand file-path)))

;;;###autoload
(defun kllib:copy-buffer-name ()
  "バッファ名をコピー."
  (interactive)
  (when-let ((name (buffer-name)))
    (kill-new name)
    (message "%s" name)))

;;;###autoload
(defun kllib:copy-directory-path ()
  "ディレクトリパスをコピー."
  (interactive)
  (when-let* ((path (kllib:file-path))
              (dir (f-dirname path)))
    (kill-new dir)
    (message "%s" dir)))

;;;###autoload
(defun kllib:copy-file-path ()
  "ファイルパスをコピー."
  (interactive)
  (when-let ((path (kllib:file-path)))
    (kill-new path)
    (message "%s" path)))

;;;###autoload
(defun kllib:copy-file-name ()
  "ファイル名をコピー."
  (interactive)
  (when-let* ((path (kllib:file-path))
              (name (f-filename path)))
    (kill-new name)
    (message "%s" name)))

;;;###autoload
(defun kllib:copy-project-directory-path ()
  "ディレクトリパスをプロジェクトルートからの相対パスでコピー."
  (interactive)
  (when-let* ((root (kllib:project-root))
              (path (kllib:file-path))
              (dir  (f-dirname path)))
    (setq dir (f-relative dir root))
    (kill-new dir)
    (message "%s" dir)))

;;;###autoload
(defun kllib:copy-project-file-path ()
  "ファイルパスをプロジェクトルートからの相対パスでコピー."
  (interactive)
  (when-let* ((root (kllib:project-root))
              (path (kllib:file-path)))
    (setq path (f-relative path root))
    (kill-new path)
    (message "%s" path)))

(defun kllib:project-root (&optional path)
  "指定した PATH のプロジェクトルートを取得."
  (when-let* ((path (or path default-directory))
              (dir  (if (f-directory? path) path (f-dirname path)))
              (project (ignore-errors (project-current nil dir))))
    (project-root project)))

(defun kllib:project-name (path)
  "指定した PATH のプロジェクト名を取得."
  (and path
       (file-name-nondirectory (directory-file-name path))))

(defun kllib:convert (string &rest options)
  "STRING を OPTIONS で指定したルールで変換する."
  (cl-dolist (option options string)
    (cl-case option
      (:pluralize   (setq string (inflection-pluralize-string   string)))
      (:singularize (setq string (inflection-singularize-string string)))
      (:camel       (setq string (string-inflection-camelcase-function   string)))
      (:pascal      (setq string (string-inflection-pascal-case-function string)))
      (:kebab       (setq string (string-inflection-kebab-case-function  string)))
      (:underscore  (setq string (string-inflection-underscore-function  string)))
      (:upcase      (setq string (string-inflection-upcase-function      string)))
      (:table       (setq string (kllib:convert string :pluralize :underscore)))
      (:class       (setq string (kllib:convert string :singularize :pascal))))))
(defun kllib:convert-pluralize   (string) "STRING を複数形にする."                 (kllib:convert string :pluralize))
(defun kllib:convert-singularize (string) "STRING を単数形にする."                 (kllib:convert string :singularize))
(defun kllib:convert-camel       (string) "STRING をキャメルケースにする."         (kllib:convert string :camel))
(defun kllib:convert-pascal      (string) "STRING をパスカルケースにする."         (kllib:convert string :pascal))
(defun kllib:convert-kebab       (string) "STRING をケバブケースにする."           (kllib:convert string :kebab))
(defun kllib:convert-underscore  (string) "STRING をスネークケースにする."         (kllib:convert string :underscore))
(defun kllib:convert-upcase      (string) "STRING を大文字にする."                 (kllib:convert string :upcase))
(defun kllib:convert-table       (string) "STRING を複数形・スネークケースにする." (kllib:convert string :table))
(defun kllib:convert-class       (string) "STRING を単数形・パスカルケースにする." (kllib:convert string :class))

(defun kllib:shell-command-to-string (command)
  "COMMAND の結果を文字列で取得."
  (s-trim (shell-command-to-string command)))

(defun kllib:shell-command-to-list (command)
  "COMMAND の結果をリストで取得."
  (s-split "\n" (kllib:shell-command-to-string command)))

(defun kllib:propertize-message (format &rest args)
  "プロパティを設定したメッセージを出力する.

  FORMAT, ARGS は `message' と同様.`"
  (let ((inhibit-read-only t))
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (apply #'format format args))
      (unless (bolp)
        (insert "\n")))))

(provide 'komunan-lisp-library)

;;; komunan-lisp-library.el ends here
