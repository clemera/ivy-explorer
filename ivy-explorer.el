;;; ivy-explorer.el --- Dynamic file browsing grid using ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/ivy-explorer
;; Version: 0.1.3
;; Package-Requires: ((emacs "25") (ivy "0.10.0"))
;; Keywords: convenience, files, matching

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
;;
;; Provides a large more easily readable grid for file browsing using
;; `ivy'. When `avy' is installed, commands for fast avy navigation
;; are available to the user, as well. Heavily inspired by
;; LustyExplorer:
;;
;; https://www.emacswiki.org/emacs/LustyExplorer
;;
;; Known Bugs:
;;
;; When the number of candidates don't fit into the ivy-explorer
;; window, moving down along the grid can change the order of elements
;; when new candidates get displayed. This can change the column the
;; user is currently in while moving vertically down or up. Although
;; this is a bit confusing the correct candidate gets selected.
;; Patches welcome!
;;
;;; Code:


(require 'ivy)

(defgroup ivy-explorer nil
  "Dynamic file browsing grid using ivy."
  :group 'ivy
  :group 'files)

(defcustom ivy-explorer-enable-counsel-explorer t
  "If non-nil remap `find-file' to `counsel-explorer'.

This will also override remappings of function/`counsel-mode' for
`find-file' (`counsel-find-file').

This variable has to be (un)set before loading `ivy-explorer' to
take effect."
  :group 'ivy-explorer
  :type 'boolean)

(defcustom ivy-explorer-use-separator t
  "Whether to draw a line as separator.

Line is drawn between the ivy explorer window and the Echo Area."
  :group 'ivy-explorer
  :type 'boolean)

(defcustom ivy-explorer-max-columns nil
  "If given the maximal number of columns to use."
  :group 'ivy-explorer
  :type 'integer)

(defcustom ivy-explorer-max-function #'ivy-explorer-max
  "Function which should return max number of canidates."
  :group 'ivy-explorer
  :type 'function)

(defface ivy-explorer-separator
  (if (featurep 'lv)
      '((t (:inherit lv-separator)))
    '((t (:inherit border))))
  "Face used to draw line between the ivy-explorer window and the echo area.
This is only used if option `ivy-explorer-use-separator' is non-nil.
Only the background color is significant."
  :group 'ivy-explorer)


;; * Ivy explorer menu

(defvar ivy-explorer--col-n nil
  "Current columns size of grid.")

(defun ivy-explorer--get-menu-string (strings &optional cols width)
  "Given a list of STRINGS create a menu string.

The menu string will be segmented into columns. If COLS is given
use at max COLS columns (defaults to 4). Decision for number of
columns is based on WIDTH which default to frame width. Returns a
cons cell with the number of columns created as the `car' and the
menu string as `cdr'."
  (with-temp-buffer
    (let* ((length (apply 'max
                          (mapcar #'string-width strings)))
           (wwidth (or width (frame-width)))
           (columns (min (or cols 4) (/ wwidth (+ 2 length))))
           (colwidth (/ wwidth columns))
           (column 0)
           (first t)
           laststring)
      (dolist (str strings)
        (unless (equal laststring str)
          (setq laststring str)
          (let ((length (string-width str)))
            (unless first
              (if (or (< wwidth (+ (max colwidth length) column))
                      (zerop length))
                  (progn
                    (insert "\n" (if (zerop length) "\n" ""))
                    (setq column 0))
                (insert " \t")
                (set-text-properties (1- (point)) (point)
                                     `(display (space :align-to ,column)))))
            (setq first (zerop length))
            (insert str)
            (setq column (+ column
                            (* colwidth (ceiling length colwidth)))))))
      (cons columns (buffer-string)))))

;; * Ivy explorer window, adapted from lv.el

(defvar display-line-numbers)
(defvar golden-ratio-mode)

(defvar ivy-explorer--window nil
  "Holds the current ivy explorer window.")

(defvar ivy-explorer-lv-force-update nil
  "When non-nil, `ivy-explorer--lv-message' will refresh.
Even for the same string.")

(defun ivy-explorer--lv ()
  "Ensure that ivy explorer window is live and return it."
  (if (window-live-p ivy-explorer--window)
      ivy-explorer--window
    (let ((ori (selected-window))
          buf)
      (prog1 (setq ivy-explorer--window
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))))
        (if (setq buf (get-buffer " *ivy-explorer*"))
            (switch-to-buffer buf)
          (switch-to-buffer " *ivy-explorer*")
          (set-window-hscroll ivy-explorer--window 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (set-window-dedicated-p ivy-explorer--window t)
          (set-window-parameter ivy-explorer--window 'no-other-window t))
        (select-window ori)))))

(defun ivy-explorer--lv-message (str)
  "Set ivy explorer window contents to string STR."
  (let* ((n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (ivy-explorer--lv)
      (unless (and (string= (buffer-string) str)
                   (null ivy-explorer-lv-force-update))
        (delete-region (point-min) (point-max))
        (insert str)
        (when (and (window-system) ivy-explorer-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face
                       'ivy-explorer-separator 'display '(space :height (1)))
           (propertize "\n" 'face
                       'ivy-explorer-separator 'line-height t)))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun ivy-explorer--lv-delete-window ()
  "Delete ivy explorer window and kill its buffer."
  (when (window-live-p ivy-explorer--window)
    (let ((buf (window-buffer ivy-explorer--window)))
      (delete-window ivy-explorer--window)
      (kill-buffer buf))))

(defun ivy-explorer--message (message)
  "Show MESSAGE in `ivy-explorer--window'."
  (let ((ivy-explorer-lv-force-update t)
        (window-size-fixed nil))
    (ivy-explorer--lv-message message)))

;; * Minibuffer commands

;; Ivy explorer avy, adapted from ivy-avy
(defun ivy-explorer-avy (&optional action)
  "Jump to one of the current candidates using `avy'.

If called from code ACTION is the action to trigger afterwards."
  (interactive)
  (with-selected-window (ivy-explorer--lv)
    (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (let* ((avy-all-windows nil)
         (avy-keys (or (cdr (assq 'ivy-avy avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq 'ivy-avy
                                   avy-styles-alist))
                        avy-style))
         (count 0)
         (candidate
          (let ((candidates))
            (save-excursion
              (save-restriction
                (narrow-to-region
                 (window-start)
                 (window-end))
                (goto-char (point-min))
                ;; ignore the first candidate if at ./
                ;; this command is meant to be used for navigation
                ;; navigate to same folder you are in makes no sense
                (unless (looking-at "./")
                  (push (cons (point)
                              (selected-window))
                        candidates)
                  (put-text-property
                   (point) (1+ (point)) 'ivy-explorer-count count))
                (goto-char
                 (or (next-single-property-change
                      (point) 'mouse-face)
                     (point-max)))
                (while (< (point) (point-max))
                  (unless (looking-at "[[:blank:]\r\n]\\|\\'")
                    (cl-incf count)
                    (put-text-property
                     (point) (1+ (point)) 'ivy-explorer-count count)
                    (push
                     (cons (point)
                           (selected-window))
                     candidates))
                  (goto-char
                   (or (next-single-property-change
                        (point)
                        'mouse-face)
                       (point-max))))))
            (setq avy-action #'identity)
            (avy--process
             (nreverse candidates)
             (avy--style-fn avy-style)))))
    (when (number-or-marker-p candidate)
      (ivy-set-index
       (get-text-property candidate 'ivy-explorer-count))
      (run-at-time 0 nil (or action 'ivy-alt-done))))))

;; adapted from ivy-hydra


(defun ivy-explorer-avy-dispatching-done-hydra ()
  "Choose action and afterwards target using `hydra'."
  (interactive)
  (let* ((actions (ivy-state-action ivy-last))
         (estimated-len (+ 25 (length
                               (mapconcat
                                (lambda (x)
                                  (format "[%s] %s" (nth 0 x) (nth 2 x)))
                                (cdr actions) ", "))))
         (n-columns (if (> estimated-len (window-width))
                        (or (and (bound-and-true-p ivy-dispatching-done-columns)
                                 ivy-dispatching-done-columns)
                            2)
                      nil)))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (ivy-explorer-avy 'ignore)
      (funcall
       (eval
        `(defhydra ivy-read-action (:color teal :columns ,n-columns)
           "action"
           ,@(mapcar (lambda (x)
                       (list (nth 0 x)
                             `(progn
                                (ivy-set-action ',(nth 1 x))
                                (ivy-done))
                             (nth 2 x)))
                     (cdr actions))
           ("M-i" nil "back")
           ("C-g" nil)))))))

(defun ivy-explorer-avy-dispatch ()
  "Choose target with avy and afterwards dispatch action."
  (interactive)
  (setq ivy-current-prefix-arg current-prefix-arg)
  (if (require 'hydra nil t)
      (call-interactively
       'ivy-explorer-avy-dispatching-done-hydra)
    (ivy-explorer-avy
     (lambda ()
       (let ((action (ivy-read-action)))
         (when action
           (ivy-set-action (ivy-read-action))
           (ivy-done)))))))

(defun ivy-explorer-dired ()
  "Open current directory in `dired'."
  (interactive)
  (ivy--cd ivy--directory)
  (ivy--exhibit)
  (ivy-done))

(defun ivy-explorer-next (arg)
  "Move cursor vertically down ARG candidates."
  (interactive "p")
  (if (> (minibuffer-depth) 1)
      (call-interactively 'ivy-next-line)
    (let* ((n (* arg ivy-explorer--col-n))
           (max (1- ivy--length))
           (colmax (- max (% (- max ivy--index) n))))
      (ivy-set-index
       (if (= ivy--index -1)
           0
         (min colmax
              (+ ivy--index n)))))))

(defun ivy-explorer-next-and-call (arg)
  "Move cursor down ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-explorer-next arg)
  (ivy--exhibit)
  (ivy-call))

(defun ivy-explorer-previous (arg)
  "Move cursor vertically up ARG candidates."
  (interactive "p")
  (if (> (minibuffer-depth) 1)
      (call-interactively 'ivy-previous-line)
    (let* ((n (* arg ivy-explorer--col-n))
           (colmin (% ivy--index n)))
      (ivy-set-index
       (if (and (= ivy--index 0)
                ivy-use-selectable-prompt)
           -1
         (max colmin
              (- ivy--index n)))))))

(defun ivy-explorer-previous-and-call (arg)
  "Move cursor up ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-explorer-previous arg)
  (ivy--exhibit)
  (ivy-call))

(defalias 'ivy-explorer-forward #'ivy-next-line
  "Move cursor forward ARG candidates.")

(defalias 'ivy-explorer-backward #'ivy-previous-line
  "Move cursor backward ARG candidates.")

(defalias 'ivy-explorer-forward-and-call #'ivy-next-line-and-call
  "Move cursor forward ARG candidates.
Call the permanent action if possible.")

(defalias 'ivy-explorer-backward-and-call #'ivy-previous-line-and-call
  "Move cursor backward ARG candidates.
Call the permanent action if possible.")

;; * Ivy explorer mode

(defvar ivy-explorer-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x d") 'ivy-explorer-dired)

      (define-key map (kbd "C-'") 'ivy-explorer-avy)
      (define-key map (kbd ",") 'ivy-explorer-avy)
      (define-key map (kbd ";") 'ivy-explorer-avy-dispatch)
      ;; TODO: create C-o ivy-explorer-hydra
      (define-key map (kbd "C-f") 'ivy-explorer-forward)
      (define-key map (kbd "C-b") 'ivy-explorer-backward)
      (define-key map (kbd "C-M-f") 'ivy-explorer-forward-and-call)
      (define-key map (kbd "C-M-b") 'ivy-explorer-backward-and-call)
      (define-key map (kbd "C-n") 'ivy-explorer-next)
      (define-key map (kbd "C-p") 'ivy-explorer-previous)
      (define-key map (kbd "C-M-n") 'ivy-explorer-next-and-call)
      (define-key map (kbd "C-M-p") 'ivy-explorer-previous-and-call)))
  "Keymap used in the minibuffer for function/`ivy-explorer-mode'.")

(defun ivy-explorer-max ()
  "Default for `ivy-explorer-max-function'."
  (* 2 (frame-height)))

(defun ivy-explorer--display-function (text)
  "Displays TEXT as `ivy-display-function'."
  (let* ((strings (or (split-string text "\n" t)
                      (list "")))
         (menu (ivy-explorer--get-menu-string
                strings ivy-explorer-max-columns))
         (mcols (car menu))
         (mstring (cdr menu)))
    (setq ivy-explorer--col-n mcols)
    (ivy-explorer--message mstring)))

(defun ivy-explorer--internal (f &rest args)
  "Invoke ivy explorer for F with ARGS."
  (let ((ivy-display-function  #'ivy-explorer--display-function)
        (completing-read-function 'ivy-completing-read)
        ;; max number of candidates
        (ivy-height (funcall ivy-explorer-max-function))
        (ivy-wrap nil)
        (ivy-minibuffer-map (make-composed-keymap
                             ivy-explorer-map ivy-minibuffer-map)))
    (apply f args)))


(defun ivy-explorer (&rest args)
  "Function to be used as `read-file-name-function'.

ARGS are bassed to `read-file-name-default'."
  (apply #'ivy-explorer--internal #'read-file-name-default args))


(defun counsel-explorer (&optional initial-input)
  "`counsel-find-file' version for ivy explorer.

INITIAL-INPUT is passed to `counsel-find-file'."
  (interactive)
  (apply #'ivy-explorer--internal #'counsel-find-file initial-input))

(defvar ivy-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (when ivy-explorer-enable-counsel-explorer
        (define-key map
          [remap find-file] #'counsel-explorer))))
  "Keymap for function/`ivy-explorer-mode'.")

;; from lispy.el
(defun ivy-explorer-raise ()
  "Make function/`ivy-explorer-mode' the first on `minor-mode-map-alist'."
  (let ((x (assq #'ivy-explorer-mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x
                  (delq #'ivy-explorer-mode minor-mode-map-alist))))))

(defvar ivy-explorer--default nil
  "Saves user configured `read-file-name-function'.")

;;;###autoload
(define-minor-mode ivy-explorer-mode
  "Globally enable `ivy-explorer' for file navigation.

`ivy-explorer-mode' is a global minor mode which changes
`read-file-name-function' which is used for file completion.

When `ivy-explorer-enable-counsel-explorer' (by default it is),
`find-file' and `counsel-find-file' will be remapped to
`counsel-explorer.', too.

See `ivy-explorer-map' for bindings used in the minibuffer."
  :group 'ivy-explorer
  :require 'ivy-explorer
  :global t
  :init-value nil
  :lighter " ivy-explorer"
  (if (not ivy-explorer-mode)
      (setq read-file-name-function ivy-explorer--default)
    (setq ivy-explorer--default read-file-name-function
          read-file-name-function #'ivy-explorer)
    (when ivy-explorer-enable-counsel-explorer
      ;; in case users activate counsel afterwards.
      (add-hook 'counsel-mode-hook 'ivy-explorer-raise))))

(provide 'ivy-explorer)
;;; ivy-explorer.el ends here
