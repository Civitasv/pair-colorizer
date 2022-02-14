;;; pair-colorizer.el --- Highlight brackets according to their depth-*- lexical-binding: t -*-

;; Author: Civitasv <hscivitasv@gmail.com>
;; Homepage: https://github.com/Civitasv/pair-colorizer
;; Version: 1.0.0
;; Keywords: faces, convenience, lisp, tools, pair, colorizer

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014-2022, Fanael Linithien
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Installation:

;; You can install it use `package-install'
;;   M-x package-install <RET> pair-colorizer
;; Alternatively, you can use use-package
;;   (use-package pair-colorizer)
;;
;; You can install it manually, steps:
;; - download pair-colorizer.el
;; - put it into your load directory
;; (add-to-list 'load-path "~/.emacs.d/xx/xx")
;; (add-hook 'prog-mode-hook #'colorize-pair-mode)
;;
;;; Commentary:
;;
;; Usage:
;;
;; To toggle the mode in the current buffer:
;;   M-x colorize-pair-mode
;; To start the mode automatically in `foo-mode', add the following to your init
;; file:
;;   (add-hook 'foo-mode-hook #'colorize-pair-mode)
;; To start the mode automatically in most programming modes (Emacs 24 and
;; above):
;;   (add-hook 'prog-mode-hook #'colorize-pair-mode)
;; 
;;
;; Customization:
;; 
;; You can set your own colorizer colors use:
;;
;;   (setq pair-colorizer-light-colors ["#xx" "#xx"])
;;   (setq pair-colorizer-dark-colors ["#xx" "#xx"])
;;
;; You can also use customize-group to customize various options, including the color theme:
;;   M-x customize-group pair-colorizer
;;
;; You can specify custom colors by customizing following faces:
;; - Colorized faces take the form `pair-colorizer-depth-N-face', with N being the
;;   length of your colors vector.
;; - You can toggle emphasise feature using `pair-colorizer-emphasise'.
;; - Emphasised faces take the form `pair-colorizer-emphasise-depth-N-face', with N being the
;;   length of your colors vector.
;; - The unmatched pair face: `pair-colorizer-unmatched-face'.
;; - The mismatched pair face: `pair-colorizer-mismatched-face'.

;;; Code:

(defgroup pair-colorizer nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "pair-colorizer-"
  :link '(url-link :tag "Website for pair-colorizer"
                   "https://github.com/Civitasv/pair-colorizer")
  :group 'applications)

(defgroup pair-colorizer-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :group 'pair-colorizer
  :group 'faces
  :link '(custom-group-link "pair-colorizer")
  :prefix "pair-colorizer-")

(defvar pair-colorizer-light-colors
  ["#707183" "#7388d6" "#909183" "#709870" "#907373"
   "#6276ba" "#858580" "#80a880" "#887070"]
  "light colors used to colorize pairs")

(defvar pair-colorizer-dark-colors
  ["#c792ea" "#f78c6c" "#c3e88d" "#89DDFF" "#bb80b3"
   "#ffcb6b" "#82aaff" "#44b9b1" "#80cbc4"]
  "dark colors used to colorize pairs")

(defvar pair-colorizer-faces
  nil)

(defvar pair-colorizer-emphasise-faces
  nil)

(defcustom pair-colorizer-pick-face-function
  #'pair-colorizer-default-pick-face
  "The function used to pick a face used to highlight a delimiter.
The function should take three arguments (DEPTH MATCH LOC), where:
  - DEPTH is the delimiter depth; when zero or negative, it's an unmatched
    delimiter.
  - MATCH is nil iff the delimiter is a mismatched closing delimiter.
  - LOC is the location of the delimiter.
The function should return a value suitable to use as a value of the `face' text
property, or nil, in which case the delimiter is not highlighted.
The function should not move the point or mark or change the match data."
  :tag "Pick Colorize face function"
  :type 'function
  :group 'pair-colorizer)

(defcustom pair-colorizer-emphasise-pick-face-function
  #'pair-colorizer-emphasise-default-pick-face
  "The function used to pick a face used to emphasise a delimiter of current cursor.
The function should take three arguments (DEPTH MATCH LOC), where:
  - DEPTH is the delimiter depth; when zero or negative, it's an unmatched
    delimiter
  - MATCH is nil iff the delimiter is a mismatched closing delimiter.
  - LOC is the location of the delimiter.
The function should return a value suitable to use as a value of the `face' text
property, or nil, in which case the delimiter is not emphasised.
The function should not move the point or mark or change the match data."
  :tag "Pick Emphasise face function"
  :type 'function
  :group 'pair-colorizer)

(defface pair-colorizer-base-face
  '((default (:inherit unspecified)))
  "Face inherited by all other pair-colorizer faces."
  :group 'pair-colorizer-faces)

(defface pair-colorizer-base-error-face
  '((default (:inherit pair-colorizer-base-face))
    (t (:foreground "#88090B")))
  "Face inherited by all other pair-colorizer error faces."
  :group 'pair-colorizer-faces)

(defface pair-colorizer-unmatched-face
  '((default (:inherit pair-colorizer-base-error-face)))
  "Face to highlight unmatched closing delimiters in."
  :group 'pair-colorizer-faces)

(defface pair-colorizer-mismatched-face
  '((default (:inherit pair-colorizer-unmatched-face)))
  "Face to highlight mismatched closing delimiters in."
  :group 'pair-colorizer-faces)

(defvar pair-colorizer--last-paren
  '())

(defun do-stuff-if-in-font-lock-mode (f)
  (unless (not font-lock-mode)
    (funcall f)))

(defun pair-colorizer-remove-text-property (pos property value)
  (if (and
       (<= (1+ pos) (point-max))
       (>= pos (point-min)))
      (with-silent-modifications 
        (remove-text-properties
         pos
         (1+ pos)
         `(,property nil
                     front-sticky nil
                     rear-nonsticky nil))
        )
    )
  )

(defun pair-colorizer-add-text-property (pos property value)
  (if (and
       (<= (1+ pos) (point-max))
       (>= pos (point-min)))
      
      (with-silent-modifications
        (add-text-properties
         pos
         (1+ pos)
         `(,property ,value
                     front-sticky nil
                     rear-nonsticky t)
         )
        )
    ))

(defun pair-colorizer-match (a b)
  (and a
       b
       (or
        (and (eq a 40) (eq b 41))
        (and (eq a 91) (eq b 93))
        (and (eq a 123) (eq b 125))
        )))

(defun pair-colorizer-analysis (p)
  "analysis point p, return current point, depth, start pair point, end pair point"
  (when (and (>= p (point-min))
             (<= p (point-max)))
    (let* ((ppss (syntax-ppss p))
           (depth (nth 0 ppss))
           (cstart (nth 1 ppss))
           (cend (if cstart
                     (save-excursion
                       (goto-char cstart)
                       (ignore-errors (forward-list))
                       (1- (point)))
                   nil)))
      (let ((matches-p (and
                        depth
                        cstart
                        cend
                        (pair-colorizer-match (char-after cstart) (char-after cend)))))
        (if matches-p
            `(,p ,depth ,cstart ,cend)
          '())))))

(defun pair-colorizer-choose-delimiter (p)
  "Always choose delimiter from current cursor" 
  (save-excursion
    (let ((current (pair-colorizer-analysis p))
          (left (pair-colorizer-analysis (1- p)))
          (right (pair-colorizer-analysis (1+ p))))
      (cond ((and (cadr current)
                  (caddr current)
                  (cadddr current)
                  (or (= (car current) (caddr current))
                      (= (car current) (cadddr current))))
             current)
            ((and (cadr left)
                  (caddr left)
                  (cadddr left)
                  (or (= (car left) (caddr left))
                      (= (car left) (cadddr left))))
             left)
            ((and (cadr right)
                  (caddr right)
                  (cadddr right)
                  (= (1- (car right)) (caddr right)))
             right)
            (t
             current)))))

(defun pair-colorizer-cancel-last-and-cache-now ()
  (defun recover (start end face)
    (defun canrecover (point)
      (let ((a (char-after point)))
        (or
         (eq a 40) (eq a 41)
         (eq a 91) (eq a 93)
         (eq a 123) (eq a 125)
         )))
    (when (canrecover start)
      (pair-colorizer-remove-text-property start 'face nil)
      (pair-colorizer-add-text-property start 'face face))
    (when (canrecover end)
      (pair-colorizer-remove-text-property end 'face nil)
      (pair-colorizer-add-text-property end 'face face))) 
  
  (with-silent-modifications
    (let ((lopen
           (car pair-colorizer-last-paren))
          (lclose
           (cadr pair-colorizer-last-paren))
          (loface
           (caddr pair-colorizer-last-paren)))
      (let* ((data (pair-colorizer-choose-delimiter (point)))
             (depth (cadr data))
             (cstart (caddr data))
             (cend (cadddr data)))
        
        (cond ((and cstart cend (or (not lopen) (not lclose)))
               ;; (message "first")
               (setq pair-colorizer-last-paren
                     `(,cstart ,cend
                               ,(get-text-property cstart 'face))))
             
              ((and lopen lclose (not cstart))
               ;; (message "second")
               (recover lopen lclose loface)
               (setq pair-colorizer-last-paren
                     '()))
              ((and lopen lclose cstart (/= lopen cstart))
               ;; (message "third")
               (recover lopen lclose loface)
               (setq pair-colorizer-last-paren
                     `(,cstart ,cend
                               ,(get-text-property cstart 'face))))
              ((and lopen lclose cstart cend (/= lclose cend))
               ;; (message "fourth")
               (setcar (cdr pair-colorizer-last-paren)
                       cend)
               ))))))

(defun pair-colorizer-highlight-current-cursor-paren ()
  (with-silent-modifications
    (let* ((data (pair-colorizer-choose-delimiter (point)))
           (depth (cadr data))
           (start (caddr data))
           (end (cadddr data)))
      (cond ((and depth start end)
             (let ((face
                    (funcall pair-colorizer-emphasise-pick-face-function depth t nil)))
               (pair-colorizer-remove-text-property start 'face nil)
               (pair-colorizer-remove-text-property end 'face nil)
               (pair-colorizer-add-text-property start 'face face)
               (pair-colorizer-add-text-property end 'face face)))))))

(defun pair-colorizer-inside-this-parenthesis-event ()
  (do-stuff-if-in-font-lock-mode
   (lambda ()
     (pair-colorizer-cancel-last-and-cache-now)
     (pair-colorizer-highlight-current-cursor-paren)
     )))

(defun pair-colorizer-max-face-count ()
  (let ((type (frame-parameter nil 'background-mode)))
    (if (equal type 'dark)
        (length pair-colorizer-dark-colors)
      (length pair-colorizer-light-colors))
    ))

(defun pair-colorizer-calculate-show-depth (depth)
  (let ((max-face-count (pair-colorizer-max-face-count)))
    (if (<= depth max-face-count)
        ;; Our nesting depth has a face defined for it.
        depth
      ;; Deeper than # of defined faces; cycle back through to
      ;; `pair-colorizer-outermost-only-face-count' + 1.
      ;; Return face # that corresponds to current nesting level.
      (+ 1 pair-colorizer-outermost-only-face-count
         (mod (- depth max-face-count 1)
              (- max-face-count
                 pair-colorizer-outermost-only-face-count))))))

(defun pair-colorizer-get-face (depth)
  (let ((d (pair-colorizer-calculate-show-depth depth)))    
    (when (not (gethash d pair-colorizer-faces))
      (let ((face (eval
                   `(defface ,(intern (format "pair-colorizer-depth-%d-face" d))
                      '((default (:inherit pair-colorizer-base-face))
                        (((class color) (background light)) :foreground ,(aref pair-colorizer-light-colors (1- d)))
                        (((class color) (background dark)) :foreground ,(aref pair-colorizer-dark-colors (1- d))))
                      ,(format "Nested delimiter face, used to colorize delimiters at depth %d." d)
                      :group 'pair-colorizer-faces))))
        (puthash d face pair-colorizer-faces)))
    
    (gethash d pair-colorizer-faces)))

(defun pair-colorizer-get-emphasise-face (depth)
  (let ((d (pair-colorizer-calculate-show-depth depth)))    
    (when (not (gethash d pair-colorizer-emphasise-faces))
      (let ((face (eval
                   `(defface ,(intern (format "pair-colorizer-emphasise-depth-%d-face" d))
                      '((default (:inherit pair-colorizer-base-face))
                        (((class color) (background light)) :foreground ,(aref pair-colorizer-light-colors (1- d)) :weight bold)
                        (((class color) (background dark)) :foreground ,(aref pair-colorizer-dark-colors (1- d)) :weight bold))
                      ,(format "Nested delimiter face, used to emphasise delimiters at depth %d." d)
                      :group 'pair-colorizer-faces))))
        (puthash d face pair-colorizer-emphasise-faces)))

    (gethash d pair-colorizer-emphasise-faces)))

(defcustom pair-colorizer-outermost-only-face-count 0
  "Number of faces to be used only for N outermost delimiter levels.

This should be smaller than length of `pair-colorizer-dark-colors' when it's dark background or
length of `pair-colorizer-light-colors' when it's light background"
  :type 'integer
  :group 'pair-colorizer)

(defcustom pair-colorizer-emphasise nil
  "Whether emphasise delimiters of current cursor"
  :type 'symbol
  :group 'pair-colorizer)

(defun pair-colorizer-default-pick-face (depth match _loc)
  "Return a face name appropriate for nesting depth DEPTH.
DEPTH and MATCH are as in `pair-colorizer-pick-face-function'.

The returned value is either `pair-colorizer-unmatched-face',
`pair-colorizer-mismatched-face', or one of the
`pair-colorizer-depth-N-face' faces, obeying
`pair-colorizer-max-face-count' and
`pair-colorizer-outermost-only-face-count'."
  (let ((max-face-count (pair-colorizer-max-face-count)))
    (cond
     ((<= depth 0)
      'pair-colorizer-unmatched-face)
     ((not match)
      'pair-colorizer-mismatched-face)
     (t
      (pair-colorizer-get-face depth)))))

(defun pair-colorizer-emphasise-default-pick-face (depth _match _loc)
  "Return a face name appropriate for nesting depth DEPTH.
DEPTH is as in `pair-colorizer-emphasise-pick-face-function'.

The returned value is one of the
`pair-colorizer-emphasise-depth-N-face' faces, obeying
`pair-colorizer-max-face-count' and
`pair-colorizer-outermost-only-face-count'."
  (let ((max-face-count (pair-colorizer-max-face-count)))
    (pair-colorizer-get-emphasise-face depth)))

(defun pair-colorizer-apply-color (loc depth match)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter."

  ;; if loc is in last-paren, then we use inside face to show it
  (let ((inside (and pair-colorizer-emphasise
                     (or
                      (and (car pair-colorizer-last-paren)
                           (= loc (car pair-colorizer-last-paren)))
                      (and (cadr pair-colorizer-last-paren)
                           (= loc (cadr pair-colorizer-last-paren)))))))
    (let ((face
           (if inside
               (funcall pair-colorizer-emphasise-pick-face-function depth match loc) 
             (funcall pair-colorizer-pick-face-function depth match loc))))
      (when face
        (if inside
            (let ((giveyouface (funcall pair-colorizer-pick-face-function depth match loc)))
              (when giveyouface
                (setcar (cddr pair-colorizer-last-paren)
                        giveyouface))))
        
        (pair-colorizer-add-text-property loc 'face face)))))

(defun pair-colorizer-char-ineligible-p (loc ppss delim-syntax-code)
  "Return t if char at LOC should not be highlighted.
PPSS is the `parse-partial-sexp' state at LOC.
DELIM-SYNTAX-CODE is the `car' of a raw syntax descriptor at LOC.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (or
   (nth 3 ppss)                ; inside string?
   (nth 4 ppss)                ; inside comment?
   (nth 5 ppss)                ; escaped according to the syntax table?
   ;; Note: no need to consider single-char openers, they're already handled
   ;; by looking at ppss.
   (cond
    ;; Two character opener, LOC at the first character?
    ((/= 0 (logand #x10000 delim-syntax-code))
     (/= 0 (logand #x20000 (or (car (syntax-after (1+ loc))) 0))))
    ;; Two character opener, LOC at the second character?
    ((/= 0 (logand #x20000 delim-syntax-code))
     (/= 0 (logand #x10000 (or (car (syntax-after (1- loc))) 0))))
    (t
     nil))))

;; Main function called by font-lock.
(defun pair-colorizer-propertize (end)
  "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."
  (when (bound-and-true-p mmm-current-submode)
    ;; `mmm-mode' is weird and apparently needs this hack, because otherwise we
    ;; may end up thinking matched parentheses are mismatched.
    (widen))
  (save-excursion
    ;; (message "End `%d'..." end)
    (let* ((last-ppss-pos (point))
           (ppss (syntax-ppss)))
      (while (> end (progn (skip-syntax-forward "^()" end)
                           (point)))
        (let* ((delim-pos (point))
               (delim-syntax (syntax-after delim-pos)))

          (setq ppss (parse-partial-sexp last-ppss-pos delim-pos nil nil ppss))
          (setq last-ppss-pos delim-pos)

          ;; `skip-syntax-forward' leaves the point at the delimiter, move past
          ;; it.
          (forward-char)
          ;; (message "char `%d'..." (char-after (nth 1 ppss)))
          (let ((delim-syntax-code (car delim-syntax)))
            (cond
             ((pair-colorizer-char-ineligible-p delim-pos ppss delim-syntax-code)
              nil)
             ;; opening delimiter, include [, ( 
             ((= 4 (logand #xFFFF delim-syntax-code))
              ;; The (1+ ...) is needed because `parse-partial-sexp' returns the
              ;; depth at the opening delimiter, not in the block being started.
              (pair-colorizer-apply-color delim-pos (1+ (nth 0 ppss)) t))
             
             ((= 5 (logand #xFFFF delim-syntax-code))
              ;; Not an opening delimiter, so it's a closing delimiter.
              ;; before color it, check if this delimiter match opening delimiter
              (let ((matches-p (eq (cdr delim-syntax) (char-after (nth 1 ppss)))))
                (pair-colorizer-apply-color delim-pos (nth 0 ppss) matches-p))))))))
    )

  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)

;; NB: no face defined here because we apply the faces ourselves instead of
;; leaving that to font-lock.
(defconst pair-colorizer-font-lock-keywords
  '(pair-colorizer-propertize))

;;;###autoload
(define-minor-mode pair-colorizer-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :init-value nil
  :lighter "" ; No modeline lighter - it's already obvious when the mode is on.
  :keymap nil
  (pair-colorizer-disable-colorize)
  (when pair-colorizer-emphasise
    (pair-colorizer-disable-emphasise))
  
  (when pair-colorizer-mode
    (pair-colorizer-enable-colorize)
    (when pair-colorizer-emphasise
      (pair-colorizer-enable-emphasise)))
  
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun pair-colorizer-enable-colorize ()
  (font-lock-add-keywords nil pair-colorizer-font-lock-keywords 'append) 
  (set (make-local-variable 'jit-lock-contextually) t)
  (set 'pair-colorizer-faces (make-hash-table :size (pair-colorizer-max-face-count)))
  
  (when (or (bound-and-true-p syntax-begin-function)
            (bound-and-true-p font-lock-beginning-of-syntax-function))
    ;; We're going to modify `syntax-begin-function', so flush the cache to
    ;; avoid getting cached values that used the old value.
    (syntax-ppss-flush-cache 0))
  ;; `syntax-begin-function' may break the assumption we rely on that
  ;; `syntax-ppss' is exactly equivalent to `parse-partial-sexp' from
  ;; `point-min'. Just don't use it, the performance hit should be negligible.
  (when (boundp 'syntax-begin-function)
    (set (make-local-variable 'syntax-begin-function) nil))
  ;; Obsolete equivalent of `syntax-begin-function'.
  (when (boundp 'font-lock-beginning-of-syntax-function)
    (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil)))

(defun pair-colorizer-disable-colorize ()
  (font-lock-remove-keywords nil pair-colorizer-font-lock-keywords)
  (set 'pair-colorizer-faces nil)) 

(defun pair-colorizer-enable-emphasise ()
  (add-hook 'post-command-hook #'pair-colorizer-inside-this-parenthesis-event 0 t)
  (set 'pair-colorizer-emphasise-faces (make-hash-table :size (pair-colorizer-max-face-count))))

(defun pair-colorizer-disable-emphasise ()
  (remove-hook 'post-command-hook #'pair-colorizer-inside-this-parenthesis-event t)
  (set (make-local-variable 'pair-colorizer-last-paren) '())
  (set 'pair-colorizer-emphasise-faces nil))

(provide 'pair-colorizer)
;;; pair-colorizer.el ends here
