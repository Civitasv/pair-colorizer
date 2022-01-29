;;; rainbow-delimiters.el --- Highlight brackets according to their depth -*- lexical-binding: t -*-

;; Author: Civitasv <hscivitasv@gmail.com>
;; Homepage: https://github.com/Civitasv/rainbow-delimiters
;; Version: 0.0.1-alpha
;; Keywords: faces, convenience, lisp, tools, delimiters

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

;; Currently, this tool is in alpha state, maybe it's not perfect,
;; so I don't upload it to Melpa or elpa, but when it is, I will.
;; 
;; You can install it manually, steps:
;; - download rainbow-delimiters.el
;; - put it into your load directory
;; (add-to-list 'load-path "~/.emacs.d/xx/xx")
;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;
;;; Commentary:
;;
;; Usage:
;;
;; To toggle the mode in the current buffer:
;;   M-x rainbow-delimiters-mode
;; To start the mode automatically in `foo-mode', add the following to your init
;; file:
;;   (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
;; To start the mode automatically in most programming modes (Emacs 24 and
;; above):
;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; 
;;
;; Customization:
;; 
;; You can set your own rainbow colors use:
;;
;;   (setq rainbow-delimiters-light-colors ["#xx" "#xx"])
;;   (setq rainbow-delimiters-dark-colors ["#xx" "#xx"])
;;
;; You can also use customize-group to customize various options, including the color theme:
;;   M-x customize-group rainbow-delimiters
;;
;; You can specify custom colors by customizing following faces:
;; - Colorized faces take the form `rainbow-delimiters-colorized-depth-N-face', with N being the
;;   length of your colors vector.
;; - You can toggle emphasise feature using `rainbow-delimiters-emphasise'.
;; - Emphasised faces take the form `rainbow-delimiters-emphasise-depth-N-face', with N being the
;;   length of your colors vector.
;; - The unmatched delimiter face: `rainbow-delimiters-unmatched-face'.
;; - The mismatched delimiter face: `rainbow-delimiters-mismatched-face'.

;;; Code:

(defgroup rainbow-delimiters nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters"
                   "https://github.com/Civitasv/rainbow-delimiters")
  :group 'applications)

(defgroup rainbow-delimiters-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :group 'rainbow-delimiters
  :group 'faces
  :link '(custom-group-link "rainbow-delimiters")
  :prefix "rainbow-delimiters-")

(defvar rainbow-delimiters-light-colors
  ["#707183" "#7388d6" "#909183" "#709870" "#907373"
   "#6276ba" "#858580" "#80a880" "#887070"]
  "light colors for rainbow delimiters")

(defvar rainbow-delimiters-dark-colors
  ["#c792ea" "#f78c6c" "#c3e88d" "#89DDFF" "#bb80b3"
   "#ffcb6b" "#82aaff" "#44b9b1" "#80cbc4"]
  "dark colors for rainbow delimiters")

(defcustom rainbow-delimiters-colorize-pick-face-function
  #'rainbow-delimiters-colorize-default-pick-face
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
  :group 'rainbow-delimiters)

(defcustom rainbow-delimiters-emphasise-pick-face-function
  #'rainbow-delimiters-emphasise-default-pick-face
  "The function used to pick a face used to emphasise a delimiter of current cursor.
The function should take three arguments (DEPTH), where:
  - DEPTH is the delimiter depth; when zero or negative, it's an unmatched
    delimiter.
The function should return a value suitable to use as a value of the `face' text
property, or nil, in which case the delimiter is not highlighted.
The function should not move the point or mark or change the match data."
  :tag "Pick Emphasise face function"
  :type 'function
  :group 'rainbow-delimiters)

(defface rainbow-delimiters-base-face
  '((default (:inherit unspecified)))
  "Face inherited by all other rainbow-delimiter faces."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-base-error-face
  '((default (:inherit rainbow-delimiters-base-face))
    (t (:foreground "#88090B")))
  "Face inherited by all other rainbow-delimiter error faces."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-unmatched-face
  '((default (:inherit rainbow-delimiters-base-error-face)))
  "Face to highlight unmatched closing delimiters in."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-mismatched-face
  '((default (:inherit rainbow-delimiters-unmatched-face)))
  "Face to highlight mismatched closing delimiters in."
  :group 'rainbow-delimiters-faces)

(defvar rainbow-delimiters--last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(defvar rainbow-delimiters--last-paren
  '())

(defun do-stuff-if-moved-post-command (f)
  (unless (equal (point) rainbow-delimiters--last-post-command-position)
    (funcall f))
  (setq rainbow-delimiters--last-post-command-position (point)))

(defun rainbow-delimiters--remove-text-property (pos property value)
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

(defun rainbow-delimiters--add-text-property (pos property value)
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
         ))
    )
  )

(defun rainbow-delimiters--match (a b)
  (and a
       b
       (or
        (and (eq a 40) (eq b 41))
        (and (eq a 91) (eq b 93))
        (and (eq a 123) (eq b 125))
        )))

(defun rainbow-delimiters--analysis (p)
  "analysis point p"
  (let* ((ppss (syntax-ppss p))
         (depth (nth 0 ppss))
         (cstart (nth 1 ppss))
         (cend (if cstart
                   (save-excursion
                     (goto-char cstart)
                     (ignore-errors (forward-list))
                     (point))
                 nil)))
    (let ((matches-p (and
                      depth
                      cstart
                      cend
                      (rainbow-delimiters--match (char-after cstart) (char-after (1- cend))))))
      (if matches-p
          `(,p ,depth ,cstart ,cend)
        '()))))

(defun rainbow-delimiters--choose-delimiter (p)
  "when both current and left exist delimiter, we choose the left" 
  (save-excursion
    (let ((current (rainbow-delimiters--analysis p))
          (left (rainbow-delimiters--analysis (1- p)))
          (right (rainbow-delimiters--analysis (1+ p))))
      (cond ((and (cadr left)
               (caddr left)
               (cadddr left)
               (= (1+ (car left)) (cadddr left)))
             left)
            ((and (cadr right)
                  (caddr right)
                  (cadddr right)
                  (= (1- (car right)) (caddr right)))
             right)
            (t
             current)))))

(defun rainbow-delimiters--cancel-last-and-cache-now (&optional force)
  (defun recover (start end face)
    (progn 
      (rainbow-delimiters--remove-text-property start 'face nil)
      (rainbow-delimiters--remove-text-property (- end 1) 'face nil)
      (rainbow-delimiters--add-text-property start 'face face)
      (rainbow-delimiters--add-text-property (- end 1) 'face face)))
  
  (with-silent-modifications
    (let* ((data (rainbow-delimiters--choose-delimiter (point)))
           (depth (cadr data))
           (cstart (caddr data))
           (cend (cadddr data)))
      (let ((lopen
             (car rainbow-delimiters--last-paren))
            (lclose
             (cadr rainbow-delimiters--last-paren))
            (loface
             (caddr rainbow-delimiters--last-paren)))
        (cond ((and cstart cend (or (not lopen) (not lclose)))
               (setq rainbow-delimiters--last-paren
                     `(,cstart ,cend
                               ,(get-text-property cstart 'face))))
              ((and lopen lclose force)
               (recover lopen lclose loface)
               (setq rainbow-delimiters--last-paren
                     '()))
              ((and lopen lclose (not cstart))
               (recover lopen lclose loface)
               (setq rainbow-delimiters--last-paren
                     '()))
              ((and lopen lclose cstart (/= lopen cstart))
               (recover lopen lclose loface)
               (setq rainbow-delimiters--last-paren
                     `(,cstart ,cend
                               ,(get-text-property cstart 'face))))
              ((and lopen lclose cstart cend (/= lclose cend))
               (setcar (cdr rainbow-delimiters--last-paren)
                       cend)
               ))))))

(defun rainbow-delimiters--highlight-current-cursor-paren ()
  (with-silent-modifications
    (let* ((data (rainbow-delimiters--choose-delimiter (point)))
           (depth (cadr data))
           (start (caddr data))
           (end (cadddr data)))
      (cond ((and depth start end)
             (let ((face
                    (funcall rainbow-delimiters-emphasise-pick-face-function depth t)))
               (rainbow-delimiters--remove-text-property start 'face nil)
               (rainbow-delimiters--remove-text-property (- end 1) 'face nil)
               (rainbow-delimiters--add-text-property start 'face face)
               (rainbow-delimiters--add-text-property (- end 1) 'face face)))))))

(defun rainbow-delimiters--inside-this-parenthesis-event ()
  (do-stuff-if-moved-post-command
   (lambda ()
     (rainbow-delimiters--cancel-last-and-cache-now)
     (rainbow-delimiters--highlight-current-cursor-paren)
     )))

(defun rainbow-delimiters-max-face-count ()
  (let ((type (frame-parameter nil 'background-mode)))
    (if (equal type 'dark)
        (length rainbow-delimiters-dark-colors)
      (length rainbow-delimiters-light-colors))
    ))

(eval-when-compile
  (defmacro rainbow-delimiters--define-depth-faces ()
    (let ((faces '())
          (light-colors rainbow-delimiters-light-colors)
          (dark-colors rainbow-delimiters-dark-colors)
          )
      (dotimes (i (rainbow-delimiters-max-face-count))
        (push `(defface ,(intern (format "rainbow-delimiters-colorize-depth-%d-face" (1+ i)))
                 '((default (:inherit rainbow-delimiters-base-face))
                   (((class color) (background light)) :foreground ,(aref light-colors i))
                   (((class color) (background dark)) :foreground ,(aref dark-colors i)))
                 ,(format "Nested delimiter face, used to colorize delimiters at depth %d." (1+ i))
                 :group 'rainbow-delimiters-faces)
              faces)

        (push `(defface ,(intern (format "rainbow-delimiters-emphasise-depth-%d-face" (1+ i)))
                 '((default (:inherit rainbow-delimiters-base-face))
                   (((class color) (background light)) :foreground ,(aref light-colors i) :height 1.2 :width normal :box (:line-width 1 :color "grey75"))
                   (((class color) (background dark)) :foreground ,(aref dark-colors i) :height 1.2 :width normal :box (:line-width 1 :color "grey75")))
                 ,(format "Nested delimiter face, used to emphasise delimiters at depth %d." (1+ i))
                 :group 'rainbow-delimiters-faces
                 )
              faces)
        )
      `(progn ,@faces))))

(rainbow-delimiters--define-depth-faces)

(defcustom rainbow-delimiters-outermost-only-face-count 0
  "Number of faces to be used only for N outermost delimiter levels.

This should be smaller than length of `rainbow-delimiters-dark-colors' when it's dark background or
length of `rainbow-delimiters-light-colors' when it's light background"
  :type 'integer
  :group 'rainbow-delimiters)

(defcustom rainbow-delimiters-emphasise t 
  "Whether emphasise delimiters of current cursor"
  :type 'symbol
  :group 'rainbow-delimiters)

(defun rainbow-delimiters-colorize-default-pick-face (depth match _loc)
  "Return a face name appropriate for nesting depth DEPTH.
DEPTH and MATCH are as in `rainbow-delimiters-colorize-pick-face-function'.

The returned value is either `rainbow-delimiters-unmatched-face',
`rainbow-delimiters-mismatched-face', or one of the
`rainbow-delimiters-depth-N-face' faces, obeying
`rainbow-delimiters-max-face-count' and
`rainbow-delimiters-outermost-only-face-count'."
  (let ((max-face-count (rainbow-delimiters-max-face-count)))
    (cond
     ((<= depth 0)
      'rainbow-delimiters-unmatched-face)
     ((not match)
      'rainbow-delimiters-mismatched-face)
     (t
      (intern-soft
       (concat "rainbow-delimiters-colorize-depth-"
               (number-to-string
                (if (<= depth max-face-count)
                    ;; Our nesting depth has a face defined for it.
                    depth
                  ;; Deeper than # of defined faces; cycle back through to
                  ;; `rainbow-delimiters-outermost-only-face-count' + 1.
                  ;; Return face # that corresponds to current nesting level.
                  (+ 1 rainbow-delimiters-outermost-only-face-count
                     (mod (- depth max-face-count 1)
                          (- max-face-count
                             rainbow-delimiters-outermost-only-face-count)))))
               "-face"))))))

(defun rainbow-delimiters-emphasise-default-pick-face (depth match)
  "Return a face name appropriate for nesting depth DEPTH.
DEPTH is as in `rainbow-delimiters-emphasise-pick-face-function'.

The returned value is one of the
`rainbow-delimiters-emphasise-depth-N-face' faces, obeying
`rainbow-delimiters-max-face-count' and
`rainbow-delimiters-outermost-only-face-count'."
  (let ((max-face-count (rainbow-delimiters-max-face-count)))
    (intern-soft
     (concat "rainbow-delimiters-emphasise-depth-"
             (number-to-string
              (if (<= depth max-face-count)
                  ;; Our nesting depth has a face defined for it.
                  depth
                ;; Deeper than # of defined faces; cycle back through to
                ;; `rainbow-delimiters-outermost-only-face-count' + 1.
                ;; Return face # that corresponds to current nesting level.
                (+ 1 rainbow-delimiters-outermost-only-face-count
                   (mod (- depth max-face-count 1)
                        (- max-face-count
                           rainbow-delimiters-outermost-only-face-count)))))
             "-face"))))

(defun rainbow-delimiters--apply-color (loc depth match)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter."

  ;; if loc is in last-paren, then we use inside face to show it
  (let ((inside (and rainbow-delimiters-emphasise
                     (or
                      (and (car rainbow-delimiters--last-paren)
                           (= loc (car rainbow-delimiters--last-paren)))
                      (and (cadr rainbow-delimiters--last-paren)
                           (= (1+ loc) (cadr rainbow-delimiters--last-paren)))))))
    
    (let ((face
           (if inside
               (funcall rainbow-delimiters-emphasise-pick-face-function depth match)
             (funcall rainbow-delimiters-colorize-pick-face-function depth match loc))))
      (when face
        (if inside
            (let ((giveyouface (funcall rainbow-delimiters-colorize-pick-face-function depth match loc)))
              (when giveyouface
                (setcar (cddr rainbow-delimiters--last-paren)
                        giveyouface))))
        
        (rainbow-delimiters--add-text-property loc 'face face)))))

(defun rainbow-delimiters--char-ineligible-p (loc ppss delim-syntax-code)
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
(defun rainbow-delimiters--propertize (end)
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
             ((rainbow-delimiters--char-ineligible-p delim-pos ppss delim-syntax-code)
              nil)
             ;; opening delimiter, include [, ( 
             ((= 4 (logand #xFFFF delim-syntax-code))
              ;; The (1+ ...) is needed because `parse-partial-sexp' returns the
              ;; depth at the opening delimiter, not in the block being started.
              (rainbow-delimiters--apply-color delim-pos (1+ (nth 0 ppss)) t))
             
             ((= 5 (logand #xFFFF delim-syntax-code))
              ;; Not an opening delimiter, so it's a closing delimiter.
              ;; before color it, check if this delimiter match opening delimiter
              (let ((matches-p (eq (cdr delim-syntax) (char-after (nth 1 ppss)))))
                (rainbow-delimiters--apply-color delim-pos (nth 0 ppss) matches-p))))))))
    )

  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)

;; NB: no face defined here because we apply the faces ourselves instead of
;; leaving that to font-lock.
(defconst rainbow-delimiters--font-lock-keywords
  '(rainbow-delimiters--propertize))

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :init-value nil
  :lighter "" ; No modeline lighter - it's already obvious when the mode is on.
  :keymap nil
  (rainbow-delimiters-disable-colorize)
  (when rainbow-delimiters-emphasise
    (rainbow-delimiters-disable-emphasise))
  
  (when rainbow-delimiters-mode
    (rainbow-delimiters-enable-colorize)
    (when rainbow-delimiters-emphasise
      (rainbow-delimiters-enable-emphasise)))
  
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun rainbow-delimiters-enable-colorize ()
  (font-lock-add-keywords nil rainbow-delimiters--font-lock-keywords 'append)
  (set (make-local-variable 'jit-lock-contextually) t)
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

(defun rainbow-delimiters-disable-colorize ()
  (font-lock-remove-keywords nil rainbow-delimiters--font-lock-keywords))

(defun rainbow-delimiters-enable-emphasise ()
  (add-hook 'post-command-hook #'rainbow-delimiters--inside-this-parenthesis-event 0 t)
  (rainbow-delimiters--cancel-last-and-cache-now)
  (rainbow-delimiters--highlight-current-cursor-paren))

(defun rainbow-delimiters-disable-emphasise ()
  (make-local-variable 'post-command-hook)
  (remove-hook 'post-command-hook #'rainbow-delimiters--inside-this-parenthesis-event t)
  (rainbow-delimiters--cancel-last-and-cache-now t)

  (set (make-variable-buffer-local 'rainbow-delimiters--last-post-command-position) 0)
  (set (make-variable-buffer-local 'rainbow-delimiters--last-paren) '()))

(provide 'rainbow-delimiters)
;;; rainbow-delimiters.el ends here

