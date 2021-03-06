;; This is a WYSIWYG emacs mode for managing files for college policy debate.
;; - TODO:
;;   - exporting to word files
;;   - cite generation etc
;;   - export / email speeches
;;     - obfuscate visible headings
;;   - block insertion from other files
;;     - tub map?
;;   - emphasis, bold, italics (last two just from original sources)
;;   - "send out" command that exports a file to dropbox and opens an
;;     email with it as an attachment

(require 'enriched)
(require 'outline)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).
From http://xahlee.blogspot.com/2011/09/emacs-lisp-function-to-trim-string.html"
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" ""
                                                      string)))

;; TODO: advice name
(defun redefine-function-for-mode (oldfn newfn mode)
  (ad-add-advice oldfn
                 `(mode-replacement nil 'enabled
                                    (advice . (lambda ()
                                                (setq ad-return-value
                                                      (if (equal mode-name ,mode)
                                                          (apply ,(symbol-function newfn)
                                                                 (ad-get-args 0))
                                                        ad-do-it)))))
                 'around
                 0))

(defmacro save-window-position (&rest body)
  `(let ((distance-from-start (count-matches "\n" (window-start) (line-end-position))))
     (let ((result (progn
                     ,@body)))
       (recenter distance-from-start)
       result)))
(defun flash-region (start end &optional timeout)
  ;; TODO: copyright?
  "Borrowed from skewer.el. Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.35) nil 'delete-overlay overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Speeches  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar debate-rounds-dir "~/rounds")
(defvar debate-name (user-login-name))

(defvar debate-active-tournament nil "Name of debate tournament used for speech creation")
(defvar debate-active-round      nil "Name of debate round used for speech creation")
(defvar debate-active-speech     nil "Name of debate round used for speech creation")

(defvar mode-line-debate '((:eval (concat "  " (or debate-active-tournament "?") "|"
                                          (or debate-active-round "?") "|"
                                          (or debate-active-speech "?")))))
(put 'mode-line-debate 'risky-local-variable t)

(defun debate-current-speech-file ()
  (concat debate-rounds-dir "/"
          (format-time-string "%Y") "/" ;; current year
          debate-active-tournament "/"
          debate-active-round "/"
          debate-active-speech "-"
          debate-active-tournament "-"
          debate-active-round "-"
          debate-name".db8"))
(defun debate-speech-buffer ()
  "Get the file visiting the current speech document. Create it if it doesn't exist."
  (and (and debate-active-tournament debate-active-round debate-active-speech)
       (let ((speechfile (debate-current-speech-file)))
         (mkdir (file-name-directory speechfile) 'parents)
         (find-file-noselect speechfile 'nowarn))))



(defun debate-change-tournament ()
  (interactive)
  (setq debate-active-tournament (read-from-minibuffer "Tournament name: "))
  (setq debate-active-round nil)
  (setq debate-active-speech nil))

(defun debate-change-round ()
  (interactive)
  (unless debate-active-tournament (debate-change-tournament))
  (let ((round (read-from-minibuffer "Round number or elim: ")))
    (setq debate-active-round ;; round
          (if (string-match "^[0-9]+$" round)
              (concat "Round " round)
            round)))
  (setq debate-active-speech nil))

(defun debate-next-speech (previous)
  (cdr (assoc-string previous '(("1AC" . "2AC")
                                ("1NC" . "2NC")
                                ("2AC" . "1AR")
                                ("2NC" . "2NR")
                                ("1NR" . "2NR")
                                ("1AR" . "2AR")
                                ("2NR" . "1NC")
                                ("2AR" . "1NC")))))

(defun debate-change-speech ()
  (interactive)
  (unless debate-active-tournament (debate-change-tournament))
  (unless debate-active-round (debate-change-round))
  (setq debate-active-speech
        (completing-read "Speech: "
                         (cond ((not debate-active-speech)
                                '("1AC" "1NC" "2AC" "2NC" "1NR" "1AR" "2NR" "2AR"))
                               ((equal (elt debate-active-speech 1) ?A)
                                '("1AC" "2AC" "1AR" "2AR"))
                               ((equal (elt debate-active-speech 1) ?N)
                                '("1NC" "2NC" "1NR" "2NR")))
                         nil
                         t nil nil
                         (debate-next-speech debate-active-speech))))



(unless (memq 'mode-line-debate mode-line-format)
  (let ((pos (memq 'mode-line-buffer-identification mode-line-format)))
    (setcdr pos (cons 'mode-line-debate (cdr pos)))))

(defun debate-add-to-speech ()
  (interactive)
  (let ((speech-buffer (debate-speech-buffer)))
    (with-current-buffer speech-buffer
      (unless (= (line-beginning-position) (point))
        (insert "\n")))
    (let ((start) (end))
      (save-excursion
        (when (not (region-active-p))
          (outline-mark-subtree))
        (setq start (region-beginning))
        (setq end (region-end)))
      (append-to-buffer speech-buffer start end)
      (flash-region start end))
    (with-current-buffer speech-buffer
      (unless (= (line-end-position) (point))
        (insert "\n"))
      (save-buffer))))

;;;; exporting
(defun debate-export-html ()
  (interactive)
  (save-match-data
    (let* ((oldbuffer (current-buffer))
           (oldfile (buffer-file-name))
           (newfile (concat (file-name-directory oldfile)
                            (file-name-base oldfile) ".html")))
      (with-temp-file newfile
        (save-excursion
          (insert-file-contents-literally oldfile nil
                                          (length debate-initial-annotation))
          (while (search-forward "\n" nil t) (replace-match "<p>\n")))
        (insert "
<head>
  <style>
  body {
    font-size: 10px;
  }
  pocket {
    font-size: 28px;
    font-weight: bold;
  }
  hat {
    font-size: 24px;
    font-weight: bold;
  }
  block {
    font-size: 20px;
    font-weight: bold;
  }
  tag {
    font-weight: bold;
    font-size: 12px;
  }
  highlight {
    background-color: #FF0;
    text-decoration: underline;
    font-weight: bold;
    font-size:11px;
  }
  underline {
    text-decoration: underline;
    font-weight: bold;
    font-size:11px;
  }
  </style>
</head>
  <body>\n")
        (end-of-buffer)
        (insert"
  </body>
</html>"))
      (message (format "Exported to %s" newfile))
      newfile)))
;;;;;;;;;;;;;;;;;;;;;;;;;   Outline Functions ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun debate-hide-sublevels ()
  (outline-back-to-heading)
  (while (not (eobp))
    (let (start (point))
      (outline-forward-same-level))))

(defun doutline-map-region (fun beg end)
  "Replacement for `outline-map-region'"
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (debate-current-heading-level) (funcall fun))
    (while (and (progn
                  (outline-next-heading)
                  (< (point) end))
                (not (eobp)))
      (funcall fun))))

(defun debate-next-preface ()
  "Replacement for `outline-next-preface'"
  (interactive)
  (debate-next-heading)
  (forward-line -1)
  (end-of-line)) 
(defun debate-next-visible-heading (&optional arg)
  ;; TODO: argument
  (interactive "p")
  (debate-near-heading 1 t))
(defun debate-previous-visible-heading (&optional arg)
  ;; TODO: argument
  (interactive "p")
  (debate-near-heading -1 t))
(defun debate-heading-hidden-p ()
  (save-excursion
    (end-of-line)
    (get-char-property (or pos (point)) 'invisible)))
(defun debate-near-heading (direction &optional invisible-ok)
  (forward-line direction)
  (while (or
          (looking-at "[\s\t]*\n")
          (not (or (eobp)
                   (and
                    (or (not (get-char-property (point) 'invisible))
                        invisible-ok)
                    (debate-current-heading-level)))))
    (forward-line direction))
  (beginning-of-line)
  (point))
(defun debate-next-heading ()
  (interactive)
  (debate-near-heading 1 t))
(defun debate-next-visible-heading (arg)
  (interactive)
  (debate-near-heading 1 t))
(defun debate-back-to-heading (&optional invisible-ok)
  (interactive)
  (if (debate-current-heading-level)
      (move-beginning-of-line 1)
    (debate-near-heading -1 invisible-ok)))
(defun debate-current-heading-level (&optional invisible-ok)
  ;; Assume we're at the heading line now
  ;; (interactive)
  (save-excursion
    ;; try not to be influenced by newlines
    (beginning-of-line) (forward-char)
    (let ((idx (position (get-text-property (point) 'category) debate-headings)))
      (if idx
          (progn
            (1+ idx))
        nil))))

(mapc (lambda (pair)
        (ad-unadvise (car pair))
        (redefine-function-for-mode (car pair)
                                    (cdr pair)
                                    "Debate")
        (ad-activate (car pair)))
      '((outline-on-heading-p . debate-current-heading-level)
        (outline-map-region . doutline-map-region)
        (outline-next-preface . debate-next-preface)
        (outline-next-heading . debate-next-heading)
        (outline-back-to-heading . debate-back-to-heading)
        (outline-end-of-heading . end-of-line)
        (outline-next-visible-heading . debate-next-visible-heading)
        (outline-previous-visible-heading . debate-previous-visible-heading)))

;;;;;;;;;;;;;;;;;;;;;;  Headings and underlining ;;;;;;;;;;;;;;;;;;;;;;
(defun debate-next-level ()
  (interactive)
  (let ((next-heading nil)
        (current-heading (save-excursion
                           (forward-line -1)
                           (debate-current-heading-level))))
    (if current-heading
        (if (= current-heading 4)
            (set-line-category 'debate-card-category)
          (debate-set-heading (1+ current-heading)))
      (debate-set-heading 4))))

(defun set-line-category (cat)
  (add-text-properties (line-beginning-position)
                       (line-end-position)
                       (list 'category cat)))

(defun debate-set-heading (level)
  (interactive "p")
  (setq level (if level (- level 1) 1))
  (setq level (if (> level (- (length debate-headings) 1))
                  (- (length debate-headings) 1)
                level))
  (message (symbol-name (nth level debate-headings)))
  (set-line-category (nth level debate-headings )))


(defun debate-increment-heading (amount)
  (interactive "p")
  (setq amount (if amount amount 1)) ;; default to +1
  (let ((current-heading (debate-current-heading-level)))
    (if current-heading
        (debate-set-heading (+ current-heading amount))
      (message "Point is not at a heading"))))

(defun debate-toggle-underline (start end)
  (interactive "r")
  (save-excursion
    (if (text-property-not-all start end 'category 'debate-underline-category)
        (add-text-properties start end '(category debate-underline-category))
      (add-text-properties start end '(category debate-card-category)))))

(defun debate-toggle-highlight (start end)
  ;; TODO: ignore non-underlined parts
  (interactive "r")
  (save-excursion
    (if (text-property-any start end 'category 'debate-underline-category)
        ;; If there is underlined (non-highlighted) text, underline
        (while (< start end)
          (when (equal (plist-get (text-properties-at start) 'category)
                       'debate-underline-category)
            (add-text-properties start (1+ start)
                                 '(category debate-highlight-category)))
          (setq start (1+ start)))
      ;; If there is no underlined text, remove all highlighting
      (while (< start end)
        (when (equal (plist-get (text-properties-at start) 'category)
                     'debate-highlight-category)
          (add-text-properties start (1+ start)
                               '(category debate-underline-category)))
        (setq start (1+ start))))))

(setplist 'debate-highlight-category
          '(face (debate-highlight-face debate-underline-face)
                 invisible nil
                 line-prefix "     "
                 wrap-prefix "     "))
(setplist 'debate-underline-category
          '(face debate-underline-face
                 invisible 'debate-2
                 line-prefix "     "
                 wrap-prefix "     "))
(setplist 'debate-card-category
          '(face debate-card-face
                 invisible 'debate-1
                 line-prefix "     "
                 wrap-prefix "     "))

(setplist 'debate-tag-category
          '(face debate-tag-face
                 line-prefix "   + "
                 wrap-prefix "     "))
(setplist 'debate-block-category
          '(face font-lock-keyword-face
                 line-prefix "  + "
                 wrap-prefix "    "))
(setplist 'debate-hat-category
          '(face font-lock-variable-name-face
                 line-prefix " + "
                 wrap-prefix "   "))
(setplist 'debate-pocket-category
          '(face font-lock-function-name-face
                 line-prefix "+ "
                 wrap-prefix "  "))
(setq debate-headings '(debate-pocket-category
                        debate-hat-category
                        debate-block-category
                        debate-tag-category))

(defface debate-card-face
  (list (cons 'default (list :foreground "dim grey")))
  "Non-underlined text for debate cards")
(defface debate-underline-face
  (list (cons 'default (list :foreground "white")))
  "Underlined text for debate cards")
(defface debate-highlight-face
  (list (cons 'default (list :background "purple4")))
  "Highlighted text for debate cards")
(defface debate-tag-face
  (list (cons 'default (list :weight 'bold
                             :foreground "light blue" )))
  "Face for tags of debate evidence")
(defface debate-block-face
  (list (cons 'default (list :weight 'bold
                             :height 150
                             :foreground "spring green")))
  "Face for blocks in debate evidence files")

(defun debate-hide-nonhighlighted ()
  (interactive)
  (save-window-position
   (add-to-invisibility-spec (cons 'debate-1 t))
   (add-to-invisibility-spec (cons 'debate-2 t))))
(defun debate-hide-nonunderlined ()
  (interactive)
  (save-window-position
   (add-to-invisibility-spec (cons 'debate-1 t))
   (remove-from-invisibility-spec (cons 'debate-2 t))))
(defun debate-show-all ()
  (interactive)
  (save-window-position
   (remove-from-invisibility-spec (cons 'debate-1 t))
   (remove-from-invisibility-spec (cons 'debate-2 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;  magic things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: compare statistically to real underlining, and to verbatim
;;   auto-underlining. seems to do pretty well (few false negatives)
;; FYI: Verbatim's algorithm is to underline the phrases (separated by
;;   punctuation) that have the most synonyms of words in the tag. I
;;   like ots better because I like underlining before writing tags.
(defun debate-auto-underline (start end)
  (interactive "r")
  (let ((plain-text (buffer-substring-no-properties start end))
        (parsed-html nil)
        (underlined-text nil))
    (delete-region start end)

    ;; Write file from a temp buffer so it loses the db8 format
    (with-temp-buffer
      (insert plain-text)
      (write-file "debate_tmp.txt"))
    (with-temp-buffer
      (shell-command "ots -h -r 70 debate_tmp.txt" (buffer-name (current-buffer)))
      (setq parsed-html (libxml-parse-html-region (point-min) (point-max))))
    (delete-file "debate_tmp.txt")
    
    ;; Brittle code. This side up. Fragile.
    ;; Better ways to do this:
    ;; - Elegant way: elisp library to wrap around libots.c. No
    ;;   need to do html parsing then.
    ;; - Easy way: use use shr-external-rendering-functions to parse
    ;;   html
    (cl-loop for elem in (nth 3 parsed-html) do
             (when (and (listp elem) (equal (first elem) 'font))
               (let ((span (nth 2 elem))
                     (do-underline nil)
                     (text nil))
                 (setq do-underline (nth 1 span))
                 
                 ;; watch out for (br . nil) elements in text
                 (setq text (apply 'concat (mapcar (lambda (x)
                                                     (if (listp x) "\n" x))
                                                   (subseq span 2))))
                 (if do-underline
                     (setq text (propertize text 'category 'debate-underline-category))
                   (setq text (propertize text 'category 'debate-card-category)))
                 (insert text))))))

(defsubst next-nonempty-line (direction)
  (save-match-data
    (forward-line direction)
    (while (looking-at "[\s\t\n]*$")
      (forward-line direction))
    (point)))


(defun debate-expand-cite ()
  ;; TODO: format cite, card text
  ;; TODO: stop shr from filling paragraphs, or reverse it <- should work now, untested <- was working, then stopped???
  ;; TODO: unescape nonn-ascii characters
  ;; TODO: sometimes gives html instead of text
  "Assume we are at the tag line.
better regexp: http://tinyurl.com/529pqm
"
  (interactive)
  (save-excursion
    (debate-set-heading 4)
    (next-nonempty-line 1)
    (let* ((url (save-excursion
                  (search-forward-regexp "https?://[^]\n\t) ]*"
                                         (line-end-position)
                                         'noerror)
                  (match-string-no-properties 0)))
           ;; TODO: allow line breaks
           (text-bounds (split-string (progn
                                        (next-nonempty-line 1)
                                        (buffer-substring (line-beginning-position)
                                                          (line-end-position)))
                                      "\\.\\.\\."))
           ;; TODO: strip space, check for formatting of bounds
           (text-begin-content (trim-string (nth 0 text-bounds)))
           (text-end-content (trim-string (nth 1 text-bounds)))
           (text-begin nil)
           (text-end nil)
           (text-full-content nil))
      ;; doesn't work: need to set shr-find-fill-point to (lambda () (point)) temporarily
      ;; TODO: async. need to keep track of position in buffer with cite
      ;; or something.
      (with-current-buffer (url-retrieve-synchronously url)
        (beginning-of-buffer)
        ;; Prevent shr from removing lines
        (cl-flet ((shr-find-fill-point () (point)))
          (shr-insert-document
           ;; TODO pdfs
           ;;   - http://www.emacswiki.org/emacs/UnPdf
           ;;   - python http://stackoverflow.com/questions/5725278/python-help-using-pdfminer-as-a-library/20905381#20905381
           (libxml-parse-html-region (point-min)
                                     (point-max)))
          (delete-region (point) (point-max)))
        (beginning-of-buffer)
        (search-forward text-begin-content nil 'noerror)
        (setq text-begin (match-beginning 0))
        (search-forward text-end-content nil 'noerror)
        (setq text-end (match-end 0))
        (setq text-full-content (buffer-substring-no-properties
                                 text-begin
                                 text-end))
        (kill-this-buffer))
      (delete-region (line-beginning-position) (line-end-position))
      (insert text-full-content))))

(defun debate-cut-card (tag text)
  "Intended to be called by external programs"
  (message "cutting a card")
  (setq tag (trim-string tag))
  (setq text (trim-string text))
  (let ((start (point)))
    (insert tag)
    (add-text-properties start (point) '(category debate-tag-category))
    (newline))
  (let ((start (point)))
    (insert text)
    (when debate-always-auto-underline (debate-auto-underline start (point)))
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  File Format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst debate-translations
  '((category (debate-highlight-category "highlight")
              (debate-underline-category "underline")
              (debate-card-category      "card")
              
              (debate-pocket-category    "pocket")
              (debate-hat-category       "hat")
              (debate-block-category     "block")
              (debate-tag-category       "tag"))))
(defvar debate-initial-annotation "Content-type: text/debate\n")
(defun debate-encode (from to buffer)
  (message "encoding")
  (format-replace-strings '(("<" . "<<")))
  (insert debate-initial-annotation)
  (format-insert-annotations
   (format-annotate-region from (point-max) debate-translations
                           'enriched-make-annotation nil)))
(defun debate-decode (from to)
  (message "decoding")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (enriched-remove-header)
      (format-deannotate-region from (point-max) debate-translations
                                'enriched-next-annotation))
    (point-max)))
(setq format-alist (subseq format-alist 0 9))
;; TODO: use add-to-list instead of setq .. append. see init.el
(setq format-alist (append (subseq format-alist 0 9)
                           '((debate
                              "Enriched text-like format for saving debate evidence"
                              "Content-[Tt]ype:[ \t]*text/debate"
                              debate-decode
                              debate-encode
                              t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  The mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar debate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'show-all)
    (define-key map (kbd "C-c C-c") 'outline-toggle-children)
    (define-key map (kbd "C-c c") 'show-subtree)
    (define-key map (kbd "C-c d") 'debate-add-to-speech)
    (define-key map (kbd "M-p") 'debate-set-heading)
    (define-key map (kbd "M-n") 'hide-sublevels)
    (define-key map (kbd "C-c C-u") 'debate-toggle-underline) 
    (define-key map (kbd "C-c C-h") 'debate-toggle-highlight)
    map)
  "Keymap for `debate-mode'")
(define-derived-mode debate-mode text-mode "Debate"
  "Major mode for managing debate evidence and speeches
\\{debate-mode-map}"
  (visual-line-mode)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'debate-next-level)
  (make-local-variable 'yank-excluded-properties)
  ;; TODO exclude face from yank
  (setq yank-excluded-properties
        (delete 'category yank-excluded-properties))
  (unless (memq 'debate buffer-file-format)
    (add-to-list 'buffer-file-format 'debate))
  (font-lock-mode nil)

  ;; outline functions and invisibility
  ;; seems to be ignoring the string value here
  (setq buffer-invisibility-spec '((outline . "...\n")))
  (set (make-local-variable outline-level) 'debate-current-heading-level))

(add-to-list 'auto-mode-alist '(".db8$" . debate-mode))
