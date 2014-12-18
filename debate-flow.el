;; An emacs mode for writing spreadsheets for flowing (taking notes on) debate rounds
;; TODO fix name conflicts / inconsistencies with debate.el

;; - TODO:
;;   - search forward for flow name in getflows and reordering should
;;     match the first page, which isn't preceded by a break
;;   - order-flows mode -- rebind keys for easy reordering, auto exit etc
;;     - also keybindings for it
;;   - pop point
;;   - better styles for table cells
;;     - smart highlighting -- highlight a sentence will automatically
;;       ignore unimportant modifiers, ignore first names of people,
;;       highlight only first letters of acronyms, etc
;;   - integrate speech + flow
;;;;;;;;;; define functions / commands ;;;;;;;;;;
;; utility functions
(defun db8-get-flow-name ()
  (save-excursion
    (beginning-of-buffer)
    (move-end-of-line nil) ;; remove trailing whitespace 
    (delete-horizontal-space nil)
    (setq result ) ;; has trailing return
    (nth 0 (split-string (thing-at-point 'line) "\n"))))
(defun db8-current-speech ()
  (save-excursion (let ((position (table--get-coordinate)))
    (table-goto-top-left-corner)
    (forward-char (car position))
    (next-line)
    (*table--cell-move-beginning-of-line)
    (thing-at(narrow-to-page 1)w-point 'word))))
(defun db8-create-flow (name speech-array)
  "Create a flow"
  (end-of-buffer nil)
  ;; insert new page if there's already stuff in the buffer that isn't
  ;; whitespace
  (delete-blank-lines)
  (unless (= (buffer-size) 0)
    (insert "\n"))
  (insert (concat name "\n"))
  (table-insert (length speech-array) 2 speech-width)
  (dotimes (i (length speech-array))
    (setq speech-name (aref speech-array i))
    (table--cell-insert-char speech-name)
    (table-forward-cell))
  (narrow-to-page))
(defun debate--divide ()
  (table-split-cell-vertically)
  (setq above-cell (table--get-coordinate))
  (setcdr above-cell (- (cdr above-cell) 1))
  (table--goto-coordinate above-cell)
  
  (progn
    (setq fardown (cdr (table--get-coordinate)))
    (search-forward (char-to-string table-cell-intersection-char))
    (unless (looking-at "\\s-*$")
      (debate--divide))
    (forward-char)))

(setq page-begin "\\(\\`\\|\n\\)")
;; interactive functions
(defun db8/create-offcase-flow (name)
  "create offcase flow"
  (interactive "Mflow: ")
  (db8-create-flow name offcase-speeches))
(defun db8/create-oncase-flow (name)
  "create offcase flow"
  (interactive "Mflow: ")
  (db8-create-flow name oncase-speeches))
(defun db8/select-flow (name)
  (interactive "Mflow name: ")
  (widen)
  (beginning-of-buffer)
  (unless (search-forward-regexp (concat page-begin name) nil t)
      (db8-create-flow name oncase-speeches))
  (narrow-to-page))
(defun db8/next-flow ()
  (interactive) (beginning-of-buffer) (narrow-to-page 1))
(defun db8/prev-flow ()
  (interactive) (beginning-of-buffer) (narrow-to-page -1))
(defun db8/new-arg ()
  (interactive)
  (let ((final-pos (table--get-coordinate))
        (orig-horizontal-chars table-cell-horizontal-chars))
    (*table--cell-newline)
    (table--update-cell 'now)
    (debate--divide)
    (setcdr final-pos (+ (cdr final-pos) 2))
    (table--goto-coordinate final-pos)
    (*table--cell-move-beginning-of-line)))
(defun db8/move-up (n)
  (interactive "p")
  (if (or (not n) (< n 0)) (setq n 1))
  (dotimes (i n)
    (*table--cell-beginning-of-buffer)
    (previous-line) (previous-line)
    (*table--cell-beginning-of-buffer)))
(defun db8/move-down (n)
  (interactive "p")
  (if (or (not n) (< n 0)) (setq n 1))
  (dotimes (i n)
    (*table--cell-end-of-buffer)
    (next-line) (next-line)
    (table--update-cell 'now)
    (*table--cell-beginning-of-buffer)
    (table--update-cell 'now)))
(defun db8/group-below () ;; TODO: abstract characters
  (interactive)
  (save-excursion
    (*table--cell-end-of-buffer)
    (next-line)
    (move-beginning-of-line nil)
    (replace-regexp "-" " " nil (line-beginning-position) (line-end-position))
    (replace-regexp "+" "|" nil (line-beginning-position) (line-end-position))
    (table-recognize)))
(defun db8/speech-top ()
  (interactive)
  (let ((position (table--get-coordinate)))
    (table-goto-top-left-corner)
    (forward-char (car position))
    (next-line)
    (db8/move-down 1)))
(defun db8/overview ()
  (interactive)
  ;; move to 
  (db8/speech-top)
  (save-excursion (table-insert-row 1))
  (db8/move-up))

  


;;;;;;;;;; functions for reorder flows dialog ;;;;;;;;;;;



;; To turn into =, maybe use (query-replace "-" "=" nil
;; (line-beginning-position) (line-end-position)). though try updating
;; the cell after switching chars first

;; table-cell-horizontal-chars    
;; table--vertical-cell-list
;; table-cell-intersection-char

(defun db8-flow-list ()
  (save-restriction
    (save-excursion
      (let ((result))
        (widen) (beginning-of-buffer)
        (while (search-forward-regexp page-begin nil t)
          (setq result (append result
                               (list (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))))
          (forward-line))
        result))))
(defcustom db8/order-flows-shortcuts
  '("a" "s" "d" "f" "j" "k" "l" ";" "q" "w" "e" "r" "u" "i" "o" "p" "t" "y" "g"
    "h" "z" "x" "c" "v" "m" "," "." "/" "b" "n" "/" "[" "]" "1" "2" "3" "4" "5"
    "6" "7" "8" "9" "0")
  "The list of characters used to reorder flows"
  :type 'list)
(setq db8-reorder-divider "~~~~~~~~~~~~ NEW ORDER ~~~~~~~~~~~~\n")
(defun db8/reorder-start ()
  (interactive)
  (setq db8-order-target (current-buffer))
  (let ((oldbuf (current-buffer))
        (flows (db8-flow-list))
        (i 0)
        (orderbuf (switch-to-buffer "*the-order*" nil t))
        (done 'nil)
        (input))
    (kill-buffer "*the-order*")
    (switch-to-buffer "*the-order*")
    (insert "~~~~~~~~~~ CURRENT ORDER ~~~~~~~~~~\n")
    (while flows
      (insert (concat
               (nth i db8/order-flows-shortcuts) " "
               (car flows)))
      (overlay-put 
       (make-overlay    (line-beginning-position)
                     (+ (line-beginning-position) 1))
       'face '(:foreground "red" :background "black"))
      (insert "\n")
      (setq flows (cdr flows))
      (setq i (+ i 1)))
    (insert db8-reorder-divider)
    (db8-reorder-with-keys)))

(defun db8-reorder-with-keys ()
  (interactive)
  (unwind-protect
      (let ((input)
            (choice)
            (done)
            (flow-name))
        (while (not done)
          (end-of-buffer)
          (search-backward db8-reorder-divider)
          (setq input (event-basic-type (read-event)))
          (if (eq input 'return)
              (keyboard-quit)
            (unless (symbolp input)
              (setq choice (char-to-string input))
              ;; re-search will return t iff a match is found
              (when (re-search-backward (concat "^" choice) nil t)
                (setq flow-name (buffer-substring (+ (point) 2)
                                                  (line-end-position)))
                (message (concat "FLOW: " flow-name "\n  " (int-to-string (point))))
                (delete-region (point) (line-end-position))
                (end-of-buffer)
                (insert (concat flow-name "\n")))))))
    (db8-reorder-finish)
    (kill-buffer "*the-order*")))
            
(defun last-page-p ()
  (save-restriction
    (narrow-to-page)
    (save-excursion
      (end-of-buffer)
      (widen)
      (eobp))))
(defun bury-page ()
  ;; If last page already, return
  (unless (last-page-p)
    (save-excursion
      (save-restriction
        (widen)
        (mark-page)
        (let ((this-page (buffer-substring (point) (mark)))
              (delete-active-region nil))
          (delete-region (point) (mark))
          (end-of-buffer)
          (newline)
          (delete-blank-lines)
          (insert (concat "" this-page))
          (end-of-buffer)
          (delete-backward-char 1))))))
  
(defun db8-reorder-finish ()
  (interactive)
  ;; strip extra returns from end
   (db8-reorder-with-keys)(save-excursion (end-of-buffer) (delete-blank-lines)
                  (beginning-of-line)
                  (when (looking-at "$") (delete-backward-char 1)))
  (beginning-of-buffer)
  (search-forward db8-reorder-divider)
  (while (and (= (forward-line) 0) (not (looking-at "$")))
    (let ((flow (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (with-current-buffer db8-order-target
        (save-restriction
          (widen)
          (save-excursion
            (beginning-of-buffer)
            (search-forward (concat page-begin flow "\n"))
            (bury-page))))))
  (message "Set the order"))

;;;;;;;;;; setup major mode ;;;;;;;;;;
(defvar debate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'db8/move-down)
    (define-key map (kbd "C-M-j") 'db8/new-arg)
    (define-key map (kbd "M-k") 'db8/move-up)
    (define-key map (kbd "C-c n") 'db8/group-below)
    (define-key map (kbd "M-p") 'db8/prev-flow)
    (define-key map (kbd "M-n") 'db8/next-flow)
    (define-key map (kbd "C-c f") 'db8/select-flow)
    (define-key map (kbd "C-c q") 'db8/create-oncase-flow)
    (define-key map (kbd "C-c w") 'db8/create-offcase-flow)
    (define-key map (kbd "C-c o") 'db8/overview)
    (define-key map (kbd "C-c d") 'table-delete-row)
    (define-key map (kbd "C-c C-r") 'db8/reorder-start)
    map))

(defcustom speech-width 20
  "width of argument cells in debate mode")
(define-derived-mode debate-mode nil "Debate"
  "Major mode for flowing debate rounds. Uses `table.el to create
a spreadsheet

\\{debate-mode-map}"
  
  ;; variables
  (set (make-local-variable 'oncase-speeches)
       ["1AC" "1NC" "2AC" "2NC" "1AR" "2NR" "2AR"])
  (set (make-local-variable 'offcase-speeches)
       ["1NC" "2AC" "2NC" "1AR" "2NR" "2AR"])
  (setq truncate-lines t))

