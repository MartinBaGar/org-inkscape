;;; org-img.el --- A generalized image management link for Org-mode -*- lexical-binding: t; -*-
;;
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1") (org "9.3"))
;; Keywords: multimedia images org
;;
;;; Commentary:
;;
;; Quick start:
;;   (setq org-img-dir "images")
;;   (add-hook 'org-mode-hook #'org-img-mode)
;;
;; Links:  [[img:mydiagram.svg]]
;; Create: M-x org-img-create  (bound to C-c i n in org-img-mode)
;; Insert existing: C-c C-l (org-insert-link), type "img:"
;;
;; Images are displayed inline, scaled proportionally to fit the window.
;; They are never cropped. Use C-c i t to toggle, C-c i r to refresh.
;;
;; SVG files are rasterised to PNG for preview via Inkscape (if available).
;; External edits to images are automatically detected and refreshed using
;; a minimal file-watcher logic (identical to org-inkscape).
;;
;; Keybindings (org-img-mode):
;;   C-c i t  toggle inline previews
;;   C-c i r  force-refresh previews (re-exports SVG caches)
;;   C-c i n  create a new image file and insert a link
;;
;;; Code:

(require 'org)
(require 'ox)
(require 'cl-lib)
(require 'filenotify)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup org-img nil
  "Settings for the org-img package."
  :group 'org
  :prefix "org-img-")

(defcustom org-img-dir "images"
  "Default sub-directory where images are stored.
img: link paths are resolved relative to this directory, which is
itself relative to the Org file. Set to nil to resolve directly
relative to the Org file."
  :type '(choice (string :tag "Directory name")
          (const  :tag "Same as Org file" nil))
  :group 'org-img)

(defcustom org-img-editor-alist
  '(("svg"  . "inkscape")
    ("png"  . "gimp")
    ("jpg"  . "gimp")
    ("jpeg" . "gimp")
    ("kra"  . "krita"))
  "Alist mapping file extensions to external editor executables."
  :type '(alist :key-type string :value-type string)
  :group 'org-img)

(defcustom org-img-svg-preview-suffix "-preview"
  "Suffix used when generating a PNG preview of an SVG file.
E.g. diagram.svg -> diagram-preview.png."
  :type 'string
  :group 'org-img)

(defcustom org-img-png-dpi 192
  "DPI passed to Inkscape when rasterising an SVG for preview.
96 = 1x, 192 = 2x (crisp on HiDPI displays)."
  :type 'integer
  :group 'org-img)

(defcustom org-img-max-width-fraction 0.9
  "Maximum image display width as a fraction of the window body width.
0.9 means images wider than 90% of the window are scaled down
proportionally. Images are never upscaled and never cropped."
  :type 'float
  :group 'org-img)

(defcustom org-img-default-svg-template
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<svg width=\"200\" height=\"200\" version=\"1.1\"
     xmlns=\"http://www.w3.org/2000/svg\">
  <rect width=\"100%\" height=\"100%\" fill=\"#eaeaea\"/>
  <circle cx=\"100\" cy=\"100\" r=\"50\" fill=\"#4488ff\"/>
  <text x=\"100\" y=\"180\" font-family=\"sans-serif\"
        font-size=\"14\" text-anchor=\"middle\">New Image</text>
</svg>"
  "Skeleton SVG written to a newly-created file."
  :type 'string
  :group 'org-img)

;;; ============================================================
;;; Path helpers
;;; ============================================================

(defun org-img--base-dir ()
  "Return the absolute base directory for resolving img: paths."
  (let ((file-dir (file-name-directory
                   (or buffer-file-name default-directory))))
    (if org-img-dir
        (expand-file-name org-img-dir file-dir)
      file-dir)))

(defun org-img--resolve-path (path)
  "Return the absolute filesystem path for img: link PATH."
  (expand-file-name path (org-img--base-dir)))

;;; ============================================================
;;; Image sizing
;;; ============================================================

(defun org-img--max-width ()
  "Return the pixel width budget for inline images, or nil on terminal.
Computed as `org-img-max-width-fraction' * window body width in pixels.
Passing this as :max-width to `create-image' scales images DOWN to fit
proportionally. Images narrower than the budget are shown at native
size. Nothing is ever cropped."
  (when (display-graphic-p)
    (let ((win-px (window-body-width nil t)))
      (when (and win-px (> win-px 0))
        (floor (* org-img-max-width-fraction win-px))))))

;;; ============================================================
;;; SVG -> PNG preview
;;; ============================================================

(defun org-img--preview-path (abs-path)
  "Return the PNG preview path for the SVG at ABS-PATH."
  (concat (file-name-sans-extension abs-path)
          org-img-svg-preview-suffix
          ".png"))

(defun org-img--get-preview (abs-path)
  "Return a displayable image path for ABS-PATH.
SVG files are rasterised to PNG via Inkscape (regenerated when stale).
All other file types are returned unchanged."
  (let ((ext (downcase (or (file-name-extension abs-path) ""))))
    (if (string= ext "svg")
        (if (not (executable-find "inkscape"))
            (progn (message "[org-img] inkscape not found; cannot preview SVG.") nil)
          (let ((png-path (org-img--preview-path abs-path)))
            (when (or (not (file-exists-p png-path))
                      (file-newer-than-file-p abs-path png-path))
              (message "[org-img] Generating preview for %s..."
                       (file-name-nondirectory abs-path))
              (call-process-shell-command
               (format "inkscape --export-type=png --export-area-drawing \
--export-dpi=%d --export-filename=%s %s"
                       org-img-png-dpi
                       (shell-quote-argument png-path)
                       (shell-quote-argument abs-path))
               nil nil nil))
            (if (file-exists-p png-path)
                png-path
              (message "[org-img] Preview generation failed for %s." abs-path)
              nil)))
      abs-path)))

;;; ============================================================
;;; Link collection & completion
;;; ============================================================

(defun org-img--get-links ()
  "Return a list of all img: link elements in the current buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "img")
        link))))

(defun org-img--complete-link ()
  "Complete an img: link path using image files in `org-img-dir'.
Used by `org-insert-link' (C-c C-l)."
  (let* ((base-dir (org-img--base-dir)))
    (unless (file-directory-p base-dir)
      (make-directory base-dir t))
    (let* ((files (directory-files base-dir nil "^[^.]"))
           (image-files (cl-remove-if-not
                         (lambda (f)
                           (string-match-p
                            "\\.\\(svg\\|png\\|jpe?g\\|kra\\|gif\\|webp\\)$"
                            (downcase f)))
                         files))
           (file (completing-read "Image file: " image-files nil nil nil nil)))
      (concat "img:" file))))

;;; ============================================================
;;; File Watcher (The minimal org-inkscape way)
;;; ============================================================

(defvar-local org-img--watchers nil
  "Alist of (abs-path . file-notify-descriptor) for this buffer.")

(defun org-img--add-watcher (abs-path)
  "Watch ABS-PATH and auto-refresh overlays on change."
  ;; Only add if we aren't already watching it
  (unless (alist-get abs-path org-img--watchers nil nil #'string-equal)
    (when (file-exists-p abs-path)
      (push (cons abs-path
                  (file-notify-add-watch 
                   abs-path '(change)
                   ;; The moment anything happens, just update the overlays!
                   (lambda (_)
                     (message "[org-img] Refreshing %s" (file-name-nondirectory abs-path))
                     (org-img--update-overlays))))
            org-img--watchers))))

(defun org-img--remove-watchers ()
  "Cancel all file-notify watchers for this buffer."
  (dolist (entry org-img--watchers)
    (file-notify-rm-watch (cdr entry)))
  (setq org-img--watchers nil))

;;; ============================================================
;;; Overlay management
;;; ============================================================

(defun org-img--our-overlays ()
  "Return all org-img overlays in the current buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'org-img-overlay))
   (overlays-in (point-min) (point-max))))

(defun org-img--overlay-link (link)
  "Place a proportionally-scaled inline image overlay for LINK."
  (let* ((path      (org-element-property :path link))
         (full-path (org-img--resolve-path path)))
    (if (not (file-exists-p full-path))
        (message "[org-img] File not found: %s" full-path)
      (let ((preview-path (org-img--get-preview full-path)))
        (when preview-path
          (let* ((img-type  (intern (downcase (file-name-extension preview-path))))
                 (max-width (org-img--max-width))
                 (image     (apply #'create-image
                                   preview-path img-type nil
                                   (when max-width (list :max-width max-width))))
                 (begin (org-element-property :begin link))
                 (end   (save-excursion
                          (goto-char (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))
                 (ov (make-overlay begin end)))
            (overlay-put ov 'display image)
            (overlay-put ov 'face 'default)
            (overlay-put ov 'org-img-overlay t)
            (overlay-put ov 'org-image-overlay t)
            (overlay-put ov 'modification-hooks
                         (list #'org-display-inline-remove-overlay))
            (when (boundp 'org-inline-image-overlays)
              (push ov org-inline-image-overlays))
            ;; Hook in the minimal watcher!
            (org-img--add-watcher full-path)))))))

(defun org-img--remove-overlays ()
  "Remove all org-img overlays from the current buffer."
  (dolist (ov (org-img--our-overlays))
    (delete-overlay ov)))

(defun org-img--update-overlays ()
  "Remove stale overlays, clear image cache, redisplay all img: links."
  (when (display-graphic-p)
    (org-img--remove-overlays)
    (clear-image-cache)
    (dolist (link (org-img--get-links))
      (org-img--overlay-link link))))

;;; ============================================================
;;; Follow (open in external editor)
;;; ============================================================

(defun org-img-follow (path _prefix)
  "Open the image at PATH in the appropriate external editor."
  (let* ((full-path (org-img--resolve-path path))
         (ext       (downcase (or (file-name-extension full-path) "")))
         (editor    (cdr (assoc ext org-img-editor-alist))))
    (unless (file-exists-p full-path)
      (user-error "[org-img] File does not exist: %s" full-path))
    (if (and editor (executable-find editor))
        (progn
          (message "[org-img] Opening %s in %s..."
                   (file-name-nondirectory full-path) editor)
          (start-process (format "org-img-%s" editor) nil editor full-path))
      (message "[org-img] No editor mapped for .%s; using system default." ext)
      (org-open-file full-path))))

;;; ============================================================
;;; Export filter
;;; ============================================================

(defun org-img-export-filter (tree _backend _info)
  "Rewrite img: links to absolute file: links before export."
  (org-element-map tree 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "img")
        (org-element-put-property link :type "file")
        (org-element-put-property link :path
                                  (org-img--resolve-path
                                   (org-element-property :path link))))))
  tree)

(add-to-list 'org-export-filter-parse-tree-functions
             #'org-img-export-filter)

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun org-img-create (filename)
  "Create image FILENAME in `org-img-dir', insert a link, open in editor."
  (interactive "sImage filename (e.g. diagram.svg): ")
  (let* ((full-path (org-img--resolve-path filename))
         (dir       (file-name-directory full-path))
         (ext       (downcase (or (file-name-extension filename) ""))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (if (file-exists-p full-path)
        (message "[org-img] %s already exists; linking to existing file." filename)
      (with-temp-file full-path
        (when (string= ext "svg")
          (insert org-img-default-svg-template))))
    (insert (format "[[img:%s]]" filename))
    (org-img-follow filename nil)))

;;;###autoload
(defun org-img-refresh ()
  "Force-delete SVG preview caches and redisplay all img: links."
  (interactive)
  (dolist (link (org-img--get-links))
    (let* ((full-path (org-img--resolve-path
                       (org-element-property :path link)))
           (ext (downcase (or (file-name-extension full-path) ""))))
      (when (string= ext "svg")
        (let ((png (org-img--preview-path full-path)))
          (when (file-exists-p png) (delete-file png))))))
  (org-img--update-overlays)
  (message "[org-img] Previews refreshed."))

;;;###autoload
(defun org-img-toggle-display ()
  "Toggle inline preview of img: links in this buffer."
  (interactive)
  (if (org-img--our-overlays)
      (progn
        (org-img--remove-overlays)
        (message "[org-img] Previews hidden."))
    (org-img--update-overlays)
    (message "[org-img] Previews shown.")))

;;; ============================================================
;;; Link Registration & Minor mode
;;; ============================================================

(org-link-set-parameters "img"
                         :follow  #'org-img-follow
                         :complete #'org-img--complete-link
                         :face    'org-link)

(defvar org-img-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i t") #'org-img-toggle-display)
    (define-key map (kbd "C-c i r") #'org-img-refresh)
    (define-key map (kbd "C-c i n") #'org-img-create)
    map)
  "Keymap for `org-img-mode'.")

;;;###autoload
(define-minor-mode org-img-mode
  "Minor mode for generalized image link management in Org Mode."
  :lighter " img"
  :keymap org-img-mode-map
  (if org-img-mode
      (progn
        (unless (executable-find "inkscape")
          (message "[org-img] Warning: inkscape not found; SVG preview unavailable."))
        (org-img--update-overlays))
    (org-img--remove-overlays)
    (org-img--remove-watchers)))

(provide 'org-img)
;;; org-img.el ends here>
