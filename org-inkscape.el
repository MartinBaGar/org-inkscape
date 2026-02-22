;;; org-inkscape.el --- Inkscape integration for Org Mode -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia inkscape diagrams svg

;;; Commentary:

;; Quick start:
;;   (setq org-inkscape-base-directory "~/org/inkscape/")
;;   (add-hook 'org-mode-hook #'org-inkscape-mode)
;;
;; Links:  [[inkscape:mydiagram.svg]]
;; Create: C-c i n
;;
;; Keybindings (org-inkscape-mode):
;;   C-c i o  open diagram at point in Inkscape
;;   C-c i t  toggle inline preview
;;   C-c i r  force refresh previews
;;   C-c i n  create new diagram and insert link
;;
;; Preview is always exported with --export-area-drawing (cropped to content).
;; SVG preview is a cropped copy of the source, PNG uses org-inkscape-dpi.

;;; Code:

(require 'filenotify)
(require 'org)


;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup org-inkscape nil
  "Inkscape integration for Org Mode."
  :group 'org
  :prefix "org-inkscape-")

(defcustom org-inkscape-base-directory nil
  "Central directory for .svg files.
Relative link paths are resolved from here.
If nil, paths resolve relative to the org file.
Example: \"~/org/inkscape/\""
  :type '(choice (const :tag "Relative to org file" nil)
          (directory :tag "Central directory"))
  :group 'org-inkscape)

(defcustom org-inkscape-image-type 'svg
  "Image format for exported previews.
Both formats are cropped to content via --export-area-drawing.
PNG additionally uses `org-inkscape-dpi' for resolution."
  :type '(choice (const :tag "SVG" svg)
          (const :tag "PNG" png))
  :group 'org-inkscape)

(defcustom org-inkscape-dpi 192
  "DPI for PNG export. 96 = 1x, 192 = 2x (sharp on HiDPI).
Has no effect when `org-inkscape-image-type' is svg."
  :type 'integer
  :group 'org-inkscape)

(defcustom org-inkscape-image-suffix "-AUTO"
  "Suffix for preview files. diagram.svg -> diagram-AUTO.svg or diagram-AUTO.png."
  :type 'string
  :group 'org-inkscape)

(defcustom org-inkscape-new-file-content
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\"
     width=\"800\" height=\"600\" viewBox=\"0 0 800 600\">
</svg>"
  "SVG content for newly created files."
  :type 'string
  :group 'org-inkscape)


;;; ============================================================
;;; Helpers
;;; ============================================================

(defun org-inkscape--log (msg &rest args)
  "Update org-inkscape log by appending MSG and ARGS."
  (apply #'message (concat "[org-inkscape] " msg) args))

(defun org-inkscape--resolve-path (path)
  "Resolve PATH to an absolute filesystem path.
Relative paths anchor to `org-inkscape-base-directory' if set,
otherwise to the current org file's directory."
  (expand-file-name
   path
   (or (and org-inkscape-base-directory
            (expand-file-name org-inkscape-base-directory))
       (and buffer-file-name
            (file-name-directory buffer-file-name))
       default-directory)))

(defun org-inkscape--image-path (abs-path)
  "Return the preview file path for ABS-PATH.
Always a separate -AUTO file so --export-area-drawing can crop it."
  (concat (file-name-sans-extension abs-path)
          org-inkscape-image-suffix
          "."
          (symbol-name org-inkscape-image-type)))


;;; ============================================================
;;; Export
;;; ============================================================

(defun org-inkscape--export (input)
  "Export SVG at INPUT to a cropped preview using --export-area-drawing.
Returns the preview path on success, nil on failure."
  (let* ((output (org-inkscape--image-path input))
         (cmd (concat "inkscape"
                      " --export-type=" (symbol-name org-inkscape-image-type)
                      " --export-area-drawing"
                      (when (eq org-inkscape-image-type 'png)
                        (format " --export-dpi=%s" org-inkscape-dpi))
                      " --export-filename=" (shell-quote-argument output)
                      " " (shell-quote-argument input))))
    (org-inkscape--log "Exporting: %s" (file-name-nondirectory input))
    (call-process-shell-command cmd nil nil nil)
    (if (file-exists-p output)
        output
      (org-inkscape--log "Export failed for: %s" input)
      nil)))

(defun org-inkscape--get-image (abs-path)
  "Return preview image for ABS-PATH, exporting only if source is newer."
  (let ((image-path (org-inkscape--image-path abs-path)))
    (if (and (file-exists-p image-path)
             (file-newer-than-file-p image-path abs-path))
        image-path
      (org-inkscape--export abs-path))))


;;; ============================================================
;;; Overlays
;;; ============================================================

(defun org-inkscape--get-links ()
  "Return all inkscape link elements in the current buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "inkscape")
        link))))

(defun org-inkscape--overlay-link (link)
  "Display LINK as an inline image overlay."
  (let* ((abs-path (org-inkscape--resolve-path
                    (org-element-property :path link)))
         (img (org-inkscape--get-image abs-path)))
    (if (not img)
        (org-inkscape--log "Could not get image for: %s" abs-path)
      (let* ((begin (org-element-property :begin link))
             (end   (save-excursion
                      (goto-char (org-element-property :end link))
                      (skip-chars-backward " \t")
                      (point)))
             (ov (make-overlay begin end)))
        (overlay-put ov 'display (create-image img org-inkscape-image-type nil))
        (overlay-put ov 'face 'default)
        (overlay-put ov 'org-image-overlay t)
        (overlay-put ov 'modification-hooks
                     (list #'org-display-inline-remove-overlay))
        (org-inkscape--add-watcher abs-path)))))

(defun org-inkscape--remove-overlays ()
  "Remove all inkscape overlays from the current buffer."
  (dolist (link (org-inkscape--get-links))
    (dolist (ov (overlays-in (org-element-property :begin link)
                             (org-element-property :end link)))
      (when (overlay-get ov 'org-image-overlay)
        (delete-overlay ov)))))

(defun org-inkscape--update-overlays ()
  "Refresh all inkscape overlays in the current buffer."
  (save-excursion
    (org-inkscape--remove-overlays)
    (clear-image-cache)
    (dolist (link (org-inkscape--get-links))
      (org-inkscape--overlay-link link))))


;;; ============================================================
;;; File watcher
;;; ============================================================

(defvar-local org-inkscape--watchers nil
  "Alist of (path . file-notify-descriptor) for this buffer.")

(defun org-inkscape--add-watcher (abs-path)
  "Watch ABS-PATH and auto-refresh overlays on change."
  (unless (alist-get abs-path org-inkscape--watchers nil nil #'string-equal)
    (push (cons abs-path
                (file-notify-add-watch abs-path '(change)
                                       (lambda (_) (org-inkscape--update-overlays))))
          org-inkscape--watchers)))

(defun org-inkscape--remove-watchers ()
  "Remove all file watchers for this buffer."
  (dolist (entry org-inkscape--watchers)
    (file-notify-rm-watch (cdr entry)))
  (setq org-inkscape--watchers nil))


;;; ============================================================
;;; Opening
;;; ============================================================

(defun org-inkscape-open (path)
  "Open PATH in Inkscape, creating the file if needed.
This is the :follow handler for inkscape links."
  (let ((abs-path (org-inkscape--resolve-path path)))
    (unless (file-exists-p abs-path)
      (make-directory (file-name-directory abs-path) t)
      (with-temp-file abs-path
        (insert org-inkscape-new-file-content))
      (org-inkscape--log "Created: %s" abs-path))
    (call-process-shell-command
     (concat "inkscape " (shell-quote-argument abs-path)) nil 0)
    (org-inkscape--add-watcher abs-path)))


;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun org-inkscape-open-at-point ()
  "Open the inkscape diagram linked at point."
  (interactive)
  (let ((link (org-element-context)))
    (if (and link (string-equal (org-element-property :type link) "inkscape"))
        (org-inkscape-open (org-element-property :path link))
      (org-inkscape--log "No inkscape link at point"))))

;;;###autoload
(defun org-inkscape-toggle-display ()
  "Toggle inline preview of inkscape diagrams in this buffer."
  (interactive)
  (if (cl-some (lambda (ov) (overlay-get ov 'org-image-overlay))
               (overlays-in (point-min) (point-max)))
      (progn (org-inkscape--remove-overlays)
             (org-inkscape--log "Previews hidden"))
    (org-inkscape--update-overlays)
    (org-inkscape--log "Previews shown")))

;;;###autoload
(defun org-inkscape-refresh ()
  "Force re-export and refresh all inkscape previews in this buffer."
  (interactive)
  (dolist (link (org-inkscape--get-links))
    (let ((img (org-inkscape--image-path
                (org-inkscape--resolve-path
                 (org-element-property :path link)))))
      (when (file-exists-p img) (delete-file img))))
  (org-inkscape--update-overlays)
  (org-inkscape--log "Previews refreshed"))

;;;###autoload
(defun org-inkscape-create ()
  "Create a new .svg file and insert a link at point."
  (interactive)
  (let* ((default-dir (expand-file-name
                       (or org-inkscape-base-directory
                           (and buffer-file-name
                                (file-name-directory buffer-file-name))
                           default-directory)))
         (path (expand-file-name
                (read-file-name "New inkscape file: " default-dir))))
    (unless (string-suffix-p ".svg" path)
      (setq path (concat path ".svg")))
    (make-directory (file-name-directory path) t)
    (unless (file-exists-p path)
      (with-temp-file path
        (insert org-inkscape-new-file-content)))
    (let ((link-path
           (if (and org-inkscape-base-directory
                    (string-prefix-p
                     (expand-file-name org-inkscape-base-directory) path))
               (file-name-nondirectory path)
             (file-relative-name path
                                 (file-name-directory
                                  (or buffer-file-name default-directory))))))
      (insert (format "[[inkscape:%s]]" link-path)))
    (org-inkscape-open path)))


;;; ============================================================
;;; Link setup
;;; ============================================================

(defun org-inkscape--complete-link ()
  "Complete an inkscape: link path."
  (let* ((base-dir (expand-file-name
                    (or org-inkscape-base-directory
                        (and buffer-file-name
                             (file-name-directory buffer-file-name))
                        default-directory)))
         (file (expand-file-name (read-file-name "inkscape file: " base-dir))))
    (concat "inkscape:"
            (if (and org-inkscape-base-directory
                     (string-prefix-p
                      (expand-file-name org-inkscape-base-directory) file))
                (file-name-nondirectory file)
              (file-relative-name file base-dir)))))

(defun org-inkscape--setup-link ()
  "Register the inkscape link type with Org."
  (org-link-set-parameters
   "inkscape"
   :follow   #'org-inkscape-open
   :complete #'org-inkscape--complete-link
   :export   (lambda (path _desc backend)
               (let ((img (org-inkscape--get-image
                           (org-inkscape--resolve-path path))))
                 (when img
                   (org-export-string-as
                    (format "[[file:%s]]" img) backend t))))))


;;; ============================================================
;;; Minor mode
;;; ============================================================

(defvar org-inkscape-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i o") #'org-inkscape-open-at-point)
    (define-key map (kbd "C-c i t") #'org-inkscape-toggle-display)
    (define-key map (kbd "C-c i r") #'org-inkscape-refresh)
    (define-key map (kbd "C-c i n") #'org-inkscape-create)
    map)
  "Keymap for `org-inkscape-mode'.")

;;;###autoload
(define-minor-mode org-inkscape-mode
  "Minor mode for Inkscape SVG integration in Org Mode."
  :lighter " inkscape"
  :keymap org-inkscape-mode-map
  (org-inkscape--setup-link)
  (if org-inkscape-mode
      (progn
        (unless (executable-find "inkscape")
          (org-inkscape--log "WARNING: inkscape executable not found"))
        (when org-inkscape-base-directory
          (make-directory (expand-file-name org-inkscape-base-directory) t))
        (org-inkscape--update-overlays))
    (org-inkscape--remove-watchers)
    (org-inkscape--remove-overlays)))


(provide 'org-inkscape)
;;; org-inkscape.el ends here
