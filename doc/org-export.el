;;; org-export.el --- Org texinfo customizations

;;; Commentary:
;; Stuff to make org and texinfo work better together.

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

(require 'cl-lib)
(require 'ox)
(require 'ox-md)
(require 'ox-texinfo)
(require 'ox-latex)
(require 'ox-koma-letter nil t)

(setq  org-export-allow-bind-keywords t  ; allows the use og #+BIND in org files
       org-export-date-timestamp-format "%Y-%m-%d"
       org-footnote-auto-label t    ; generate numbered footnotes like [fn:1]
       )


;;;; TEXINFO
;;;

;;; use strike-through to markup vars in texinfo
(add-to-list 'org-texinfo-text-markup-alist '(strike-through . "@var{%s}"))

(defun org-texinfo-ref-open (path)
  "Open the reference.

PATH is the reference headline."
  (let ((headline (org-find-exact-headline-in-buffer path (current-buffer) t)))
    (if headline
        (goto-char headline)
      ;; try to find anchor
      (let ((anchor (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (search-forward (format "@anchor{%s}" path) nil t)
                        (when (match-string 0)
                          (point))))))
        (when anchor
          (goto-char anchor))))))

(defun org-texinfo-reference-export (ref-type path description backend)
  "Format the texinfo reference.
Argument REF-TYPE The reference type.
Argument PATH Path to reference.
Argument DESCRIPTION Reference Description.
Argument BACKEND Org export backend."
  (when (eql backend 'texinfo)
    (format "@%s{%s}"
            ref-type
            (if description
                (format "%s,,%s" path description)
              path))))

(defun org-texinfo-xref-export (path desc backend)
  "Export xref with PATH DESC for BACKEND."
  (org-texinfo-reference-export "xref" path desc backend))

(org-link-set-parameters "texixref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-xref-export)

(defun org-texinfo-ref-export (path desc backend)
  "Export ref with PATH DESC for BACKEND."
  (org-texinfo-reference-export "ref" path desc backend))

(org-link-set-parameters "texiref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-ref-export)

(defun org-texinfo-pxref-export (path desc backend)
  "Export pxref with PATH DESC for BACKEND."
  (org-texinfo-reference-export "pxref" path desc backend))

(org-link-set-parameters "texipxref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-pxref-export)

;;;; LATEX
;;;

;;; extend latex "log" files
(setq org-latex-logfiles-extensions
      '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "glg" "glo" "gls" "idx" "ist"
        "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl" "ilg" "ind"
        "lof" "lot" "lol" "xwm"))

;;; misc.
(defun toggle-org-latex-hyperref-colorlinks (&optional force-colorlinks)
  "Toggel colorlinks=true in LaTeX hyperref setup.

This is great for printing the document in grayscale.

With prefix argumnet or if FORCE-COLORLINKS is non-nil set
hypersetup to include colorlinks=true."
  (interactive "P")
  (let ((prefix "\\hypersetup{\n pdftitle={%t},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=")
        (suffix "}\n")
        (colorlinksp (string-match "colorlinks=true" org-latex-hyperref-template)))
    (setq org-latex-hyperref-template
          (concat prefix
                  (if (or force-colorlinks
                          (not colorlinksp))
                      "true"
                    "false")
                  suffix))))

;;; latex settings
(setq org-latex-prefer-user-labels t
      org-latex-hyperref-template (toggle-org-latex-hyperref-colorlinks t)
      org-latex-compiler "xelatex"
      org-latex-pdf-process '("%latex -interaction nonstopmode %f"
                              "%bib %b"
                              "makeindex %b"
                              "PATH=\"/usr/bin:$PATH\" makeglossaries %b"  ; use system perl for makeglossaries
                              "%latex -interaction nonstopmode %f"
                              "%latex -interaction nonstopmode %f"))

;;; org export filters
(defvar org-export-latex-add-link-footnotes nil
  "If non-nil links will be added as footnotes if exported to latex.")

(defun org-export-latex-link-footnote (text backend info)
  "Create a footnote in latex for each link.

So when printed the information isn't lost.
Argument TEXT is the link's text.
Argument BACKEND is the use export backend.
Argument INFO - I have no idea what this does."
  (when (and org-export-latex-add-link-footnotes
             (org-export-derived-backend-p backend 'latex)
             (string-match "\\\\href{\\(.*\\)}{\\(.*\\)}" text))
    (when (cl-some (lambda (type)
                     (string-prefix-p type (match-string 1 text)))
                   '("http" "https" "ftp" "mailto" "doi"))
      (format "%s \\footnote{\\url{%s}} " text (match-string 1 text)))))
(add-to-list 'org-export-filter-link-functions #'org-export-latex-link-footnote)

;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (shell . t)))

(provide 'org-export)

;;; org-export.el ends here
