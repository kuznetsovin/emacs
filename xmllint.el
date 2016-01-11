(defun xml-format ()
  "XML formating"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --encode utf-8 --format -" (buffer-name) t)
  )
)

(defun xml-xsd-validate (xsd-schema)
  "XSD validator"
  (interactive (list (ido-read-file-name "Select XSD Schema: ")))
  (setq validate-file (buffer-file-name))
  (setq xml-valid-cmd (format "xmllint --schema %s --noout %s" xsd-schema validate-file))
  (shell-command xml-valid-cmd "*XSD Validate*")
)
