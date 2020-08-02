;;; tweak-files.lisp -- tweak mac file creators
;;;
;;;


;;; When the Unix tar distribution is unpacked on the mac, all the files
;;; end up with a random creator.  This will tweak the creator on all of the
;;; files of type :TEXT to either :CCL2 or :YHS2, as appropriate.

(dolist (f (directory "Macintosh HD:haskell:**:*"))
  (let ((type  (pathname-type f)))
    (when (eq (ccl:mac-file-type f) :text)
      (cond ((or (equal type "lisp") (equal type "scm"))
             (ccl:set-mac-file-creator f :ccl2))
            ((or (equal type "hs") (equal type "hi") (equal type "hu"))
             (ccl:set-mac-file-creator f :yhs2))
            (t
             (ccl:set-mac-file-creator f :|ttxt|))
            ))))
 
