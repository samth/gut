#lang typed/racket

(provide 
 sxpath
 sxml:text
 select-single-node-text)

(require/typed
 (planet lizorkin/sxml:2:1/sxml)
 (sxpath (String (Listof (Pair Symbol String)) -> ((Listof Any) -> (Listof Any))))
 (sxml:text ((Listof Any) -> String)))

 ;; Returns a function which selects the text from 
 ;; the nodes selected by the given sxpath.
 (define-syntax select-single-node-text
   (syntax-rules ()
     ((_ path-exp ns)
      (let ((sxp (sxpath path-exp ns)))
	(lambda: ((nodelst : (Listof Any)))
	  (sxml:text (sxp nodelst)))))))

