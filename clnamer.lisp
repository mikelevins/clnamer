;;;; clnamer.lisp

(defmethod long-enough? ((s string))
  (> (length s) 2))

(defmethod triples ((str string))
  (loop for i from 0 below (- (length str) 2)
     collect (subseq str i (+ i 3))))

(defmethod read-names ((filename string))
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil nil)
       then (read-line in nil nil nil)
       while line
       collect line)))

(defmethod mergeable? ((left string)(right string))
  (let ((left-length (length left)))
    (and (char= (elt right 0)
                (elt left (- left-length 2)))
         (char= (elt right 1)
                (elt left (- left-length 1))))))

(defmethod merge-parts ((left string)(right string))
  (concatenate 'string left (subseq right 2)))

(defun any (sequence)
  (elt sequence
       (random (length sequence))))

(defmethod find-extension ((start string)(parts list))
  (any (remove-if-not (lambda (part)(mergeable? start part))
                      parts)))

(defmethod extend-name ((start string)(parts list)(ends list))
  (let ((next (find-extension start parts)))
    (if next
        (if (find next ends :test #'equalp)
            (merge-parts start next)
            (extend-name (merge-parts start next)
                         parts ends))
        start)))

(defmethod generate-names ((count integer)(samples-file string))
  (let* ((samples (read-names samples-file))
         (name-triples (mapcar #'triples samples))
         (starts (mapcar #'first name-triples))
         (parts (remove-if #'null (reduce #'append (mapcar #'rest name-triples))))
         (ends (remove-if #'null (mapcar #'(lambda (s)(first (last s))) name-triples))))
    (loop for i from 0 below count
       collect (extend-name (any starts) parts ends))))

;;; (defparameter $rulefile "/Users/mikel/Workshop/src/clnamer/us.names")
;;; (generate-names 20 $rulefile)
