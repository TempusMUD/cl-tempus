(in-package :cxml-xmls)

(defun map-node/qnames (handler node include-xmlns-attributes)
  (sax:start-document handler)
  (labels ((walk (node)
             (when (node-ns node)
               (error "serializing without :INCLUDE-NAMESPACE-URI, but node ~
                       was created with a namespace URI"))
             (let* ((attlist
                     (compute-attributes/qnames node include-xmlns-attributes))
                    (qname (string-rod (node-name node)))
                    (lname (nth-value 1 (cxml::split-qname qname))))
               (sax:start-element handler nil lname qname attlist)
               (dolist (child (node-children node))
                 (typecase child
                   (list (walk child))
                   ((or string rod)
                    (sax:characters handler (string-rod child)))
                   (t
                    (sax:characters handler (string-rod (princ-to-string child))))))
               (sax:end-element handler nil lname qname))))
    (walk node))
  (sax:end-document handler))

(defun compute-attributes/lnames (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
                      (unless (listp name)
                        (setf name (cons name nil)))
                      (destructuring-bind (lname &rest uri) name
                        (cond
                          ((not (equal uri "http://www.w3.org/2000/xmlns/"))
                           (sax:make-attribute
                            ;; let the normalizer fix the qname
                            :qname (if uri
                                       (string-rod (concatenate 'string
                                                                "dummy:"
                                                                lname))
                                       (string-rod lname))
                            :local-name (string-rod lname)
                            :namespace-uri uri
                            :value (string-rod (princ-to-string value))
                            :specified-p t))
                          (xmlnsp
                           (sax:make-attribute
                            :qname (string-rod
                                    (if lname
                                        (concatenate 'string "xmlns:" lname)
                                        "xmlns"))
                            :local-name (string-rod lname)
                            :namespace-uri uri
                            :value (string-rod (princ-to-string value))
                            :specified-p t))))))
                  (node-attrs node))))

(defun compute-attributes/qnames (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
                      (when (listp name)
                        (error "serializing without :INCLUDE-NAMESPACE-URI, ~
                                but attribute was created with a namespace ~
                                URI"))
                      (if (or xmlnsp
                              (not (cxml::xmlns-attr-p (string-rod name))))
                          (sax:make-attribute :qname (string-rod name)
                                              :value (string-rod (princ-to-string value))
                                              :specified-p t)
                          nil)))
                  (node-attrs node))))