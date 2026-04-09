;ex6:
(defun make-ar-seq-list (a1 d n)
  (if (zerop n) nil
        (cons a1 (make-ar-seq-list (+ a1 d) d (1- n)))))
(defun make-ar-seq-list-tail (a1 d n)
  (labels ((iter (a1 d n acc)
             (if (zerop n) (reverse acc)
                   (iter (+ a1 d) d (1- n) (cons a1 acc)))))
    (iter a1 d n nil)))
(defun make-geom-seq-list (a1 q n)
  (if (zerop n) nil
    (cons a1 (make-geom-seq-list (* a1 q) q (1- n)))))
(defun make-geom-seq-list-tail (a1 q n)
  (labels ((iter (a1 q n acc)
             (if (zerop n) (reverse acc)
               (iter (* q a1) q (1- n) (cons a1 acc)))))
    (iter a1 q n nil)))
(defun cpy-list (list)
  (if (null list) nil
    (cons (car list)
          (cpy-list (cdr list)))))
(defun remove (el list)
  (cond ((null list) nil)
        ((eql (car list) el) (remove el (cdr list)))
        (t (cons (car list)
                 (remove el (cdr list))))))
(defun tailp (list1 list2)
  (cond ((null list1) t)
        ((eql list1 list2) t)
        ((null list2) nil)
        (t (tailp list1 (cdr list2)))))
(defun ldiff-2 (list1 list2)
  (labels ((iter (l1 l2)
             (cond ((or (null l2)(null l1)) nil)
                   ((eql (car l1) (car l2)) nil)
              (t (cons (car l1)
                     (iter (cdr l1) (cdr l2)))))))
    (if (tailp list2 list1)
        (iter list1 list2)
      (cpy-list list1))))
(defun factorials-list (len)
  (labels ((iter (ir acc len)
             (cond ((= ir len) nil)
                   ((= ir 0) (cons 1 (iter  (1+ ir) acc len)))
                   (t (cons (* ir acc) (iter (1+ ir) (* acc ir) len))))))
    (iter 0 1 len)))
(defun fib-to-list2 (len)
  (labels ((iter (ir prev curr len)
             (cond ((= ir len) nil)
                   ((= ir 0) (cons 0 (iter (1+ ir) prev curr len)))
                   ((= ir 1) (cons 1 (iter (1+ ir) prev curr len)))
                   (t (cons (+ prev curr) (iter (1+ ir) curr (+ curr prev) len))))))
    (iter 0 0 1 len))) 
(defun lst-tails (list)
  (cond ((null list) (list nil))
        (t(cons list
              (lst-tails (cdr list))))))
#|

(defun my-sum-help (list n len)
  (if (>= n len)
      0
    (+ (nth n list) (my-sum-help list (+ n 1) len)))))
(defun my-sum (list)
(my-sum-help list 0 (length list))))


Kolikrát se během vyhodnocování výrazu 
(my-sum (list 1 2 3 4 5 6 7 8 9 10)) zavolá funkce cdr?

nth vola cdr:pro kazde n nkrat
lenght vola cdr :9

9+9+8+7+6+5+4+3+2+1=54

|#
                         
          
(defun subtract-lst-2 (l1 l2)
  (if (null (or l2 l1)) nil
        (cons (- (car l1) (car l2))
              (substract-lst-2 (cdr l1) (cdr l2)))))

(defun sclr-product (l1 l2)
  (apply '+ (mapcar #'* l1 l2)))

(defun vctr-len (vector)
  (apply #'sqrt (mapcar #'power2 vector)))

(defun union-2 (set1 set2)
  (cond ((null set1) set2)
        ((elementp (car set1) set2) (union-2 (cdr set1) set2))
        (t (cons (car set1)
                 (union-2 (cdr set1) set2)))))
(defun rem-dupl (list)
  (cond ((null list) nil)
        ((elementp (car list) (cdr list))
         (rem-dupl (cdr list)))
        (t (cons (car list)
                 (rem-dupl (cdr list))))))
(defun eql-sets-p (set1 set2)
  (and (= (length set1) (length set2))
              (subsetp set1 set2)))


;8:
(defun member (el list)
  (cond ((null list) nil)
        ((eql (car list) el) list)
        (t (member el (cdr list)))))

(defun my-member-t (el list test)
  (cond ((null list) nil)
        ((funcall test el (car list)) list)
        (t (my-member-t el (cdr list) test))))
(defun my-member-if (test list)
  (cond ((null list) nil)
        ((funcall test (car list)) list)
        (t (my-member-if test (cdr list)))))
(defun countif (list test)
  (cond ((null list) 0)
        ((funcall test (car list)) (1+ (count-if (cdr list) test)))
        (t (count-if (crd list) test))))
(defun countt (el list test)
  (cond ((null list) 0)
        ((funcall test el (car list)) (1+ (countt el (cdr list) test)))
        (t (countt el (crd list) test))))


(defun merge-sort-algo (list pred)
  (if (or (null list) (null (cdr list))) list
        (merge-procedure pred 
                         (merge-sort-algo (even-conses list) pred)
                         (merge-sort-algo (odd-conses list) pred))))
(defun merge-procedure (pred list1 list2 )
  (cond ((null list1) list2)
        ((null list2) list1)
        ((funcall pred (car list1) (car list2))
         (cons (car list1) (merge-procedure pred (cdr list1) list2)))
        (t (cons (car list2)
                 (merge-procedure pred list1 (cdr list2))))))
  
(defun arit-mean (n &rest numbers)
        (/ (foldr #'+ numbers n) (1+ (length numbers))))

(defun eql-lists-p-rest (list1 &rest lists)
  (every lists (lambda (sublist) (eql-lst-p list1 sublist))))
(defun lenght-2 (list)
  (foldr (lambda (el acc) (1+ acc)) list 0))
(defun my-mapcar-1 (fun list)
  (if (null list) nil
  (foldr (lambda (el acc) (cons (funcall fun el) acc)) list nil)))
  
(defun my-foldl (fun list init)
  (labels ((iter (acc list)
             (if (null list) acc
               (iter (funcall fun acc (car list)) (cdr list)))))
    (iter init list)))


            
;(1 2 3)
;(funcall f 1(funcall f 2(funcall f 3 init)))-foldr
;(f (f (f init 1) 2) 3)-foldl


(defun my-foldr (fun list init)
  (if (null list) init
    (funcall fun (car list) (my-foldr fun (cdr list) init))))


;ex9:
;(funcall (lambda (x) (funcall x 10)) (lambda (x) 20))



; x v prni lambda je = (lambda (x) 20)

;(funcall (lambda (x) 20) 10)
;20


;(let ((x a)) tělo )
;(funcall (lambda (x) (telo)) a)

;rekurze bez jmena fce:
;fce bere sama sebe jako arg

(let ((f (lambda (fun n)
           (if (= n 0) 1
             (* n (funcall fun fun (1- n)))))))
  (funcall f f 3))
(defun my-const-seq-p (k seq)
  (labels ((iter (ptr k seq)
             (cond ((= (1- k) ptr) t)
                   ((eql (mem seq ptr) (mem seq (1+ ptr))) 
                    (iter (1+ ptr) k seq))
                   (t nil))))
    (iter 0 k seq)))
(defun my-increasing-seq-p (seq k)
  (labels ((iter (i seq k)
             (cond ((= (1- k) i) t)
                   ((> (mem seq i) (mem seq (1+ i))) (iter (1+ i) seq k))
                   (t nil))))
    (iter 0 seq k)))
(defun my-from-nth (seq n)
  (lambda (k)
    (mem seq (+ n k))))

(defun my-even-members (seq)
  (lambda (n) (mem seq (* 2 n))))
(defun my-zero-row-p (tbl row)
  (labels ((iter (col)
             (cond ((> col 9) t)
                   ((zerop (funcall tbl row col)) (iter (1+ col)))
                   (t nil))))
    (iter 0)))
(defun my-transpose-table (row col)
  (lambda (row column) (tbl col row)))

;10:
(defun flatten (struct)
  (cond ((null struct) nil)
        ((consp struct) 
         (append (flatten (car struct))
               (flatten (cdr struct))))
        (t (list struct))))


(defun deep-rev (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (deep-rev-list struct))))


(defun deep-rev-list(subtrees)
  (if (null subtrees) nil
    (append (deep-rev-list (cdr subtrees))
          (list (deep-rev (car subtrees))))))

#|4. Funkce count je napsána takto:
(defun count (x)
  (cond ((consp x) (+ 1 (count (car x)) (count (cdr x))))
        ((null x) 100)
        (t 0)))
Jakou hodnotu má výraz (count'(1 (2 3) 4 (5 . 6)))?
 |#
 
#|       
(count'(1 (2 3) 4 (5 . 6)))
1+((2 3) 4 (5. 6))
1+(2 3) + (4 (5.6))
1+ 0 +1+ 100+ 1+0+1+1+100
207
|#

(defun cpy-str (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (cons (cpy-str (car struct))
                 (cpy-str (cdr struct))))))
(defun deep-reverse-tr (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (deep-reverse-tr-hlp struct))))
(defun deep-reverse-tr-hlp (substruct)
  (if (null substruct) nil
    (append (deep-reverse-tr-hlp (cdr substruct))
            (list (deep-reverse-tr (car substruct))))))

;(a (c (b) (d)) (f (g (h)) (i)))

#|
(tree-node 'a
           (list (tree-node 'c
                      (list (tree-node 'b nil)
                            (tree-node 'd nil)))
                      (tree-node 'f
                                (list  (tree-node 'g 
                                            (tree-node 'h nil))
                                 (tree-node 'i
                                            nil)))))
|#


(defun tree-maximal-paths (node)
  (if (null (node-children node)) (list (list (node-value node)))
    (mapcar 
     (lambda (path)
       (cons (node-value node) path))
       (foldr #'append 
              (mapcar #'tree-maximal-paths (node-children node))
              ;kazdy podstrom cesta
nil))))
(defun tree-height-4 (node)
  (if (null (node-children node)) 0
    (1+ (tree-height-4-hlp (cdr (node-children node))))))
(defun tree-height-4-hlp (subtrees)
  (if (null subtrees) 0
    (max (tree-height-4 (car subtrees))
         (tree-height-4-hlp (cdr subtrees)))))
